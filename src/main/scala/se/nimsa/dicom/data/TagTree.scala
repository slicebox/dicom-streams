/*
 * Copyright 2018 Lars Edenbrandt
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package se.nimsa.dicom.data

import se.nimsa.dicom.data.TagPath._
import se.nimsa.dicom.data.TagTree._

import scala.annotation.tailrec

sealed trait TagTree extends TagPathLike {

  override type P = TagTree
  override type T = TagTreeTrunk
  override type E = EmptyTagTree.type

  override protected val empty: E = EmptyTagTree

  def tag: Int

  /**
    * @param that tag path to test
    * @return `true` if the input tag path is equal to this tag path. TagTree nodes are compared pairwise from the end
    *         towards the start of the paths. Node types, tag numbers as well as item indices where applicable must be
    *         equal. Paths of different lengths cannot be equal.
    * @example (0010,0010) == (0010,0010)
    * @example (0010,0010) != (0010,0020)
    * @example (0010,0010) != (0008,9215)[1].(0010,0010)
    * @example (0008,9215)[*].(0010,0010) != (0008,9215)[1].(0010,0010)
    * @example (0008,9215)[3].(0010,0010) == (0008,9215)[3].(0010,0010)
    */
  override def equals(that: Any): Boolean = (this, that) match {
    case (t: TagTree, p: TagTree) if t.isEmpty && p.isEmpty => true
    case (t: TagTreeTag, p: TagTreeTag) => t.tag == p.tag && t.previous == p.previous
    case (t: TagTreeItem, p: TagTreeItem) => t.tag == p.tag && t.item == p.item && t.previous == p.previous
    case (t: TagTreeAnyItem, p: TagTreeAnyItem) => t.tag == p.tag && t.previous == p.previous
    case _ => false
  }

  def isPath: Boolean = this match {
    case EmptyTagTree => true
    case _: TagTreeAnyItem => false
    case t => t.previous.isPath
  }

  def hasBranch(tagPath: TagPath): Boolean = (this, tagPath) match {
    case (EmptyTagTree, EmptyTagPath) => true
    case (t: TagTreeTag, p: TagPathTag) => t.tag == p.tag && t.previous.hasBranch(p.previous)
    case (t: TagTreeItem, p: TagPath with ItemIndex) => t.item == p.item && t.tag == p.tag && t.previous.hasBranch(p.previous)
    case (t: TagTreeItem, p: TagPathSequence) => t.tag == p.tag && t.previous.hasBranch(p.previous)
    case (t: TagTreeItem, p: TagPathSequenceEnd) => t.tag == p.tag && t.previous.hasBranch(p.previous)
    case (t: TagTreeAnyItem, p: TagPath with ItemIndex) => t.tag == p.tag && t.previous.hasBranch(p.previous)
    case (t: TagTreeAnyItem, p: TagPathSequence) => t.tag == p.tag && t.previous.hasBranch(p.previous)
    case (t: TagTreeAnyItem, p: TagPathSequenceEnd) => t.tag == p.tag && t.previous.hasBranch(p.previous)
    case _ => false
  }

  private def matchTrunk(that: TagPath): Boolean =
    this.toList.zip(that.toList).forall {
      case (t: TagTreeItem, p: TagPath with ItemIndex) => t.tag == p.tag && t.item == p.item
      case (t: TagTreeItem, p: TagPathSequence) => t.tag == p.tag
      case (t: TagTreeItem, p: TagPathSequenceEnd) => t.tag == p.tag
      case (t: TagTreeAnyItem, p: TagPath with ItemIndex) => t.tag == p.tag
      case (t: TagTreeAnyItem, p: TagPathSequence) => t.tag == p.tag
      case (t: TagTreeAnyItem, p: TagPathSequenceEnd) => t.tag == p.tag
      case (t: TagTreeTag, p: TagPathTag) => t.tag == p.tag
      case _ => false
    }

  def hasTrunk(tagPath: TagPath): Boolean = if (this.depth >= tagPath.depth) matchTrunk(tagPath) else false

  def isTrunkOf(tagPath: TagPath): Boolean = if (this.depth <= tagPath.depth) matchTrunk(tagPath) else false

  def hasTwig(tagPath: TagPath): Boolean =
    ((this, tagPath) match {
      case (EmptyTagTree, EmptyTagPath) => true
      case (t: TagTreeAnyItem, p: TagPath with ItemIndex) => t.tag == p.tag
      case (t: TagTreeAnyItem, p: TagPathSequence) => t.tag == p.tag
      case (t: TagTreeAnyItem, p: TagPathSequenceEnd) => t.tag == p.tag
      case (t: TagTreeItem, p: TagPath with ItemIndex) => t.tag == p.tag && t.item == p.item
      case (t: TagTreeItem, p: TagPathSequence) => t.tag == p.tag
      case (t: TagTreeItem, p: TagPathSequenceEnd) => t.tag == p.tag
      case (t: TagPathTag, p: TagPathTag) => t.tag == p.tag
      case _ => false
    }) && ((this.previous, tagPath.previous) match {
      case (_, EmptyTagPath) => true
      case (EmptyTagTree, _) => false
      case (thisPrev, thatPrev) => thisPrev.hasTwig(thatPrev)
    })

  /**
    * Drop n steps of this path from the left
    *
    * @param n the number of steps to omit, counted from the lef-most root path
    * @return a new TagPath
    */
  def drop(n: Int): TagTree = {
    def drop(path: TagTree, i: Int): TagTree =
      if (i < 0)
        EmptyTagTree
      else if (i == 0)
        path match {
          case EmptyTagTree => EmptyTagTree
          case p: TagTreeItem => TagTree.fromItem(p.tag, p.item)
          case p: TagTreeAnyItem => TagTree.fromAnyItem(p.tag)
          case p => TagTree.fromTag(p.tag)
        }
      else
        drop(path.previous, i - 1) match {
          case p: TagTreeTrunk => path match {
            case pi: TagTreeItem => p.thenItem(pi.tag, pi.item)
            case pi: TagTreeAnyItem => p.thenAnyItem(pi.tag)
            case pt: TagTreeTag => p.thenTag(pt.tag)
            case _ => EmptyTagTree
          }
          case _ => EmptyTagTree // cannot happen
        }

    drop(path = this, i = depth - n - 1)
  }

  def toString(lookup: Boolean): String = {
    def toTagString(tag: Int): String = if (lookup) {
      val keyword = Dictionary.keywordOf(tag)
      if (keyword.isEmpty) tagToString(tag) else keyword
    }
    else
      tagToString(tag)

    @tailrec
    def toTagTreeString(path: TagTree, tail: String): String = {
      val itemIndexSuffix = path match {
        case _: TagTreeAnyItem => "[*]"
        case s: TagTreeItem => s"[${
          s.item
        }]"
        case _ => ""
      }
      val head = toTagString(path.tag) + itemIndexSuffix
      val part = head + tail
      if (path.isRoot) part else toTagTreeString(path.previous, "." + part)
    }

    if (isEmpty) "<empty path>" else toTagTreeString(path = this, tail = "")
  }

  override def hashCode(): Int = this match {
    case EmptyTagTree => 0
    case s: TagTreeItem => 31 * (31 * (31 * previous.hashCode() + tag.hashCode()) * s.item.hashCode())
    case _ => 31 * (31 * previous.hashCode() + tag.hashCode())
  }
}

object TagTree {

  /**
    * A tag path that points to a non-sequence tag
    *
    * @param tag      the tag number
    * @param previous a link to the part of this tag part to the left of this tag
    */
  class TagTreeTag private[TagTree](val tag: Int, val previous: TagTreeTrunk) extends TagTree

  /**
    * A tag path that points to a sequence (all items or specific item), or the empty tag path
    */
  trait TagTreeTrunk extends TagTree {

    /**
      * Path to a specific tag
      *
      * @param tag tag number
      * @return the tag path
      */
    def thenTag(tag: Int) = new TagTreeTag(tag, this)

    /**
      * Path to all items in a sequence
      *
      * @param tag tag number
      * @return the tag path
      */
    def thenAnyItem(tag: Int) = new TagTreeAnyItem(tag, this)

    /**
      * Path to a specific item within a sequence
      *
      * @param tag  tag number
      * @param item item index
      * @return the tag path
      */
    def thenItem(tag: Int, item: Int) = new TagTreeItem(tag, item, this)
  }

  /**
    * A tag path that points to all items of a sequence
    *
    * @param tag      the sequence tag number
    * @param previous a link to the part of this tag part to the left of this tag
    */
  class TagTreeAnyItem private[TagTree](val tag: Int, val previous: TagTreeTrunk) extends TagTreeTrunk

  /**
    * A tag path that points to an item in a sequence
    *
    * @param tag      the sequence tag number
    * @param item     defines the item index in the sequence
    * @param previous a link to the part of this tag part to the left of this tag
    */
  class TagTreeItem private[TagTree](val tag: Int, val item: Int, val previous: TagTreeTrunk) extends TagTreeTrunk

  /**
    * Empty tag path
    */
  object EmptyTagTree extends TagTreeTrunk {
    def tag: Int = throw new NoSuchElementException("Empty tag path")
    val previous: TagTreeTrunk = EmptyTagTree
  }

  /**
    * Create a path to a specific tag
    *
    * @param tag tag number
    * @return the tag path
    */
  def fromTag(tag: Int): TagTreeTag = EmptyTagTree.thenTag(tag)

  /**
    * Create a path to all items in a sequence
    *
    * @param tag tag number
    * @return the tag path
    */
  def fromAnyItem(tag: Int): TagTreeAnyItem = EmptyTagTree.thenAnyItem(tag)

  /**
    * Create a path to a specific item within a sequence
    *
    * @param tag  tag number
    * @param item item index
    * @return the tag path
    */
  def fromItem(tag: Int, item: Int): TagTreeItem = EmptyTagTree.thenItem(tag, item)

  /**
    * Parse the string representation of a tag path into a tag path object. Tag paths can either be specified using tag
    * numbers or their corresponding keywords.
    *
    * Examples: (0008,9215)[1].(0010,0010) = DerivationCodeSequence[1].(0010,0010) = (0008,9215)[1].PatientName =
    * DerivationCodeSequence[1].PatientName
    *
    * @param s string to parse
    * @return a tag path
    * @throws IllegalArgumentException for malformed input
    */
  def parse(s: String): TagTree = {
    def isSeq(s: String): Boolean = s.last == ']'

    def indexPart(s: String): String = s.substring(s.lastIndexOf('[') + 1, s.length - 1)

    def tagPart(s: String): String = s.substring(0, s.indexOf('['))

    def parseTag(s: String): Int = try Integer.parseInt(s.substring(1, 5) + s.substring(6, 10), 16) catch {
      case _: Throwable =>
        Dictionary.tagOf(s)
    }

    def parseIndex(s: String): Option[Int] = if (s == "*") None else Some(Integer.parseInt(s))

    def createTag(s: String): TagTreeTag = TagTree.fromTag(parseTag(s))

    def addTag(s: String, path: TagTreeTrunk): TagTreeTag = path.thenTag(parseTag(s))

    def createSeq(s: String): TagTreeTrunk = parseIndex(indexPart(s))
      .map(index => TagTree.fromItem(parseTag(tagPart(s)), index))
      .getOrElse(TagTree.fromAnyItem(parseTag(tagPart(s))))

    def addSeq(s: String, path: TagTreeTrunk): TagTreeTrunk = parseIndex(indexPart(s))
      .map(index => path.thenItem(parseTag(tagPart(s)), index))
      .getOrElse(path.thenAnyItem(parseTag(tagPart(s))))


    val tags = if (s.indexOf('.') > 0) s.split("\\.").toList else List(s)
    val seqTags = if (tags.length > 1) tags.init else Nil // list of sequence tags, if any
    val lastTag = tags.last // tag or sequence
    try {
      seqTags.headOption.map(first => seqTags.tail.foldLeft(createSeq(first))((path, tag) => addSeq(tag, path))) match {
        case Some(path) => if (isSeq(lastTag)) addSeq(lastTag, path) else addTag(lastTag, path)
        case None => if (isSeq(lastTag)) createSeq(lastTag) else createTag(lastTag)
      }
    } catch {
      case e: Exception => throw new IllegalArgumentException("Tag path could not be parsed", e)
    }
  }

}
