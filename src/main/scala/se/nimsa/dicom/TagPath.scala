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

package se.nimsa.dicom

import scala.annotation.tailrec

sealed trait TagPath {

  import TagPath._

  def tag: Int
  def previous: TagPathTrunk

  /**
    * `true` if this tag path points to the root dataset, at depth 0
    */
  lazy val isRoot: Boolean = previous.isEmpty
  lazy val isEmpty: Boolean = this eq EmptyTagPath

  def toList: List[TagPath] = {
    @tailrec
    def toList(path: TagPath, tail: List[TagPath]): List[TagPath] =
      if (path.isRoot) path :: tail else toList(path.previous, path :: tail)
    if (isEmpty) Nil else toList(path = this, tail = Nil)
  }

  /**
    * Test if this tag path is less than the input path, comparing their parts pairwise according to the following rules
    * (1) a is less than b if the a's tag number is less b's tag number
    * (2) a is less than b if tag numbers are equal and a's item index is less than b's item index
    * in all other cases a is not less than b
    *
    * @param that the tag path to compare with
    * @return `true` if this tag path is less than the input path
    */
  def <(that: TagPath): Boolean = {
    val thisList = this.toList
    val thatList = that.toList

    thisList.zip(thatList).foreach {
      case (EmptyTagPath, thatPath) => !thatPath.isEmpty
      case (_, EmptyTagPath) => false
      case (thisPath, thatPath) if thisPath.tag != thatPath.tag =>
        return intToUnsignedLong(thisPath.tag) < intToUnsignedLong(thatPath.tag)
      case (thisPath: TagPathSequenceItem, thatPath: TagPathSequenceItem) if thisPath.item != thatPath.item =>
        return thisPath.item < thatPath.item
      case _ => // tags and item numbers are equal, check next
    }
    thisList.length < thatList.length
  }

  /**
    * @param that tag path to test
    * @return `true` if the input tag path is equal to this tag path. TagPath nodes are compared pairwise from the end
    *         towards the start of the paths. Node types, tag numbers as well as item indices where applicable must be
    *         equal. Paths of different lengths cannot be equal.
    * @example (0010,0010) == (0010,0010)
    * @example (0010,0010) != (0010,0020)
    * @example (0010,0010) != (0008,9215)[1].(0010,0010)
    * @example (0008,9215)[*].(0010,0010) != (0008,9215)[1].(0010,0010)
    * @example (0008,9215)[3].(0010,0010) == (0008,9215)[3].(0010,0010)
    */
  override def equals(that: Any): Boolean = (this, that) match {
    case (p1: TagPath, p2: TagPath) if p1.isEmpty && p2.isEmpty => true
    case (p1: TagPathTag, p2: TagPathTag) => p1.tag == p2.tag && p1.previous == p2.previous
    case (p1: TagPathSequenceItem, p2: TagPathSequenceItem) => p1.tag == p2.tag && p1.item == p2.item && p1.previous == p2.previous
    case (p1: TagPathSequenceAny, p2: TagPathSequenceAny) => p1.tag == p2.tag && p1.previous == p2.previous
    case _ => false
  }

  /**
    * @param tag tag number
    * @return `true` if this tag path contains the input tag number
    * @example (0008,9215)[*].(0010,0010) contains 0x00089215
    * @example (0008,9215)[*].(0010,0010) contains 0x00100010
    * @example (0008,9215)[*].(0010,0010) does not contain 0x00100020
    */
  def contains(tag: Int): Boolean = toList.map(_.tag).contains(tag)

  /**
    * A super-path of another path is a path of equal depth with the same sequence of tag numbers. Differences are in the
    * specification of items. A path with a less restrictive specification of items (wildcard/all
    * items instead of item index) is said to be a super-path of the more general path.
    *
    * The input path is a super-path of this path if and only if this path is a sub-path of the input path.
    *
    * @param that tag path to test
    * @return `true` if the input tag path is a super-path of this path
    * @example (0008,9215)[1].(0010,0010) is a super-path of (0008,9215)[1].(0010,0010)
    * @example (0008,9215)[*].(0010,0010) is a super-path of (0008,9215)[1].(0010,0010)
    * @example (0008,9215)[1].(0010,0010) is not a super-path of (0008,9215)[*].(0010,0010) (it is a sub-path)
    */
  def hasSuperPath(that: TagPath): Boolean = (this, that) match {
    case (EmptyTagPath, EmptyTagPath) => true
    case (p1: TagPathTag, p2: TagPathTag) => p1.tag == p2.tag && p1.previous.hasSuperPath(p2.previous)
    case (p1: TagPathSequenceItem, p2: TagPathSequenceItem) => p1.item == p2.item && p1.tag == p2.tag && p1.previous.hasSuperPath(p2.previous)
    case (p1: TagPathSequenceItem, p2: TagPathSequenceAny) => p1.tag == p2.tag && p1.previous.hasSuperPath(p2.previous)
    case (p1: TagPathSequenceAny, p2: TagPathSequenceAny) => p1.tag == p2.tag && p1.previous.hasSuperPath(p2.previous)
    case _ => false
  }

  /**
    * A sub-path of another path is a path of equal length with the same sequence of tag numbers. Differences are in the
    * specification of items. A path with a more restrictive specification of items (item index instead of wildcard/all
    * items) is said to be a sub-path of the more general path.
    *
    * The input path is a sub-path of this path if and only if this path is a super-path of the input path.
    *
    * @param that tag path to test
    * @return `true` if the input tag path is a sub-path of this path
    * @example (0008,9215)[1].(0010,0010) is a sub-path of (0008,9215)[1].(0010,0010)
    * @example (0008,9215)[1].(0010,0010) is a sub-path of (0008,9215)[*].(0010,0010)
    * @example (0008,9215)[*].(0010,0010) is not a sub-path of (0008,9215)[1].(0010,0010) (it is a super-path)
    */
  def hasSubPath(that: TagPath): Boolean = that.hasSuperPath(this)

  private[TagPath] def startsWith(that: TagPath,
                                  f1: (TagPathSequenceItem, TagPathSequenceAny) => Boolean,
                                  f2: (TagPathSequenceAny, TagPathSequenceItem) => Boolean): Boolean = {
    if (this.depth >= that.depth)
      this.toList.zip(that.toList).forall {
        case (thisSeq: TagPathSequenceAny, thatSeq: TagPathSequenceAny) => thisSeq.tag == thatSeq.tag
        case (thisSeq: TagPathSequenceItem, thatSeq: TagPathSequenceAny) => f1(thisSeq, thatSeq)
        case (thisSeq: TagPathSequenceAny, thatSeq: TagPathSequenceItem) => f2(thisSeq, thatSeq)
        case (thisSeq: TagPathSequenceItem, thatSeq: TagPathSequenceItem) => thisSeq.tag == thatSeq.tag && thisSeq.item == thatSeq.item
        case (thisTag: TagPathTag, thatTag: TagPathTag) => thisTag.tag == thatTag.tag
        case _ => false
      }
    else
      false
  }
  /**
    * Tests if the n first nodes of this path is equal (see definition of `equels`) to the input path of depth n
    *
    * @param that tag path to test
    * @return `true` if the input tag path is equal to the the start of this tag path
    */
  def startsWith(that: TagPath): Boolean = startsWith(that, (_, _) => false, (_, _) => false)

  /**
    * Tests if the input path of depth n is a sub-path (see definition of `hasSubPath`) of the n first nodes of this path
    *
    * @param that tag path to test
    * @return `true` if the input tag path is a sub-path of the start of this tag path
    */
  def startsWithSubPath(that: TagPath): Boolean = startsWith(that, (_, _) => false, (s1, s2) => s1.tag == s2.tag)

  /**
    * Tests if the input path of depth n is a super-path (see definition of `hasSuperPath`) of the n first nodes of this path
    *
    * @param that tag path to test
    * @return `true` if the input tag path is a super-path of the start of this tag path
    */
  def startsWithSuperPath(that: TagPath): Boolean = startsWith(that, (s1, s2) => s1.tag == s2.tag, (_, _) => false)

  private[TagPath] def endsWith(that: TagPath,
                                f1: (TagPathSequenceItem, TagPathSequenceAny) => Boolean,
                                f2: (TagPathSequenceAny, TagPathSequenceItem) => Boolean): Boolean =
    ((this, that) match {
      case (EmptyTagPath, EmptyTagPath) => true
      case (thisSeq: TagPathSequenceAny, thatSeq: TagPathSequenceAny) => thisSeq.tag == thatSeq.tag
      case (thisSeq: TagPathSequenceItem, thatSeq: TagPathSequenceAny) => f1(thisSeq, thatSeq)
      case (thisSeq: TagPathSequenceAny, thatSeq: TagPathSequenceItem) => f2(thisSeq, thatSeq)
      case (thisSeq: TagPathSequenceItem, thatSeq: TagPathSequenceItem) => thisSeq.tag == thatSeq.tag && thisSeq.item == thatSeq.item
      case (thisTag: TagPathTag, thatTag: TagPathTag) => thisTag.tag == thatTag.tag
      case _ => false
    }) && ((this.previous, that.previous) match {
      case (EmptyTagPath, EmptyTagPath) => true
      case (_, EmptyTagPath) => true
      case (EmptyTagPath, _) => false
      case (thisPrev, thatPrev) => thisPrev.endsWith(thatPrev, f1, f2)
    })

  /**
    * Tests if the n last nodes of this path is equal (see definition of `equels`) to the input path of depth n
    *
    * @param that tag path to test
    * @return `true` if the input tag path is equal to the end of this tag path
    */
  def endsWith(that: TagPath): Boolean = endsWith(that, (_, _) => false, (_, _) => false)

  /**
    * Tests if the input path of depth n is a sub-path (see definition of `hasSubPath`) of the n last nodes of this path
    *
    * @param that tag path to test
    * @return `true` if the input tag path is a sub-path of the end of this tag path
    */
  def endsWithSubPath(that: TagPath): Boolean = endsWith(that, (_, _) => false, (s1, s2) => s1.tag == s2.tag)

  /**
    * Tests if the input path of depth n is a super-path (see definition of `hasSuperPath`) of the n last nodes of this path
    *
    * @param that tag path to test
    * @return `true` if the input tag path is a super-path of the end of this tag path
    */
  def endsWithSuperPath(that: TagPath): Boolean = endsWith(that, (s1, s2) => s1.tag == s2.tag, (_, _) => false)

  /**
    * Depth of this tag path. A tag path that points to a tag in a sequence in a sequence has depth 3. A tag path that
    * points to a tag in the root dataset has depth 1. Empty paths have depth 0.
    *
    * @return the depth of this tag path, counting from 0
    */
  def depth: Int = {
    @tailrec
    def depth(path: TagPath, d: Int): Int = if (path.isRoot) d else depth(path.previous, d + 1)
    if (isEmpty) 0 else depth(this, 1)
  }

  /**
    * @return the root (left-most) element of this tag path
    */
  def head: TagPath = take(1)

  /**
    * @return the tag path following the root element, that is, all elements to the right of the root element. Returns
    *         empty path if there are no such elements.
    */
  def tail: TagPath = drop(1)

  /**
    * Drop n steps of this path from the left
    * @param n the number of steps to omit, counted from the lef-most root path
    * @return a new TagPath
    */
  def drop(n: Int): TagPath = {
    def drop(path: TagPath, i: Int): TagPath =
      if (i < 0)
        EmptyTagPath
      else if (i == 0)
        path match {
          case EmptyTagPath => EmptyTagPath
          case p: TagPathSequenceAny => TagPath.fromSequence(p.tag)
          case p: TagPathSequenceItem => TagPath.fromSequence(p.tag, p.item)
          case p => TagPath.fromTag(p.tag)
        }
      else
        drop(path.previous, i - 1) match {
          case p: TagPathTrunk => path match {
            case ps: TagPathSequenceAny => p.thenSequence(ps.tag)
            case pi: TagPathSequenceItem => p.thenSequence(pi.tag, pi.item)
            case pt: TagPathTag => p.thenTag(pt.tag)
            case _ => EmptyTagPath
          }
          case _ => EmptyTagPath // cannot happen
        }
    drop(path = this, i = depth - n - 1)
  }

  /**
    * The first n steps of this path, counted from the left
    * @param n the number of steps to take, counted from the left-most root path
    * @return a new TagPath
    */
  def take(n: Int): TagPath = {
    @tailrec
    def take(path: TagPath, i: Int): TagPath =
      if (i <= 0) path else take(path.previous, i - 1)
    take(path = this, i = depth - n)
  }

  override def toString: String = {
    @tailrec
    def toTagPathString(path: TagPath, tail: String): String = {
      val itemIndexSuffix = path match {
        case _: TagPathSequenceAny => "[*]"
        case s: TagPathSequenceItem => s"[${s.item}]"
        case _ => ""
      }
      val head = tagToString(path.tag) + itemIndexSuffix
      val part = head + tail
      if (path.isRoot) part else toTagPathString(path.previous, "." + part)
    }
    if (isEmpty) "<empty path>" else toTagPathString(path = this, tail = "")
  }

  override def hashCode(): Int = this match {
    case EmptyTagPath => 0
    case s: TagPathSequenceItem => 31 * (31 * (31 * previous.hashCode() + tag.hashCode()) * s.item.hashCode())
    case _ => 31 * (31 * previous.hashCode() + tag.hashCode())
  }
}

object TagPath {

  /**
    * A tag path that points to a non-sequence tag
    *
    * @param tag      the tag number
    * @param previous a link to the part of this tag part to the left of this tag
    */
  class TagPathTag private[TagPath](val tag: Int, val previous: TagPathTrunk) extends TagPath

  /**
    * A tag path that points to a sequence
    */
  trait TagPathTrunk extends TagPath {

    /**
      * Path to a specific tag
      *
      * @param tag tag number
      * @return the tag path
      */
    def thenTag(tag: Int) = new TagPathTag(tag, this)

    /**
      * Path to all items in a sequence
      *
      * @param tag tag number
      * @return the tag path
      */
    def thenSequence(tag: Int) = new TagPathSequenceAny(tag, this)

    /**
      * Path to a specific item within a sequence
      *
      * @param tag  tag number
      * @param item item index
      * @return the tag path
      */
    def thenSequence(tag: Int, item: Int) = new TagPathSequenceItem(tag, item, this)
  }

  /**
    * A tag path that points to all items of a sequence
    *
    * @param tag      the sequence tag number
    * @param previous a link to the part of this tag part to the left of this tag
    */
  class TagPathSequenceAny private[TagPath](val tag: Int, val previous: TagPathTrunk) extends TagPathTrunk

  /**
    * A tag path that points to an item in a sequence
    *
    * @param tag      the sequence tag number
    * @param item     defines the item index in the sequence
    * @param previous a link to the part of this tag part to the left of this tag
    */
  class TagPathSequenceItem private[TagPath](val tag: Int, val item: Int, val previous: TagPathTrunk) extends TagPathTrunk

  /**
    * Empty tag path
    */
  object EmptyTagPath extends TagPathTrunk {
    def tag: Int = throw new NoSuchElementException("Empty tag path")
    val previous: TagPathTrunk = EmptyTagPath
  }

  /**
    * Create a path to a specific tag
    *
    * @param tag tag number
    * @return the tag path
    */
  def fromTag(tag: Int): TagPathTag = EmptyTagPath.thenTag(tag)

  /**
    * Create a path to all items in a sequence
    *
    * @param tag tag number
    * @return the tag path
    */
  def fromSequence(tag: Int): TagPathSequenceAny = EmptyTagPath.thenSequence(tag)

  /**
    * Create a path to a specific item within a sequence
    *
    * @param tag  tag number
    * @param item item index
    * @return the tag path
    */
  def fromSequence(tag: Int, item: Int): TagPathSequenceItem = EmptyTagPath.thenSequence(tag, item)

  /**
    * Parse the string representation of a tag path into a tag path object.
    *
    * @param s string to parse
    * @return a tag path
    * @throws IllegalArgumentException for malformed input
    */
  def parse(s: String): TagPath = {
    def isSeq(s: String) = s.length > 11
    def parseTagNumber(s: String) = Integer.parseInt(s.substring(1, 5) + s.substring(6, 10), 16)
    def parseIndex(s: String) = if (s.charAt(12) == '*') None else Some(Integer.parseInt(s.substring(12, s.length - 1)))
    def createTag(s: String) = TagPath.fromTag(parseTagNumber(s))
    def createSeq(s: String) = parseIndex(s)
      .map(index => TagPath.fromSequence(parseTagNumber(s), index))
      .getOrElse(TagPath.fromSequence(parseTagNumber(s)))
    def addSeq(s: String, path: TagPathTrunk) = parseIndex(s)
      .map(index => path.thenSequence(parseTagNumber(s), index))
      .getOrElse(path.thenSequence(parseTagNumber(s)))
    def addTag(s: String, path: TagPathTrunk) = path.thenTag(parseTagNumber(s))

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
