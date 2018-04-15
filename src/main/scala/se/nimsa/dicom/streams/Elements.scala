package se.nimsa.dicom.streams

import akka.util.ByteString
import se.nimsa.dicom.TagPath.TagPathTrunk
import se.nimsa.dicom.VR.VR
import se.nimsa.dicom.streams.Elements._
import se.nimsa.dicom.{CharacterSets, TagPath, Value}

case class Elements(characterSets: CharacterSets, elements: List[Element]) {
  def apply(tagPath: TagPath): Option[Element] = elements.find(_.tagPath == tagPath)
  def apply(tag: Int): Option[Element] = apply(TagPath.fromTag(tag))
  def sequence(tagPath: TagPathTrunk): Elements =
    Elements(characterSets, elements.filter(_.tagPath.startsWithSuperPath(tagPath)))
  def insert(element: Element): Elements =
    Elements(characterSets, (element :: elements).sortWith((a, b) => a.tagPath < b.tagPath))
  def insert(sequence: List[Element]): Elements =
    Elements(characterSets, (sequence ::: elements).sortWith((a, b) => a.tagPath < b.tagPath))
}

object Elements {

  sealed trait Element {
    val tagPath: TagPath
    val length: Long
    val value: Value
    val bigEndian: Boolean
  }

  case class ValueElement(tagPath: TagPath, bigEndian: Boolean, vr: VR, explicitVR: Boolean, length: Long, value: Value) extends Element
  case class SequenceElement(tagPath: TagPath, bigEndian: Boolean, length: Long) extends Element { val value: Value = Value.empty }
  case class SequenceDelimitationElement(tagPath: TagPath, bigEndian: Boolean) extends Element { val length: Long = 0; val value: Value = Value.empty }
  case class ItemElement(tagPath: TagPath, bigEndian: Boolean, index: Int, length: Long) extends Element { val value: Value = Value.empty }
  case class ItemDelimitationElement(tagPath: TagPath, bigEndian: Boolean) extends Element { val length: Long = 0; val value: Value = Value.empty }
  case class FragmentsElement(tagPath: TagPath, bigEndian: Boolean, vr: VR) extends Element { val length: Long = -1; val value: Value = Value.empty }
  case class FragmentElement(tagPath: TagPath, bigEndian: Boolean, index: Int, length: Long, bytes: ByteString) extends Element { val value: Value = Value.empty }

}

