package se.nimsa.dicom.streams

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Keep, Sink}
import akka.util.ByteString
import se.nimsa.dicom.TagPath._
import se.nimsa.dicom.VR.VR
import se.nimsa.dicom.Value.ByteStringExtension
import se.nimsa.dicom._
import se.nimsa.dicom.streams.DicomParts._

import scala.concurrent.Future

object AttributeFolds {

  trait Attribute {
    val tagPath: TagPath
    val bigEndian: Boolean
  }

  case class ValueAttribute(tagPath: TagPath, bigEndian: Boolean, vr: VR, value: Value) extends Attribute
  case class SequenceAttribute(tagPath: TagPath, bigEndian: Boolean) extends Attribute { val vr: VR = VR.SQ }
  case class SequenceDelimitationAttribute(tagPath: TagPath, bigEndian: Boolean) extends Attribute
  case class ItemAttribute(tagPath: TagPath, bigEndian: Boolean) extends Attribute
  case class ItemDelimitationAttribute(tagPath: TagPath, bigEndian: Boolean) extends Attribute
  case class FragmentsAttribute(tagPath: TagPath, bigEndian: Boolean, vr: VR) extends Attribute
  case class FragmentAttribute(tagPath: TagPath, bigEndian: Boolean, value: ByteString) extends Attribute

  case class Attributes(characterSets: CharacterSets, attributes: List[Attribute]) {
    def apply(tagPath: TagPath): Option[Attribute] = attributes.find(_.tagPath == tagPath)
    def apply(tag: Int): Option[Attribute] = apply(TagPath.fromTag(tag))
    def sequence(tagPath: TagPathTrunk): Attributes =
      Attributes(characterSets, attributes.filter(_.tagPath.startsWithSuperPath(tagPath)))
  }

  def attributesFlow: Flow[DicomPart, Attribute, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[Attribute] with TagPathTracking[Attribute] {
      var currentValue: Option[ValueAttribute] = None
      var currentFragment: Option[FragmentAttribute] = None

      override def onPart(part: DicomPart): List[Attribute] = part match {
        case header: DicomHeader =>
          currentValue = tagPath.map(tp => ValueAttribute(tp, header.bigEndian, header.vr, Value.empty))
          Nil
        case fragmentsItem: DicomFragmentsItem =>
          currentFragment = tagPath.map(tp => FragmentAttribute(tp, fragmentsItem.bigEndian, ByteString.empty))
          Nil
        case valueChunk: DicomValueChunk if currentFragment.isDefined =>
          currentFragment = currentFragment.map(f => f.copy(value = f.value ++ valueChunk.bytes))
          if (valueChunk.last) {
            val result = currentFragment.map(_ :: Nil).getOrElse(Nil)
            currentFragment = None
            result
          } else Nil
        case valueChunk: DicomValueChunk =>
          currentValue = currentValue.map(d => d.copy(value = d.value ++ valueChunk.bytes.toValue))
          if (valueChunk.last) currentValue.map(_ :: Nil).getOrElse(Nil) else Nil
        case _ => Nil
      }
    })


  def attributesSink: Sink[Attribute, Future[Attributes]] = Flow[Attribute]
    .fold(Attributes(CharacterSets.defaultOnly, List.empty)) { (attributes, attribute) =>
      attribute match {
        case v: ValueAttribute if v.tagPath == TagPath.fromTag(Tag.SpecificCharacterSet) =>
          Attributes(CharacterSets(v.value), attributes.attributes :+ v)
        case a: Attribute =>
          attributes.copy(attributes = attributes.attributes :+ a)
      }
    }
    .toMat(Sink.head)(Keep.right)
}
