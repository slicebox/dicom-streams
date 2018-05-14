package se.nimsa.dicom.streams

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Keep, Sink}
import akka.util.ByteString
import se.nimsa.dicom._
import DicomParts._

import scala.concurrent.Future

object ElementFolds {

  case class TpElement(tagPath: TagPath, element: Element) {
    def ++(bytes: ByteString): TpElement =
      copy(element = element.copy(value = element.value ++ bytes))
  }
  object TpElement {
    def empty(tagPath: TagPath, header: DicomHeader) =
      TpElement(tagPath, Element(header.tag, header.bigEndian, header.vr, header.explicitVR, header.length, ByteString.empty))
    def empty(tagPath: TagPath, sequence: DicomSequence) =
      TpElement(tagPath, Element(sequence.tag, sequence.bigEndian, VR.SQ, sequence.explicitVR, sequence.length, ByteString.empty))
    def empty(tagPath: TagPath, fragments: DicomFragments) =
      TpElement(tagPath, Element(fragments.tag, fragments.bigEndian, fragments.vr, explicitVR = true, fragments.length, ByteString.empty))
  }

  def elementsFlow: Flow[DicomPart, TpElement, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[TpElement] with TagPathTracking[TpElement] {
      var currentValue: Option[TpElement] = None
      var currentFragment: Option[TpElement] = None

      override def onPart(part: DicomPart): List[TpElement] = part match {
        case header: DicomHeader =>
          currentValue = Some(TpElement.empty(tagPath, header))
          Nil
        case sequence: DicomSequence =>
          TpElement.empty(tagPath, sequence) :: Nil
        case fragments: DicomFragments =>
          currentFragment = Some(TpElement.empty(tagPath, fragments))
          Nil
        case fragmentsItem: DicomFragmentsItem =>
          currentFragment = currentFragment.map(fragment => fragment.copy(
            tagPath = tagPath,
            element = fragment.element.copy(length = fragmentsItem.length, value = ByteString.empty)
          ))
          Nil
        case _: DicomFragmentsDelimitation =>
          currentFragment = None
          Nil
        case valueChunk: DicomValueChunk if currentFragment.isDefined =>
          currentFragment = currentFragment.map(_ ++ valueChunk.bytes)
          if (valueChunk.last) currentFragment.map(_ :: Nil).getOrElse(Nil) else Nil
        case valueChunk: DicomValueChunk =>
          currentValue = currentValue.map(_ ++ valueChunk.bytes)
          if (valueChunk.last) currentValue.map(_ :: Nil).getOrElse(Nil) else Nil
        case _ => Nil
      }
    })


  val elementsSink: Sink[TpElement, Future[Elements]] = Flow[TpElement]
    .fold(Elements.empty) {
      case (elements, tagPathElement) =>
        if (tagPathElement.tagPath == TagPath.fromTag(Tag.SpecificCharacterSet))
          elements
            .updateCharacterSets(CharacterSets(tagPathElement.element.value))
            .update(tagPathElement.tagPath, tagPathElement.element)
        else
          elements
            .update(tagPathElement.tagPath, tagPathElement.element)
    }
    .toMat(Sink.head)(Keep.right)
}
