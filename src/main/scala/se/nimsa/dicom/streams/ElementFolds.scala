package se.nimsa.dicom.streams

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Keep, Sink}
import akka.util.ByteString
import se.nimsa.dicom.data.DicomElements.Elements
import se.nimsa.dicom.data.DicomParts._
import se.nimsa.dicom.data._

import scala.concurrent.Future

/**
  * Flow, Sink etc that combine DICOM parts into data element aggregates.
  */
object ElementFolds {

  /**
    * Helper class that keeps track of an element and its tag path in a dataset
    *
    * @param tagPath tag path in dataset
    * @param element the element
    */
  case class TpElement(tagPath: TagPath, element: Element) {
    def ++(bytes: ByteString): TpElement =
      copy(element = element.copy(value = element.value ++ bytes))
  }

  object TpElement {
    def empty(tagPath: TagPath, header: HeaderPart) =
      TpElement(tagPath, Element(header.tag, header.bigEndian, header.vr, header.explicitVR, header.length, ByteString.empty))
    def empty(tagPath: TagPath, sequence: SequencePart) =
      TpElement(tagPath, Element(sequence.tag, sequence.bigEndian, VR.SQ, sequence.explicitVR, sequence.length, ByteString.empty))
    def empty(tagPath: TagPath, fragments: FragmentsPart) =
      TpElement(tagPath, Element(fragments.tag, fragments.bigEndian, fragments.vr, explicitVR = true, fragments.length, ByteString.empty))
  }

  /**
    * @return a `Flow` that aggregates `DicomPart`s into data elements. Each element holds header and complete value
    *         information. Items, and sequence and item delimitations are not output, as their occurrence is implicit
    *         from the change in tag path between two elements.
    */
  def elementsFlow: Flow[DicomPart, TpElement, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[TpElement] with TagPathTracking[TpElement] {
      var currentValue: Option[TpElement] = None
      var currentFragment: Option[TpElement] = None

      override def onPart(part: DicomPart): List[TpElement] = part match {
        case header: HeaderPart =>
          currentValue = Some(TpElement.empty(tagPath, header))
          Nil
        case sequence: SequencePart =>
          TpElement.empty(tagPath, sequence) :: Nil
        case fragments: FragmentsPart =>
          currentFragment = Some(TpElement.empty(tagPath, fragments))
          Nil
        case fragmentsItem: FragmentsItemPart =>
          currentFragment = currentFragment.map(fragment => fragment.copy(
            tagPath = tagPath,
            element = fragment.element.copy(length = fragmentsItem.length, value = ByteString.empty)
          ))
          Nil
        case _: FragmentsDelimitationPart =>
          currentFragment = None
          Nil
        case valueChunk: ValueChunk if currentFragment.isDefined =>
          currentFragment = currentFragment.map(_ ++ valueChunk.bytes)
          if (valueChunk.last) currentFragment.map(_ :: Nil).getOrElse(Nil) else Nil
        case valueChunk: ValueChunk =>
          currentValue = currentValue.map(_ ++ valueChunk.bytes)
          if (valueChunk.last) currentValue.map(_ :: Nil).getOrElse(Nil) else Nil
        case _ => Nil
      }
    })


  /**
    * A `Sink` that combines data elements into an `Elements` structure. If the `SpecificCharacterSet` element occurs,
    * the character sets of the `Elements` structure is updated accordingly.
    */
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
