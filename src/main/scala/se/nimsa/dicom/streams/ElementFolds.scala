package se.nimsa.dicom.streams

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Keep, Sink}
import se.nimsa.dicom.Value.ByteStringExtension
import se.nimsa.dicom._
import se.nimsa.dicom.streams.DicomParts._
import se.nimsa.dicom.streams.Elements._

import scala.concurrent.Future

object ElementFolds {

  def elementsFlow: Flow[DicomPart, Element, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[Element] with TagPathTracking[Element] {
      var currentValue: Option[ValueElement] = None
      var currentFragment: Option[FragmentElement] = None

      override def onPart(part: DicomPart): List[Element] = part match {
        case header: DicomHeader =>
          currentValue = tagPath.map(tp => ValueElement(tp, header.bigEndian, header.vr, header.explicitVR, header.length, Value.empty))
          Nil
        case fragmentsItem: DicomFragmentsItem =>
          currentFragment = tagPath.map(tp => FragmentElement(tp, fragmentsItem.bigEndian, fragmentsItem.index, fragmentsItem.length, Value.empty))
          Nil
        case valueChunk: DicomValueChunk if currentFragment.isDefined =>
          currentFragment = currentFragment.map(f => f.copy(value = f.value ++ valueChunk.bytes.toValue))
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


  val elementsSink: Sink[Element, Future[Elements]] = Flow[Element]
    .fold(Elements(CharacterSets.defaultOnly, List.empty)) { (elements, element) =>
      element match {
        case v: ValueElement if v.tagPath == TagPath.fromTag(Tag.SpecificCharacterSet) =>
          Elements(CharacterSets(v.value), elements.elements :+ v)
        case a: Element =>
          elements.copy(elements = elements.elements :+ a)
      }
    }
    .toMat(Sink.head)(Keep.right)
}
