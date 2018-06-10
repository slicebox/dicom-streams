package se.nimsa.dicom.streams

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Keep, Sink}
import akka.util.ByteString
import se.nimsa.dicom.data.DicomParts._
import se.nimsa.dicom.data.Elements._
import se.nimsa.dicom.data.{Elements, _}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Flow, Sink etc that combine DICOM parts into data element aggregates.
  */
object ElementFolds {

  /**
    * @return a `Flow` that aggregates `DicomPart`s into data elements. Each element holds header and complete value
    *         information.
    */
  def elementsFlow: Flow[DicomPart, Element, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[Element] with GuaranteedDelimitationEvents[Element] with GuaranteedValueEvent[Element] {
      var bytes: ByteString = ByteString.empty
      var currentValue: Option[ValueElement] = None
      var currentFragment: Option[FragmentElement] = None

      override def onPart(part: DicomPart): List[Element] = part match {

        // Begin aggregate values
        case header: HeaderPart =>
          currentValue = Option(ValueElement.empty(header.tag, header.vr, header.bigEndian, header.explicitVR))
          bytes = ByteString.empty
          Nil
        case item: ItemPart if inFragments =>
          currentFragment = Option(FragmentElement.empty(item.index, item.length, item.bigEndian))
          bytes = ByteString.empty
          Nil

        // aggregate, emit if at end
        case valueChunk: ValueChunk =>
          bytes = bytes ++ valueChunk.bytes
          if (valueChunk.last)
            if (inFragments)
              currentFragment.map(_.copy(value = Value(bytes)) :: Nil).getOrElse(Nil)
            else
              currentValue.map(_.copy(value = Value(bytes)) :: Nil).getOrElse(Nil)
          else
            Nil

        // types that directly map to elements
        case sequence: SequencePart =>
          SequenceElement(sequence.tag, sequence.length, sequence.bigEndian, sequence.explicitVR) :: Nil
        case fragments: FragmentsPart =>
          FragmentsElement(fragments.tag, fragments.vr, fragments.bigEndian, fragments.explicitVR) :: Nil
        case item: ItemPart =>
          ItemElement(item.index, item.length, item.bigEndian) :: Nil
        case itemDelimitation: ItemDelimitationPart =>
          ItemDelimitationElement(itemDelimitation.index, itemDelimitation.bigEndian) :: Nil
        case sequenceDelimitation: SequenceDelimitationPart =>
          SequenceDelimitationElement(sequenceDelimitation.bigEndian) :: Nil

        case _ => Nil
      }
    })

  /**
    * Data holder for `elementsSink`
    */
  private case class ElementsSinkData(elementsStack: Seq[Elements] = Seq(Elements.empty()),
                                      sequenceStack: Seq[Sequence] = Seq.empty,
                                      fragments: Option[Fragments] = None) {
    def updated(elements: Elements): ElementsSinkData = copy(elementsStack = elements +: elementsStack.tail)
    def updated(sequence: Sequence): ElementsSinkData = copy(sequenceStack = sequence +: sequenceStack.tail)
    def updated(fragments: Option[Fragments]): ElementsSinkData = copy(fragments = fragments)
    def pushElements(elements: Elements): ElementsSinkData = copy(elementsStack = elements +: elementsStack)
    def pushSequence(sequence: Sequence): ElementsSinkData = copy(sequenceStack = sequence +: sequenceStack)
    def popElements(): ElementsSinkData = copy(elementsStack = elementsStack.tail)
    def popSequence(): ElementsSinkData = copy(sequenceStack = sequenceStack.tail)
    def hasSequence: Boolean = sequenceStack.nonEmpty
    def hasFragments: Boolean = fragments.nonEmpty
  }

  /**
    * A `Sink` that combines data elements into an `Elements` structure. If the `SpecificCharacterSet` element occurs,
    * the character sets of the `Elements` structure is updated accordingly. If the `TimezoneOffsetFromUTC` element
    * occurs, the zone offset is updated accordingly.
    */
  def elementsSink(implicit ec: ExecutionContext): Sink[Element, Future[Elements]] = Flow[Element]
    .toMat(
      Sink.fold[ElementsSinkData, Element](ElementsSinkData()) { case (sinkData, element) =>
        element match {

          case valueElement: ValueElement =>
            val elements = sinkData.elementsStack.head
            sinkData.updated(elements.setElement(valueElement))

          case fragments: FragmentsElement =>
            sinkData.updated(Some(Fragments.empty(fragments)))

          case fragmentElement: FragmentElement =>
            val updatedFragments = sinkData.fragments.map(_ + Fragment.fromElement(fragmentElement))
            sinkData.updated(updatedFragments)

          case _: SequenceDelimitationElement if sinkData.hasFragments =>
            val updatedElements = sinkData.elementsStack.head.setFragments(sinkData.fragments.get)
            sinkData.updated(updatedElements).updated(None)

          case sequenceElement: SequenceElement =>
            sinkData.pushSequence(Sequence.empty(sequenceElement))

          case itemElement: ItemElement if sinkData.hasSequence =>
            val sequence = sinkData.sequenceStack.head + Item.empty(itemElement)
            val elements = sinkData.elementsStack.head
            val newElements = Elements.empty(elements.characterSets, elements.zoneOffset)
            sinkData.pushElements(newElements).updated(sequence)

          case _: ItemDelimitationElement if sinkData.hasSequence =>
            val sequence = sinkData.sequenceStack.head + sinkData.elementsStack.head
            sinkData.popElements().updated(sequence)

          case _: SequenceDelimitationElement if sinkData.hasSequence =>
            val updatedElements = sinkData.elementsStack.head.setSequence(sinkData.sequenceStack.head)
            sinkData.updated(updatedElements).popSequence()

          case _ =>
            sinkData
        }
      }
        .mapMaterializedValue(_.map(_.elementsStack.headOption.getOrElse(Elements.empty())))
    )(Keep.right)
}
