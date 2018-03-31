package se.nimsa.dicom.streams

import akka.NotUsed
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import se.nimsa.dicom.streams.DicomParts._
import se.nimsa.dicom.{TagPath, padToEvenLength}

object CollectFlow {

  case class DicomFragment(index: Int, bigEndian: Boolean, valueChunks: Seq[DicomValueChunk]) {
    def bytes: ByteString = valueChunks.map(_.bytes).fold(ByteString.empty)(_ ++ _)
  }

  case class DicomAttribute(header: DicomHeader, valueChunks: Seq[DicomValueChunk]) {
    val tag: Int = header.tag
    val length: Long = header.length
    def valueBytes: ByteString = valueChunks.map(_.bytes).fold(ByteString.empty)(_ ++ _)
    def bytes: ByteString = header.bytes ++ valueBytes
    def bigEndian: Boolean = header.bigEndian

    def asDicomParts: Seq[DicomPart] = header +: valueChunks

    def withUpdatedValue(newValue: ByteString): DicomAttribute = {
      val paddedValue = padToEvenLength(newValue, header.vr)
      val updatedHeader = header.withUpdatedLength(paddedValue.length)
      DicomAttribute(updatedHeader, Seq(DicomValueChunk(header.bigEndian, paddedValue, last = true)))
    }

    override def toString = s"${getClass.getSimpleName} header=$header, value chunks=${valueBytes.toList}"
  }

  case class DicomAttributes(tag: String, attributes: Seq[DicomAttribute]) extends DicomPart {
    def bigEndian: Boolean = attributes.headOption.exists(_.bigEndian)
    def bytes: ByteString = attributes.map(_.bytes).reduce(_ ++ _)

    override def toString = s"${getClass.getSimpleName} tag=$tag attributes=${attributes.toList}"
  }

  /**
    * Collect the attributes specified by the input set of tags while buffering all elements of the stream. When the
    * stream has moved past the last attribute to collect, a DicomAttributes element is emitted containing a list of
    * DicomAttribute-parts with the collected information, followed by all buffered elements. Remaining elements in the
    * stream are immediately emitted downstream without buffering.
    *
    * This flow is used when there is a need to "look ahead" for certain information in the stream so that streamed
    * elements can be processed correctly according to this information. As an example, an implementation may have
    * different graph paths for different modalities and the modality must be known before any elements are processed.
    *
    * @param tags          tag numbers of attributes to collect. Collection (and hence buffering) will end when the
    *                      stream moves past the highest tag number
    * @param attributesTag a tag for the resulting DicomAttributes to separate this from other such elements in the same
    *                      flow
    * @param maxBufferSize the maximum allowed size of the buffer (to avoid running out of memory). The flow will fail
    *                      if this limit is exceed. Set to 0 for an unlimited buffer size
    * @return A DicomPart Flow which will begin with a DicomAttributesPart followed by the input elements
    */
  def collectFlow(tags: Set[Int], attributesTag: String, maxBufferSize: Int = 1000000): Flow[DicomPart, DicomPart, NotUsed] = {
    val maxTag = if (tags.isEmpty) 0 else tags.max
    val tagCondition = (tagPath: TagPath) => tagPath.isRoot && tags.contains(tagPath.tag)
    val stopCondition = if (tags.isEmpty)
      (_: TagPath) => true
    else
      (tagPath: TagPath) => tagPath.isRoot && tagPath.tag > maxTag
    collectFlow(tagCondition, stopCondition, attributesTag, maxBufferSize)
  }

  /**
    * Collect attributes whenever the input tag condition yields `true` while buffering all elements of the stream. When
    * the stop condition yields `true`, a DicomAttributes element is emitted containing a list of
    * DicomAttributeParts with the collected information, followed by all buffered elements. Remaining elements in the
    * stream are immediately emitted downstream without buffering.
    *
    * This flow is used when there is a need to "look ahead" for certain information in the stream so that streamed
    * elements can be processed correctly according to this information. As an example, an implementation may have
    * different graph paths for different modalities and the modality must be known before any elements are processed.
    *
    * @param tagCondition  function determining the condition(s) for which attributes are collected
    * @param stopCondition function determining the condition for when collection should stop and attributes are emitted
    * @param attributesTag a tag for the resulting DicomAttributes to separate this from other such elements in the same
    *                      flow
    * @param maxBufferSize the maximum allowed size of the buffer (to avoid running out of memory). The flow will fail
    *                      if this limit is exceed. Set to 0 for an unlimited buffer size
    * @return A DicomPart Flow which will begin with a DicomAttributesPart followed by the input elements
    */
  def collectFlow(tagCondition: TagPath => Boolean, stopCondition: TagPath => Boolean, attributesTag: String, maxBufferSize: Int): Flow[DicomPart, DicomPart, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with TagPathTracking[DicomPart] with EndEvent[DicomPart] {

      var reachedEnd = false
      var currentBufferSize = 0
      var currentAttribute: Option[DicomAttribute] = None
      var buffer: List[DicomPart] = Nil
      var attributes: List[DicomAttribute] = Nil

      def attributesAndBuffer(): List[DicomPart] = {
        val parts = DicomAttributes(attributesTag, attributes) :: buffer

        reachedEnd = true
        buffer = Nil
        currentBufferSize = 0

        parts
      }

      override def onEnd(): List[DicomPart] =
        if (reachedEnd)
          Nil
        else
          attributesAndBuffer()

      override def onPart(part: DicomPart): List[DicomPart] = {
        if (reachedEnd)
          part :: Nil
        else {
          if (maxBufferSize > 0 && currentBufferSize > maxBufferSize)
            throw new DicomStreamException("Error collecting attributes: max buffer size exceeded")

          buffer = buffer :+ part
          currentBufferSize = currentBufferSize + part.bytes.size

          part match {
            case _: DicomHeader if tagPath.exists(stopCondition) =>
              attributesAndBuffer()

            case header: DicomHeader if tagPath.exists(tagCondition) =>
              currentAttribute = Some(DicomAttribute(header, Seq.empty))
              if (header.length == 0) {
                attributes = attributes :+ currentAttribute.get
                currentAttribute = None
              }
              Nil

            case _: DicomHeader =>
              currentAttribute = None
              Nil

            case valueChunk: DicomValueChunk =>

              currentAttribute match {
                case Some(attribute) =>
                  val updatedAttribute = attribute.copy(valueChunks = attribute.valueChunks :+ valueChunk)
                  currentAttribute = Some(updatedAttribute)
                  if (valueChunk.last) {
                    attributes = attributes :+ updatedAttribute
                    currentAttribute = None
                  }
                  Nil

                case None => Nil
              }

            case _ => Nil
          }
        }
      }
    })


}
