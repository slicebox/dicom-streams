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

package se.nimsa.dicom.streams

import java.util.zip.Deflater

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import se.nimsa.dicom.CharacterSets.utf8Charset
import se.nimsa.dicom.VR.VR
import se.nimsa.dicom._
import se.nimsa.dicom.streams.CollectFlow._
import se.nimsa.dicom.streams.ModifyFlow._
import se.nimsa.dicom.streams.DicomParsing.{Info, dicomInfo}
import se.nimsa.dicom.streams.DicomParts._

/**
  * Various flows for transforming streams of <code>DicomPart</code>s.
  */
object DicomFlows {

  case class ValidationContext(sopClassUID: String, transferSyntax: String)


  /**
    * Print each element and then pass it on unchanged.
    *
    * @return the associated flow
    */
  val printFlow: Flow[DicomPart, DicomPart, NotUsed] = Flow.fromFunction { a =>
    println(a)
    a
  }

  /**
    * Filter a stream of dicom parts such that all elements except those with tags in the white list are discarded.
    * Only elements in the root dataset are considered. All other parts such as preamble are discarded.
    *
    * Note that it is up to the user of this function to make sure the modified DICOM data is valid.
    *
    * @param tagsWhitelist list of tags to keep.
    * @return the associated filter Flow
    */
  def whitelistFilter(tagsWhitelist: Set[Int]): Flow[DicomPart, DicomPart, NotUsed] =
    tagFilter(_ => false)(currentPath => currentPath.isRoot && tagsWhitelist.contains(currentPath.tag))

  /**
    * Filter a stream of dicom parts such that elements with tag paths in the black list are discarded. Tag paths in
    * the blacklist are removed in the root dataset as well as any sequences, and entire sequences or items in sequences
    * can be removed.
    *
    * Note that it is up to the user of this function to make sure the modified DICOM data is valid.
    *
    * @param blacklistPaths list of tag paths to discard.
    * @return the associated filter Flow
    */
  def blacklistFilter(blacklistPaths: Set[TagPath]): Flow[DicomPart, DicomPart, NotUsed] =
    tagFilter(_ => true)(currentPath => !blacklistPaths.exists(currentPath.startsWithSuperPath))

  /**
    * Filter a stream of dicom parts such that all elements that are group length elements except
    * file meta information group length, will be discarded. Group Length (gggg,0000) Standard Data Elements
    * have been retired in the standard.
    *
    * Note that it is up to the user of this function to make sure the modified DICOM data is valid.
    *
    * @return the associated filter Flow
    */
  def groupLengthDiscardFilter: Flow[DicomPart, DicomPart, NotUsed] =
    tagFilter(_ => true)(tagPath => !DicomParsing.isGroupLength(tagPath.tag) || DicomParsing.isFileMetaInformation(tagPath.tag))

  /**
    * Discards the file meta information.
    *
    * @return the associated filter Flow
    */
  def fmiDiscardFilter: Flow[DicomPart, DicomPart, NotUsed] =
    tagFilter(_ => false)(tagPath => !DicomParsing.isFileMetaInformation(tagPath.tag))

  /**
    * Filter a stream of dicom parts leaving only those for which the supplied tag condition is `true`. As the stream of
    * dicom parts is flowing, a `TagPath` state is updated. For each such update, the tag condition is evaluated. If it
    * renders `false`, parts are discarded until it renders `true` again.
    *
    * Note that it is up to the user of this function to make sure the modified DICOM data is valid. When filtering
    * items from a sequence, item indices are preserved (i.e. not updated).
    *
    * @param tagCondition     function that determines if dicom parts should be discarded based on the current tag path
    * @param defaultCondition determines whether to keep or discard elements with no tag path such as the preamble and
    *                         synthetic dicom parts inserted to hold state.
    * @return the filtered flow
    */
  def tagFilter(defaultCondition: DicomPart => Boolean)(tagCondition: TagPath => Boolean): Flow[DicomPart, DicomPart, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with TagPathTracking[DicomPart] {

      var keeping = false

      def update(part: DicomPart): Unit =
        keeping = tagPath match {
          case Some(path) => tagCondition(path)
          case None => defaultCondition(part)
        }

      def emit(part: DicomPart): List[DicomPart] = if (keeping) part :: Nil else Nil

      def updateThenEmit(part: DicomPart): List[DicomPart] = {
        update(part)
        emit(part)
      }

      override def onPart(part: DicomPart): List[DicomPart] = updateThenEmit(part)
    })

  /**
    * A flow which passes on the input bytes unchanged, but fails for non-DICOM files, determined by the first
    * element found
    */
  val validateFlow: Flow[ByteString, ByteString, NotUsed] =
    Flow[ByteString].via(new ValidateFlow(None, drainIncoming = false))

  /**
    * A flow which passes on the input bytes unchanged, fails for non-DICOM files, validates for DICOM files with supported
    * Media Storage SOP Class UID, Transfer Syntax UID combination passed as context
    */
  def validateFlowWithContext(contexts: Seq[ValidationContext], drainIncoming: Boolean): Flow[ByteString, ByteString, NotUsed] =
    Flow[ByteString].via(new ValidateFlow(Some(contexts), drainIncoming))

  /**
    * A flow which deflates the dataset but leaves the meta information intact. Useful when the dicom parsing in `DicomParseFlow`
    * has inflated a deflated (`1.2.840.10008.1.2.1.99 or 1.2.840.10008.1.2.4.95`) file, and analyzed and possibly transformed its
    * elements. At that stage, in order to maintain valid DICOM information, one can either change the transfer syntax to
    * an appropriate value for non-deflated data, or deflate the data again. This flow helps with the latter.
    *
    * @return the associated `DicomPart` `Flow`
    */
  val deflateDatasetFlow: Flow[DicomPart, DicomPart, NotUsed] =
    Flow[DicomPart]
      .concat(Source.single(DicomEndMarker))
      .statefulMapConcat {
        () =>
          var inFmi = false
          val buffer = new Array[Byte](2048)
          val deflater = new Deflater(-1, true)

          def deflate(dicomPart: DicomPart) = {
            val input = dicomPart.bytes
            deflater.setInput(input.toArray)
            var output = ByteString.empty
            while (!deflater.needsInput) {
              val bytesDeflated = deflater.deflate(buffer)
              output = output ++ ByteString(buffer.take(bytesDeflated))
            }
            if (output.isEmpty) Nil else DicomDeflatedChunk(dicomPart.bigEndian, output) :: Nil
          }

          def finishDeflating() = {
            deflater.finish()
            var output = ByteString.empty
            var done = false
            while (!done) {
              val bytesDeflated = deflater.deflate(buffer)
              if (bytesDeflated == 0)
                done = true
              else
                output = output ++ ByteString(buffer.take(bytesDeflated))
            }
            deflater.end()
            if (output.isEmpty) Nil else DicomDeflatedChunk(bigEndian = false, output) :: Nil
          }

        {
          case preamble: DicomPreamble => // preamble, do not deflate
            preamble :: Nil
          case DicomEndMarker => // end of stream, make sure deflater writes final bytes if deflating has occurred
            if (deflater.getBytesRead > 0)
              finishDeflating()
            else
              Nil
          case deflatedChunk: DicomDeflatedChunk => // already deflated, pass as-is
            deflatedChunk :: Nil
          case header: DicomHeader if header.isFmi => // FMI, do not deflate and remember we are in FMI
            inFmi = true
            header :: Nil
          case header: DicomHeader => // Dataset header, remember we are no longer in FMI, deflate
            inFmi = false
            deflate(header)
          case dicomPart if inFmi => // For any dicom part within FMI, do not deflate
            dicomPart :: Nil
          case dicomPart => // For all other cases, deflate
            deflate(dicomPart)
        }
      }

  /**
    * Remove elements from stream that may contain large quantities of data (bulk data)
    *
    * Rules ported from [[https://github.com/dcm4che/dcm4che/blob/3.3.8/dcm4che-core/src/main/java/org/dcm4che3/io/BulkDataDescriptor.java#L58 dcm4che]].
    * Defined [[http://dicom.nema.org/medical/dicom/current/output/html/part04.html#table_Z.1-1 here in the DICOM standard]].
    *
    * Note that it is up to the user of this function to make sure the modified DICOM data is valid.
    *
    * @return the associated DicomPart Flow
    */
  val bulkDataFilter: Flow[DicomPart, DicomPart, NotUsed] =
    Flow[DicomPart]
      .statefulMapConcat {

        def normalizeRepeatingGroup(tag: Int) = {
          val gg000000 = tag & 0xffe00000
          if (gg000000 == 0x50000000 || gg000000 == 0x60000000) tag & 0xffe0ffff else tag
        }

        () =>
          var sequenceStack = Seq.empty[DicomSequence]
          var discarding = false

        {
          case sq: DicomSequence =>
            sequenceStack = sq +: sequenceStack
            sq :: Nil
          case sqd: DicomSequenceDelimitation =>
            sequenceStack = sequenceStack.drop(1)
            sqd :: Nil
          case dh: DicomHeader =>
            discarding =
              normalizeRepeatingGroup(dh.tag) match {
                case Tag.PixelDataProviderURL => true
                case Tag.AudioSampleData => true
                case Tag.CurveData => true
                case Tag.SpectroscopyData => true
                case Tag.OverlayData => true
                case Tag.EncapsulatedDocument => true
                case Tag.FloatPixelData => true
                case Tag.DoubleFloatPixelData => true
                case Tag.PixelData => sequenceStack.isEmpty
                case Tag.WaveformData => sequenceStack.length == 1 && sequenceStack.head.tag == Tag.WaveformSequence
                case _ => false
              }
            if (discarding) Nil else dh :: Nil
          case dvc: DicomValueChunk =>
            if (discarding) Nil else dvc :: Nil
          case p: DicomPart =>
            discarding = false
            p :: Nil
        }
      }

  /**
    * Buffers all file meta information elements and calculates their lengths, then emits the correct file meta
    * information group length attribute followed by remaining FMI.
    */
  def fmiGroupLengthFlow: Flow[DicomPart, DicomPart, NotUsed] = Flow[DicomPart]
    .via(collectFlow(tagPath => tagPath.isRoot && DicomParsing.isFileMetaInformation(tagPath.tag), tagPath => !DicomParsing.isFileMetaInformation(tagPath.tag), "fmigrouplength", 0))
    .via(tagFilter(_ => true)(tagPath => !DicomParsing.isFileMetaInformation(tagPath.tag)))
    .concat(Source.single(DicomEndMarker))
    .statefulMapConcat {

      () =>
        var fmi = List.empty[DicomPart]
        var firstHeader: Option[DicomHeader] = None
        var hasEmitted = false

      {
        case fmiElements: CollectedElements if fmiElements.tag == "fmigrouplength" =>
          if (fmiElements.elements.nonEmpty) {
            val info = dicomInfo(fmiElements.elements.head.header.bytes).getOrElse(Info(bigEndian = false, explicitVR = true, hasFmi = true))
            val fmiElementsNoLength = fmiElements.elements.filter(_.header.tag != Tag.FileMetaInformationGroupLength)
            val length = fmiElementsNoLength.map(_.bytes.length).sum
            val lengthBytes = tagToBytes(Tag.FileMetaInformationGroupLength, info.bigEndian) ++
              (if (info.explicitVR) ByteString("UL") ++ shortToBytes(4, info.bigEndian) else intToBytes(4, info.bigEndian))
            val lengthHeader = DicomHeader(Tag.FileMetaInformationGroupLength, VR.UL, 4, isFmi = true, info.bigEndian, info.explicitVR, lengthBytes)
            val lengthChunk = DicomValueChunk(info.bigEndian, intToBytes(length, info.bigEndian), last = true)
            val fmiParts = fmiElementsNoLength.toList.flatMap(element => element.header :: element.valueChunks.toList)
            fmi = lengthHeader :: lengthChunk :: fmiParts
          }
          Nil

        case preamble: DicomPreamble =>
          if (hasEmitted)
            preamble :: Nil
          else {
            hasEmitted = true
            preamble :: fmi
          }

        case header: DicomHeader =>
          if (firstHeader.isEmpty) firstHeader = Some(header)
          if (hasEmitted)
            header :: Nil
          else {
            hasEmitted = true
            fmi ::: header :: Nil
          }

        case DicomEndMarker =>
          if (hasEmitted)
            Nil
          else {
            hasEmitted = true
            fmi
          }

        case part => part :: Nil
      }
    }

  /**
    * Remove all DICOM parts that do not contribute to file bytes
    */
  val syntheticPartsFilter: Flow[DicomPart, DicomPart, NotUsed] = Flow[DicomPart].filter(_.bytes.nonEmpty)

  /**
    * This flow guarantees that all `DicomHeader` parts are immediately followed by one or more `DicomValueChunk`s.
    * Normally, an empty element consists of a `DicomHeader` only. To allow the flow to keep track of inserted parts,
    * empty `DicomValueChunk`s are of the subtype `DicomValueChunkMarker`.
    */
  val guaranteedValueFlow: Flow[DicomPart, DicomPart, NotUsed] =
    DicomFlowFactory.create(new IdentityFlow with GuaranteedValueEvent[DicomPart] {
      override def onValueChunk(part: DicomValueChunk): List[DicomPart] = part :: Nil
    })

  /**
    * This flow guarantees that the end of sequences are marked with a `DicomSequenceDelimitation` and that the end of
    * items are marked with a `DicomSequenceItemDelimitation`. Normally, this is not the case for sequences and items
    * with determinate length. To allow the flow to keep track of inserted parts downstream, inserted delimitations are
    * of the subtypes `DicomSequenceDelimitationMarkser` and `DicomSequenceItemDelimitationMarker`.
    *
    * @return the associated flow
    */
  def guaranteedDelimitationFlow: Flow[DicomPart, DicomPart, NotUsed] =
    DicomFlowFactory.create(new IdentityFlow with GuaranteedDelimitationEvents[DicomPart] {
      override def onSequenceItemEnd(part: DicomSequenceItemDelimitation): List[DicomPart] =
        part match {
          case m: DicomSequenceItemDelimitationMarker => m :: super.onSequenceItemEnd(m)
          case p => super.onSequenceItemEnd(p)
        }
      override def onSequenceEnd(part: DicomSequenceDelimitation): List[DicomPart] =
        part match {
          case DicomSequenceDelimitationMarker => part :: super.onSequenceEnd(part)
          case p => super.onSequenceEnd(p)
        }
    })

  /**
    * Sets any sequences and/or items with known length to undefined length (length = -1) and inserts
    * delimiters.
    */
  def toUndefinedLengthSequences: Flow[DicomPart, DicomPart, NotUsed] =
    guaranteedDelimitationFlow
      .via(DicomFlowFactory.create(new IdentityFlow { // map to indeterminate length
        val indeterminateBytes = ByteString(0xFF, 0xFF, 0xFF, 0xFF)
        val zeroBytes = ByteString(0x00, 0x00, 0x00, 0x00)

        override def onSequenceStart(part: DicomSequence): List[DicomPart] =
          super.onSequenceStart(part.copy(length = -1, bytes = part.bytes.dropRight(4) ++ indeterminateBytes))
        override def onSequenceEnd(part: DicomSequenceDelimitation): List[DicomPart] =
          super.onSequenceEnd(part.copy(bytes = tagToBytes(0xFFFEE0DD, part.bigEndian) ++ zeroBytes))
        override def onSequenceItemStart(part: DicomSequenceItem): List[DicomPart] =
          super.onSequenceItemStart(part.copy(length = -1, bytes = part.bytes.dropRight(4) ++ indeterminateBytes))
        override def onSequenceItemEnd(part: DicomSequenceItemDelimitation): List[DicomPart] =
          super.onSequenceItemEnd(part.copy(bytes = tagToBytes(0xFFFEE00D, part.bigEndian) ++ zeroBytes))
      }))

  /**
    * Convert all string values to UTF-8 corresponding to the DICOM character set ISO_IR 192. First collects the
    * SpecificCharacterSet element. Any subsequent elements of VR type LO, LT, PN, SH, ST or UT will be decoded
    * using the character set(s) of the dataset, and replaced by a value encoded using UTF-8.
    *
    * Changing element values will change and update the length of individual elements, but does not update group
    * length elements and sequences and items with specified length. The recommended approach is to remove group
    * length elements and make sure sequences and items have undefined length using appropriate flows.
    *
    * @return the associated DicomPart Flow
    */
  def toUtf8Flow: Flow[DicomPart, DicomPart, NotUsed] = Flow[DicomPart]
    .via(collectFlow(Set(Tag.SpecificCharacterSet), "toutf8"))
    .via(modifyFlow(TagModification.contains(TagPath.fromTag(Tag.SpecificCharacterSet), _ => ByteString("ISO_IR 192"), insert = true)))
    .statefulMapConcat {

      () =>
        var characterSets: CharacterSets = CharacterSets.defaultOnly
        var currentHeader: Option[DicomHeader] = None
        var currentValue = ByteString.empty

      {
        case attr: CollectedElements if attr.tag == "toutf8" =>
          characterSets = attr.elements.headOption.map(a => CharacterSets(a.value)).getOrElse(characterSets)
          Nil
        case header: DicomHeader =>
          if (header.length > 0 && CharacterSets.isVrAffectedBySpecificCharacterSet(header.vr)) {
            currentHeader = Some(header)
            currentValue = ByteString.empty
            Nil
          } else {
            currentHeader = None
            header :: Nil
          }
        case value: DicomValueChunk if currentHeader.isDefined =>
          currentValue = currentValue ++ value.bytes
          if (value.last) {
            val newValue = currentHeader
              .map(h => characterSets.decode(h.vr, currentValue).getBytes(utf8Charset))
              .map(ByteString.apply)
            val newLength = newValue.map(_.length)
            val newElement = for {
              h <- currentHeader
              v <- newValue
              l <- newLength
            } yield h.withUpdatedLength(l) :: DicomValueChunk(h.bigEndian, v, last = true) :: Nil
            newElement.getOrElse(Nil)
          } else Nil
        case p: DicomPart =>
          p :: Nil
      }
    }

  /**
    * Ensure that the data has transfer syntax explicit VR little endian. Changes the TransferSyntaxUID, if present,
    * to 1.2.840.10008.1.2.1 to reflect this.
    *
    * @return the associated DicomPart Flow
    */
  def toExplicitVrLittleEndianFlow: Flow[DicomPart, DicomPart, NotUsed] = modifyFlow(
    TagModification.contains(TagPath.fromTag(Tag.TransferSyntaxUID), _ => padToEvenLength(ByteString(UID.ExplicitVRLittleEndian), VR.UI), insert = false))
    .via(fmiGroupLengthFlow)
    .statefulMapConcat {

      case class SwapResult(bytes: ByteString, carry: ByteString)

      def swap(k: Int, b: ByteString): SwapResult =
        SwapResult(b.grouped(k).map(_.reverse).reduce(_ ++ _), b.takeRight(b.length % k))

      () =>
        var currentVr: Option[VR] = None
        var carryBytes = ByteString.empty

        def updatedValue(swapResult: SwapResult, last: Boolean): DicomValueChunk = {
          carryBytes = swapResult.carry
          if (last && carryBytes.nonEmpty)
            throw new DicomStreamException("Dicom value length does not match length specified in header")
          DicomValueChunk(bigEndian = false, swapResult.bytes, last = last)
        }

      {
        case h: DicomHeader if h.bigEndian || !h.explicitVR =>
          if (h.bigEndian) {
            carryBytes = ByteString.empty
            currentVr = Some(h.vr)
          } else currentVr = None
          DicomHeader(h.tag, h.vr, h.length, h.isFmi, bigEndian = false, explicitVR = true) :: Nil

        case v: DicomValueChunk if currentVr.isDefined && v.bigEndian =>
          currentVr match {
            case Some(vr) if vr == VR.US || vr == VR.SS || vr == VR.OW || vr == VR.AT => // 2 byte
              updatedValue(swap(2, carryBytes ++ v.bytes), v.last) :: Nil
            case Some(vr) if vr == VR.OF || vr == VR.UL || vr == VR.SL || vr == VR.FL => // 4 bytes
              updatedValue(swap(4, carryBytes ++ v.bytes), v.last) :: Nil
            case Some(vr) if vr == VR.OD || vr == VR.FD => // 8 bytes
              updatedValue(swap(8, carryBytes ++ v.bytes), v.last) :: Nil
            case _ => v :: Nil
          }

        case p: DicomPart if !p.bigEndian => p :: Nil

        case s: DicomSequence =>
          DicomSequence(s.tag, s.length, bigEndian = false,
            tagToBytesLE(s.tag) ++ ByteString('S', 'Q', 0, 0) ++ s.bytes.takeRight(4).reverse) :: Nil

        case _: DicomSequenceDelimitation =>
          DicomSequenceDelimitation(bigEndian = false,
            tagToBytesLE(0xFFFEE0DD) ++ ByteString(0, 0, 0, 0)) :: Nil

        case i: DicomSequenceItem =>
          DicomSequenceItem(i.index, i.length, bigEndian = false,
            tagToBytesLE(0xFFFEE000) ++ i.bytes.takeRight(4).reverse) :: Nil

        case i: DicomSequenceItemDelimitation =>
          DicomSequenceItemDelimitation(i.index, bigEndian = false,
            tagToBytesLE(0xFFFEE00D) ++ ByteString(0, 0, 0, 0)) :: Nil

        case f: DicomFragments =>
          if (f.bigEndian) {
            carryBytes = ByteString.empty
            currentVr = Some(f.vr)
          } else currentVr = None
          DicomFragments(f.tag, f.length, f.vr, bigEndian = false,
            tagToBytesLE(f.tag) ++ f.bytes.drop(4).take(4) ++ f.bytes.takeRight(4).reverse) :: Nil

        case _: DicomFragmentsDelimitation =>
          DicomFragmentsDelimitation(bigEndian = false,
            tagToBytesLE(0xFFFEE0DD) ++ ByteString(0, 0, 0, 0)) :: Nil

        case p => p :: Nil
      }
    }
}


