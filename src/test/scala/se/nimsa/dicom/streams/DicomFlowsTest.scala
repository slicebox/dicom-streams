package se.nimsa.dicom.streams

import java.io.File

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{FileIO, Source}
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom._
import se.nimsa.dicom.TestData._
import se.nimsa.dicom.streams.DicomParts._
import se.nimsa.dicom.streams.ParseFlow._
import se.nimsa.dicom.streams.DicomFlows._
import se.nimsa.dicom.streams.ModifyFlow._
import se.nimsa.dicom.streams.TestUtils._

import scala.concurrent.ExecutionContextExecutor


class DicomFlowsTest extends TestKit(ActorSystem("DicomFlowsSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "A print flow" should "not change incoming elements" in {
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(printFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  "The DICOM parts filter" should "block all attributes not on the white list" in {
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(whitelistFilter(Set(Tag.StudyDate)))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "also apply to FMI is instructed" in {
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(whitelistFilter(Set(Tag.StudyDate)))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "only apply to attributes in the root dataset" in {
    val bytes = sequence(Tag.DerivationCodeSequence) ++ item() ++ patientNameJohnDoe() ++ studyDate() ++ itemEnd() ++ sequenceEnd()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(whitelistFilter(Set(Tag.StudyDate)))

    source.runWith(TestSink.probe[DicomPart])
      .expectDicomComplete()
  }

  it should "also work on fragments" in {
    val bytes = pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragment(4) ++ ByteString(5, 6, 7, 8) ++ fragmentsEnd()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(whitelistFilter(Set.empty))

    source.runWith(TestSink.probe[DicomPart])
      .expectDicomComplete()
  }

  "The DICOM group length discard filter" should "discard group length elements except 0002,0000" in {
    val groupLength = ByteString(8, 0, 0, 0, 85, 76, 4, 0) ++ intToBytesLE(studyDate().size)
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ groupLength ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(groupLengthDiscardFilter)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "discard group length elements except 0002,0000 when testing with dicom file" in {
    val file = new File(getClass.getResource("CT0055.dcm").toURI)
    val source = FileIO.fromPath(file.toPath)
      .via(parseFlow)
      .via(groupLengthDiscardFilter)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.FileMetaInformationVersion)
      .expectValueChunk()
  }

  "The DICOM file meta information discard filter" should "discard file meta informaton" in {
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(fmiDiscardFilter)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "discard file meta information when testing with dicom files" in {
    val file = new File(getClass.getResource("CT0055.dcm").toURI)
    val source = FileIO.fromPath(file.toPath)
      .via(parseFlow)
      .via(fmiDiscardFilter)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk()
      .expectHeader(Tag.ImageType)
  }

  "The DICOM tag filter" should "filter attributes in sequences" in {
    val bytes = sequence(Tag.DerivationCodeSequence) ++ item() ++ studyDate() ++ patientNameJohnDoe() ++ itemEnd() ++ sequenceEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(tagFilter(_ => true)(tagPath => tagPath.tag != Tag.PatientName))

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  "The DICOM blacklist filter" should "filter elements matching the blacklist condition" in {
    val bytes = preamble ++ fmiGroupLength(fmiVersion, tsuidExplicitLE) ++ fmiVersion ++ tsuidExplicitLE ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(tagFilter(_ => false)(tagPath => !DicomParsing.isFileMetaInformation(tagPath.tag)))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "filter elements matching the blacklist condition when testing with sample dicom files" in {
    val file = new File(getClass.getResource("CT0055.dcm").toURI)
    val source = FileIO.fromPath(file.toPath)
      .via(new ParseFlow())
      .via(tagFilter(_ => false)(tagPath => !DicomParsing.isFileMetaInformation(tagPath.tag)))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk()
      .expectHeader(Tag.ImageType)
  }

  it should "block the entire sequence when a sequence tag is on the black list" in {
    val bytes = studyDate() ++
      (sequence(Tag.DerivationCodeSequence) ++ item() ++ patientNameJohnDoe() ++
        (sequence(Tag.AbstractPriorCodeSequence) ++ item() ++ patientNameJohnDoe() ++ itemEnd() ++ sequenceEnd()) ++
        itemEnd() ++ sequenceEnd()) ++
      patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(blacklistFilter(Set(TagPath.fromSequence(Tag.DerivationCodeSequence))))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "block a single item inside a sequence" in {
    val bytes = studyDate() ++
      sequence(Tag.DerivationCodeSequence) ++ item() ++ patientNameJohnDoe() ++ itemEnd() ++ item() ++ studyDate() ++ itemEnd() ++ sequenceEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(blacklistFilter(Set(TagPath.fromTag(Tag.StudyDate), TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1))))

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(2)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "filter leave the dicom file unchanged when blacklist condition does not match any attribute" in {
    val file = new File(getClass.getResource("CT0055.dcm").toURI)
    val source = FileIO.fromPath(file.toPath)
      .via(new ParseFlow())
      .via(tagFilter(_ => true)(tagPath => !DicomParsing.isPrivateAttribute(tagPath.tag)))

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.FileMetaInformationVersion)
      .expectValueChunk()
  }


  "The DICOM whitelist filter" should "filter elements not matching the whitelist condition" in {
    val bytes = preamble ++ fmiGroupLength(fmiVersion, tsuidExplicitLE) ++ fmiVersion ++ tsuidExplicitLE ++ patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(tagFilter(_ => false)(tagPath => groupNumber(tagPath.tag) >= 8))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "filter elements not matching the whitelist condition when testing with sample dicom files" in {
    val file = new File(getClass.getResource("CT0055.dcm").toURI)
    val source = FileIO.fromPath(file.toPath)
      .via(parseFlow)
      .via(tagFilter(_ => false)(tagPath => tagPath.tag == Tag.PatientName))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  "The deflate flow" should "recreate the dicom parts of a dataset which has been deflated and inflated again" in {
    val bytes = fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(deflateDatasetFlow)
      .via(modifyFlow(
        TagModification.contains(TagPath.fromTag(Tag.FileMetaInformationGroupLength), _ => fmiGroupLength(tsuidDeflatedExplicitLE), insert = false),
        TagModification.contains(TagPath.fromTag(Tag.TransferSyntaxUID), _ => tsuidDeflatedExplicitLE.drop(8), insert = false)))
      .map(_.bytes)
      .via(new ParseFlow())

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "not deflate meta information" in {
    val bytes = fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(deflateDatasetFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectDeflatedChunk()
  }

  it should "not ouput anything when the stream is empty" in {
    val bytes = ByteString.empty

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(deflateDatasetFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectDicomComplete()
  }

  "The bulk data filter flow" should "remove pixel data" in {
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe() ++ pixelData(1000)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(bulkDataFilter)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "not remove pixel data in sequences" in {
    val bytes = sequence(Tag.DerivationCodeSequence) ++ item() ++ patientNameJohnDoe() ++ pixelData(100) ++ itemEnd() ++ sequenceEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(bulkDataFilter)

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectHeader(Tag.PixelData)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "only remove waveform data when inside waveform sequence" in {
    val bytes = waveformSeqStart() ++ item() ++ patientNameJohnDoe() ++ waveformData(100) ++ itemEnd() ++ sequenceEnd() ++ patientNameJohnDoe() ++ waveformData(100)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(bulkDataFilter)

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.WaveformSequence)
      .expectItem(1)
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectHeader(Tag.WaveformData)
      .expectValueChunk()
      .expectDicomComplete()
  }

  "The FMI group length flow" should "calculate and emit the correct group length attribute" in {
    val correctLength = tsuidExplicitLE.length
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE.drop(2)) ++ tsuidExplicitLE ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(fmiGroupLengthFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk(intToBytesLE(correctLength))
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "work also in flows with file meta information only" in {
    val correctLength = tsuidExplicitLE.length
    val bytes = preamble ++ tsuidExplicitLE // missing file meta information group length

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(fmiGroupLengthFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk(intToBytesLE(correctLength))
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "work in flows without preamble" in {
    val correctLength = tsuidExplicitLE.length
    val bytes = tsuidExplicitLE ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(fmiGroupLengthFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk(intToBytesLE(correctLength))
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "not emit anything in empty flows" in {
    val bytes = ByteString.empty

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(fmiGroupLengthFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectDicomComplete()
  }

  it should "not emit a group length attribute when there is no FMI" in {
    val bytes = preamble ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(fmiGroupLengthFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "keep a zero length group length attribute" in {
    val bytes = fmiGroupLength(ByteString.empty) ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(fmiGroupLengthFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk(ByteString(0, 0, 0, 0))
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "ignore DICOM parts of unknown type" in {
    case object SomePart extends DicomPart {
      def bigEndian: Boolean = false
      def bytes: ByteString = ByteString.empty
    }

    val correctLength = tsuidExplicitLE.length
    val bytes = preamble ++ tsuidExplicitLE // missing file meta information group length

    val source = Source.single(bytes)
      .via(parseFlow)
      .prepend(Source.single(SomePart))
      .via(fmiGroupLengthFlow)

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case SomePart => true
      }
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk(intToBytesLE(correctLength))
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk()
      .expectDicomComplete()
  }

  "The sequence length filter" should "replace determinate length sequences and items with indeterminate, and insert delimitations" in {
    val bytes =
      sequence(Tag.DerivationCodeSequence, 56) ++ item(16) ++ studyDate() ++ item() ++ studyDate() ++ itemEnd() ++
        sequence(Tag.AbstractPriorCodeSequence) ++ item() ++ studyDate() ++ itemEnd() ++ item(16) ++ studyDate() ++ sequenceEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUndefinedLengthSequences)

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence, -1)
      .expectItem(1, -1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectItem(2)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectSequence(Tag.AbstractPriorCodeSequence, -1)
      .expectItem(1, -1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectItem(2)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "handle sequences that end with an item delimitation" in {
    val bytes =
      sequence(Tag.DerivationCodeSequence, 32) ++ item() ++ studyDate() ++ itemEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUndefinedLengthSequences)

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence, -1)
      .expectItem(1, -1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "should not remove length from items in fragments" in {
    val bytes =
      pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragmentsEnd() ++
        sequence(Tag.DerivationCodeSequence, 40) ++ item(32) ++
        pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragmentsEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUndefinedLengthSequences)

    source.runWith(TestSink.probe[DicomPart])
      .expectFragments()
      .expectFragment(1, 4)
      .expectValueChunk()
      .expectFragmentsDelimitation()
      .expectSequence(Tag.DerivationCodeSequence, -1)
      .expectItem(1, -1)
      .expectFragments()
      .expectFragment(1, 4)
      .expectValueChunk()
      .expectFragmentsDelimitation()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "work in datasets with nested sequences" in {
    val bytes = studyDate() ++ sequence(Tag.DerivationCodeSequence, 60) ++ item(52) ++ studyDate() ++
      sequence(Tag.DerivationCodeSequence, 24) ++ item(16) ++ studyDate() ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUndefinedLengthSequences)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectSequence(Tag.DerivationCodeSequence, -1)
      .expectItem(1, -1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectSequence(Tag.DerivationCodeSequence, -1)
      .expectItem(1, -1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "handle empty sequences and items" in {
    val bytes =
      sequence(Tag.DerivationCodeSequence, 52) ++ item(16) ++ studyDate() ++ item(0) ++ item(12) ++ sequence(Tag.DerivationCodeSequence, 0)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUndefinedLengthSequences)

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence, -1)
      .expectItem(1, -1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectItem(2, -1)
      .expectItemDelimitation()
      .expectItem(3, -1)
      .expectSequence(Tag.DerivationCodeSequence, -1)
      .expectSequenceDelimitation()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  "The utf8 flow" should "transform a japanese patient name encoded with multiple character sets to valid utf8" in {
    val specificCharacterSet = tagToBytesLE(Tag.SpecificCharacterSet) ++ ByteString("CS") ++
      shortToBytesLE(0x001E) ++ padToEvenLength(ByteString("ISO 2022 IR 13\\ISO 2022 IR 87"), VR.CS)
    val patientName = tagToBytesLE(0x00100010) ++ ByteString("PN") ++ shortToBytesLE(0x0038) ++
      padToEvenLength(ByteString(
        0xD4, 0xCF, 0xC0, 0xDE, 0x5E, 0xC0, 0xDB, 0xB3, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28,
        0x4A, 0x5E, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x4A, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64,
        0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x4A, 0x5E, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B,
        0x28, 0x4A), VR.PN)

    val bytes = specificCharacterSet ++ patientName

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUtf8Flow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk(ByteString("ﾔﾏﾀﾞ^ﾀﾛｳ=山田^太郎=やまだ^たろう"))
      .expectDicomComplete()
  }

  it should "set specific character set to ISO_IR 192 (UTF-8)" in {
    val specificCharacterSet = tagToBytesLE(Tag.SpecificCharacterSet) ++ ByteString("CS") ++
      shortToBytesLE(0x001E) ++ padToEvenLength(ByteString("ISO 2022 IR 13\\ISO 2022 IR 87"), VR.CS)

    val bytes = specificCharacterSet

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUtf8Flow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk(ByteString("ISO_IR 192"))
      .expectDicomComplete()
  }

  it should "transform data without the specific character set attribute, decoding using the default character set" in {
    val bytes = patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUtf8Flow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk(ByteString("ISO_IR 192"))
      .expectHeader(Tag.PatientName)
      .expectValueChunk(patientNameJohnDoe().drop(8))
      .expectDicomComplete()
  }

  it should "transform data contained in sequences" in {
    val specificCharacterSet = tagToBytesLE(Tag.SpecificCharacterSet) ++ ByteString("CS") ++
      shortToBytesLE(0x001E) ++ padToEvenLength(ByteString("ISO 2022 IR 13\\ISO 2022 IR 87"), VR.CS)
    val patientName = tagToBytesLE(0x00100010) ++ ByteString("PN") ++ shortToBytesLE(0x0004) ++ padToEvenLength(ByteString(0xD4, 0xCF, 0xC0, 0xDE), VR.PN)

    val bytes = specificCharacterSet ++ sequence(Tag.DerivationCodeSequence) ++ item() ++ patientName ++ itemEnd() ++ sequenceEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUtf8Flow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk()
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.PatientName)
      .expectValueChunk(ByteString("ﾔﾏﾀﾞ"))
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "not transform data with VR that doesn't support non-default encodings" in {
    val specificCharacterSet = tagToBytesLE(Tag.SpecificCharacterSet) ++ ByteString("CS") ++
      shortToBytesLE(0x001E) ++ padToEvenLength(ByteString("ISO 2022 IR 13\\ISO 2022 IR 87"), VR.CS)
    val patientNameCS = tagToBytesLE(0x00100010) ++ ByteString("CS") ++ shortToBytesLE(0x0004) ++ padToEvenLength(ByteString(0xD4, 0xCF, 0xC0, 0xDE), VR.PN)

    val bytes = specificCharacterSet ++ patientNameCS

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUtf8Flow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk(ByteString(0xD4, 0xCF, 0xC0, 0xDE))
      .expectDicomComplete()
  }

  it should "not change a file already encoded with ISO_IR 192 (UTF-8)" in {
    val specificCharacterSet = tagToBytesLE(Tag.SpecificCharacterSet) ++ ByteString("CS") ++ shortToBytesLE(0x000A) ++ ByteString("ISO_IR 192")
    val patientName = tagToBytesLE(Tag.PatientName) ++ ByteString("PN") ++ shortToBytesLE(0x000C) ++ ByteString("ABC^ÅÖ^ﾔ")
    val bytes = specificCharacterSet ++ patientName

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUtf8Flow)
      .map(_.bytes)
      .reduce(_ ++ _)

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectNextChainingPF { case newBytes => newBytes shouldBe bytes }
      .expectComplete()
  }

  it should "leave and empty attribute empty" in {
    val bytes = emptyPatientName()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUtf8Flow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.SpecificCharacterSet)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectDicomComplete()
  }

  "The explicit VR little endian flow" should "convert explicit VR big endian to explicit VR little endian" in {
    val bigEndian = true
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitBE) ++ tsuidExplicitBE ++ patientNameJohnDoe(bigEndian) ++
      rows(bigEndian) ++ dataPointRows(bigEndian) ++ apexPosition(bigEndian) ++
      sequence(Tag.DerivationCodeSequence, bigEndian) ++ item(bigEndian) ++ studyDate(bigEndian) ++
      itemEnd(bigEndian) ++ sequenceEnd(bigEndian) ++ pixeDataFragments(bigEndian) ++ fragment(1000, bigEndian) ++
      (1 to 500).map(_.toShort).map(shortToBytesBE).reduce(_ ++ _) ++ fragmentsEnd(bigEndian)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toExplicitVrLittleEndianFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectPreamble()
      .expectHeader(Tag.FileMetaInformationGroupLength)
      .expectValueChunk()
      .expectHeader(Tag.TransferSyntaxUID)
      .expectValueChunk(tsuidExplicitLE.drop(8))
      .expectHeader(Tag.PatientName)
      .expectValueChunk(patientNameJohnDoe().drop(8))
      .expectHeader(Tag.Rows)
      .expectValueChunk(rows().drop(8))
      .expectHeader(Tag.DataPointRows)
      .expectValueChunk(dataPointRows().drop(8))
      .expectHeader(Tag.ApexPosition)
      .expectValueChunk(apexPosition().drop(8))
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk(studyDate().drop(8))
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectFragments()
      .expectFragment(1, 1000)
      .expectValueChunk((1 to 500).map(_.toShort).map(shortToBytesLE).reduce(_ ++ _))
      .expectFragmentsDelimitation()
      .expectDicomComplete()
  }

  it should "convert implicit VR little endian to explicit VR little endian" in {
    val bytes = patientNameJohnDoeImplicit

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toExplicitVrLittleEndianFlow)
      .map(_.bytes)
      .reduce(_ ++ _)

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectNextChainingPF { case newBytes: ByteString => newBytes shouldBe patientNameJohnDoe() }
      .expectComplete()
  }

  it should "not change a file already encoded with explicit VR little endian" in {
    val bigEndian = false
    val bytes = preamble ++ fmiGroupLength(tsuidExplicitLE) ++ tsuidExplicitLE ++ patientNameJohnDoe(bigEndian) ++
      rows(bigEndian) ++ dataPointRows(bigEndian) ++ apexPosition(bigEndian) ++
      sequence(Tag.DerivationCodeSequence, bigEndian) ++ item(bigEndian) ++ studyDate(bigEndian) ++
      itemEnd(bigEndian) ++ sequenceEnd(bigEndian) ++ pixeDataFragments(bigEndian) ++ fragment(1000, bigEndian) ++
      (1 to 500).map(_.toShort).map(shortToBytesLE).reduce(_ ++ _) ++ fragmentsEnd(bigEndian)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toExplicitVrLittleEndianFlow)
      .map(_.bytes)
      .reduce(_ ++ _)

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectNextChainingPF { case newBytes: ByteString => newBytes shouldBe bytes }
      .expectComplete()
  }

  it should "leave and empty attribute empty" in {
    val bytes = emptyPatientName(bigEndian = true)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toExplicitVrLittleEndianFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.PatientName)
      .expectDicomComplete()
  }
}
