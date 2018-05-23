package se.nimsa.dicom.streams

import java.io.File

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{FileIO, Flow, Sink, Source}
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.data._
import se.nimsa.dicom.data.TagPath.EmptyTagPath
import se.nimsa.dicom.data.{Tag, TagPath}
import se.nimsa.dicom.streams.ParseFlow.parseFlow

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}

class DicomFlowTest extends TestKit(ActorSystem("DicomFlowSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  import DicomFlows._
  import TestUtils._
  import se.nimsa.dicom.data.DicomParts._
  import se.nimsa.dicom.data.TestData._

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "The dicom flow" should "call the correct events for streamed dicom parts" in {
    val bytes = preamble ++ fmiGroupLength(transferSyntaxUID()) ++ transferSyntaxUID() ++
      patientNameJohnDoe() ++ sequence(Tag.DerivationCodeSequence) ++ item() ++ studyDate() ++ itemEnd() ++ sequenceEnd() ++
      pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragmentsEnd()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new DicomFlow[DicomPart] {
        override def onFragmentsEnd(part: FragmentsDelimitationPart): List[DicomPart] = TestPart("Fragments End") :: Nil
        override def onFragmentsItemStart(part: FragmentsItemPart): List[DicomPart] = TestPart("Fragments Item Start") :: Nil
        override def onFragmentsStart(part: FragmentsPart): List[DicomPart] = TestPart("Fragments Start") :: Nil
        override def onHeader(part: HeaderPart): List[DicomPart] = TestPart("Header") :: Nil
        override def onPreamble(part: PreamblePart): List[DicomPart] = TestPart("Preamble") :: Nil
        override def onSequenceEnd(part: SequenceDelimitationPart): List[DicomPart] = TestPart("Sequence End") :: Nil
        override def onSequenceItemEnd(part: ItemDelimitationPart): List[DicomPart] = TestPart("Sequence Item End") :: Nil
        override def onSequenceItemStart(part: SequenceItemPart): List[DicomPart] = TestPart("Sequence Item Start") :: Nil
        override def onSequenceStart(part: SequencePart): List[DicomPart] = TestPart("Sequence Start") :: Nil
        override def onValueChunk(part: ValueChunk): List[DicomPart] = TestPart("Value Chunk") :: Nil
        override def onDeflatedChunk(part: DeflatedChunk): List[DicomPart] = Nil
        override def onUnknownPart(part: UnknownPart): List[DicomPart] = Nil
        override def onPart(part: DicomPart): List[DicomPart] = Nil
      }))

    source.runWith(TestSink.probe[DicomPart])
      .expectTestPart("Preamble")
      .expectTestPart("Header")
      .expectTestPart("Value Chunk")
      .expectTestPart("Header")
      .expectTestPart("Value Chunk")
      .expectTestPart("Header")
      .expectTestPart("Value Chunk")
      .expectTestPart("Sequence Start")
      .expectTestPart("Sequence Item Start")
      .expectTestPart("Header")
      .expectTestPart("Value Chunk")
      .expectTestPart("Sequence Item End")
      .expectTestPart("Sequence End")
      .expectTestPart("Fragments Start")
      .expectTestPart("Fragments Item Start")
      .expectTestPart("Value Chunk")
      .expectTestPart("Fragments End")
      .expectDicomComplete()
  }

  "The guaranteed delimitation flow" should "insert delimitation parts at the end of sequences and items with determinate length" in {
    val bytes =
      sequence(Tag.DerivationCodeSequence, 56) ++ item(16) ++ studyDate() ++ item() ++ studyDate() ++ itemEnd() ++
        sequence(Tag.AbstractPriorCodeSequence) ++ item() ++ studyDate() ++ itemEnd() ++ item(16) ++ studyDate() ++ sequenceEnd()

    var expectedDelimitationLengths = List(0, 8, 0, 8, 0, 8)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new IdentityFlow with GuaranteedDelimitationEvents[DicomPart] {
        override def onSequenceItemEnd(part: ItemDelimitationPart): List[DicomPart] = {
          part.bytes.length shouldBe expectedDelimitationLengths.head
          expectedDelimitationLengths = expectedDelimitationLengths.tail
          super.onSequenceItemEnd(part)
        }
        override def onSequenceEnd(part: SequenceDelimitationPart): List[DicomPart] = {
          part.bytes.length shouldBe expectedDelimitationLengths.head
          expectedDelimitationLengths = expectedDelimitationLengths.tail
          super.onSequenceEnd(part)
        }
      }))

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence, 56)
      .expectItem(1, 16)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      //.expectItemDelimitation() // delimitations not emitted by default
      .expectItem(2)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      //.expectSequenceDelimitation()
      .expectSequence(Tag.AbstractPriorCodeSequence, -1)
      .expectItem(1, -1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectItem(2, 16)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      //.expectItemDelimitation()
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
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
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
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
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
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectItemDelimitation()
      .expectItem(2)
      .expectItemDelimitation()
      .expectItem(3)
      .expectSequence(Tag.DerivationCodeSequence)
      .expectSequenceDelimitation()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "handle empty elements in sequences" in {
    val bytes =
      sequence(Tag.DerivationCodeSequence, 44) ++ item(36) ++ emptyPatientName() ++
        sequence(Tag.DerivationCodeSequence, 16) ++ item(8) ++ emptyPatientName()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(toUndefinedLengthSequences)

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.PatientName)
      .expectSequence(Tag.DerivationCodeSequence)
      .expectItem(1)
      .expectHeader(Tag.PatientName)
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectItemDelimitation()
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "call event only once when used twice in flow" in {
    val bytes = sequence(Tag.DerivationCodeSequence, 24) ++ item(16) ++ patientNameJohnDoe()

    var nItemDelims = 0
    var nSeqDelims = 0

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new IdentityFlow with GuaranteedDelimitationEvents[DicomPart]))
      .via(DicomFlowFactory.create(new IdentityFlow with GuaranteedDelimitationEvents[DicomPart] {
        override def onSequenceItemEnd(part: ItemDelimitationPart): List[DicomPart] = {
          nItemDelims += 1
          super.onSequenceItemEnd(part)
        }
        override def onSequenceEnd(part: SequenceDelimitationPart): List[DicomPart] = {
          nSeqDelims += 1
          super.onSequenceEnd(part)
        }
      }))

    Await.ready(source.runWith(Sink.ignore), 5.seconds)

    nItemDelims shouldBe 1
    nSeqDelims shouldBe 1
  }

  "The guaranteed value flow" should "call onValueChunk callback also after length zero headers" in {
    val bytes = patientNameJohnDoe() ++ emptyPatientName()

    var expectedChunkLengths = List(8, 0)

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new IdentityFlow with GuaranteedValueEvent[DicomPart] {
        override def onValueChunk(part: ValueChunk): List[DicomPart] = {
          part.bytes.length shouldBe expectedChunkLengths.head
          expectedChunkLengths = expectedChunkLengths.tail
          super.onValueChunk(part)
        }
      }))

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.PatientName)
      .expectValueChunk(8)
      .expectHeader(Tag.PatientName)
      .expectDicomComplete()
  }

  it should "call event only once when flow is used twice" in {
    val bytes = emptyPatientName()

    var nEvents = 0

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new IdentityFlow with GuaranteedValueEvent[DicomPart]))
      .via(DicomFlowFactory.create(new IdentityFlow with GuaranteedValueEvent[DicomPart] {
        override def onValueChunk(part: ValueChunk): List[DicomPart] = {
          nEvents += 1
          super.onValueChunk(part)
        }
      }))

    Await.ready(source.runWith(Sink.ignore), 5.seconds)
    nEvents shouldBe 1
  }

  val startEventTestFlow: Flow[DicomPart, DicomPart, NotUsed] =
    DicomFlowFactory.create(new IdentityFlow with StartEvent[DicomPart] {
      override def onStart(): List[DicomPart] = DicomStartMarker :: Nil
    })

  "The start event flow" should "notify when dicom stream starts" in {
    val bytes = patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(startEventTestFlow)

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case DicomStartMarker => true
      }
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  val endEventTestFlow: Flow[DicomPart, DicomPart, NotUsed] =
    DicomFlowFactory.create(new IdentityFlow with EndEvent[DicomPart] {
      override def onEnd(): List[DicomPart] = DicomEndMarker :: Nil
    })

  "The end event flow" should "notify when dicom stream ends" in {
    val bytes = patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(endEventTestFlow)

    source.runWith(TestSink.probe[DicomPart])
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .request(1)
      .expectNextChainingPF {
        case DicomEndMarker => true
      }
      .expectDicomComplete()
  }

  "DICOM flows with tag path tracking" should "update the tag path through attributes, sequences and fragments" in {
    val bytes = preamble ++ fmiGroupLength(transferSyntaxUID()) ++ transferSyntaxUID() ++ // FMI
      studyDate() ++
      sequence(Tag.EnergyWindowInformationSequence) ++ item() ++ studyDate() ++ itemEnd() ++ item() ++ // sequence
      sequence(Tag.EnergyWindowRangeSequence, 24) ++ item(16) ++ studyDate() ++ // nested sequence (determinate length)
      itemEnd() ++ sequenceEnd() ++
      patientNameJohnDoe() ++ // attribute
      pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragmentsEnd()

    var expectedPaths = List(
      EmptyTagPath, // preamble
      TagPath.fromTag(Tag.FileMetaInformationGroupLength), // FMI group length header
      TagPath.fromTag(Tag.FileMetaInformationGroupLength), // FMI group length value
      TagPath.fromTag(Tag.TransferSyntaxUID), // Transfer syntax header
      TagPath.fromTag(Tag.TransferSyntaxUID), // Transfer syntax value
      TagPath.fromTag(Tag.StudyDate), // Patient name header
      TagPath.fromTag(Tag.StudyDate), // Patient name value
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence), // sequence start
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 1), // item start
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 1).thenTag(Tag.StudyDate), // study date header
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 1).thenTag(Tag.StudyDate), // study date value
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 1), // item end
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2), // item start
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2).thenSequence(Tag.EnergyWindowRangeSequence), // sequence start
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2).thenSequence(Tag.EnergyWindowRangeSequence, 1), // item start
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2).thenSequence(Tag.EnergyWindowRangeSequence, 1).thenTag(Tag.StudyDate), // Study date header
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2).thenSequence(Tag.EnergyWindowRangeSequence, 1).thenTag(Tag.StudyDate), // Study date value
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2).thenSequence(Tag.EnergyWindowRangeSequence, 1), //  item end (inserted)
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2).thenSequence(Tag.EnergyWindowRangeSequence), // sequence end (inserted)
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence, 2), // item end
      TagPath.fromSequence(Tag.EnergyWindowInformationSequence), // sequence end
      TagPath.fromTag(Tag.PatientName), // Patient name header
      TagPath.fromTag(Tag.PatientName), // Patient name value
      TagPath.fromTag(Tag.PixelData), // fragments start
      TagPath.fromTag(Tag.PixelData), // item start
      TagPath.fromTag(Tag.PixelData), // fragment data
      TagPath.fromTag(Tag.PixelData) // fragments end
    )

    def check(tagPath: TagPath): Unit = {
      tagPath shouldBe expectedPaths.head
      expectedPaths = expectedPaths.tail
    }

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with TagPathTracking[DicomPart] {
        override def onPart(part: DicomPart): List[DicomPart] = {
          check(tagPath)
          part :: Nil
        }
      }))

    source.runWith(TestSink.probe[DicomPart])
      .request(27 - 2) // two events inserted
      .expectNextN(27 - 2)
  }

  it should "support using the same tracking more than once within a flow" in {
    val bytes = sequence(Tag.DerivationCodeSequence, 24) ++ item(16) ++ patientNameJohnDoe()

    // must be def, not val
    def flow = DicomFlowFactory.create(new IdentityFlow with TagPathTracking[DicomPart])

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(flow)
      .via(flow)

    source.runWith(TestSink.probe[DicomPart])
      .expectSequence(Tag.DerivationCodeSequence, 24)
      .expectItem(1, 16)
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "support sequences and items with explicit length" in {
    val bytes = patientNameJohnDoe() ++
      sequence(Tag.DigitalSignaturesSequence, 680) ++
      item(672) ++
      element(Tag.MACIDNumber, ByteString(1, 1), bigEndian = false, explicitVR = true) ++
      element(Tag.DigitalSignatureUID, "1" * 54) ++
      element(Tag.CertificateType, "A" * 14) ++
      element(Tag.CertificateOfSigner, ByteString(new Array[Byte](426)), bigEndian = false, explicitVR = true) ++
      element(Tag.Signature, ByteString(new Array[Byte](128)), bigEndian = false, explicitVR = true)

    var expectedPaths = List(
      TagPath.fromTag(Tag.PatientName),
      TagPath.fromTag(Tag.PatientName),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence), // sequence start
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1), // item start
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.MACIDNumber),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.MACIDNumber),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.DigitalSignatureUID),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.DigitalSignatureUID),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.CertificateType),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.CertificateType),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.CertificateOfSigner),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.CertificateOfSigner),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.Signature),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1).thenTag(Tag.Signature),
      TagPath.fromSequence(Tag.DigitalSignaturesSequence, 1), // item end (inserted)
      TagPath.fromSequence(Tag.DigitalSignaturesSequence)) // sequence end (inserted)

    def check(tagPath: TagPath): Unit = {
      tagPath shouldBe expectedPaths.head
      expectedPaths = expectedPaths.tail
    }

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with TagPathTracking[DicomPart] {
        override def onPart(part: DicomPart): List[DicomPart] = {
          check(tagPath)
          part :: Nil
        }
      }))

    source.runWith(TestSink.probe[DicomPart])
      .request(12)
      .expectNextN(12)
  }

  it should "track an entire file without exception" in {
    val file = new File(getClass.getResource("../data/test001.dcm").toURI)
    val source = FileIO.fromPath(file.toPath)
      .via(parseFlow)
      .via(DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with TagPathTracking[DicomPart] {
        override def onPart(part: DicomPart): List[DicomPart] = part :: Nil
      }))

    Await.result(source.runWith(Sink.ignore), 5.seconds)
    succeed
  }

  "The onStart event" should "be called for all combined flow stages" in {
    def flow() = DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with StartEvent[DicomPart] {
      var state = 1
      override def onStart(): List[DicomPart] = {
        state = 0
        super.onStart()
      }
      override def onPart(part: DicomPart): List[DicomPart] = {
        state shouldBe 0
        part :: Nil
      }
    })

    val source = Source.single(DicomEndMarker).via(flow()).via(flow()).via(flow())

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case DicomEndMarker => true
      }
      .expectDicomComplete()
  }

  it should "be called once for flows with more than one capability using the onStart event" in {
    val flow = DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with GuaranteedDelimitationEvents[DicomPart] with StartEvent[DicomPart] {
      var nCalls = 0
      override def onStart(): List[DicomPart] = {
        nCalls += 1
        super.onStart()
      }
      override def onPart(part: DicomPart): List[DicomPart] = {
        nCalls shouldBe 1
        part :: Nil
      }
    })

    val source = Source.single(DicomEndMarker).via(flow)

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case DicomEndMarker => true
      }
      .expectDicomComplete()
  }

  "Prepending elements in combined flows" should "not insert everything at the beginning" in {
    val source = Source.single(1)
    val flow = Flow[Int].map(_ + 1).prepend(Source.single(0))

    val combined = source.via(flow).via(flow) // 1 -> 0 2 -> 0 1 3

    combined.runWith(TestSink.probe[Int])
      .request(3)
      .expectNextChainingPF {
        case 0 => true
      }
      .expectNextChainingPF {
        case 1 => true
      }
      .expectNextChainingPF {
        case 3 => true
      }
      .expectComplete()
  }
}
