package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.{Tag, VR}
import se.nimsa.dicom.TestData.patientNameJohnDoe
import se.nimsa.dicom.streams.ParseFlow.parseFlow
import se.nimsa.dicom.streams.DicomParts.{DicomHeader, DicomValueChunk}

import scala.concurrent.ExecutionContextExecutor

class CollectFlowTest extends TestKit(ActorSystem("DicomCollectFlowSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "A collect attributes flow" should "first produce an attributes part followed by the input dicom parts" in {
    val bytes = studyDate() ++ patientNameJohnDoe()
    val tags = Set(Tag.StudyDate, Tag.PatientName)
    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectAttributesFlow(tags, "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case DicomAttributes(tag, attributes) =>
          tag shouldBe "tag"
          attributes should have length 2
          attributes.head.header.tag shouldBe Tag.StudyDate
          attributes(1).header.tag shouldBe Tag.PatientName
      }
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "produce an empty attributes part when stream is empty" in {
    val bytes = ByteString.empty

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectAttributesFlow(Set.empty, "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case DicomAttributes(_, attributes) => attributes shouldBe empty
      }
      .expectDicomComplete()
  }

  it should "produce an empty attributes part when no relevant attributes are present" in {
    val bytes = patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectAttributesFlow(Set(Tag.Modality, Tag.SeriesInstanceUID), "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case DicomAttributes(_, attributes) => attributes shouldBe empty
      }
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "apply the stop tag appropriately" in {
    val bytes = studyDate() ++ patientNameJohnDoe() ++ pixelData(2000)

    val source = Source.single(bytes)
      .via(new ParseFlow(chunkSize = 500))
      .via(collectAttributesFlow(Set(Tag.StudyDate, Tag.PatientName), "tag", maxBufferSize = 1000))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case DicomAttributes(tag, attributes) =>
          tag shouldBe "tag"
          attributes should have length 2
          attributes.head.header.tag shouldBe Tag.StudyDate
          attributes.last.header.tag shouldBe Tag.PatientName
      }
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectHeader(Tag.PixelData)
      .expectValueChunk()
      .expectValueChunk()
      .expectValueChunk()
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "fail if max buffer size is exceeded" in {
    val bytes = studyDate() ++ patientNameJohnDoe() ++ pixelData(2000)

    val source = Source.single(bytes)
      .via(new ParseFlow(chunkSize = 500))
      .via(collectAttributesFlow(_.tag == Tag.PatientName, _.tag > Tag.PixelData, "tag", maxBufferSize = 1000))

    source.runWith(TestSink.probe[DicomPart])
      .expectDicomError()
  }

  "DicomAttribute" should "update its value bytes" in {
    val updated = DicomAttribute(
      DicomHeader(Tag.PatientName, VR.PN, 8, isFmi = false, bigEndian = false, explicitVR = true),
      Seq(DicomValueChunk(bigEndian = false, patientNameJohnDoe().drop(8), last = true)))
      .withUpdatedValue(ByteString("ABC"))
    updated.length shouldBe 4
    updated.valueBytes shouldBe ByteString("ABC ")
  }

}
