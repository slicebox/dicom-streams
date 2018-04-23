package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.DicomParts.DicomPart
import se.nimsa.dicom.Tag
import se.nimsa.dicom.TestData._
import se.nimsa.dicom.streams.CollectFlow._
import se.nimsa.dicom.streams.ParseFlow.parseFlow
import se.nimsa.dicom.streams.TestUtils._

import scala.concurrent.ExecutionContextExecutor

class CollectFlowTest extends TestKit(ActorSystem("CollectFlowSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "A collect elements flow" should "first produce an elements part followed by the input dicom parts" in {
    val bytes = studyDate() ++ patientNameJohnDoe()
    val tags = Set(Tag.StudyDate, Tag.PatientName)
    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectFlow(tags, "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case CollectedElements(tag, _, elements) =>
          tag shouldBe "tag"
          elements should have length 2
          elements.head.header.tag shouldBe Tag.StudyDate
          elements(1).header.tag shouldBe Tag.PatientName
      }
      .expectHeader(Tag.StudyDate)
      .expectValueChunk()
      .expectHeader(Tag.PatientName)
      .expectValueChunk()
      .expectDicomComplete()
  }

  it should "produce an empty elements part when stream is empty" in {
    val bytes = ByteString.empty

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectFlow(Set.empty, "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case CollectedElements(_, _, elements) => elements shouldBe empty
      }
      .expectDicomComplete()
  }

  it should "produce an empty elements part when no relevant data elements are present" in {
    val bytes = patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectFlow(Set(Tag.Modality, Tag.SeriesInstanceUID), "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case CollectedElements(_, _, elements) => elements shouldBe empty
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
      .via(collectFlow(Set(Tag.StudyDate, Tag.PatientName), "tag", maxBufferSize = 1000))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case CollectedElements(tag, _, elements) =>
          tag shouldBe "tag"
          elements should have length 2
          elements.head.header.tag shouldBe Tag.StudyDate
          elements.last.header.tag shouldBe Tag.PatientName
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
      .via(collectFlow(_.tag == Tag.PatientName, _.tag > Tag.PixelData, "tag", maxBufferSize = 1000))

    source.runWith(TestSink.probe[DicomPart])
      .expectDicomError()
  }

}
