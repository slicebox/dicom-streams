package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.DicomParts.{DicomPart, ElementsPart}
import se.nimsa.dicom.{Tag, TagPath}
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
    val tags: Set[TagPath] = Set(Tag.StudyDate, Tag.PatientName).map(TagPath.fromTag)
    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectFlow(tags, "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case e: ElementsPart =>
          e.label shouldBe "tag"
          e.data should have size 2
          e.sortedElements.head.element.header.tag shouldBe Tag.StudyDate
          e.sortedElements(1).element.header.tag shouldBe Tag.PatientName
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
        case e: ElementsPart => e.data shouldBe empty
      }
      .expectDicomComplete()
  }

  it should "produce an empty elements part when no relevant data elements are present" in {
    val bytes = patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(parseFlow)
      .via(collectFlow(Set(Tag.Modality, Tag.SeriesInstanceUID).map(TagPath.fromTag), "tag"))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case e: ElementsPart => e.data shouldBe empty
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
      .via(collectFlow(Set(Tag.StudyDate, Tag.PatientName).map(TagPath.fromTag), "tag", maxBufferSize = 1000))

    source.runWith(TestSink.probe[DicomPart])
      .request(1)
      .expectNextChainingPF {
        case e: ElementsPart =>
          e.label shouldBe "tag"
          e.data should have size 2
          e.sortedElements.head.element.header.tag shouldBe Tag.StudyDate
          e.sortedElements.last.element.header.tag shouldBe Tag.PatientName
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
