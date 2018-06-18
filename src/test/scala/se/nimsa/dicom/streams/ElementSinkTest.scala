package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.data.Elements.Element
import se.nimsa.dicom.data.Tag
import se.nimsa.dicom.data.TestData._
import se.nimsa.dicom.streams.ElementFlows.elementsFlow
import se.nimsa.dicom.streams.TestUtils._

import scala.concurrent.ExecutionContextExecutor

class ElementSinkTest extends TestKit(ActorSystem("ElementFoldsSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "A DICOM elements flow" should "combine headers and value chunks into elements" in {
    val bytes = patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(elementsFlow)

    source.runWith(TestSink.probe[Element])
      .expectElement(Tag.PatientName)
      .expectElement(Tag.StudyDate)
      .expectDicomComplete()
  }

  it should "combine items in fragments into fragment elements" in {
    val bytes = pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragment(4) ++ ByteString(5, 6, 7, 8) ++ fragmentsEnd()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(elementsFlow)

    source.runWith(TestSink.probe[Element])
      .expectFragments(Tag.PixelData)
      .expectFragment(4)
      .expectFragment(4)
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

  it should "handle attributes and fragments of zero length" in {
    val bytes = ByteString(8, 0, 32, 0, 68, 65, 0, 0) ++ patientNameJohnDoe() ++
      pixeDataFragments() ++ fragment(0) ++ fragment(4) ++ ByteString(5, 6, 7, 8) ++ fragmentsEnd()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(elementsFlow)

    source.runWith(TestSink.probe[Element])
      .expectElement(Tag.StudyDate, ByteString.empty)
      .expectElement(Tag.PatientName, ByteString("John^Doe"))
      .expectFragments(Tag.PixelData)
      .expectFragment(0)
      .expectFragment(4)
      .expectSequenceDelimitation()
      .expectDicomComplete()
  }

}
