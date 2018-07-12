package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.data.Elements.{SequenceDelimitationElement, SequenceElement, _}
import se.nimsa.dicom.data.{Tag, UID, _}
import se.nimsa.dicom.streams.ElementSink.elementSink

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}

class ElementSinkTest extends TestKit(ActorSystem("ElementSinkSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "An element sink" should "aggregate streamed elements into an Elements" in {
    val elementList = List(
      ValueElement.fromString(Tag.TransferSyntaxUID, UID.ExplicitVRLittleEndian),
      ValueElement.fromString(Tag.StudyDate, "20040329"),
      SequenceElement(Tag.DerivationCodeSequence, indeterminateLength),
      ItemElement(1, indeterminateLength),
      ValueElement.fromString(Tag.StudyDate, "20040329"),
      ItemDelimitationElement(1),
      ItemElement(2, indeterminateLength),
      SequenceElement(Tag.DerivationCodeSequence, indeterminateLength),
      ItemElement(1, indeterminateLength),
      ValueElement.fromString(Tag.StudyDate, "20040329"),
      ItemDelimitationElement(1),
      SequenceDelimitationElement(),
      ItemDelimitationElement(2),
      SequenceDelimitationElement(),
      ValueElement.fromString(Tag.PatientName, "Doe^John"),
      FragmentsElement(Tag.PixelData, VR.OB),
      FragmentElement(1, 4, Value(ByteString(1, 2, 3, 4))),
      FragmentElement(2, 4, Value(ByteString(1, 2, 3, 4))),
      SequenceDelimitationElement())

    val elements = Await.result(Source(elementList).runWith(elementSink), 5.seconds)

    elements.toElements shouldBe elementList
  }

  it should "handle zero length values, fragments, sequences and items" in {
    val elementList = List(
      ValueElement.fromString(Tag.StudyDate, ""),
      SequenceElement(Tag.DerivationCodeSequence, indeterminateLength),
      SequenceDelimitationElement(),
      SequenceElement(Tag.DerivationCodeSequence, 0),
      SequenceDelimitationElement(marker = true),
      SequenceElement(Tag.DerivationCodeSequence, indeterminateLength),
      ItemElement(1, indeterminateLength),
      ItemDelimitationElement(1),
      ItemElement(2, 0),
      ItemDelimitationElement(2, marker = true),
      SequenceDelimitationElement(),
      FragmentsElement(Tag.PixelData, VR.OB),
      FragmentElement(1, 0, Value.empty),
      SequenceDelimitationElement(),
      FragmentsElement(Tag.PixelData, VR.OB),
      SequenceDelimitationElement())

    val elements = Await.result(Source(elementList).runWith(elementSink), 5.seconds)

    elements.toElements shouldBe elementList
  }

  it should "convert an empty offsets table item to and empty list of offsets" in {
    val elementList = List(
      FragmentsElement(Tag.PixelData, VR.OB),
      FragmentElement(1, 0, Value.empty),
      FragmentElement(2, 0, Value(ByteString(1, 2, 3, 4l))),
      SequenceDelimitationElement())

    val elements = Await.result(Source(elementList).runWith(elementSink), 5.seconds)
    val fragments = elements.getFragments(Tag.PixelData).get

    fragments.offsets shouldBe defined
    fragments.offsets.get shouldBe empty
  }

  it should "map an offsets table to a list of offsets" in {
    val elementList = List(
      FragmentsElement(Tag.PixelData, VR.OB),
      FragmentElement(1, 0, Value(intToBytesLE(1) ++ intToBytesLE(2) ++ intToBytesLE(3) ++ intToBytesLE(4))),
      SequenceDelimitationElement())

    val elements = Await.result(Source(elementList).runWith(elementSink), 5.seconds)
    val fragments = elements.getFragments(Tag.PixelData).get

    fragments.offsets shouldBe defined
    fragments.offsets.get shouldBe List(1, 2, 3, 4)
  }
}
