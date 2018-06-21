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

    val elements = Await.result(
      Source(elementList)
        .runWith(elementSink),
      5.seconds)

    println(elements)
    println()

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

    val elements = Await.result(
      Source(elementList)
        .runWith(elementSink),
      5.seconds)

    println(elements)

    elements.toElements shouldBe elementList
  }

}
