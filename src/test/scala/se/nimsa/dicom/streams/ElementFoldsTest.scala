package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.TestData._
import se.nimsa.dicom.streams.ElementFolds._
import se.nimsa.dicom.streams.TestUtils._
import se.nimsa.dicom.{Tag, TagPath}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}

class ElementFoldsTest extends TestKit(ActorSystem("ElementFoldsSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "A DICOM attributes flow" should "combine headers and value chunks into attributes" in {
    val bytes = patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(elementsFlow)

    source.runWith(TestSink.probe[TpElement])
      .expectElement(TagPath.fromTag(Tag.PatientName))
      .expectElement(TagPath.fromTag(Tag.StudyDate))
      .expectDicomComplete()
  }

  it should "combine items in fragments into fragment elements" in {
    val bytes = pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragment(4) ++ ByteString(5, 6, 7, 8) ++ fragmentsEnd()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(elementsFlow)

    source.runWith(TestSink.probe[TpElement])
      .expectElement(TagPath.fromTag(Tag.PixelData), ByteString(1,2,3,4))
      .expectElement(TagPath.fromTag(Tag.PixelData), ByteString(5,6,7,8))
      .expectDicomComplete()
  }

  it should "handle attributes and fragments of zero length" in {
    val bytes = ByteString(8, 0, 32, 0, 68, 65, 0, 0) ++ patientNameJohnDoe() ++
      pixeDataFragments() ++ fragment(0) ++ fragment(4) ++ ByteString(5, 6, 7, 8) ++ fragmentsEnd()

    val source = Source.single(bytes)
      .via(new ParseFlow())
      .via(elementsFlow)

    source.runWith(TestSink.probe[TpElement])
      .expectElement(TagPath.fromTag(Tag.StudyDate), ByteString.empty)
      .expectElement(TagPath.fromTag(Tag.PatientName), ByteString("John^Doe"))
      .expectElement(TagPath.fromTag(Tag.PixelData), ByteString.empty)
      .expectElement(TagPath.fromTag(Tag.PixelData), ByteString(5,6,7,8))
      .expectDicomComplete()
  }

  "The elements sink" should "aggregate all elements" in {
    val bytes = preamble ++ fmiGroupLength(transferSyntaxUID()) ++ transferSyntaxUID() ++ // FMI
      patientNameJohnDoe() ++ // attribute
      sequence(Tag.DerivationCodeSequence) ++ item() ++ studyDate() ++ itemEnd() ++ item() ++ // sequence
      sequence(Tag.DerivationCodeSequence, 24) ++ item(16) ++ studyDate() ++ // nested sequence (determinate length)
      itemEnd() ++ sequenceEnd() ++
      pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragmentsEnd()

    val elements = Await.result(
      Source.single(bytes)
      .via(new ParseFlow())
      .via(elementsFlow)
      .runWith(elementsSink), 5.seconds)

    elements.data should have size 8

    elements.tagPaths shouldBe List(
      TagPath.fromTag(Tag.FileMetaInformationGroupLength),
      TagPath.fromTag(Tag.TransferSyntaxUID),
      TagPath.fromSequence(Tag.DerivationCodeSequence),
      TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.StudyDate),
      TagPath.fromSequence(Tag.DerivationCodeSequence, 2).thenSequence(Tag.DerivationCodeSequence),
      TagPath.fromSequence(Tag.DerivationCodeSequence, 2).thenSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.StudyDate),
      TagPath.fromTag(Tag.PatientName),
      TagPath.fromTag(Tag.PixelData)
    )
  }

  it should "update the character sets field if element is present" in {
    val bytes = characterSetsJis() ++ studyDate() ++ patientNameJohnDoe()

    val elements = Await.result(
      Source.single(bytes)
        .via(new ParseFlow())
        .via(elementsFlow)
        .runWith(elementsSink), 5.seconds)

    elements.characterSets.charsetNames shouldBe Seq("ISO 2022 IR 13", "ISO 2022 IR 87")
  }

}
