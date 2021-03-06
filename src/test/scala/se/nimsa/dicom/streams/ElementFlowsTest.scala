package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.data.Elements._
import se.nimsa.dicom.data.TagPath.EmptyTagPath
import se.nimsa.dicom.data._
import se.nimsa.dicom.data.TestData._
import se.nimsa.dicom.streams.ElementFlows._
import se.nimsa.dicom.streams.TestUtils._

import scala.concurrent.ExecutionContextExecutor

class ElementFlowsTest extends TestKit(ActorSystem("ElementFlowsSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "A DICOM elements flow" should "combine headers and value chunks into elements" in {
    val bytes = patientNameJohnDoe() ++ studyDate()

    val source = Source.single(bytes)
      .via(ParseFlow())
      .via(elementFlow)

    source.runWith(TestSink.probe[Element])
      .expectElement(Tag.PatientName)
      .expectElement(Tag.StudyDate)
      .expectDicomComplete()
  }

  it should "combine items in fragments into fragment elements" in {
    val bytes = pixeDataFragments() ++ item(4) ++ ByteString(1, 2, 3, 4) ++ item(4) ++ ByteString(5, 6, 7, 8) ++ sequenceDelimitation()

    val source = Source.single(bytes)
      .via(ParseFlow())
      .via(elementFlow)

    source.runWith(TestSink.probe[Element])
      .expectFragments(Tag.PixelData)
      .expectFragment(4)
      .expectFragment(4)
      .expectSequenceDelimitation(marker = false)
      .expectDicomComplete()
  }

  it should "handle elements and fragments of zero length" in {
    val bytes = ByteString(8, 0, 32, 0, 68, 65, 0, 0) ++ patientNameJohnDoe() ++
      pixeDataFragments() ++ item(0) ++ item(4) ++ ByteString(5, 6, 7, 8) ++ sequenceDelimitation()

    val source = Source.single(bytes)
      .via(ParseFlow())
      .via(elementFlow)

    source.runWith(TestSink.probe[Element])
      .expectElement(Tag.StudyDate, ByteString.empty)
      .expectElement(Tag.PatientName, ByteString("John^Doe"))
      .expectFragments(Tag.PixelData)
      .expectFragment(0)
      .expectFragment(4)
      .expectSequenceDelimitation(marker = false)
      .expectDicomComplete()
  }

  it should "handle determinate length sequences and items" in {
    val bytes =
      sequence(Tag.DerivationCodeSequence, 24) ++ item(16) ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(ParseFlow())
      .via(elementFlow)

    source.runWith(TestSink.probe[Element])
      .expectSequence(Tag.DerivationCodeSequence, 24)
      .expectItem(1, 16)
      .expectElement(Tag.PatientName)
      .expectItemDelimitation(1, marker = true)
      .expectSequenceDelimitation(marker = true)
      .expectDicomComplete()
  }

  "The tag path flow" should "pair elements with their respective tag paths" in {
    val bytes = preamble ++ fmiGroupLength(transferSyntaxUID()) ++ transferSyntaxUID() ++
      studyDate() ++
      sequence(Tag.DerivationCodeSequence) ++ item() ++ patientNameJohnDoe() ++ itemDelimitation() ++ sequenceDelimitation() ++
      pixeDataFragments() ++ item(4) ++ ByteString(1, 2, 3, 4) ++ sequenceDelimitation()

    val source = Source.single(bytes)
      .via(ParseFlow())
      .via(elementFlow)
      .via(tagPathFlow)

    source.runWith(TestSink.probe[(TagPath, Element)])
      .request(1)
      .expectNextChainingPF {
        case (EmptyTagPath, PreambleElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: ValueElement) if e.tag == Tag.FileMetaInformationGroupLength && tp == TagPath.fromTag(Tag.FileMetaInformationGroupLength) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: ValueElement) if e.tag == Tag.TransferSyntaxUID && tp == TagPath.fromTag(Tag.TransferSyntaxUID) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: ValueElement) if e.tag == Tag.StudyDate && tp == TagPath.fromTag(Tag.StudyDate) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: SequenceElement) if e.tag == Tag.DerivationCodeSequence && tp == TagPath.fromSequence(Tag.DerivationCodeSequence) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: ItemElement) if e.index == 1 && tp == TagPath.fromItem(Tag.DerivationCodeSequence, 1) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: ValueElement) if e.tag == Tag.PatientName && tp == TagPath.fromItem(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientName) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: ItemDelimitationElement) if e.index == 1 && tp == TagPath.fromItemEnd(Tag.DerivationCodeSequence, 1) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, _: SequenceDelimitationElement) if tp == TagPath.fromSequenceEnd(Tag.DerivationCodeSequence) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: FragmentsElement) if e.tag == Tag.PixelData && tp == TagPath.fromTag(Tag.PixelData) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, e: FragmentElement) if e.index == 1 && tp == TagPath.fromTag(Tag.PixelData) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (tp: TagPath, _: SequenceDelimitationElement) if tp == TagPath.fromTag(Tag.PixelData) => true
      }
      .request(1)
      .expectComplete()
  }

  it should "handle determinate length sequences and items" in {
    val bytes =
      sequence(Tag.DerivationCodeSequence, 24) ++ item(16) ++ patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(ParseFlow())
      .via(elementFlow)
      .via(tagPathFlow)

    source.runWith(TestSink.probe[(TagPath, Element)])
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, e: ItemElement) if e.index == 1 => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: ValueElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, e: ItemDelimitationElement) if e.marker && e.index == 1 => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, e: SequenceDelimitationElement) if e.marker => true
      }
      .request(1)
      .expectComplete()
  }

  it should "handle zero length values, sequence and items" in {
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

    val source = Source(elementList)
      .via(tagPathFlow)

    source.runWith(TestSink.probe[(TagPath, Element)])
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, e: ValueElement) if e.length == 0 => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceDelimitationElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceDelimitationElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: ItemElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: ItemDelimitationElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: ItemElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: ItemDelimitationElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceDelimitationElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: FragmentsElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: FragmentElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceDelimitationElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: FragmentsElement) => true
      }
      .request(1)
      .expectNextChainingPF {
        case (_: TagPath, _: SequenceDelimitationElement) => true
      }
      .request(1)
      .expectComplete()
  }

}
