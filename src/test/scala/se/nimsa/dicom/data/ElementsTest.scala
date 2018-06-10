package se.nimsa.dicom.data

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{AsyncFlatSpecLike, BeforeAndAfterAll, Matchers}
import se.nimsa.dicom.data.DicomParsing.{defaultCharacterSet, systemZone}
import se.nimsa.dicom.data.Elements._
import se.nimsa.dicom.data.TestData.{studyDate => testStudyDate, _}
import se.nimsa.dicom.streams.ElementFolds._
import se.nimsa.dicom.streams.ParseFlow.parseFlow

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}

class ElementsTest extends TestKit(ActorSystem("ElementsSpec")) with AsyncFlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  val studyDate: ValueElement = ValueElement(Tag.StudyDate, ByteString("20041230"))
  val patientName: ValueElement = ValueElement(Tag.PatientName, ByteString("John^Doe"))
  val patientID1: ValueElement = ValueElement(Tag.PatientID, ByteString("12345678"))
  val patientID2: ValueElement = ValueElement(Tag.PatientID, ByteString("87654321"))
  val patientID3: ValueElement = ValueElement(Tag.PatientID, ByteString("18273645"))
  val seq: Sequence = Sequence(Tag.DerivationCodeSequence, indeterminateLength, bigEndian = false, explicitVR = true, Array(
    Item(indeterminateLength, bigEndian = false, Elements(defaultCharacterSet, systemZone, SortedMap(Tag.PatientID -> patientID1))),
    Item(indeterminateLength, bigEndian = false, Elements(defaultCharacterSet, systemZone, SortedMap(Tag.PatientID -> patientID2)))
  ))

  val elements = Elements(defaultCharacterSet, systemZone, SortedMap(
    Tag.StudyDate -> studyDate,
    Tag.DerivationCodeSequence -> seq,
    Tag.PatientName -> patientName))

  "Elements" should "return an existing tag" in {
    elements(Tag.PatientName) shouldBe Some(patientName)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe Some(patientID1)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 2).thenTag(Tag.PatientID)) shouldBe Some(patientID2)
  }

  it should "return elements based on tag condition" in {
    val elements2 = elements(Tag.PatientID) = patientID3
    elements2.filterTags(_ == Tag.PatientID) shouldBe Elements(elements.characterSets, elements.zoneOffset, SortedMap(Tag.PatientID -> patientID3))
  }

  it should "return elements based on tag, element condition" in {
    val elements2 = elements(Tag.PatientID) = patientID3
    elements2.filter { case (tag, _) => tag == Tag.PatientID } shouldBe Elements(elements.characterSets, elements.zoneOffset, SortedMap(Tag.PatientID -> patientID3))
  }

  it should "return a nested elements" in {
    elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 1)) shouldBe Some(Elements(elements.characterSets, elements.zoneOffset, SortedMap(Tag.PatientID -> patientID1)))
    elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 2)) shouldBe Some(Elements(elements.characterSets, elements.zoneOffset, SortedMap(Tag.PatientID -> patientID2)))
  }

  it should "remove element if present" in {
    elements.remove(Tag.DerivationCodeSequence) shouldBe Elements(elements.characterSets, elements.zoneOffset, SortedMap(
      Tag.StudyDate -> studyDate,
      Tag.PatientName -> patientName))
    elements.remove(Tag.PatientName) shouldBe elements.copy(data = elements.data - Tag.PatientName)
    elements.remove(Tag.StudyDate) shouldBe elements.copy(data = elements.data - Tag.StudyDate)
    elements.remove(Tag.Modality) shouldBe elements
  }

  it should "insert elements in the correct position" in {
    val characterSets = ValueElement(Tag.SpecificCharacterSet, ByteString("CS1 "))
    val modality = ValueElement(Tag.Modality, ByteString("NM"))
    elements.update(Tag.PatientID, patientID3).data shouldBe SortedMap(
      Tag.StudyDate -> studyDate,
      Tag.DerivationCodeSequence -> seq,
      Tag.PatientName -> patientName,
      Tag.PatientID -> patientID3)
    elements.update(TagPath.fromTag(Tag.SpecificCharacterSet), characterSets).data shouldBe SortedMap(
      Tag.SpecificCharacterSet -> characterSets,
      Tag.StudyDate -> studyDate,
      Tag.DerivationCodeSequence -> seq,
      Tag.PatientName -> patientName)
    elements.update(TagPath.fromTag(Tag.Modality), modality).data shouldBe SortedMap(
      Tag.StudyDate -> studyDate,
      Tag.Modality -> modality,
      Tag.DerivationCodeSequence -> seq,
      Tag.PatientName -> patientName)
  }

  it should "overwrite element if already present" in {
    val newPatientName = patientName.setValue(Value(ByteString("Jane^Doe")))
    val updated = elements.update(Tag.PatientName, newPatientName)

    updated.size shouldBe elements.size
    updated.getElement(Tag.PatientName).get.value.bytes.utf8String shouldBe "Jane^Doe"
  }

  it should "update character sets" in {
    val updatedCs = elements.updateCharacterSets(CharacterSets(ByteString("\\ISO 2022 IR 127"))).characterSets
    updatedCs.charsetNames shouldBe Seq("", "ISO 2022 IR 127")
  }

  it should "aggregate the bytes of all its elements" in {
    val bytes = fmiGroupLength(transferSyntaxUID()) ++ transferSyntaxUID() ++ // FMI
      testStudyDate() ++
      sequence(Tag.DerivationCodeSequence) ++ item() ++ testStudyDate() ++ itemEnd() ++ item() ++ // sequence
      sequence(Tag.DerivationCodeSequence) ++ item() ++ testStudyDate() ++ itemEnd() ++ sequenceEnd() ++ // nested sequence (determinate length)
      itemEnd() ++ sequenceEnd() ++
      patientNameJohnDoe() ++ // attribute
      pixeDataFragments() ++ fragment(4) ++ ByteString(1, 2, 3, 4) ++ fragmentsEnd()

    val elements = Await.result(
      Source.single(bytes)
        .via(parseFlow)
        .via(elementsFlow)
        .runWith(elementsSink),
      5.seconds)

    elements.toBytes shouldBe bytes
  }

  it should "return an empty byte string when aggregating bytes with no data" in {
    Elements.empty().toBytes shouldBe ByteString.empty
  }

  it should "render an informative string representation" in {
    val s = elements.toString
    s.count(_.toString == System.lineSeparator) shouldBe 9
  }
}
