package se.nimsa.dicom.data

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import se.nimsa.dicom.data.DicomParsing.defaultCharacterSet
import se.nimsa.dicom.data.Elements._
import se.nimsa.dicom.data.TestData.{studyDate => testStudyDate, _}
import se.nimsa.dicom.streams.ElementFlows.elementFlow
import se.nimsa.dicom.streams.ElementSink._
import se.nimsa.dicom.streams.ParseFlow.parseFlow

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}

class ElementsTest extends TestKit(ActorSystem("ElementsSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  val studyDate: ValueElement = ValueElement(Tag.StudyDate, ByteString("20041230"))
  val patientName: ValueElement = ValueElement(Tag.PatientName, ByteString("John^Doe"))
  val patientID1: ValueElement = ValueElement(Tag.PatientID, ByteString("12345678"))
  val patientID2: ValueElement = ValueElement(Tag.PatientID, ByteString("87654321"))
  val patientID3: ValueElement = ValueElement(Tag.PatientID, ByteString("18273645"))
  val seq: Sequence = Sequence(Tag.DerivationCodeSequence, indeterminateLength, bigEndian = false, explicitVR = true, List(
    Item(indeterminateLength, bigEndian = false, Elements(defaultCharacterSet, systemZone, Vector(patientID1))),
    Item(indeterminateLength, bigEndian = false, Elements(defaultCharacterSet, systemZone, Vector(patientID2)))
  ))

  val elements = Elements(defaultCharacterSet, systemZone, Vector(studyDate, seq, patientName))

  "Elements" should "return an existing tag" in {
    elements(Tag.PatientName) shouldBe Some(patientName)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe Some(patientID1)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 2).thenTag(Tag.PatientID)) shouldBe Some(patientID2)
  }

  it should "return elements based on tag condition" in {
    val elements2 = elements.set(patientID3)
    elements2.filter(_.tag == Tag.PatientID) shouldBe Elements(elements.characterSets, elements.zoneOffset, Vector(patientID3))
  }

  it should "return a nested elements" in {
    elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 1)) shouldBe Some(Elements(elements.characterSets, elements.zoneOffset, Vector(patientID1)))
    elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 2)) shouldBe Some(Elements(elements.characterSets, elements.zoneOffset, Vector(patientID2)))
  }

  it should "remove element if present" in {
    elements.remove(Tag.DerivationCodeSequence) shouldBe Elements(elements.characterSets, elements.zoneOffset, Vector(studyDate, patientName))
    elements.remove(Tag.PatientName) shouldBe elements.copy(data = Vector(studyDate, seq))
    elements.remove(Tag.StudyDate) shouldBe elements.copy(data = Vector(seq, patientName))
    elements.remove(Tag.Modality) shouldBe elements
  }

  it should "insert elements in the correct position" in {
    val characterSets = ValueElement(Tag.SpecificCharacterSet, ByteString("CS1 "))
    val modality = ValueElement(Tag.Modality, ByteString("NM"))
    elements.set(patientID3).data shouldBe Vector(studyDate, seq, patientName, patientID3)
    elements.set(characterSets).data shouldBe Vector(characterSets, studyDate, seq, patientName)
    elements.set(modality).data shouldBe Vector(studyDate, modality, seq, patientName)
  }

  it should "overwrite element if already present" in {
    val newPatientName = patientName.setValue(Value(ByteString("Jane^Doe")))
    val updated = elements.set(newPatientName)

    updated.size shouldBe elements.size
    updated.getElement(Tag.PatientName).get.value.bytes.utf8String shouldBe "Jane^Doe"
  }

  it should "update character sets" in {
    val updatedCs = elements.setCharacterSets(CharacterSets(ByteString("\\ISO 2022 IR 127"))).characterSets
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
        .via(elementFlow)
        .runWith(elementSink),
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

  it should "return the specified element based on tag path" in {
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 2).thenTag(Tag.PatientID)) shouldBe Some(patientID2)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 3).thenTag(Tag.PatientID)) shouldBe None
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 3).thenTag(Tag.PatientID)) shouldBe None
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 2).thenTag(Tag.PatientName)) shouldBe None
    elements(TagPath.fromSequence(Tag.AbstractPriorCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe None
  }

  it should "not accept wildcards in tag paths" in {
    intercept[IllegalArgumentException] {
      elements(TagPath.fromSequence(Tag.DerivationCodeSequence).thenTag(Tag.PatientID))
    }
  }

  it should "return the specified seqeunce based on tag path" in {
    elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 1)) shouldBe seq.item(1).map(_.elements)
  }

  it should "update element specified by tag path" in {
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe Some(patientID1)
    val e2 = elements.set(TagPath.fromSequence(Tag.DerivationCodeSequence, 1), patientID2)
    e2(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe Some(patientID2)
  }
}
