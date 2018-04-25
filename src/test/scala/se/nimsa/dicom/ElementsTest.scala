package se.nimsa.dicom

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{AsyncFlatSpecLike, BeforeAndAfterAll, Matchers}
import se.nimsa.dicom.streams.ElementFolds.TpElement

import scala.concurrent.ExecutionContextExecutor

class ElementsTest extends TestKit(ActorSystem("ElementsSpec")) with AsyncFlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  val studyDate = Element(Tag.StudyDate, bigEndian = false, VR.DA, explicitVR = true, length = 8, ByteString(20041230))
  val patientName = Element(Tag.PatientName, bigEndian = false, VR.PN, explicitVR = true, length = 8, ByteString("John^Doe"))
  val patientID = Element(Tag.PatientID, bigEndian = false, VR.LO, explicitVR = true, length = 8, ByteString("12345678"))

  val studyDateTag: TagPath = TagPath.fromTag(Tag.StudyDate)
  val patientNameTag: TagPath = TagPath.fromTag(Tag.PatientName)
  val patientIDTag: TagPath = TagPath.fromTag(Tag.PatientID)
  val patientIDSeqTag: TagPath = TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)

  val elements = Elements(CharacterSets.defaultOnly, Map(
    studyDateTag -> studyDate,
    patientIDSeqTag -> patientID,
    patientNameTag -> patientName))

  "Elements" should "return an existing tag" in {
    elements(patientNameTag) shouldBe Some(patientName)
    elements(Tag.PatientName) shouldBe Some(patientName)
    elements(patientIDSeqTag) shouldBe Some(patientID)
  }

  it should "return elements based on tag path condition" in {
    val elements2 = elements(patientIDTag) = patientID
    elements2(_.endsWith(patientIDTag)) shouldBe Elements(elements.characterSets, Map(
      patientIDSeqTag -> patientID,
      patientIDTag -> patientID))
  }

  it should "return a nested elements" in {
    elements.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence)) shouldBe Elements(CharacterSets.defaultOnly, Map(patientIDSeqTag -> patientID))
    elements.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence, 1)) shouldBe Elements(CharacterSets.defaultOnly, Map(patientIDSeqTag -> patientID))
  }

  it should "remove element if present" in {
    elements.remove(_.startsWithSuperPath(TagPath.fromSequence(Tag.DerivationCodeSequence))) shouldBe Elements(elements.characterSets, Map(
      studyDateTag -> studyDate,
      patientNameTag -> patientName))
    elements.remove(_.endsWith(patientNameTag)) shouldBe Elements(elements.characterSets, Map(
      studyDateTag -> studyDate,
      patientIDSeqTag -> patientID))
    elements.remove(_.equals(studyDateTag)) shouldBe Elements(elements.characterSets, Map(
      patientIDSeqTag -> patientID,
      patientNameTag -> patientName))
    elements.remove(_.endsWith(TagPath.fromTag(Tag.Modality))) shouldBe elements
  }

  it should "insert elements in the correct position" in {
    val characterSets = Element(Tag.SpecificCharacterSet, bigEndian = false, VR.CS, explicitVR = true, length = 4, ByteString("CS1 "))
    val modality = Element(Tag.Modality, bigEndian = false, VR.CS, explicitVR = true, length = 2, ByteString("NM"))
    val characterSetsTag = TagPath.fromTag(Tag.SpecificCharacterSet)
    val modalityTag = TagPath.fromTag(Tag.Modality)
    elements.update(patientIDTag, patientID).data shouldBe Map(
      studyDateTag -> studyDate,
      patientIDSeqTag -> patientID,
      patientNameTag -> patientName,
      patientIDTag -> patientID)
    elements.update(TagPath.fromTag(Tag.SpecificCharacterSet), characterSets).data shouldBe Map(
      characterSetsTag -> characterSets,
      studyDateTag -> studyDate,
      patientIDSeqTag -> patientID,
      patientNameTag -> patientName)
    elements.update(TagPath.fromTag(Tag.Modality), modality).data shouldBe Map(
      studyDateTag -> studyDate,
      modalityTag -> modality,
      patientIDSeqTag -> patientID,
      patientNameTag -> patientName)
  }

  it should "overwrite element if already present" in {
    val newPatientName = patientName.copy(value = ByteString("Jane^Doe"))
    val updated = elements.update(patientNameTag, newPatientName)

    updated.data.size shouldBe elements.data.size
    updated.data.last._2.value.utf8String shouldBe "Jane^Doe"
  }

  it should "update character sets" in {
    val updatedCs = elements.updateCharacterSets(CharacterSets(ByteString("\\ISO 2022 IR 127"))).characterSets
    updatedCs.charsetNames shouldBe Seq("", "ISO 2022 IR 127")
  }

  it should "return properly sorted elements" in {
    elements.toList shouldBe List(
      TpElement(studyDateTag, studyDate),
      TpElement(patientIDSeqTag, patientID),
      TpElement(patientNameTag, patientName))
    elements.elements shouldBe List(studyDate, patientID, patientName)
    elements.tagPaths shouldBe List(studyDateTag, patientIDSeqTag, patientNameTag)
  }

  it should "aggregate the bytes of all its elements" in {
    Elements(CharacterSets.defaultOnly, Map(
      TagPath.fromTag(Tag.PatientName) -> Element(Tag.PatientName, bigEndian = false, VR.PN, explicitVR = true, length = 8, TestData.patientNameJohnDoe().drop(8)),
      TagPath.fromTag(Tag.PatientID) -> Element(Tag.PatientID, bigEndian = false, VR.LO, explicitVR = true, length = 8, TestData.patientID().drop(8))
    )).bytes shouldBe (TestData.patientNameJohnDoe() ++ TestData.patientID())
  }
}
