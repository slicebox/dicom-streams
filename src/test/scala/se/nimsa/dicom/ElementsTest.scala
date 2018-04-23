package se.nimsa.dicom

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{AsyncFlatSpecLike, BeforeAndAfterAll, Matchers}

import scala.concurrent.ExecutionContextExecutor

class ElementsTest extends TestKit(ActorSystem("ElementsSpec")) with AsyncFlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  val studyDate = Element(TagPath.fromTag(Tag.StudyDate), bigEndian = false, VR.DA, explicitVR = true, length = 8, ByteString(20041230))
  val patientName = Element(TagPath.fromTag(Tag.PatientName), bigEndian = false, VR.PN, explicitVR = true, length = 8, ByteString("John^Doe"))
  val sequencePatientID = Element(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID), bigEndian = false, VR.LO, explicitVR = true, length = 8, ByteString("12345678"))
  val elements = Elements(CharacterSets.defaultOnly, List(studyDate, sequencePatientID, patientName))

  "Elements" should "return an existing tag" in {
    elements(Tag.PatientName) shouldBe Some(patientName)
    elements(TagPath.fromTag(Tag.PatientName)) shouldBe Some(patientName)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe Some(sequencePatientID)
  }

  it should "return a nested elements" in {
    elements.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence)) shouldBe Elements(CharacterSets.defaultOnly, List(sequencePatientID))
    elements.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence, 1)) shouldBe Elements(CharacterSets.defaultOnly, List(sequencePatientID))
  }

  it should "remove element if present" in {
    elements.remove(_.startsWithSuperPath(TagPath.fromSequence(Tag.DerivationCodeSequence))) shouldBe Elements(elements.characterSets, List(studyDate, patientName))
    elements.remove(_.endsWith(TagPath.fromTag(Tag.PatientName))) shouldBe Elements(elements.characterSets, List(studyDate, sequencePatientID))
    elements.remove(_.equals(TagPath.fromTag(Tag.StudyDate))) shouldBe Elements(elements.characterSets, List(sequencePatientID, patientName))
    elements.remove(_.endsWith(TagPath.fromTag(Tag.Modality))) shouldBe elements
  }

  it should "insert elements in the correct position" in {
    val patientID = Element(TagPath.fromTag(Tag.PatientID), bigEndian = false, VR.LO, explicitVR = true, length = 6, ByteString("123456"))
    val characterSets = Element(TagPath.fromTag(Tag.SpecificCharacterSet), bigEndian = false, VR.CS, explicitVR = true, length = 4, ByteString("CS1 "))
    val modality = Element(TagPath.fromTag(Tag.Modality), bigEndian = false, VR.CS, explicitVR = true, length = 2, ByteString("NM"))
    elements.insert(patientID).elements shouldBe List(studyDate, sequencePatientID, patientName, patientID)
    elements.insert(characterSets).elements shouldBe List(characterSets, studyDate, sequencePatientID, patientName)
    elements.insert(modality).elements shouldBe List(studyDate, modality, sequencePatientID, patientName)
  }

  it should "overwrite element if already present" in {
    val newPatientName = patientName.copy(value = ByteString("Jane^Doe"))
    val updated = elements.insert(newPatientName)

    updated.elements.length shouldBe elements.elements.length
    updated.elements.last.value.utf8String shouldBe "Jane^Doe"
  }

}
