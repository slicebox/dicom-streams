package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{AsyncFlatSpecLike, BeforeAndAfterAll, Matchers}
import se.nimsa.dicom._

import scala.concurrent.ExecutionContextExecutor

class ElementsTest extends TestKit(ActorSystem("ElementsSpec")) with AsyncFlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  val studyDate = Element(TagPath.fromTag(Tag.StudyDate), bigEndian = false, VR.DA, explicitVR = true, length = 8, ByteString(20041230))
  val patientName = Element(TagPath.fromTag(Tag.PatientName), bigEndian = false, VR.PN, explicitVR = true, length = 8, ByteString("John^Doe"))
  val sequencePatientID = Element(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID), bigEndian = false, VR.LO, explicitVR = true, length = 8, ByteString("12345678"))
  val attr = Elements(CharacterSets.defaultOnly, List(studyDate, sequencePatientID, patientName))

  "Elements" should "return an existing tag" in {
    attr(Tag.PatientName) shouldBe Some(patientName)
    attr(TagPath.fromTag(Tag.PatientName)) shouldBe Some(patientName)
    attr(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe Some(sequencePatientID)
  }

  it should "return a nested elements" in {
    attr.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence)) shouldBe Elements(CharacterSets.defaultOnly, List(sequencePatientID))
    attr.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence, 1)) shouldBe Elements(CharacterSets.defaultOnly, List(sequencePatientID))
  }


}
