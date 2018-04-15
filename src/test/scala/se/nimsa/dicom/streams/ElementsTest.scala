package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{AsyncFlatSpecLike, BeforeAndAfterAll, Matchers}
import se.nimsa.dicom.Value.ByteStringExtension
import se.nimsa.dicom.streams.Elements._
import se.nimsa.dicom.{CharacterSets, Tag, TagPath, VR}

import scala.concurrent.ExecutionContextExecutor

class ElementsTest extends TestKit(ActorSystem("ElementsSpec")) with AsyncFlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  val studyDate = ValueElement(TagPath.fromTag(Tag.StudyDate), bigEndian = false, VR.DA, explicitVR = true, length = 8, ByteString(20041230).toValue)
  val patientName = ValueElement(TagPath.fromTag(Tag.PatientName), bigEndian = false, VR.PN, explicitVR = true, length = 8, ByteString("John^Doe").toValue)
  val sequence = SequenceElement(TagPath.fromSequence(Tag.DerivationCodeSequence), bigEndian = false, length = -1)
  val item = ItemElement(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1), bigEndian = false, index = 1, length = -1)
  val sequencePatientID = ValueElement(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1).thenTag(Tag.PatientID), bigEndian = false, VR.LO, explicitVR = true, length = 8, ByteString("12345678").toValue)
  val itemDelim = ItemDelimitationElement(item.tagPath, bigEndian = false)
  val sequenceDelim = SequenceDelimitationElement(TagPath.fromSequence(Tag.DerivationCodeSequence), bigEndian = false)
  val attr = Elements(CharacterSets.defaultOnly, List(studyDate, sequence, item, sequencePatientID, itemDelim, sequenceDelim, patientName))

  "Elements" should "return an existing tag" in {
    attr(Tag.PatientName) shouldBe Some(patientName)
    attr(TagPath.fromTag(Tag.PatientName)) shouldBe Some(patientName)
    attr(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1).thenTag(Tag.PatientID)) shouldBe Some(sequencePatientID)
  }

  it should "return a nested elements" in {
    attr.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence)) shouldBe Elements(CharacterSets.defaultOnly, List(sequence, item, sequencePatientID, itemDelim, sequenceDelim))
    attr.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1)) shouldBe Elements(CharacterSets.defaultOnly, List(item, sequencePatientID, itemDelim))
  }


}
