package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{AsyncFlatSpecLike, BeforeAndAfterAll, Matchers}
import se.nimsa.dicom.{CharacterSets, Tag, TagPath, VR}
import se.nimsa.dicom.streams.AttributeFolds.{Attributes, _}
import se.nimsa.dicom.Value.ByteStringExtension

import scala.concurrent.ExecutionContextExecutor

class AttributeFoldsTest extends TestKit(ActorSystem("DicomAttributesSinkSpec")) with AsyncFlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  val studyDate = ValueAttribute(TagPath.fromTag(Tag.StudyDate), bigEndian = false, VR.DA, ByteString(20041230).toValue)
  val patientName = ValueAttribute(TagPath.fromTag(Tag.PatientName), bigEndian = false, VR.PN, ByteString("John^Doe").toValue)
  val sequence = SequenceAttribute(TagPath.fromSequence(Tag.DerivationCodeSequence), bigEndian =  false)
  val item = ItemAttribute(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1), bigEndian = false)
  val sequencePatientID = ValueAttribute(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1).thenTag(Tag.PatientID), bigEndian = false, VR.LO, ByteString("12345678").toValue)
  val itemDelim = ItemDelimitationAttribute(item.tagPath, bigEndian = false)
  val sequenceDelim = SequenceDelimitationAttribute(TagPath.fromSequence(Tag.DerivationCodeSequence), bigEndian =  false)
  val attr = Attributes(CharacterSets.defaultOnly, List(studyDate, sequence, item, sequencePatientID, itemDelim, sequenceDelim, patientName))

  "Attributes" should "return an existing tag" in {
    attr(Tag.PatientName) shouldBe Some(patientName)
    attr(TagPath.fromTag(Tag.PatientName)) shouldBe Some(patientName)
    attr(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1).thenTag(Tag.PatientID)) shouldBe Some(sequencePatientID)
  }

  it should "return a nested Attributes" in {
    attr.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence)) shouldBe Attributes(CharacterSets.defaultOnly, List(sequence, item, sequencePatientID, itemDelim, sequenceDelim))
    attr.sequence(TagPath.fromSequence(Tag.DerivationCodeSequence).thenItem(1)) shouldBe Attributes(CharacterSets.defaultOnly, List(item, sequencePatientID, itemDelim))
  }

}
