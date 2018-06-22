package se.nimsa.dicom.data

import java.time.{LocalDate, ZoneOffset}

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

  def create(elements: ElementSet*): Elements = Elements(defaultCharacterSet, systemZone, elements.toVector)

  val studyDate: ValueElement = ValueElement.fromString(Tag.StudyDate, "20041230")
  val patientName: ValueElement = ValueElement.fromString(Tag.PatientName, "John^Doe")
  val patientID1: ValueElement = ValueElement.fromString(Tag.PatientID, "12345678")
  val patientID2: ValueElement = ValueElement.fromString(Tag.PatientID, "87654321")
  val patientID3: ValueElement = ValueElement.fromString(Tag.PatientID, "18273645")
  val seq: Sequence = Sequence(Tag.DerivationCodeSequence, indeterminateLength, bigEndian = false, explicitVR = true, List(
    Item(indeterminateLength, bigEndian = false, Elements(defaultCharacterSet, systemZone, Vector(patientID1))),
    Item(indeterminateLength, bigEndian = false, Elements(defaultCharacterSet, systemZone, Vector(patientID2)))
  ))

  val elements: Elements = create(studyDate, seq, patientName)

  "Elements" should "return an existing element" in {
    elements(Tag.PatientName) shouldBe Some(patientName)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)) shouldBe Some(patientID1)
    elements(TagPath.fromSequence(Tag.DerivationCodeSequence, 2).thenTag(Tag.PatientID)) shouldBe Some(patientID2)
  }

  it should "return None for missing element" in {
    elements(Tag.SeriesDate) shouldBe None
  }

  it should "return value elements only" in {
    elements.getElement(Tag.PatientName) shouldBe defined
    elements.getElement(Tag.SeriesDate) shouldBe empty
  }

  it should "return the value of an element" in {
    val value = elements.getValue(Tag.PatientName)
    value shouldBe defined
    value.get.length should be > 0
    elements.getValue(Tag.SeriesDate) shouldBe empty
  }

  it should "return the bytes of an element" in {
    elements.getBytes(Tag.PatientName) shouldBe defined
  }

  it should "return all strings in value with VM > 1" in {
    val elements = create(ValueElement.fromString(Tag.ImageType, """ORIGINAL\RECON TOMO"""))
    val strings = elements.getStrings(Tag.ImageType)
    strings should have length 2
    strings(1) shouldBe "RECON TOMO"
  }

  it should "return a concatenated string in value with VM > 1" in {
    val elements = create(ValueElement.fromString(Tag.ImageType, """ORIGINAL\RECON TOMO"""))
    val singleString = elements.getSingleString(Tag.ImageType)
    singleString shouldBe Some("""ORIGINAL\RECON TOMO""")
  }

  it should "return the first string of a value with VM > 1" in {
    val elements = create(ValueElement.fromString(Tag.ImageType, """ORIGINAL\RECON TOMO"""))
    val string = elements.getString(Tag.ImageType)
    string shouldBe Some("ORIGINAL")
  }

  it should "return shorts" in {
    val elements = create(ValueElement.fromString(Tag.ReferencedFrameNumber, """1\2\3"""))
    elements.getShorts(Tag.ReferencedFrameNumber) shouldBe Seq(1, 2, 3).map(_.toShort)
    elements.getShort(Tag.ReferencedFrameNumber) shouldBe Some(1.toShort)
  }

  it should "return ints" in {
    val elements = create(ValueElement.fromString(Tag.ReferencedFrameNumber, """1\2\3"""))
    elements.getInts(Tag.ReferencedFrameNumber) shouldBe Seq(1, 2, 3)
    elements.getInt(Tag.ReferencedFrameNumber) shouldBe Some(1)
  }

  it should "return longs" in {
    val elements = create(ValueElement.fromString(Tag.ReferencedFrameNumber, """1\2\3"""))
    elements.getLongs(Tag.ReferencedFrameNumber) shouldBe Seq(1L, 2L, 3L)
    elements.getLong(Tag.ReferencedFrameNumber) shouldBe Some(1L)
  }

  it should "return floats" in {
    val elements = create(ValueElement.fromString(Tag.ReferencedFrameNumber, """1\2\3"""))
    elements.getFloats(Tag.ReferencedFrameNumber) shouldBe Seq(1f, 2f, 3f)
    elements.getFloat(Tag.ReferencedFrameNumber) shouldBe Some(1f)
  }

  it should "return doubles" in {
    val elements = create(ValueElement.fromString(Tag.ReferencedFrameNumber, """1\2\3"""))
    elements.getDoubles(Tag.ReferencedFrameNumber) shouldBe Seq(1.0, 2.0, 3.0)
    elements.getDouble(Tag.ReferencedFrameNumber) shouldBe Some(1.0)
  }

  it should "return dates" in {
    val dates = Seq(LocalDate.parse("2005-01-01"), LocalDate.parse("2010-01-01"))
    val elements = create(ValueElement(Tag.StudyDate, Value.fromDates(VR.DA, dates)))
    elements.getDates(Tag.StudyDate) shouldBe dates
    elements.getDate(Tag.StudyDate) shouldBe dates.headOption
  }

  it should "return date times" in {
    val dates = Seq(LocalDate.parse("2005-01-01"), LocalDate.parse("2010-01-01"))
      .map(_.atStartOfDay(ZoneOffset.of("+04:00")))
    val elements = create(ValueElement(Tag.InstanceCoercionDateTime, Value.fromDateTimes(VR.DT, dates)))
    elements.getDateTimes(Tag.InstanceCoercionDateTime) shouldBe dates
    elements.getDateTime(Tag.InstanceCoercionDateTime) shouldBe dates.headOption
  }

  it should "return patient names" in {
    val names = Seq("Doe^John", "Doe^Jane")
    val patientNames = names.flatMap(PatientName.parse)
    val elements = create(ValueElement.fromString(Tag.PatientName, names.mkString("\\")))
    elements.getPatientNames(Tag.PatientName) shouldBe patientNames
    elements.getPatientName(Tag.PatientName) shouldBe patientNames.headOption
  }

  it should "return sequences" in {
    val seq = elements.getSequence(Tag.DerivationCodeSequence)
    seq shouldBe defined
    seq.get.tag shouldBe Tag.DerivationCodeSequence
  }

  it should "return items" in {
    val item = elements.getItem(Tag.DerivationCodeSequence, 1)
    item shouldBe defined
    elements.getItem(Tag.DerivationCodeSequence, 0) shouldBe empty
    elements.getItem(Tag.DerivationCodeSequence, 2) shouldBe defined
    elements.getItem(Tag.DerivationCodeSequence, 3) shouldBe empty
    item shouldBe elements.getSequence(Tag.DerivationCodeSequence).get.item(1)
  }

  it should "return nested elements" in {
    elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 1)) shouldBe Some(Elements(elements.characterSets, elements.zoneOffset, Vector(patientID1)))
    elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 2)) shouldBe Some(Elements(elements.characterSets, elements.zoneOffset, Vector(patientID2)))
    elements.getNested(Tag.DerivationCodeSequence, 1) shouldBe elements.getNested(TagPath.fromSequence(Tag.DerivationCodeSequence, 1))
  }

  it should "return fragments" in {
    val elements = create(Fragments(Tag.PixelData, VR.OB, bigEndian = false, explicitVR = true, Some(Nil),
      List(Fragment(4, Value(ByteString(1,2,3,4))))))
    elements.getFragments(Tag.PixelData) shouldBe defined
  }

  it should "return elements based on tag condition" in {
    val elements2 = elements.set(patientID3)
    elements2.filter(_.tag == Tag.PatientID) shouldBe Elements(elements.characterSets, elements.zoneOffset, Vector(patientID3))
  }

  it should "remove element if present" in {
    elements.remove(Tag.DerivationCodeSequence) shouldBe Elements(elements.characterSets, elements.zoneOffset, Vector(studyDate, patientName))
    elements.remove(Tag.PatientName) shouldBe elements.copy(data = Vector(studyDate, seq))
    elements.remove(Tag.StudyDate) shouldBe elements.copy(data = Vector(seq, patientName))
    elements.remove(Tag.Modality) shouldBe elements
  }

  it should "insert elements in the correct position" in {
    val characterSets = ValueElement.fromString(Tag.SpecificCharacterSet, "CS1 ")
    val modality = ValueElement.fromString(Tag.Modality, "NM")
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

  it should "update zone offset" in {
    val updatedZo = elements.setZoneOffset(ZoneOffset.of("-06:00")).zoneOffset
    updatedZo.toString shouldBe "-06:00"
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
