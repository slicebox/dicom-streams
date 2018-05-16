package se.nimsa.dicom

import org.scalatest.{FlatSpecLike, Matchers}
import se.nimsa.dicom.TestData._


class DicomPartsTest extends FlatSpecLike with Matchers {

  import DicomParts._

  DicomHeader.getClass.getSimpleName should "should return a new header with modified length for explicitVR, LE" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(patientNameJohnDoe(), assumeBigEndian = false).get
    val header = DicomHeader(tag, vr, length, isFmi = false, bigEndian = false, explicitVR = true, patientNameJohnDoe().take(8))
    val updatedHeader = header.withUpdatedLength(5)

    updatedHeader.length shouldEqual 5
    updatedHeader.bytes.take(6) shouldEqual header.bytes.take(6)
    updatedHeader.bytes(6) shouldEqual 5
    updatedHeader.bytes(7) shouldEqual 0
  }

  it should "should return a new header with modified length for explicitVR, BE" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(patientNameJohnDoe(bigEndian = true), assumeBigEndian = true).get
    val header = DicomHeader(tag, vr, length, isFmi = false, bigEndian = true, explicitVR = true, patientNameJohnDoe(bigEndian = true).take(8))
    val updatedHeader = header.withUpdatedLength(5)

    updatedHeader.length shouldEqual 5
    updatedHeader.bytes.take(6) shouldEqual header.bytes.take(6)
    updatedHeader.bytes(6) shouldEqual 0
    updatedHeader.bytes(7) shouldEqual 5
  }

  it should "should return a new header with modified length for explicitVR, LE with 12-byte headers" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(pixelData(100), assumeBigEndian = false).get
    val header = DicomHeader(tag, vr, length, isFmi = false, bigEndian = false, explicitVR = true, pixelData(100).take(12))
    val updatedHeader = header.withUpdatedLength(200)

    updatedHeader.length shouldEqual 200
    updatedHeader.bytes.take(8) shouldEqual header.bytes.take(8)
    updatedHeader.bytes.slice(8, 12) shouldEqual intToBytesLE(200)
  }

  it should "should return a new header with modified length for explicitVR, BE with 12-byte headers" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(pixelData(100, bigEndian = true), assumeBigEndian = true).get
    val header = DicomHeader(tag, vr, length, isFmi = false, bigEndian = true, explicitVR = true, pixelData(100, bigEndian = true).take(12))
    val updatedHeader = header.withUpdatedLength(200)

    updatedHeader.length shouldEqual 200
    updatedHeader.bytes.take(8) shouldEqual header.bytes.take(8)
    updatedHeader.bytes.slice(8, 12) shouldEqual intToBytesBE(200)
  }

  it should "should return a new header with modified length for implicitVR, LE" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderImplicitVR(patientNameJohnDoe(explicitVR = false)).get
    val header = DicomHeader(tag, vr, length, isFmi = false, bigEndian = false, explicitVR = false, patientNameJohnDoe(explicitVR = false).take(8))
    val updatedHeader = header.withUpdatedLength(5)

    updatedHeader.length shouldEqual 5
    updatedHeader.bytes.take(4) shouldEqual header.bytes.take(4)
    updatedHeader.bytes(4) shouldEqual 5
    updatedHeader.bytes(5) shouldEqual 0
  }

  it should "create a valid explicit VR little endian byte sequence representation when constructed without explicit bytes" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(patientNameJohnDoe(), assumeBigEndian = false).get
    val bytes = DicomHeader(tag, vr, length, isFmi = false, bigEndian = false, explicitVR = true).bytes
    bytes shouldBe patientNameJohnDoe().take(8)
  }

  it should "create a valid implicit VR little endian byte sequence representation when constructed without explicit bytes" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(patientNameJohnDoe(), assumeBigEndian = false).get
    val bytes = DicomHeader(tag, vr, length, isFmi = false, bigEndian = false, explicitVR = false).bytes
    bytes shouldBe patientNameJohnDoe(explicitVR = false).take(8)
  }

  it should "create a valid explicit VR big endian byte sequence representation when constructed without explicit bytes" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(patientNameJohnDoe(bigEndian = true), assumeBigEndian = true).get
    val bytes = DicomHeader(tag, vr, length, isFmi = false, bigEndian = true, explicitVR = true).bytes
    bytes shouldBe patientNameJohnDoe(bigEndian = true).take(8)
  }

  it should "create a valid explicit VR little endian 12-byte representation when constructed without explicit bytes" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(pixelData(100), assumeBigEndian = false).get
    val bytes = DicomHeader(tag, vr, length, isFmi = false, bigEndian = false, explicitVR = true).bytes
    bytes shouldBe pixelData(100).take(12)
  }

  it should "create a valid explicit VR big endian 12-byte representation when constructed without explicit bytes" in {
    val (tag, vr, _, length) = DicomParsing.readHeaderExplicitVR(pixelData(100, bigEndian = true), assumeBigEndian = true).get
    val bytes = DicomHeader(tag, vr, length, isFmi = false, bigEndian = true, explicitVR = true).bytes
    bytes shouldBe pixelData(100, bigEndian = true).take(12)
  }

  "ElementsPart" should "aggregate the bytes of all its elements" in {
    new ElementsPart("label", CharacterSets.defaultOnly, Map(
      TagPath.fromTag(Tag.PatientName) -> Element.explicitLE(Tag.PatientName, VR.PN, patientNameJohnDoe().drop(8)),
      TagPath.fromTag(Tag.PatientID) -> Element.explicitLE(Tag.PatientID, VR.LO, patientID().drop(8))
    )).bytes shouldBe (patientNameJohnDoe() ++ patientID())
  }
}
