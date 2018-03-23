package se.nimsa.dicom

import akka.util.ByteString

object TestData {

  val pad0 = ByteString(0)

  val preamble: ByteString = ByteString.fromArray(new Array[Byte](128)) ++ ByteString("DICM")

  def fmiGroupLength(fmis: ByteString*): ByteString = tagToBytesLE(0x00020000) ++ ByteString("UL") ++ shortToBytesLE(0x0004) ++ intToBytesLE(fmis.map(_.length).sum)

  // File Meta Information Version
  val fmiVersion: ByteString = tagToBytesLE(0x00020001) ++ ByteString('O', 'B', 0x00, 0x00) ++ intToBytesLE(0x00000002) ++ ByteString(0x00, 0x01)
  // (not conforming to standard)
  val fmiVersionImplicit: ByteString = tagToBytesLE(0x00020001) ++ intToBytesLE(0x00000002) ++ ByteString(0x00, 0x01)

  val mediaStorageSOPClassUID: ByteString = tagToBytesLE(0x00020002) ++ ByteString("UI") ++ shortToBytesLE(0x001A) ++ ByteString(UID.CTImageStorage) ++ pad0
  def sopClassUID(bigEndian: Boolean = false): ByteString = tagToBytes(0x00080016, bigEndian) ++ ByteString("UI") ++ shortToBytes(0x001A, bigEndian) ++ ByteString(UID.CTImageStorage) ++ pad0
  // (not conforming to standard)
  val mediaStorageSOPClassUIDImplicitLE: ByteString = tagToBytesLE(0x00020002) ++ intToBytesLE(0x0000001A) ++ ByteString(UID.CTImageStorage) ++ pad0
  val mediaStorageSOPInstanceUIDImplicitLE: ByteString = tagToBytesLE(0x00020003) ++ intToBytesLE(0x00000038) ++ ByteString("1.2.276.0.7230010.3.1.4.1536491920.17152.1480884676.735") ++ pad0

  def instanceCreatorUID(bigEndian: Boolean = false): ByteString = tagToBytes(0x00080014, bigEndian) ++ ByteString("UI") ++ shortToBytes(0x0014, bigEndian) ++ ByteString("1.2.840.113619.6.184")

  val mediaStorageSOPInstanceUID: ByteString = tagToBytesLE(0x00020003) ++ ByteString("UI") ++ shortToBytesLE(0x0038) ++ ByteString("1.2.276.0.7230010.3.1.4.1536491920.17152.1480884676.735") ++ pad0

  // Transfer Syntax UIDs
  val tsuidExplicitLE: ByteString = tagToBytesLE(0x00020010) ++ ByteString("UI") ++ shortToBytesLE(0x0014) ++ ByteString(UID.ExplicitVRLittleEndian) ++ pad0
  val tsuidExplicitBE: ByteString = tagToBytesLE(0x00020010) ++ ByteString("UI") ++ shortToBytesLE(0x0014) ++ ByteString(UID.ExplicitVRBigEndianRetired) ++ pad0
  val tsuidImplicit: ByteString = tagToBytesLE(0x00020010) ++ ByteString("UI") ++ shortToBytesLE(0x0012) ++ ByteString(UID.ImplicitVRLittleEndian) ++ pad0
  val tsuidDeflatedExplicitLE: ByteString = tagToBytesLE(0x00020010) ++ ByteString("UI") ++ shortToBytesLE(0x0016) ++ ByteString(UID.DeflatedExplicitVRLittleEndian)
  // (not conforming to standard)
  val tsuidExplicitLEImplicit: ByteString = tagToBytesLE(0x00020010) ++ intToBytesLE(0x00000014) ++ ByteString(UID.ExplicitVRLittleEndian) ++ pad0

  def patientNameJohnDoe(bigEndian: Boolean = false): ByteString = tagToBytes(0x00100010, bigEndian) ++ ByteString("PN") ++ shortToBytes(0x0008, bigEndian) ++ ByteString("John^Doe")
  val patientNameJohnDoeImplicit: ByteString = tagToBytesLE(0x00100010) ++ intToBytesLE(0x00000008) ++ ByteString("John^Doe")
  def emptyPatientName(bigEndian: Boolean = false): ByteString = tagToBytes(0x00100010, bigEndian) ++ ByteString("PN") ++ shortToBytes(0x0000, bigEndian)

  def patientID(bigEndian: Boolean = false): ByteString = tagToBytes(0x00100020, bigEndian) ++ ByteString("LO") ++ shortToBytes(0x0008, bigEndian) ++ ByteString("12345678")

  def studyDate(bigEndian: Boolean = false): ByteString = tagToBytes(0x00080020, bigEndian) ++ ByteString("DA") ++ shortToBytes(0x0008, bigEndian) ++ ByteString("19700101")

  def rows(bigEndian: Boolean = false): ByteString = tagToBytes(0x00280010, bigEndian) ++ ByteString("US") ++ shortToBytes(0x0002, bigEndian) ++ shortToBytes(512, bigEndian)
  def dataPointRows(bigEndian: Boolean = false): ByteString = tagToBytes(0x00289001, bigEndian) ++ ByteString("UL") ++ shortToBytes(0x0004, bigEndian) ++ intToBytes(1234, bigEndian)
  def apexPosition(bigEndian: Boolean = false): ByteString = tagToBytes(0x00209308, bigEndian) ++ ByteString("FD") ++ shortToBytes(0x0008, bigEndian) ++ doubleToBytes(math.Pi, bigEndian)

  def item(bigEndian: Boolean = false): ByteString = item(0xFFFFFFFF, bigEndian)
  def item(length: Int): ByteString = item(length, bigEndian = false)
  def item(length: Int, bigEndian: Boolean): ByteString = tagToBytes(0xFFFEE000, bigEndian) ++ intToBytes(length, bigEndian)
  def fragment(length: Int, bigEndian: Boolean = false): ByteString = item(length, bigEndian)

  def itemEnd(bigEndian: Boolean = false): ByteString = tagToBytes(0xFFFEE00D, bigEndian) ++ intToBytes(0x00000000, bigEndian)
  def sequenceEnd(bigEndian: Boolean = false): ByteString = tagToBytes(0xFFFEE0DD, bigEndian) ++ intToBytes(0x00000000, bigEndian)
  def fragmentsEnd(bigEndian: Boolean = false): ByteString = sequenceEnd(bigEndian)
  def sequenceEndNonZeroLength(bigEndian: Boolean = false): ByteString = tagToBytes(0xFFFEE0DD, bigEndian) ++ intToBytes(0x00000010, bigEndian)
  def pixeDataFragments(bigEndian: Boolean = false): ByteString = tagToBytes(0x7FE00010, bigEndian) ++ ByteString('O', 'W', 0, 0) ++ intToBytes(0xFFFFFFFF, bigEndian)

  def sequence(tag: Int, bigEndian: Boolean = false): ByteString = sequence(tag, 0xFFFFFFFF, bigEndian)
  def sequence(tag: Int, length: Int): ByteString = sequence(tag, length, bigEndian = false)
  def sequence(tag: Int, length: Int, bigEndian: Boolean): ByteString = tagToBytes(tag, bigEndian) ++ ByteString('S', 'Q', 0, 0) ++ intToBytes(length, bigEndian)

  def waveformSeqStart(bigEndian: Boolean = false): ByteString = tagToBytes(0x54000100, bigEndian) ++ ByteString('S', 'Q', 0, 0) ++ intToBytes(0xFFFFFFFF, bigEndian)

  def pixelData(length: Int, bigEndian: Boolean = false): ByteString = tagToBytes(0x7FE00010, bigEndian) ++ ByteString('O', 'W', 0, 0) ++ intToBytes(length, bigEndian) ++ ByteString(new Array[Byte](length))
  def waveformData(length: Int, bigEndian: Boolean = false): ByteString = tagToBytes(0x54001010, bigEndian) ++ ByteString('O', 'W', 0, 0) ++ intToBytes(length, bigEndian) ++ ByteString(new Array[Byte](length))

}
