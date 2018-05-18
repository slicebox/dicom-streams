package se.nimsa.dicom

import java.time.{LocalDate, ZoneOffset, ZonedDateTime}

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import se.nimsa.dicom.Element.{ComponentGroup, PatientName}
import se.nimsa.dicom.TestData._
import se.nimsa.dicom.VR.VR

class ElementTest extends FlatSpec with Matchers {

  def toElement(bigEndian: Boolean, vr: VR, bytes: ByteString) = Element(Tag.PatientID, bigEndian, vr, explicitVR = true, bytes.length, padToEvenLength(bytes, vr))

  "Formatting bytes into multiple strings" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.SH, ByteString.empty).toStrings() shouldBe Seq.empty
  }

  it should "throw an exception for null input" in {
    intercept[NullPointerException] {
      toElement(bigEndian = false, VR.SH, null).toStrings() shouldBe Seq.empty
    }
  }

  it should "split a string according to the DICOM multiple value delimiter" in {
    toElement(bigEndian = false, VR.SH, ByteString("one\\two\\three")).toStrings() shouldBe Seq("one", "two", "three")
  }

  it should "trim any characters at beginning and end" in {
    toElement(bigEndian = false, VR.SH, ByteString(0x20, 0x20, 0x20, 0x20, 0x41, 0x41, 0x20, 0x20, 0x20)).toStrings() shouldBe Seq("AA")
  }

  it should "trim any characters at or below 0x20 at beginning and end of each value" in {
    toElement(bigEndian = false, VR.SH, ByteString("  one \\ two \\three  ")).toStrings() shouldBe Seq("one", "two", "three")
  }

  it should "split and trim strings with multiple character set encodings" in {
    val nameBytes = ByteString(0x20, 0xD4, 0xCF, 0xC0, 0xDE, 0x5C, 0x20, 0xC0, 0xDB, 0xB3, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28, 0x4A, 0x5C, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x4A, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64, 0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x4A, 0x5C, 0x20, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B, 0x28, 0x4A)
    toElement(bigEndian = false, VR.SH, nameBytes).toStrings(new CharacterSets(Seq("ISO 2022 IR 13", "ISO 2022 IR 87"))) shouldBe Seq("ﾔﾏﾀﾞ", "ﾀﾛｳ=山田", "太郎=やまだ", "たろう")
  }

  "Formatting bytes into a single string" should "return empty string for empty byte string" in {
    toElement(bigEndian = false, VR.SH, ByteString.empty).toSingleString() shouldBe empty
  }

  it should "not split a string with DICOM multiple value delimiters" in {
    toElement(bigEndian = false, VR.SH, ByteString("one\\two\\three")).toSingleString() shouldBe "one\\two\\three"
  }

  it should "trim the string components" in {
    toElement(bigEndian = false, VR.SH, ByteString("   one two  ")).toSingleString() shouldBe "one two"
  }

  "Parsing int values" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.SL, ByteString.empty).toInts shouldBe Seq.empty
  }

  it should "parse multiple int values" in {
    toElement(bigEndian = false, VR.SL, intToBytesLE(1234) ++ intToBytesLE(1234567890)).toInts shouldBe Seq(1234, 1234567890)
  }

  it should "return int values for all numerical VRs" in {
    toElement(bigEndian = false, VR.FL, floatToBytes(math.Pi.toFloat, bigEndian = false)).toInts shouldBe Seq(3)
    toElement(bigEndian = false, VR.FD, doubleToBytes(math.Pi, bigEndian = false)).toInts shouldBe Seq(3)
    toElement(bigEndian = false, VR.SS, shortToBytes(-3, bigEndian = false)).toInts shouldBe Seq(-3)
    toElement(bigEndian = false, VR.US, shortToBytes(-3, bigEndian = false)).toInts shouldBe Seq((1 << 16) - 3)
    toElement(bigEndian = false, VR.SL, intToBytes(-3, bigEndian = false)).toInts shouldBe Seq(-3)
    toElement(bigEndian = false, VR.UL, intToBytes(-3, bigEndian = false)).toInts shouldBe Seq(-3)
    toElement(bigEndian = false, VR.DS, ByteString("3.1415")).toInts shouldBe Seq(3)
    toElement(bigEndian = false, VR.IS, ByteString("-3")).toInts shouldBe Seq(-3)
    toElement(bigEndian = false, VR.AT, ByteString("-3")).toInts shouldBe empty
  }

  "Parsing a single int value" should "return the first entry among multiple values" in {
    toElement(bigEndian = false, VR.SL, intToBytesLE(1234) ++ intToBytesLE(1234567890)).toInt shouldBe Some(1234)
  }

  it should "return None if no entry exists" in {
    toElement(bigEndian = false, VR.SL, ByteString.empty).toInt shouldBe None
  }

  "Parsing long values" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.SL, ByteString.empty).toLongs shouldBe Seq.empty
  }

  it should "parse multiple long values" in {
    toElement(bigEndian = false, VR.SL, intToBytesLE(1234) ++ intToBytesLE(1234567890)).toLongs shouldBe Seq(1234L, 1234567890L)
  }

  it should "return long values for all numerical VRs" in {
    toElement(bigEndian = false, VR.FL, floatToBytes(math.Pi.toFloat, bigEndian = false)).toLongs shouldBe Seq(3L)
    toElement(bigEndian = false, VR.FD, doubleToBytes(math.Pi, bigEndian = false)).toLongs shouldBe Seq(3L)
    toElement(bigEndian = false, VR.SS, shortToBytes(-3, bigEndian = false)).toLongs shouldBe Seq(-3L)
    toElement(bigEndian = false, VR.US, shortToBytes(-3, bigEndian = false)).toLongs shouldBe Seq((1 << 16) - 3L)
    toElement(bigEndian = false, VR.SL, intToBytes(-3, bigEndian = false)).toLongs shouldBe Seq(-3L)
    toElement(bigEndian = false, VR.UL, intToBytes(-3, bigEndian = false)).toLongs shouldBe Seq((1L << 32) - 3L)
    toElement(bigEndian = false, VR.DS, ByteString("3.1415")).toLongs shouldBe Seq(3L)
    toElement(bigEndian = false, VR.IS, ByteString("-3")).toLongs shouldBe Seq(-3L)
    toElement(bigEndian = false, VR.AT, ByteString("-3")).toLongs shouldBe empty
  }

  "Parsing a single long value" should "return the first entry among multiple values" in {
    toElement(bigEndian = false, VR.SL, longToBytesLE(1234L) ++ longToBytesLE(1234567890L)).toLong shouldBe Some(1234L)
  }

  it should "return None if no entry exists" in {
    toElement(bigEndian = false, VR.SL, ByteString.empty).toLong shouldBe None
  }

  "Parsing short values" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.SS, ByteString.empty).toShorts shouldBe Seq.empty
  }

  it should "parse short values" in {
    toElement(bigEndian = false, VR.SS, shortToBytesLE(1234) ++ shortToBytesLE(12345)).toShorts shouldBe Seq(1234.toShort, 12345.toShort)
  }

  it should "return short values for all numerical VRs" in {
    toElement(bigEndian = false, VR.FL, floatToBytes(math.Pi.toFloat, bigEndian = false)).toShorts shouldBe Seq(3.toShort)
    toElement(bigEndian = false, VR.FD, doubleToBytes(math.Pi, bigEndian = false)).toShorts shouldBe Seq(3.toShort)
    toElement(bigEndian = false, VR.SS, shortToBytes(-3, bigEndian = false)).toShorts shouldBe Seq(-3.toShort)
    toElement(bigEndian = false, VR.US, shortToBytes(-3, bigEndian = false)).toShorts shouldBe Seq(-3.toShort)
    toElement(bigEndian = false, VR.SL, intToBytes(-3, bigEndian = false)).toShorts shouldBe Seq(-3.toShort)
    toElement(bigEndian = false, VR.UL, intToBytes(-3, bigEndian = false)).toShorts shouldBe Seq(-3.toShort)
    toElement(bigEndian = false, VR.DS, ByteString("3.1415")).toShorts shouldBe Seq(3.toShort)
    toElement(bigEndian = false, VR.IS, ByteString("-3")).toShorts shouldBe Seq(-3.toShort)
    toElement(bigEndian = false, VR.AT, ByteString("-3")).toShorts shouldBe empty
  }

  "Parsing a single short value" should "return the first entry among multiple values" in {
    toElement(bigEndian = false, VR.SS, shortToBytesLE(1234) ++ shortToBytesLE(12345)).toShort shouldBe Some(1234.toShort)
  }

  it should "return None if no entry exists" in {
    toElement(bigEndian = false, VR.SS, ByteString.empty).toShort shouldBe None
  }

  "Parsing float values" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.FL, ByteString.empty).toFloats shouldBe Seq.empty
  }

  it should "parse float values" in {
    toElement(bigEndian = false, VR.FL, floatToBytes(1234F, bigEndian = false) ++ floatToBytes(1.234F, bigEndian = false)).toFloats shouldBe Seq(1234F, 1.234F)
  }

  it should "return float values for all numerical VRs" in {
    toElement(bigEndian = false, VR.FL, floatToBytes(math.Pi.toFloat, bigEndian = false)).toFloats shouldBe Seq(math.Pi.toFloat)
    toElement(bigEndian = false, VR.FD, doubleToBytes(math.Pi, bigEndian = false)).toFloats shouldBe Seq(math.Pi.toFloat)
    toElement(bigEndian = false, VR.SS, shortToBytes(-3, bigEndian = false)).toFloats shouldBe Seq(-3.toFloat)
    toElement(bigEndian = false, VR.US, shortToBytes(-3, bigEndian = false)).toFloats shouldBe Seq(((1 << 16) - 3).toFloat)
    toElement(bigEndian = false, VR.SL, intToBytes(-3, bigEndian = false)).toFloats shouldBe Seq(-3.toFloat)
    toElement(bigEndian = false, VR.UL, intToBytes(-3, bigEndian = false)).toFloats shouldBe Seq(((1L << 32) - 3).toFloat)
    toElement(bigEndian = false, VR.DS, ByteString("3.1415")).toFloats shouldBe Seq(3.1415.toFloat)
    toElement(bigEndian = false, VR.IS, ByteString("-3")).toFloats shouldBe Seq(-3.toFloat)
    toElement(bigEndian = false, VR.AT, ByteString("-3")).toFloats shouldBe empty
  }

  "Parsing a single float value" should "return the first entry among multiple values" in {
    toElement(bigEndian = false, VR.FL, floatToBytes(1234F, bigEndian = false) ++ floatToBytes(1.234F, bigEndian = false)).toFloat shouldBe Some(1234F)
  }

  it should "return None if no entry exists" in {
    toElement(bigEndian = false, VR.FL, ByteString.empty).toFloat shouldBe None
  }

  "Parsing double values" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.FD, ByteString.empty).toDoubles shouldBe Seq.empty
  }

  it should "parse double values" in {
    toElement(bigEndian = false, VR.FD, doubleToBytes(1234, bigEndian = false) ++ doubleToBytes(1.234, bigEndian = false)).toDoubles shouldBe Seq(1234.0, 1.234)
  }

  it should "return double values for all numerical VRs" in {
    toElement(bigEndian = false, VR.FL, floatToBytes(math.Pi.toFloat, bigEndian = false)).toDoubles shouldBe Seq(math.Pi.toFloat.toDouble)
    toElement(bigEndian = false, VR.FD, doubleToBytes(math.Pi, bigEndian = false)).toDoubles shouldBe Seq(math.Pi)
    toElement(bigEndian = false, VR.SS, shortToBytes(-3, bigEndian = false)).toDoubles shouldBe Seq(-3.0)
    toElement(bigEndian = false, VR.US, shortToBytes(-3, bigEndian = false)).toDoubles shouldBe Seq((1 << 16) - 3.0)
    toElement(bigEndian = false, VR.SL, intToBytes(-3, bigEndian = false)).toDoubles shouldBe Seq(-3.toFloat)
    toElement(bigEndian = false, VR.UL, intToBytes(-3, bigEndian = false)).toDoubles shouldBe Seq((1L << 32) - 3.0)
    toElement(bigEndian = false, VR.DS, ByteString("3.1415")).toDoubles shouldBe Seq(3.1415)
    toElement(bigEndian = false, VR.IS, ByteString("-3")).toDoubles shouldBe Seq(-3.0)
    toElement(bigEndian = false, VR.AT, ByteString("-3")).toDoubles shouldBe empty
  }

  "Parsing a single double value" should "return the first entry among multiple values" in {
    toElement(bigEndian = false, VR.FD, doubleToBytes(1234, bigEndian = false) ++ doubleToBytes(1.234, bigEndian = false)).toDouble shouldBe Some(1234.0)
  }

  it should "return None if no entry exists" in {
    toElement(bigEndian = false, VR.FD, ByteString.empty).toDouble shouldBe None
  }

  "Parsing date strings" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.DA, ByteString.empty).toDates shouldBe Seq.empty
  }

  it should "parse properly formatted date strings" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(bigEndian = false, VR.DA, ByteString("20040329\\2004.03.29")).toDates shouldBe Seq(date, date)
  }

  it should "ignore improperly formatted entries" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(bigEndian = false, VR.DA, ByteString("20040329\\one\\2004.03.29")).toDates shouldBe Seq(date, date)
    toElement(bigEndian = false, VR.DA, ByteString("one")).toDates shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(bigEndian = false, VR.DA, ByteString(" 20040329 \\20040329 \\one\\2004.03.29  ")).toDates shouldBe Seq(date, date, date)
  }

  "Parsing a single date string" should "return the first valid entry among multiple values" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(bigEndian = false, VR.DA, ByteString("one\\20040329\\20050401")).toDate shouldBe Some(date)
  }

  "Parsing date time strings" should "return empty sequence for empty byte string" in {
    toElement(bigEndian = false, VR.DT, ByteString.empty).toDateTimes() shouldBe Seq.empty
  }

  it should "parse partial date time strings" in {
    val zone = ZonedDateTime.now().getOffset
    val yyyy = ZonedDateTime.of(2004, 1, 1, 0, 0, 0, 0, zone)
    val yyyyMM = ZonedDateTime.of(2004, 3, 1, 0, 0, 0, 0, zone)
    val yyyyMMdd = ZonedDateTime.of(2004, 3, 29, 0, 0, 0, 0, zone)
    val yyyyMMddHH = ZonedDateTime.of(2004, 3, 29, 11, 0, 0, 0, zone)
    val yyyyMMddHHmm = ZonedDateTime.of(2004, 3, 29, 11, 59, 0, 0, zone)
    val yyyyMMddHHmmss = ZonedDateTime.of(2004, 3, 29, 11, 59, 35, 0, zone)
    val yyyyMMddHHmmssS = ZonedDateTime.of(2004, 3, 29, 11, 59, 35, 123456000, zone)
    val yyyyMMddHHmmssSZ = ZonedDateTime.of(2004, 3, 29, 11, 59, 35, 123456000, ZoneOffset.UTC)
    toElement(bigEndian = false, VR.DT, ByteString(
      "2004\\200403\\20040329\\2004032911\\200403291159\\20040329115935\\20040329115935.123456\\20040329115935.123456+0000\\20040329115935.123456-0000"
    )).toDateTimes() shouldBe Seq(yyyy, yyyyMM, yyyyMMdd, yyyyMMddHH, yyyyMMddHHmm, yyyyMMddHHmmss, yyyyMMddHHmmssS, yyyyMMddHHmmssSZ, yyyyMMddHHmmssSZ)
  }

  it should "ignore improperly formatted entries" in {
    toElement(bigEndian = false, VR.DT, ByteString("200\\2004ab\\20040\\2004032\\200403291\\20040329115\\2004032911593\\200403291159356\\20040329115935.1234567\\20040329115935.12345+000\\20040329115935.123456+00000")).toDateTimes() shouldBe empty
  }

  it should "allow time zone also with null components" in {
    val dateTime = ZonedDateTime.of(2004, 1, 1, 0, 0, 0, 0, ZoneOffset.of("+0500"))
    toElement(bigEndian = false, VR.DT, ByteString("2004+0500")).toDateTime() shouldBe Some(dateTime)
  }

  it should "trim whitespace" in {
    val dateTime = ZonedDateTime.of(2004, 3, 29, 5, 35, 59, 12345000, ZoneOffset.UTC)
    toElement(bigEndian = false, VR.DT, ByteString(" 20040329053559.012345+0000 \\20040329053559.012345+0000 \\one\\20040329053559.012345+0000  ")).toDateTimes() shouldBe Seq(dateTime, dateTime, dateTime)
  }

  it should "parse time zones" in {
    val dateTime = ZonedDateTime.of(2004, 3, 29, 5, 35, 59, 12345000, ZoneOffset.ofHours(3))
    toElement(bigEndian = false, VR.DT, ByteString("20040329053559.012345+0300")).toDateTime() shouldBe Some(dateTime)
  }

  "Parsing a single date time string" should "return the first valid entry among multiple values" in {
    val dateTime = ZonedDateTime.of(2004, 3, 29, 5, 35, 59, 12345000, ZoneOffset.UTC)
    toElement(bigEndian = false, VR.DT, ByteString("one\\20040329053559.012345+0000\\20050329053559.012345+0000")).toDateTime() shouldBe Some(dateTime)
  }

  "String representations of elements" should "format OW values" in {
    toElement(bigEndian = false, VR.OW, ByteString(Array(1, 2, 3, 4, 5, 6, 7, 8).map(_.toByte))).toSingleString() shouldBe "0201 0403 0605 0807"
    toElement(bigEndian = true, VR.OW, ByteString(Array(1, 2, 3, 4, 5, 6, 7, 8).map(_.toByte))).toSingleString() shouldBe "0102 0304 0506 0708"
  }

  it should "format OB values" in {
    toElement(bigEndian = false, VR.OB, ByteString(Array(1, 2, 3, 4, 5, 6, 7, 8).map(_.toByte))).toSingleString() shouldBe "01 02 03 04 05 06 07 08"
  }

  it should "format OF values" in {
    toElement(bigEndian = false, VR.OF, intToBytesLE(java.lang.Float.floatToIntBits(1.2F)) ++ intToBytesLE(java.lang.Float.floatToIntBits(56.78F))).toSingleString() shouldBe "1.2 56.78"
  }

  it should "format OD values" in {
    toElement(bigEndian = false, VR.OD, longToBytesLE(java.lang.Double.doubleToLongBits(1.2)) ++ longToBytesLE(java.lang.Double.doubleToLongBits(56.78))).toSingleString() shouldBe "1.2 56.78"
  }

  it should "format AT values" in {
    toElement(bigEndian = false, VR.AT, ByteString(Array(1, 2, 3, 4).map(_.toByte))).toSingleString() shouldBe "(0201,0403)"
  }

  it should "format UL values" in {
    toElement(bigEndian = false, VR.UL, ByteString(Array(1, 2, 3, 4).map(_.toByte))).toSingleString() shouldBe 0x04030201.toString
    toElement(bigEndian = false, VR.UL, ByteString(Array(255, 255, 255, 255).map(_.toByte))).toSingleString() shouldBe 0xFFFFFFFFL.toString
  }

  it should "format US values" in {
    toElement(bigEndian = false, VR.US, ByteString(Array(1, 2).map(_.toByte))).toSingleString() shouldBe 0x0201.toString
    toElement(bigEndian = false, VR.US, ByteString(Array(255, 255).map(_.toByte))).toSingleString() shouldBe 0xFFFF.toString
  }

  it should "format SL values" in {
    toElement(bigEndian = false, VR.SL, ByteString(Array(1, 2, 3, 4).map(_.toByte))).toSingleString() shouldBe 0x04030201.toString
    toElement(bigEndian = false, VR.SL, ByteString(Array(255, 255, 255, 255).map(_.toByte))).toSingleString() shouldBe "-1"
  }

  it should "format SS values" in {
    toElement(bigEndian = false, VR.SS, ByteString(Array(1, 2).map(_.toByte))).toSingleString() shouldBe 0x0201.toString
    toElement(bigEndian = false, VR.SS, ByteString(Array(255, 255).map(_.toByte))).toSingleString() shouldBe "-1"
  }

  it should "format FL values" in {
    toElement(bigEndian = false, VR.FL, intToBytesLE(java.lang.Float.floatToIntBits(math.Pi.toFloat))).toSingleString() shouldBe math.Pi.toFloat.toString
  }

  it should "format FD values" in {
    toElement(bigEndian = false, VR.FD, longToBytesLE(java.lang.Double.doubleToLongBits(math.Pi))).toSingleString() shouldBe math.Pi.toString
  }

  it should "format ST values" in {
    val e = toElement(bigEndian = false, VR.ST, ByteString("   Short text   \\   and some more   "))
    e.toStrings() should have length 1
    e.toSingleString() shouldBe "   Short text   \\   and some more"
  }

  it should "format DT values" in {
    val dt = "20040329053559.012345+0300"
    toElement(bigEndian = false, VR.DT, ByteString(dt)).toSingleString() shouldBe dt
  }

  "Parsing a patient name" should "divide into parts and components" in {
    toElement(bigEndian = false, VR.PN, ByteString("aFamily=iFamily=pFamily^aGiven=iGiven=pGiven^aMiddle=iMiddle=pMiddle^aPrefix=iPrefix=pPrefix^aSuffix=iSuffix=pSuffix"))
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("aFamily", "iFamily", "pFamily"),
      ComponentGroup("aGiven", "iGiven", "pGiven"),
      ComponentGroup("aMiddle", "iMiddle", "pMiddle"),
      ComponentGroup("aPrefix", "iPrefix", "pPrefix"),
      ComponentGroup("aSuffix", "iSuffix", "pSuffix")))
  }

  it should "handle null components" in {
    toElement(bigEndian = false, VR.PN, ByteString("=iFamily=pFamily^^aMiddle^aPrefix==pPrefix^==pSuffix"))
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("", "iFamily", "pFamily"),
      ComponentGroup("", "", ""),
      ComponentGroup("aMiddle", "", ""),
      ComponentGroup("aPrefix", "", "pPrefix"),
      ComponentGroup("", "", "pSuffix")))

    toElement(bigEndian = false, VR.PN, ByteString("aFamily=iFamily^^aMiddle"))
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("aFamily", "iFamily", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("aMiddle", "", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("", "", "")))
  }

  it should "trim whitespace within each component" in {
    toElement(bigEndian = false, VR.PN, ByteString("   aFamily   =   iFamily   ^^   aMiddle   "))
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("aFamily", "iFamily", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("aMiddle", "", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("", "", "")))
  }

  "An element" should "update its value bytes" in {
    val updated = Element(Tag.PatientName, patientNameJohnDoe().drop(8))
      .withUpdatedValue(ByteString("ABC"))
    updated.length shouldBe 4
    updated.value shouldBe ByteString("ABC ")
  }

  val tag = 0
  val explicit = true
  val le = false

  "Creating an element" should "produce the expected bytes from string(s)" in {
    Element.fromString(tag, VR.PN, "John^Doe", le, explicit).toSingleString() shouldBe "John^Doe"
    Element.fromString(tag, VR.PN, "John^Doe", bigEndian = true, explicit).toSingleString() shouldBe "John^Doe"
    Element.fromString(tag, VR.PN, "John^Doe", le, explicitVR = false).toSingleString() shouldBe "John^Doe"
    Element.fromStrings(tag, VR.PN, Seq("John^Doe", "Jane^Doe")).toStrings() shouldBe Seq("John^Doe", "Jane^Doe")

    Element.fromString(tag, VR.AT, "00A01234", le, explicit).toInt.get shouldBe 0x00A01234
    Element.fromString(tag, VR.FL, "3.1415", le, explicit).toFloat.get shouldBe 3.1415F
    Element.fromString(tag, VR.FD, "3.1415", le, explicit).toDouble.get shouldBe 3.1415
    Element.fromString(tag, VR.SL, "-1024", le, explicit).toInt.get shouldBe -1024
    Element.fromString(tag, VR.SS, "-1024", le, explicit).toShort.get shouldBe -1024.toShort
    Element.fromString(tag, VR.UL, "4294967295", le, explicit).toLong.get shouldBe 4294967295L
    Element.fromString(tag, VR.US, "65535", le, explicit).toInt.get shouldBe 65535
  }

  it should "produce the expected bytes from short(s)" in {
    Element.fromShort(tag, VR.US, 512.toShort, le, explicit).toShort.get shouldBe 512.toShort
    Element.fromShort(tag, VR.US, 512.toShort, bigEndian = true, explicit).toShort.get shouldBe 512.toShort
    Element.fromShort(tag, VR.US, 512.toShort, le, explicitVR = false).toShort.get shouldBe 512.toShort
    Element.fromShorts(tag, VR.US, Seq(512, 256).map(_.toShort)).toShorts shouldBe Seq(512.toShort, 256.toShort)

    Element.fromShort(tag, VR.FL, 3.1415.toShort, le, explicit).toFloat.get shouldBe 3.0F
    Element.fromShort(tag, VR.FD, 3.1415.toShort, le, explicit).toDouble.get shouldBe 3.0
    Element.fromShort(tag, VR.SL, (-1024).toShort, le, explicit).toInt.get shouldBe -1024
    Element.fromShort(tag, VR.SS, (-1024).toShort, le, explicit).toShort.get shouldBe -1024.toShort
    Element.fromShort(tag, VR.UL, 42.toShort, le, explicit).toLong.get shouldBe 42L
    Element.fromShort(tag, VR.US, 42.toShort, le, explicit).toInt.get shouldBe 42
  }

  it should "produce the expected bytes from int(s)" in {
    Element.fromInt(tag, VR.UL, 1234, le, explicit).toInt.get shouldBe 1234
    Element.fromInt(tag, VR.UL, 1234, bigEndian = true, explicit).toInt.get shouldBe 1234
    Element.fromInt(tag, VR.UL, 1234, le, explicitVR = false).toInt.get shouldBe 1234
    Element.fromInts(tag, VR.UL, Seq(512, 256)).toInts shouldBe Seq(512, 256)

    Element.fromInt(tag, VR.AT, 0x00A01234, le, explicit).toInt.get shouldBe 0x00A01234
    Element.fromInt(tag, VR.FL, 3.1415.toInt, le, explicit).toFloat.get shouldBe 3.0F
    Element.fromInt(tag, VR.FD, 3.1415.toInt, le, explicit).toDouble.get shouldBe 3.0
    Element.fromInt(tag, VR.SL, -1024, le, explicit).toInt.get shouldBe -1024
    Element.fromInt(tag, VR.SS, -1024, le, explicit).toShort.get shouldBe -1024.toShort
    Element.fromInt(tag, VR.UL, 42, le, explicit).toLong.get shouldBe 42L
    Element.fromInt(tag, VR.US, 65535, le, explicit).toInt.get shouldBe 65535
  }

  it should "produce the expected bytes from long(s)" in {
    Element.fromLong(tag, VR.UL, 1234L, le, explicit).toLong.get shouldBe 1234L
    Element.fromLong(tag, VR.UL, 1234L, bigEndian = true, explicit).toLong.get shouldBe 1234L
    Element.fromLong(tag, VR.UL, 1234L, le, explicitVR = false).toLong.get shouldBe 1234L
    Element.fromLongs(tag, VR.UL, Seq(512L, 256L)).toLongs shouldBe Seq(512L, 256L)

    Element.fromLong(tag, VR.AT, 0x00A01234L, le, explicit).toInt.get shouldBe 0x00A01234
    Element.fromLong(tag, VR.FL, 3.1415.toLong, le, explicit).toFloat.get shouldBe 3.0F
    Element.fromLong(tag, VR.FD, 3.1415.toLong, le, explicit).toDouble.get shouldBe 3.0
    Element.fromLong(tag, VR.SL, -1024L, le, explicit).toInt.get shouldBe -1024
    Element.fromLong(tag, VR.SS, -1024L, le, explicit).toShort.get shouldBe -1024.toShort
    Element.fromLong(tag, VR.UL, 4294967295L, le, explicit).toLong.get shouldBe 4294967295L
    Element.fromLong(tag, VR.US, 65535L, le, explicit).toInt.get shouldBe 65535
  }

  it should "produce the expected bytes from float(s)" in {
    Element.fromFloat(tag, VR.FL, 3.14F, le, explicit).toFloat.get shouldBe 3.14F
    Element.fromFloat(tag, VR.FL, 3.14F, bigEndian = true, explicit).toFloat.get shouldBe 3.14F
    Element.fromFloat(tag, VR.FL, 3.14F, le, explicitVR = false).toFloat.get shouldBe 3.14F
    Element.fromFloats(tag, VR.FL, Seq(512F, 256F)).toFloats shouldBe Seq(512F, 256F)

    Element.fromFloat(tag, VR.AT, 0x00A01234.toFloat, le, explicit).toInt.get shouldBe 0x00A01234
    Element.fromFloat(tag, VR.FL, 3.1415F, le, explicit).toFloat.get shouldBe 3.1415F
    Element.fromFloat(tag, VR.FD, 3.1415F, le, explicit).toDouble.get.toFloat shouldBe 3.1415F
    Element.fromFloat(tag, VR.SL, -1024F, le, explicit).toInt.get shouldBe -1024
    Element.fromFloat(tag, VR.SS, -1024F, le, explicit).toShort.get shouldBe -1024.toShort
    Element.fromFloat(tag, VR.UL, 42.0F, le, explicit).toLong.get shouldBe 42L
    Element.fromFloat(tag, VR.US, 65535F, le, explicit).toInt.get shouldBe 65535
  }

  it should "produce the expected bytes from double(s)" in {
    Element.fromDouble(tag, VR.FL, 3.14, le, explicit).toDouble.get.toFloat shouldBe 3.14F
    Element.fromDouble(tag, VR.FL, 3.14, bigEndian = true, explicit).toDouble.get.toFloat shouldBe 3.14F
    Element.fromDouble(tag, VR.FL, 3.14, le, explicitVR = false).toDouble.get.toFloat shouldBe 3.14F
    Element.fromDoubles(tag, VR.FL, Seq(512.0, 256.0)).toDoubles shouldBe Seq(512.0, 256.0)

    Element.fromDouble(tag, VR.AT, 0x00A01234.toDouble, le, explicit).toInt.get shouldBe 0x00A01234
    Element.fromDouble(tag, VR.FL, 3.1415, le, explicit).toFloat.get shouldBe 3.1415F
    Element.fromDouble(tag, VR.FD, 3.1415, le, explicit).toDouble.get.toFloat shouldBe 3.1415F
    Element.fromDouble(tag, VR.SL, -1024.0, le, explicit).toInt.get shouldBe -1024
    Element.fromDouble(tag, VR.SS, -1024.0, le, explicit).toShort.get shouldBe -1024.toShort
    Element.fromDouble(tag, VR.UL, 42.0, le, explicit).toLong.get shouldBe 42L
    Element.fromDouble(tag, VR.US, 65535.0, le, explicit).toInt.get shouldBe 65535
  }

  it should "produce the expected bytes from date(s)" in {
    val date1 = LocalDate.of(2004, 3, 29)
    val date2 = LocalDate.of(2004, 3, 30)
    Element.fromDate(tag, VR.DA, date1, le, explicit).toDate.get shouldBe date1
    Element.fromDate(tag, VR.DA, date1, bigEndian = true, explicit).toDate.get shouldBe date1
    Element.fromDate(tag, VR.DA, date1, le, explicitVR = false).toDate.get shouldBe date1
    Element.fromDates(tag, VR.DA, Seq(date1, date2)).toDates shouldBe Seq(date1, date2)

    Element.fromDate(tag, VR.DT, date1, le, explicit).toDate.get shouldBe date1
    Element.fromDate(tag, VR.LT, date1, le, explicit).toSingleString() shouldBe "20040329"
  }

  it should "produce the expected bytes from date-time(s)" in {
    val dt1 = ZonedDateTime.of(2004, 3, 29, 11, 59, 35, 123456000, ZoneOffset.UTC)
    val dt2 = ZonedDateTime.of(2004, 3, 29, 11, 59, 36, 123456000, ZoneOffset.UTC)
    Element.fromDateTime(tag, VR.DT, dt1, le, explicit).toDateTime().get shouldBe dt1
    Element.fromDateTime(tag, VR.DT, dt1, bigEndian = true, explicit).toDateTime().get shouldBe dt1
    Element.fromDateTime(tag, VR.DT, dt1, le, explicitVR = false).toDateTime().get shouldBe dt1
    Element.fromDateTimes(tag, VR.DT, Seq(dt1, dt2)).toDateTimes() shouldBe Seq(dt1, dt2)

    Element.fromDateTime(tag, VR.LT, dt1, le, explicit).toSingleString() shouldBe "20040329115935.123456+0000"
  }

  it should "produce the expected bytes from patient name(s)" in {
    val pn1 = PatientName(ComponentGroup("family", "i", "p"), ComponentGroup("given", "i", "p"), ComponentGroup("middle", "i", "p"), ComponentGroup("prefix", "i", "p"), ComponentGroup("suffix", "i", "p"))
    val pn2 = pn1.copy(familyName = ComponentGroup("otherfamily", "i", "p"))
    Element.fromPatientName(tag, VR.PN, pn1, le, explicit).toPatientName().get shouldBe pn1
    Element.fromPatientName(tag, VR.PN, pn1, bigEndian = true, explicit).toPatientName().get shouldBe pn1
    Element.fromPatientName(tag, VR.PN, pn1, le, explicitVR = false).toPatientName().get shouldBe pn1
    Element.fromPatientNames(tag, VR.PN, Seq(pn1, pn2)).toPatientNames() shouldBe Seq(pn1, pn2)

    Element.fromPatientName(tag, VR.PN, pn1, le, explicit).toSingleString() shouldBe "family=i=p^given=i=p^middle=i=p^prefix=i=p^suffix=i=p"
  }

}
