package se.nimsa.dicom

import java.time.{LocalDate, ZoneOffset, ZonedDateTime}

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import se.nimsa.dicom.Element.{ComponentGroup, PatientName}
import se.nimsa.dicom.TestData.patientNameJohnDoe
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
    toElement(bigEndian = false, VR.OW, ByteString(Array(1,2,3,4,5,6,7,8).map(_.toByte))).toSingleString() shouldBe "0201 0403 0605 0807"
    toElement(bigEndian = true, VR.OW, ByteString(Array(1,2,3,4,5,6,7,8).map(_.toByte))).toSingleString() shouldBe "0102 0304 0506 0708"
  }

  it should "format OB values" in {
    toElement(bigEndian = false, VR.OB, ByteString(Array(1,2,3,4,5,6,7,8).map(_.toByte))).toSingleString() shouldBe "01 02 03 04 05 06 07 08"
  }

  it should "format OF values" in {
    toElement(bigEndian = false, VR.OF, intToBytesLE(java.lang.Float.floatToIntBits(1.2F)) ++ intToBytesLE(java.lang.Float.floatToIntBits(56.78F))).toSingleString() shouldBe "1.2 56.78"
  }

  it should "format OD values" in {
    toElement(bigEndian = false, VR.OD, longToBytesLE(java.lang.Double.doubleToLongBits(1.2)) ++ longToBytesLE(java.lang.Double.doubleToLongBits(56.78))).toSingleString() shouldBe "1.2 56.78"
  }

  it should "format AT values" in {
    toElement(bigEndian = false, VR.AT, ByteString(Array(1,2,3,4).map(_.toByte))).toSingleString() shouldBe "(0201,0403)"
  }

  it should "format UL values" in {
    toElement(bigEndian = false, VR.UL, ByteString(Array(1,2,3,4).map(_.toByte))).toSingleString() shouldBe 0x04030201.toString
    toElement(bigEndian = false, VR.UL, ByteString(Array(255,255,255,255).map(_.toByte))).toSingleString() shouldBe 0xFFFFFFFFL.toString
  }

  it should "format US values" in {
    toElement(bigEndian = false, VR.US, ByteString(Array(1,2).map(_.toByte))).toSingleString() shouldBe 0x0201.toString
    toElement(bigEndian = false, VR.US, ByteString(Array(255,255).map(_.toByte))).toSingleString() shouldBe 0xFFFF.toString
  }

  it should "format SL values" in {
    toElement(bigEndian = false, VR.SL, ByteString(Array(1,2,3,4).map(_.toByte))).toSingleString() shouldBe 0x04030201.toString
    toElement(bigEndian = false, VR.SL, ByteString(Array(255,255,255,255).map(_.toByte))).toSingleString() shouldBe "-1"
  }

  it should "format SS values" in {
    toElement(bigEndian = false, VR.SS, ByteString(Array(1,2).map(_.toByte))).toSingleString() shouldBe 0x0201.toString
    toElement(bigEndian = false, VR.SS, ByteString(Array(255,255).map(_.toByte))).toSingleString() shouldBe "-1"
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
    val updated = Element.explicitLE(Tag.PatientName, VR.PN, patientNameJohnDoe().drop(8))
      .withUpdatedValue(ByteString("ABC"))
    updated.length shouldBe 4
    updated.value shouldBe ByteString("ABC ")
  }

}
