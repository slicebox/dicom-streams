package se.nimsa.dicom

import java.time.{LocalDate, ZoneOffset, ZonedDateTime}

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import se.nimsa.dicom.Element.{ComponentGroup, PatientName}
import se.nimsa.dicom.VR.VR

class ElementTest extends FlatSpec with Matchers {

  def toElement(bytes: ByteString, bigEndian: Boolean, vr: VR) = Element(TagPath.fromTag(Tag.PatientID), bigEndian, vr, explicitVR = true, bytes.length, padToEvenLength(bytes, vr))

  "Formatting bytes into multiple strings" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.SH).toStrings() shouldBe Seq.empty
  }

  it should "throw an exception for null input" in {
    intercept[NullPointerException] {
      toElement(null, bigEndian = false, VR.SH).toStrings() shouldBe Seq.empty
    }
  }

  it should "split a string according to the DICOM multiple value delimiter" in {
    toElement(ByteString("one\\two\\three"), bigEndian = false, VR.SH).toStrings() shouldBe Seq("one", "two", "three")
  }

  it should "trim any characters at beginning and end" in {
    toElement(ByteString(0x20, 0x20, 0x20, 0x20, 0x41, 0x41, 0x20, 0x20, 0x20), bigEndian = false, VR.SH).toStrings() shouldBe Seq("AA")
  }

  it should "trim any characters at or below 0x20 at beginning and end of each value" in {
    toElement(ByteString("  one \\ two \\three  "), bigEndian = false, VR.SH).toStrings() shouldBe Seq("one", "two", "three")
  }

  it should "split and trim strings with multiple character set encodings" in {
    val nameBytes = ByteString(0x20, 0xD4, 0xCF, 0xC0, 0xDE, 0x5C, 0x20, 0xC0, 0xDB, 0xB3, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28, 0x4A, 0x5C, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x4A, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64, 0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x4A, 0x5C, 0x20, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B, 0x28, 0x4A)
    toElement(nameBytes, bigEndian = false, VR.SH).toStrings(new CharacterSets(Seq("ISO 2022 IR 13", "ISO 2022 IR 87"))) shouldBe Seq("ﾔﾏﾀﾞ", "ﾀﾛｳ=山田", "太郎=やまだ", "たろう")
  }

  "Formatting bytes into a single string" should "return empty string for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.SH).toSingleString() shouldBe empty
  }

  it should "not split a string with DICOM multiple value delimiters" in {
    toElement(ByteString("one\\two\\three"), bigEndian = false, VR.SH).toSingleString() shouldBe "one\\two\\three"
  }

  it should "trim the string components" in {
    toElement(ByteString("   one two  "), bigEndian = false, VR.SH).toSingleString() shouldBe "one two"
  }

  "Parsing int values" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.SL).toInts shouldBe Seq.empty
  }

  it should "parse multiple int values" in {
    toElement(intToBytesLE(1234) ++ intToBytesLE(1234567890), bigEndian = false, VR.SL).toInts shouldBe Seq(1234, 1234567890)
  }

  "Parsing a single int value" should "return the first entry among multiple values" in {
    toElement(intToBytesLE(1234) ++ intToBytesLE(1234567890), bigEndian = false, VR.SL).toInt shouldBe Some(1234)
  }

  it should "return None if no entry exists" in {
    toElement(ByteString.empty, bigEndian = false, VR.SL).toInt shouldBe None
  }

  "Parsing short values" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.SS).toShorts shouldBe Seq.empty
  }

  it should "parse short values" in {
    toElement(shortToBytesLE(1234) ++ shortToBytesLE(12345), bigEndian = false, VR.SS).toShorts shouldBe Seq(1234.toShort, 12345.toShort)
  }

  "Parsing a single short value" should "return the first entry among multiple values" in {
    toElement(shortToBytesLE(1234) ++ shortToBytesLE(12345), bigEndian = false, VR.SS).toShort shouldBe Some(1234.toShort)
  }

  it should "return None if no entry exists" in {
    toElement(ByteString.empty, bigEndian = false, VR.SS).toShort shouldBe None
  }

  "Parsing float values" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.FL).toFloats shouldBe Seq.empty
  }

  it should "parse float values" in {
    toElement(floatToBytes(1234F, bigEndian = false) ++ floatToBytes(1.234F, bigEndian = false), bigEndian = false, VR.FL).toFloats shouldBe Seq(1234F, 1.234F)
  }

//  it should "treat commas and dots as decimal separator" in {
//    toElement(ByteString("1.2\\1,2"), bigEndian = false, VR.FL).toFloats shouldBe Seq(1.2F, 1.2F)
//  }

  "Parsing a single float value" should "return the first entry among multiple values" in {
    toElement(floatToBytes(1234F, bigEndian = false) ++ floatToBytes(1.234F, bigEndian = false), bigEndian = false, VR.FL).toFloat shouldBe Some(1234F)
  }

  it should "return None if no entry exists" in {
    toElement(ByteString.empty, bigEndian = false, VR.FL).toFloat shouldBe None
  }

  "Parsing double values" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.FD).toDoubles shouldBe Seq.empty
  }

  it should "parse double values" in {
    toElement(doubleToBytes(1234, bigEndian = false) ++ doubleToBytes(1.234, bigEndian = false), bigEndian = false, VR.FD).toDoubles shouldBe Seq(1234.0, 1.234)
  }

//  it should "treat commas and dots as decimal separator" in {
//    toElement(ByteString("1.2\\1,2"), bigEndian = false, VR.FD).toDoubles shouldBe Seq(1.2, 1.2)
//  }

  "Parsing a single double value" should "return the first entry among multiple values" in {
    toElement(doubleToBytes(1234, bigEndian = false) ++ doubleToBytes(1.234, bigEndian = false), bigEndian = false, VR.FD).toDouble shouldBe Some(1234.0)
  }

  it should "return None if no entry exists" in {
    toElement(ByteString.empty, bigEndian = false, VR.FD).toDouble shouldBe None
  }

  "Parsing date strings" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.DA).toDates shouldBe Seq.empty
  }

  it should "parse properly formatted date strings" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(ByteString("20040329\\2004.03.29"), bigEndian = false, VR.DA).toDates shouldBe Seq(date, date)
  }

  it should "ignore improperly formatted entries" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(ByteString("20040329\\one\\2004.03.29"), bigEndian = false, VR.DA).toDates shouldBe Seq(date, date)
    toElement(ByteString("one"), bigEndian = false, VR.DA).toDates shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(ByteString(" 20040329 \\20040329 \\one\\2004.03.29  "), bigEndian = false, VR.DA).toDates shouldBe Seq(date, date, date)
  }

  "Parsing a single date string" should "return the first valid entry among multiple values" in {
    val date = LocalDate.of(2004, 3, 29)
    toElement(ByteString("one\\20040329\\20050401"), bigEndian = false, VR.DA).toDate shouldBe Some(date)
  }

  "Parsing date time strings" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.DT).toDateTimes() shouldBe Seq.empty
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
    toElement(
      ByteString(
        "2004\\200403\\20040329\\2004032911\\200403291159\\20040329115935\\20040329115935.123456\\20040329115935.123456+0000\\20040329115935.123456-0000"
      ), bigEndian = false, VR.DT
    ).toDateTimes() shouldBe Seq(yyyy, yyyyMM, yyyyMMdd, yyyyMMddHH, yyyyMMddHHmm, yyyyMMddHHmmss, yyyyMMddHHmmssS, yyyyMMddHHmmssSZ, yyyyMMddHHmmssSZ)
  }

  it should "ignore improperly formatted entries" in {
    toElement(ByteString("200\\2004ab\\20040\\2004032\\200403291\\20040329115\\2004032911593\\200403291159356\\20040329115935.1234567\\20040329115935.12345+000\\20040329115935.123456+00000"), bigEndian = false, VR.DT).toDateTimes() shouldBe empty
  }

  it should "allow time zone also with null components" in {
    val dateTime = ZonedDateTime.of(2004, 1, 1, 0, 0, 0, 0, ZoneOffset.of("+0500"))
    toElement(ByteString("2004+0500"), bigEndian = false, VR.DT).toDateTime() shouldBe Some(dateTime)
  }

  it should "trim whitespace" in {
    val dateTime = ZonedDateTime.of(2004, 3, 29, 5, 35, 59, 12345000, ZoneOffset.UTC)
    toElement(ByteString(" 20040329053559.012345+0000 \\20040329053559.012345+0000 \\one\\20040329053559.012345+0000  "), bigEndian = false, VR.DT).toDateTimes() shouldBe Seq(dateTime, dateTime, dateTime)
  }

  it should "parse time zones" in {
    val dateTime = ZonedDateTime.of(2004, 3, 29, 5, 35, 59, 12345000, ZoneOffset.ofHours(3))
    toElement(ByteString("20040329053559.012345+0300"), bigEndian = false, VR.DT).toDateTime() shouldBe Some(dateTime)
  }

  "Parsing a single date time string" should "return the first valid entry among multiple values" in {
    val dateTime = ZonedDateTime.of(2004, 3, 29, 5, 35, 59, 12345000, ZoneOffset.UTC)
    toElement(ByteString("one\\20040329053559.012345+0000\\20050329053559.012345+0000"), bigEndian = false, VR.DT).toDateTime() shouldBe Some(dateTime)
  }

  "String representations of elements" should "format OW values" in {
    toElement(ByteString(Array(1,2,3,4,5,6,7,8).map(_.toByte)), bigEndian = false, VR.OW).toSingleString() shouldBe "0201 0403 0605 0807"
    toElement(ByteString(Array(1,2,3,4,5,6,7,8).map(_.toByte)), bigEndian = true, VR.OW).toSingleString() shouldBe "0102 0304 0506 0708"
  }

  it should "format OB values" in {
    toElement(ByteString(Array(1,2,3,4,5,6,7,8).map(_.toByte)), bigEndian = false, VR.OB).toSingleString() shouldBe "01 02 03 04 05 06 07 08"
  }

  it should "format AT values" in {
    toElement(ByteString(Array(1,2,3,4).map(_.toByte)), bigEndian = false, VR.AT).toSingleString() shouldBe "(0201,0403)"
  }

  it should "format UL values" in {
    toElement(ByteString(Array(1,2,3,4).map(_.toByte)), bigEndian = false, VR.UL).toSingleString() shouldBe 0x04030201.toString
    toElement(ByteString(Array(255,255,255,255).map(_.toByte)), bigEndian = false, VR.UL).toSingleString() shouldBe 0xFFFFFFFFL.toString
  }

  it should "format US values" in {
    toElement(ByteString(Array(1,2).map(_.toByte)), bigEndian = false, VR.US).toSingleString() shouldBe 0x0201.toString
    toElement(ByteString(Array(255,255).map(_.toByte)), bigEndian = false, VR.US).toSingleString() shouldBe 0xFFFF.toString
  }

  it should "format SL values" in {
    toElement(ByteString(Array(1,2,3,4).map(_.toByte)), bigEndian = false, VR.SL).toSingleString() shouldBe 0x04030201.toString
    toElement(ByteString(Array(255,255,255,255).map(_.toByte)), bigEndian = false, VR.SL).toSingleString() shouldBe "-1"
  }

  it should "format SS values" in {
    toElement(ByteString(Array(1,2).map(_.toByte)), bigEndian = false, VR.SS).toSingleString() shouldBe 0x0201.toString
    toElement(ByteString(Array(255,255).map(_.toByte)), bigEndian = false, VR.SS).toSingleString() shouldBe "-1"
  }

  it should "format FL values" in {
    toElement(intToBytesLE(java.lang.Float.floatToIntBits(math.Pi.toFloat)), bigEndian = false, VR.FL).toSingleString() shouldBe math.Pi.toFloat.toString
  }

  it should "format FD values" in {
    toElement(longToBytesLE(java.lang.Double.doubleToLongBits(math.Pi)), bigEndian = false, VR.FD).toSingleString() shouldBe math.Pi.toString
  }

  it should "format ST values" in {
    val e = toElement(ByteString("   Short text   \\   and some more   "), bigEndian = false, VR.ST)
    e.toStrings() should have length 1
    e.toSingleString() shouldBe "   Short text   \\   and some more"
  }

  it should "format DT values" in {
    val dt = "20040329053559.012345+0300"
    toElement(ByteString(dt), bigEndian = false, VR.DT).toSingleString() shouldBe dt
  }

  "Parsing a patient name" should "divide into parts and components" in {
    toElement(ByteString("aFamily=iFamily=pFamily^aGiven=iGiven=pGiven^aMiddle=iMiddle=pMiddle^aPrefix=iPrefix=pPrefix^aSuffix=iSuffix=pSuffix"), bigEndian = false, VR.PN)
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("aFamily", "iFamily", "pFamily"),
      ComponentGroup("aGiven", "iGiven", "pGiven"),
      ComponentGroup("aMiddle", "iMiddle", "pMiddle"),
      ComponentGroup("aPrefix", "iPrefix", "pPrefix"),
      ComponentGroup("aSuffix", "iSuffix", "pSuffix")))
  }

  it should "handle null components" in {
    toElement(ByteString("=iFamily=pFamily^^aMiddle^aPrefix==pPrefix^==pSuffix"), bigEndian = false, VR.PN)
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("", "iFamily", "pFamily"),
      ComponentGroup("", "", ""),
      ComponentGroup("aMiddle", "", ""),
      ComponentGroup("aPrefix", "", "pPrefix"),
      ComponentGroup("", "", "pSuffix")))

    toElement(ByteString("aFamily=iFamily^^aMiddle"), bigEndian = false, VR.PN)
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("aFamily", "iFamily", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("aMiddle", "", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("", "", "")))
  }

  it should "trim whitespace within each component" in {
    toElement(ByteString("   aFamily   =   iFamily   ^^   aMiddle   "), bigEndian = false, VR.PN)
      .toPatientNames() shouldBe Seq(PatientName(
      ComponentGroup("aFamily", "iFamily", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("aMiddle", "", ""),
      ComponentGroup("", "", ""),
      ComponentGroup("", "", "")))
  }
}
