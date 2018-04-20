package se.nimsa.dicom

import java.util.GregorianCalendar

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}
import se.nimsa.dicom.VR.VR

class ElementTest extends FlatSpec with Matchers {

  def toElement(bytes: ByteString, bigEndian: Boolean, vr: VR) = Element(TagPath.fromTag(Tag.PatientID), bigEndian, vr, explicitVR = true, bytes.length, padToEvenLength(bytes, vr))

  "Formatting bytes into multiple strings" should "return empty sequence for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.SH).toStrings shouldBe Seq.empty
  }

  it should "throw an exception for null input" in {
    intercept[NullPointerException] {
      toElement(null, bigEndian = false, VR.SH).toStrings shouldBe Seq.empty
    }
  }

  it should "split a string according to the DICOM multiple value delimiter" in {
    toElement(ByteString("one\\two\\three"), bigEndian = false, VR.SH).toStrings shouldBe Seq("one", "two", "three")
  }

  it should "trim any characters at beginning and end" in {
    toElement(ByteString(0x20, 0x20, 0x20, 0x20, 0x41, 0x41, 0x20, 0x20, 0x20), bigEndian = false, VR.SH).toStrings shouldBe Seq("AA")
  }

  it should "trim any characters at or below 0x20 at beginning and end of each value" in {
    toElement(ByteString("  one \\ two \\three  "), bigEndian = false, VR.SH).toStrings shouldBe Seq("one", "two", "three")
  }

  it should "split and trim strings with multiple character set encodings" in {
    val nameBytes = ByteString(0x20, 0xD4, 0xCF, 0xC0, 0xDE, 0x5C, 0x20, 0xC0, 0xDB, 0xB3, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28, 0x4A, 0x5C, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x4A, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64, 0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x4A, 0x5C, 0x20, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B, 0x28, 0x4A)
    println("hej")
    toElement(nameBytes, bigEndian = false, VR.SH).toStrings(new CharacterSets(Seq("ISO 2022 IR 13", "ISO 2022 IR 87"))) shouldBe Seq("ﾔﾏﾀﾞ", "ﾀﾛｳ=山田", "太郎=やまだ", "たろう")
  }

  "Formatting bytes into a single string" should "return empty string for empty byte string" in {
    toElement(ByteString.empty, bigEndian = false, VR.SH).toSingleString shouldBe empty
  }

  it should "not split a string with DICOM multiple value delimiters" in {
    toElement(ByteString("one\\two\\three"), bigEndian = false, VR.SH).toSingleString shouldBe "one\\two\\three"
  }

  it should "trim the string components" in {
    toElement(ByteString("   one two  "), bigEndian = false, VR.SH).toSingleString shouldBe "one two"
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
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toElement(ByteString("20040329\\2004.03.29"), bigEndian = false, VR.DA).toDates shouldBe Seq(date, date)
  }

  it should "ignore improperly formatted entries" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toElement(ByteString("20040329\\one\\2004.03.29"), bigEndian = false, VR.DA).toDates shouldBe Seq(date, date)
    toElement(ByteString("one"), bigEndian = false, VR.DA).toDates shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toElement(ByteString(" 20040329 \\20040329 \\one\\2004.03.29  "), bigEndian = false, VR.DA).toDates shouldBe Seq(date, date, date)
  }

  "Parsing a single date string" should "return the first valid entry among multiple values" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toElement(ByteString("one\\20040329\\20050401"), bigEndian = false, VR.DA).toDate shouldBe Some(date)
  }

}
