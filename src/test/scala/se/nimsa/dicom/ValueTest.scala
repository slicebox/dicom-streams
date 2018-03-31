package se.nimsa.dicom

import java.util.GregorianCalendar

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}

class ValueTest extends FlatSpec with Matchers {

  import CharacterSets._
  import Value._

  "Formatting bytes into multiple strings" should "return empty sequence for empty byte string" in {
    ByteString.empty.toValue.toStrings(VR.PN, defaultOnly) shouldBe Seq.empty
  }

  it should "throw an exception for null input" in {
    intercept[NullPointerException] {
      new Value(null).toStrings(VR.PN, defaultOnly) shouldBe Seq.empty
    }
  }

  it should "split a string according to the DICOM multiple value delimiter" in {
    ByteString("one\\two\\three").toValue.toStrings(VR.PN, defaultOnly) shouldBe Seq("one", "two", "three")
  }

  it should "trim any characters at or below 0x20 at beginning and end" in {
    ByteString(0x0, 0x5, 0x15, 0x20, 0x41, 0x41, 0x20, 0x10, 0x0).toValue.toStrings(VR.PN, defaultOnly) shouldBe Seq("AA")
  }

  it should "trim any characters at or below 0x20 at beginning and end of each value" in {
    ByteString("  one \\ two \\three  ").toValue.toStrings(VR.PN, defaultOnly) shouldBe Seq("one", "two", "three")
  }

  it should "split and trim strings with multiple character set encodings" in {
    val nameBytes = ByteString(0x20, 0xD4, 0xCF, 0xC0, 0xDE, 0x5C, 0x20, 0xC0, 0xDB, 0xB3, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28, 0x4A, 0x5C, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x4A, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64, 0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x4A, 0x5C, 0x20, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B, 0x28, 0x4A)
    nameBytes.toValue.toStrings(VR.PN, new CharacterSets(Seq("ISO 2022 IR 13", "ISO 2022 IR 87"))) shouldBe Seq("ﾔﾏﾀﾞ", "ﾀﾛｳ=山田", "太郎=やまだ", "たろう")
  }

  "Formatting bytes into a single string" should "return empty string for empty byte string" in {
    ByteString.empty.toValue.toSingleString(VR.PN, defaultOnly) shouldBe empty
  }

  it should "not split a string with DICOM multiple value delimiters" in {
    ByteString("one\\two\\three").toValue.toSingleString(VR.PN, defaultOnly) shouldBe "one\\two\\three"
  }

  it should "not trim the string" in {
    ByteString("   one two  ").toValue.toSingleString(VR.PN, defaultOnly) shouldBe "   one two  "
  }

  "Parsing long values" should "return empty sequence for empty byte string" in {
    ByteString.empty.toValue.toLongs shouldBe Seq.empty
  }

  it should "parse properly formatted long values" in {
    ByteString("1234\\123456789012345").toValue.toLongs shouldBe Seq(1234L, 123456789012345L)
  }

  it should "ignore improperly formatted entries" in {
    ByteString("1234\\3.14\\one\\456").toValue. toLongs shouldBe Seq(1234L, 456L)
    ByteString("one").toValue.toLongs shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    ByteString(" 1234 \\3.14 \\one\\456  ").toValue.toLongs shouldBe Seq(1234L, 456L)
  }

  "Parsing a single long value" should "return the first valid entry among multiple values" in {
    ByteString("one\\45\\55").toValue.toLong shouldBe Some(45L)
  }

  it should "return None if no valid entry exists" in {
    ByteString.empty.toValue.toLong shouldBe None
    ByteString("").toValue.toLong shouldBe None
    ByteString("one\\two").toValue.toLong shouldBe None
  }

  "Parsing int values" should "return empty sequence for empty byte string" in {
    ByteString("one\\two").toValue.toInts shouldBe Seq.empty
  }

  it should "parse properly formatted int values" in {
    ByteString("1234\\1234567890").toValue.toInts shouldBe Seq(1234, 1234567890)
  }

  it should "ignore improperly formatted entries" in {
    ByteString("1234\\3.14\\one\\456").toValue.toInts shouldBe Seq(1234, 456)
    ByteString("one").toValue.toInts shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    ByteString(" 1234 \\3.14 \\one\\456  ").toValue.toInts shouldBe Seq(1234, 456)
  }

  "Parsing a single int value" should "return the first valid entry among multiple values" in {
    ByteString("one\\45\\55").toValue.toInt shouldBe Some(45)
  }

  it should "return None if no valid entry exists" in {
    ByteString.empty.toValue.toInt shouldBe None
    ByteString("").toValue.toInt shouldBe None
    ByteString("one\\two").toValue.toInt shouldBe None
  }

  "Parsing short values" should "return empty sequence for empty byte string" in {
    ByteString.empty.toValue.toShorts shouldBe Seq.empty
  }

  it should "parse properly formatted short values" in {
    ByteString("1234\\12345").toValue.toShorts shouldBe Seq(1234.toShort, 12345.toShort)
  }

  it should "ignore improperly formatted entries" in {
    ByteString("1234\\3.14\\one\\456").toValue.toShorts shouldBe Seq(1234.toShort, 456.toShort)
    ByteString("one").toValue.toShorts shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    ByteString(" 1234 \\3.14 \\one\\456  ").toValue.toShorts shouldBe Seq(1234.toShort, 456.toShort)
  }

  "Parsing a single short value" should "return the first valid entry among multiple values" in {
    ByteString("one\\45\\55").toValue.toShort shouldBe Some(45.toShort)
  }

  it should "return None if no valid entry exists" in {
    ByteString.empty.toValue.toShort shouldBe None
    ByteString("").toValue.toShort shouldBe None
    ByteString("one\\two").toValue.toShort shouldBe None
  }

  "Parsing float values" should "return empty sequence for empty byte string" in {
    ByteString.empty.toValue.toFloats shouldBe Seq.empty
  }

  it should "parse properly formatted float values" in {
    ByteString("1234\\1.234").toValue.toFloats shouldBe Seq(1234.toFloat, 1.234.toFloat)
  }

  it should "ignore improperly formatted entries" in {
    ByteString("1234\\3.14\\one\\456").toValue.toFloats shouldBe Seq(1234.toFloat, 3.14.toFloat, 456.toFloat)
    ByteString("one").toValue.toFloats shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    ByteString(" 1.234 \\3.14 \\one\\4.56  ").toValue.toFloats shouldBe Seq(1.234.toFloat, 3.14.toFloat, 4.56.toFloat)
  }

  it should "treat commas and dots as decimal separator" in {
    ByteString("1.2\\1,2").toValue.toFloats shouldBe Seq(1.2.toFloat, 1.2.toFloat)
  }

  "Parsing a single float value" should "return the first valid entry among multiple values" in {
    ByteString("one\\4.5\\55").toValue.toFloat shouldBe Some(4.5.toFloat)
  }

  it should "return None if no valid entry exists" in {
    ByteString.empty.toValue.toFloat shouldBe None
    ByteString("").toValue.toFloat shouldBe None
    ByteString("one\\two").toValue.toFloat shouldBe None
  }

  "Parsing double values" should "return empty sequence for empty byte string" in {
    ByteString.empty.toValue.toDoubles shouldBe Seq.empty
  }

  it should "parse properly formatted double values" in {
    ByteString("1234\\1.234").toValue.toDoubles shouldBe Seq(1234, 1.234)
  }

  it should "ignore improperly formatted entries" in {
    ByteString("1234\\3.14\\one\\456").toValue.toDoubles shouldBe Seq(1234, 3.14, 456)
    ByteString("one").toValue.toDoubles shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    ByteString(" 1.234 \\3.14 \\one\\4.56  ").toValue.toDoubles shouldBe Seq(1.234, 3.14, 4.56)
  }

  it should "treat commas and dots as decimal separator" in {
    ByteString("1.2\\1,2").toValue.toDoubles shouldBe Seq(1.2, 1.2)
  }

  "Parsing a single double value" should "return the first valid entry among multiple values" in {
    ByteString("one\\4.5\\55").toValue.toDouble shouldBe Some(4.5)
  }

  it should "return None if no valid entry exists" in {
    ByteString.empty.toValue.toDouble shouldBe None
    ByteString("").toValue.toDouble shouldBe None
    ByteString("one\\two").toValue.toDouble shouldBe None
  }

  "Parsing date strings" should "return empty sequence for empty byte string" in {
    ByteString.empty.toValue.toDates shouldBe Seq.empty
  }

  it should "parse properly formatted date strings" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    ByteString("20040329\\2004.03.29").toValue.toDates shouldBe Seq(date, date)
  }

  it should "ignore improperly formatted entries" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    ByteString("20040329\\one\\2004.03.29").toValue.toDates shouldBe Seq(date, date)
    ByteString("one").toValue.toDates shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    ByteString(" 20040329 \\20040329 \\one\\2004.03.29  ").toValue.toDates shouldBe Seq(date, date, date)
  }

  "Parsing a single date string" should "return the first valid entry among multiple values" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    ByteString("one\\20040329\\20050401").toValue.toDate shouldBe Some(date)
  }

}
