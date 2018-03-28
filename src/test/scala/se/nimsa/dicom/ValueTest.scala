package se.nimsa.dicom

import java.util.GregorianCalendar

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}

class ValueTest extends FlatSpec with Matchers {

  import CharacterSets._
  import Value._

  "Formatting bytes into multiple strings" should "return empty sequence for empty byte string" in {
    toStrings(VR.PN, defaultOnly, ByteString.empty) shouldBe Seq.empty
  }

  it should "throw an exception for null input" in {
    intercept[NullPointerException] {
      toStrings(VR.PN, defaultOnly, null) shouldBe Seq.empty
    }
  }

  it should "split a string according to the DICOM multiple value delimiter" in {
    toStrings(VR.PN, defaultOnly, ByteString("one\\two\\three")) shouldBe Seq("one", "two", "three")
  }

  it should "trim any characters at or below 0x20 at beginning and end" in {
    toStrings(VR.PN, defaultOnly, ByteString(0x0, 0x5, 0x15, 0x20, 0x41, 0x41, 0x20, 0x10, 0x0)) shouldBe Seq("AA")
  }

  it should "trim any characters at or below 0x20 at beginning and end of each value" in {
    toStrings(VR.PN, defaultOnly, ByteString("  one \\ two \\three  ")) shouldBe Seq("one", "two", "three")
  }

  it should "split and trim strings with multiple character set encodings" in {
    val nameBytes = ByteString(0x20, 0xD4, 0xCF, 0xC0, 0xDE, 0x5C, 0x20, 0xC0, 0xDB, 0xB3, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28, 0x4A, 0x5C, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x4A, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64, 0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x4A, 0x5C, 0x20, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B, 0x28, 0x4A)
    toStrings(VR.PN, new CharacterSets(Seq("ISO 2022 IR 13", "ISO 2022 IR 87")), nameBytes) shouldBe Seq("ﾔﾏﾀﾞ", "ﾀﾛｳ=山田", "太郎=やまだ", "たろう")
  }

  "Formatting bytes into a single string" should "return empty string for empty byte string" in {
    toSingleString(VR.PN, defaultOnly, ByteString.empty) shouldBe empty
  }

  it should "not split a string with DICOM multiple value delimiters" in {
    toSingleString(VR.PN, defaultOnly, ByteString("one\\two\\three")) shouldBe "one\\two\\three"
  }

  it should "not trim the string" in {
    toSingleString(VR.PN, defaultOnly, ByteString("   one two  ")) shouldBe "   one two  "
  }

  "Parsing long values" should "return empty sequence for empty byte string" in {
    toLongs(ByteString.empty) shouldBe Seq.empty
  }

  it should "parse properly formatted long values" in {
    toLongs(ByteString("1234\\123456789012345")) shouldBe Seq(1234L, 123456789012345L)
  }

  it should "ignore improperly formatted entries" in {
    toLongs(ByteString("1234\\3.14\\one\\456")) shouldBe Seq(1234L, 456L)
    toLongs(ByteString("one")) shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    toLongs(ByteString(" 1234 \\3.14 \\one\\456  ")) shouldBe Seq(1234L, 456L)
  }

  "Parsing a single long value" should "return the first valid entry among multiple values" in {
    toLong(ByteString("one\\45\\55")) shouldBe Some(45L)
  }

  it should "return None if no valid entry exists" in {
    toLong(ByteString.empty) shouldBe None
    toLong(ByteString("")) shouldBe None
    toLong(ByteString("one\\two")) shouldBe None
  }

  "Parsing int values" should "return empty sequence for empty byte string" in {
    toInts(ByteString.empty) shouldBe Seq.empty
  }

  it should "parse properly formatted int values" in {
    toInts(ByteString("1234\\1234567890")) shouldBe Seq(1234, 1234567890)
  }

  it should "ignore improperly formatted entries" in {
    toInts(ByteString("1234\\3.14\\one\\456")) shouldBe Seq(1234, 456)
    toInts(ByteString("one")) shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    toInts(ByteString(" 1234 \\3.14 \\one\\456  ")) shouldBe Seq(1234, 456)
  }

  "Parsing a single int value" should "return the first valid entry among multiple values" in {
    toInt(ByteString("one\\45\\55")) shouldBe Some(45)
  }

  it should "return None if no valid entry exists" in {
    toInt(ByteString.empty) shouldBe None
    toInt(ByteString("")) shouldBe None
    toInt(ByteString("one\\two")) shouldBe None
  }

  "Parsing short values" should "return empty sequence for empty byte string" in {
    toShorts(ByteString.empty) shouldBe Seq.empty
  }

  it should "parse properly formatted short values" in {
    toShorts(ByteString("1234\\12345")) shouldBe Seq(1234.toShort, 12345.toShort)
  }

  it should "ignore improperly formatted entries" in {
    toShorts(ByteString("1234\\3.14\\one\\456")) shouldBe Seq(1234.toShort, 456.toShort)
    toShorts(ByteString("one")) shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    toShorts(ByteString(" 1234 \\3.14 \\one\\456  ")) shouldBe Seq(1234.toShort, 456.toShort)
  }

  "Parsing a single short value" should "return the first valid entry among multiple values" in {
    toShort(ByteString("one\\45\\55")) shouldBe Some(45.toShort)
  }

  it should "return None if no valid entry exists" in {
    toShort(ByteString.empty) shouldBe None
    toShort(ByteString("")) shouldBe None
    toShort(ByteString("one\\two")) shouldBe None
  }

  "Parsing float values" should "return empty sequence for empty byte string" in {
    toFloats(ByteString.empty) shouldBe Seq.empty
  }

  it should "parse properly formatted float values" in {
    toFloats(ByteString("1234\\1.234")) shouldBe Seq(1234.toFloat, 1.234.toFloat)
  }

  it should "ignore improperly formatted entries" in {
    toFloats(ByteString("1234\\3.14\\one\\456")) shouldBe Seq(1234.toFloat, 3.14.toFloat, 456.toFloat)
    toFloats(ByteString("one")) shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    toFloats(ByteString(" 1.234 \\3.14 \\one\\4.56  ")) shouldBe Seq(1.234.toFloat, 3.14.toFloat, 4.56.toFloat)
  }

  it should "treat commas and dots as decimal separator" in {
    toFloats(ByteString("1.2\\1,2")) shouldBe Seq(1.2.toFloat, 1.2.toFloat)
  }

  "Parsing a single float value" should "return the first valid entry among multiple values" in {
    toFloat(ByteString("one\\4.5\\55")) shouldBe Some(4.5.toFloat)
  }

  it should "return None if no valid entry exists" in {
    toFloat(ByteString.empty) shouldBe None
    toFloat(ByteString("")) shouldBe None
    toFloat(ByteString("one\\two")) shouldBe None
  }

  "Parsing double values" should "return empty sequence for empty byte string" in {
    toDoubles(ByteString.empty) shouldBe Seq.empty
  }

  it should "parse properly formatted double values" in {
    toDoubles(ByteString("1234\\1.234")) shouldBe Seq(1234, 1.234)
  }

  it should "ignore improperly formatted entries" in {
    toDoubles(ByteString("1234\\3.14\\one\\456")) shouldBe Seq(1234, 3.14, 456)
    toDoubles(ByteString("one")) shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    toDoubles(ByteString(" 1.234 \\3.14 \\one\\4.56  ")) shouldBe Seq(1.234, 3.14, 4.56)
  }

  it should "treat commas and dots as decimal separator" in {
    toDoubles(ByteString("1.2\\1,2")) shouldBe Seq(1.2, 1.2)
  }

  "Parsing a single double value" should "return the first valid entry among multiple values" in {
    toDouble(ByteString("one\\4.5\\55")) shouldBe Some(4.5)
  }

  it should "return None if no valid entry exists" in {
    toDouble(ByteString.empty) shouldBe None
    toDouble(ByteString("")) shouldBe None
    toDouble(ByteString("one\\two")) shouldBe None
  }

  "Parsing date strings" should "return empty sequence for empty byte string" in {
    toDates(ByteString.empty) shouldBe Seq.empty
  }

  it should "parse properly formatted date strings" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toDates(ByteString("20040329\\2004.03.29")) shouldBe Seq(date, date)
  }

  it should "ignore improperly formatted entries" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toDates(ByteString("20040329\\one\\2004.03.29")) shouldBe Seq(date, date)
    toDates(ByteString("one")) shouldBe Seq.empty
  }

  it should "trim whitespace" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toDates(ByteString(" 20040329 \\20040329 \\one\\2004.03.29  ")) shouldBe Seq(date, date, date)
  }

  "Parsing a single date string" should "return the first valid entry among multiple values" in {
    val date = new GregorianCalendar(2004, 2, 29, 0, 0, 0).getTime
    toDate(ByteString("one\\20040329\\20050401")) shouldBe Some(date)
  }

}
