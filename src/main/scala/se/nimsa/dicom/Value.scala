package se.nimsa.dicom

import java.text.{ParsePosition, SimpleDateFormat}
import java.util.Date

import akka.util.ByteString
import se.nimsa.dicom.VR.VR

class Value(val bytes: ByteString) {
  import Value._

  def toStrings(vr: VR, characterSets: CharacterSets): Seq[String] = if (bytes.isEmpty) Seq.empty else split(characterSets.decode(vr, bytes))
  def toSingleString(vr: VR, characterSets: CharacterSets): String = characterSets.decode(vr, bytes)
  def toStrings: Seq[String] = toStrings(VR.CS, CharacterSets.defaultOnly)
  def toSingleString: String = toSingleString(VR.CS, CharacterSets.defaultOnly)
  def toLongs: Seq[Long] = if (bytes.isEmpty) Seq.empty[Long] else split(bytesToString).flatMap(stringToLong)
  def toLong: Option[Long] = toLongs.headOption
  def toInts: Seq[Int] = if (bytes.isEmpty) Seq.empty else split(bytesToString).flatMap(stringToInt)
  def toInt: Option[Int] = toInts.headOption
  def toShorts: Seq[Short] = if (bytes.isEmpty) Seq.empty else split(bytesToString).flatMap(stringToShort)
  def toShort: Option[Short] = toShorts.headOption
  def toFloats: Seq[Float] = if (bytes.isEmpty) Seq.empty else split(bytesToString).flatMap(stringToFloat)
  def toFloat: Option[Float] = toFloats.headOption
  def toDoubles: Seq[Double] = if (bytes.isEmpty) Seq.empty else split(bytesToString).flatMap(stringToDouble)
  def toDouble: Option[Double] = toDoubles.headOption
  def toDates: Seq[Date] = if (bytes.isEmpty) Seq.empty else split(bytesToString).flatMap(stringToDate)
  def toDate: Option[Date] = toDates.headOption
  private def bytesToString: String = bytes.utf8String

  def ++(value: Value): Value = Value(bytes ++ value.bytes)

  override def toString: String = toSingleString(VR.PN, CharacterSets.defaultOnly)
}

/**
  * Utility methods for parsing DICOM data element value bytes to strings, numbers, dates, etc.
  */
object Value {

  final val empty = Value(ByteString.empty)

  final lazy val multiValueDelimiter = """\"""
  final lazy val multiValueDelimiterRegexp = """\\"""

  final lazy val dateFormat1: SimpleDateFormat = new SimpleDateFormat("yyyyMMdd")
  final lazy val dateFormat2: SimpleDateFormat = new SimpleDateFormat("yyyy.MM.dd")

  implicit class ByteStringExtension(val bytes: ByteString) extends AnyVal {
    def toValue: Value = new Value(bytes)
  }

  def apply(bytes: ByteString): Value = new Value(bytes)

  def split(s: String): Array[String] = s.split(multiValueDelimiterRegexp).map(_.trim)
  def stringToLong(s: String): Option[Long] = try Option(java.lang.Long.parseLong(s)) catch { case _: Throwable => None }
  def stringToInt(s: String): Option[Int] = try Option(java.lang.Integer.parseInt(s)) catch { case _: Throwable => None }
  def stringToShort(s: String): Option[Short] = try Option(java.lang.Short.parseShort(s)) catch { case _: Throwable => None }
  def stringToFloat(s: String): Option[Float] = try Option(java.lang.Float.parseFloat(s.replace(',', '.'))) catch { case _: Throwable => None }
  def stringToDouble(s: String): Option[Double] = try Option(java.lang.Double.parseDouble(s.replace(',', '.'))) catch { case _: Throwable => None }
  def stringToDate(s: String): Option[Date] = try {
    val x = s.trim
    val pos = new ParsePosition(0)
    val d1 = dateFormat1.parse(x, pos)
    if (pos.getIndex == x.length) Option(d1) else {
      pos.setIndex(0)
      val d2 = dateFormat2.parse(x, pos)
      if (pos.getIndex == x.length) Option(d2) else None
    }
  } catch { case _: Throwable => None }

}
