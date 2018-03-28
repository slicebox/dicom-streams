package se.nimsa.dicom

import java.text.{ParsePosition, SimpleDateFormat}
import java.util.Date

import akka.util.ByteString
import se.nimsa.dicom.VR.VR

/**
  * Utility methods for parsing DICOM attribute value bytes to strings, numbers, dates, etc.
  */
object Value {

  final lazy val multiValueDelimiter = "\\"
  final lazy val multiValueDelimiterRegexp = "\\\\"

  private final lazy val dateFormat1: SimpleDateFormat = new SimpleDateFormat("yyyyMMdd")
  private final lazy val dateFormat2: SimpleDateFormat = new SimpleDateFormat("yyyy.MM.dd")

  def toStrings(vr: VR, characterSets: CharacterSets, bytes: ByteString): Seq[String] = if (bytes.isEmpty) Seq.empty else split(characterSets.decode(vr, bytes))
  def toSingleString(vr: VR, characterSets: CharacterSets, bytes: ByteString): String = characterSets.decode(vr, bytes)
  def toLongs(bytes: ByteString): Seq[Long] = if (bytes.isEmpty) Seq.empty[Long] else split(toString(bytes)).flatMap(stringToLong)
  def toLong(bytes: ByteString): Option[Long] = toLongs(bytes).headOption
  def toInts(bytes: ByteString): Seq[Int] = if (bytes.isEmpty) Seq.empty else split(toString(bytes)).flatMap(stringToInt)
  def toInt(bytes: ByteString): Option[Int] = toInts(bytes).headOption
  def toShorts(bytes: ByteString): Seq[Short] = if (bytes.isEmpty) Seq.empty else split(toString(bytes)).flatMap(stringToShort)
  def toShort(bytes: ByteString): Option[Short] = toShorts(bytes).headOption
  def toFloats(bytes: ByteString): Seq[Float] = if (bytes.isEmpty) Seq.empty else split(toString(bytes)).flatMap(stringToFloat)
  def toFloat(bytes: ByteString): Option[Float] = toFloats(bytes).headOption
  def toDoubles(bytes: ByteString): Seq[Double] = if (bytes.isEmpty) Seq.empty else split(toString(bytes)).flatMap(stringToDouble)
  def toDouble(bytes: ByteString): Option[Double] = toDoubles(bytes).headOption
  def toDates(bytes: ByteString): Seq[Date] = if (bytes.isEmpty) Seq.empty else split(toString(bytes)).flatMap(stringToDate)
  def toDate(bytes: ByteString): Option[Date] = toDates(bytes).headOption

  private def toString(bytes: ByteString) = bytes.utf8String
  private def split(s: String) = s.split(multiValueDelimiterRegexp).map(_.trim)
  private def stringToLong(s: String) = try Option(java.lang.Long.parseLong(s)) catch { case _: Throwable => None }
  private def stringToInt(s: String) = try Option(java.lang.Integer.parseInt(s)) catch { case _: Throwable => None }
  private def stringToShort(s: String) = try Option(java.lang.Short.parseShort(s)) catch { case _: Throwable => None }
  private def stringToFloat(s: String) = try Option(java.lang.Float.parseFloat(s.replace(',', '.'))) catch { case _: Throwable => None }
  private def stringToDouble(s: String) = try Option(java.lang.Double.parseDouble(s.replace(',', '.'))) catch { case _: Throwable => None }
  private def stringToDate(s: String) = try {
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
