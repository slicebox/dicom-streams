package se.nimsa.dicom

import java.text.{ParsePosition, SimpleDateFormat}
import java.util.Date

import akka.util.ByteString
import se.nimsa.dicom.VR._

import scala.collection.mutable.ArrayBuffer

case class Element(tagPath: TagPath, bigEndian: Boolean, vr: VR, explicitVR: Boolean, length: Long, value: ByteString) {

  import Element._

  def toStrings(characterSets: CharacterSets): Seq[String] = if (value.isEmpty) Seq.empty else
    vr match {
      case AT => splitTrim.map(b => tagToString(bytesToTag(b, bigEndian)))
      case FL | OF => toFloats.map(_.toString)
      case FD | OD => toDoubles.map(_.toString)
      case SL => toInts.map(_.toString)
      case SS => toShorts.map(_.toString)
      case UL => toInts.map(Integer.toUnsignedString)
      case US => toShorts.map(java.lang.Short.toUnsignedInt).map(_.toString)
      case OB | OW => trimSplit.map(_.map(byte => Integer.toHexString(byte.toInt)).mkString(" "))
      case ST | LT | UT => trimSplit.map(characterSets.decode(vr, _))
      case _ => splitTrim.map(characterSets.decode(vr, _))
    }

  def toSingleString(characterSets: CharacterSets): String = toStrings(characterSets).mkString(multiValueDelimiter)

  def toStrings: Seq[String] = toStrings(CharacterSets.defaultOnly)
  def toSingleString: String = toSingleString(CharacterSets.defaultOnly)

  def toInts: Seq[Int] = trimSplit.filter(_.length == 4).map(b => bytesToInt(b, bigEndian))
  def toShorts: Seq[Short] = trimSplit.filter(_.length == 2).map(b => bytesToShort(b, bigEndian))
  def toFloats: Seq[Float] = trimSplit.filter(_.length == 4).map(b => bytesToFloat(b, bigEndian))
  def toDoubles: Seq[Double] = trimSplit.filter(_.length == 8).map(b => bytesToDouble(b, bigEndian))
  def toDates: Seq[Date] = splitTrim.map(_.utf8String).flatMap(parseDate)

  def toInt: Option[Int] = toInts.headOption
  def toShort: Option[Short] = toShorts.headOption
  def toFloat: Option[Float] = toFloats.headOption
  def toDouble: Option[Double] = toDoubles.headOption
  def toDate: Option[Date] = toDates.headOption

  private def trimSplit: Seq[ByteString] = split(trim(value, vr), vr)
  private def splitTrim: Seq[ByteString] = split(value, vr).map(b => trim(b, vr))
}

object Element {

  final lazy val multiValueDelimiter = """\"""
  final lazy val multiValueDelimiterChar = '\\'

  final lazy val dateFormat1: SimpleDateFormat = new SimpleDateFormat("yyyyMMdd")
  final lazy val dateFormat2: SimpleDateFormat = new SimpleDateFormat("yyyy.MM.dd")

  def parseDate(s: String): Option[Date] = try {
    val x = s.trim
    val pos = new ParsePosition(0)
    val d1 = dateFormat1.parse(x, pos)
    if (pos.getIndex == x.length) Option(d1) else {
      pos.setIndex(0)
      val d2 = dateFormat2.parse(x, pos)
      if (pos.getIndex == x.length) Option(d2) else None
    }
  } catch {
    case _: Throwable => None
  }

  private def split(bytes: ByteString, vr: VR): Seq[ByteString] = vr match {
    case AE | AS | CS | DA | DS | DT | IS | LO | PN | SH | TM | UC | UI | UR =>
      val separatorIndices = ArrayBuffer(-1)
      for (i <- bytes.indices)
        if (bytes(i) == multiValueDelimiterChar)
          separatorIndices += i
      separatorIndices += bytes.length
      separatorIndices
        .sliding(2)
        .map { indices =>
          val (start, end) = (indices.head + 1, indices.last)
          bytes.slice(start, end)
        }
        .filter(_.nonEmpty)
        .toSeq
    case US | SS => bytes.grouped(2).toSeq
    case UL | SL | AT | FL => bytes.grouped(4).toSeq
    case FD => bytes.grouped(8).toSeq
    case _ => Seq(bytes)
  }

  private def trim(bytes: ByteString, vr: VR): ByteString =
    vr match {
      case AE | AS | CS | DA | DS | DT | IS | LO | SH | TM | UR => trimLeading(trimTrailing(bytes, vr.paddingByte), vr.paddingByte)
      case ST | LT | UT | PN | UC => trimTrailing(bytes, vr.paddingByte)
      case OB | UI => trimTrailing(bytes, vr.paddingByte)
      case _ => bytes
    }

  private def trimLeading(bytes: ByteString, c: Int): ByteString = {
    var index = 0
    while (index < bytes.length && (bytes(index) & 0xFF) == c)
      index += 1
    if (index > 0) bytes.drop(index) else bytes
  }

  private def trimTrailing(bytes: ByteString, c: Int): ByteString = {
    var index = bytes.length - 1
    while (index >= 0 && (bytes(index) & 0xFF) == c)
      index -= 1
    val n = bytes.length - 1 - index
    if (n > 0) bytes.dropRight(n) else bytes
  }

}