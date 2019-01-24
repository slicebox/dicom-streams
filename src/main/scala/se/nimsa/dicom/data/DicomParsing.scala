/*
 * Copyright 2018 Lars Edenbrandt
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package se.nimsa.dicom.data

import java.net.URI
import java.time.format.{DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField._
import java.time._

import akka.util.ByteString
import se.nimsa.dicom.data.VR.VR
import se.nimsa.dicom.streams.DicomStreamException

/**
  * Helper methods for parsing binary DICOM data to DICOM parts and other types.
  */
trait DicomParsing {

  val dicomPreambleLength = 132

  case class Info(tag: Int, vr: VR, bigEndian: Boolean, explicitVR: Boolean, hasFmi: Boolean)

  def headerInfo(data: ByteString): Option[Info] =
    headerInfo(data, assumeBigEndian = false)
      .orElse(headerInfo(data, assumeBigEndian = true))

  private def headerInfo(data: ByteString, assumeBigEndian: Boolean): Option[Info] = {
    val tag = bytesToTag(data, assumeBigEndian)
    val vr = Dictionary.vrOf(tag)
    if (vr == VR.UN)
      None
    else {
      if (bytesToVR(data.drop(4)) == vr.code)
        Some(Info(
          tag,
          vr,
          bigEndian = assumeBigEndian,
          explicitVR = true,
          hasFmi = isFileMetaInformation(tag)))
      else if (intToUnsignedLong(bytesToInt(data.drop(4), assumeBigEndian)) >= 0)
        if (assumeBigEndian)
          throw new DicomStreamException("Implicit VR Big Endian encoded DICOM Stream")
        else
          Some(Info(
            tag,
            vr,
            bigEndian = false,
            explicitVR = false,
            hasFmi = isFileMetaInformation(tag)))
      else
        None
    }
  }

  def lengthToLong(length: Int): Long = if (length == indeterminateLength) -1L else intToUnsignedLong(length)

  def tagVr(data: ByteString, bigEndian: Boolean, explicitVr: Boolean): (Int, VR) = {
    val tag = bytesToTag(data, bigEndian)
    if (tag == 0xFFFEE000 || tag == 0xFFFEE00D || tag == 0xFFFEE0DD)
      (tag, null)
    else if (explicitVr)
      (tag, VR.valueOf(bytesToVR(data.drop(4))))
    else
      (tag, Dictionary.vrOf(tag))
  }

  def isPreamble(data: ByteString): Boolean = data.length >= dicomPreambleLength && data.slice(dicomPreambleLength - 4, dicomPreambleLength) == ByteString('D', 'I', 'C', 'M')
  def isHeader(data: ByteString): Boolean = headerInfo(data).isDefined
  def isFileMetaInformation(tag: Int): Boolean = (tag & 0xFFFF0000) == 0x00020000
  def isPrivate(tag: Int): Boolean = groupNumber(tag) % 2 == 1
  def isGroupLength(tag: Int): Boolean = elementNumber(tag) == 0
  def isDeflated(transferSyntaxUid: String): Boolean = transferSyntaxUid == UID.DeflatedExplicitVRLittleEndian || transferSyntaxUid == UID.JPIPReferencedDeflate


  // parsing of value bytes for various value representations to higher types

  final val multiValueDelimiter = """\"""
  final val multiValueDelimiterRegex = """\\"""

  def split(bytes: ByteString, size: Int): Seq[ByteString] = bytes.grouped(size).filter(_.length == size).toSeq
  def split(s: String): Seq[String] = s.split(multiValueDelimiterRegex)

  def trim(s: String): String = s.trim
  def trimPadding(s: String, paddingByte: Byte): String = {
    var index = s.length - 1
    while (index >= 0 && s(index) <= paddingByte)
      index -= 1
    val n = s.length - 1 - index
    if (n > 0) s.dropRight(n) else s
  }

  def parseAT(value: ByteString, bigEndian: Boolean): Seq[Int] = split(value, 4).map(b => bytesToTag(b, bigEndian))
  def parseSL(value: ByteString, bigEndian: Boolean): Seq[Int] = split(value, 4).map(bytesToInt(_, bigEndian))
  def parseUL(value: ByteString, bigEndian: Boolean): Seq[Long] = parseSL(value, bigEndian).map(intToUnsignedLong)
  def parseSS(value: ByteString, bigEndian: Boolean): Seq[Short] = split(value, 2).map(bytesToShort(_, bigEndian))
  def parseUS(value: ByteString, bigEndian: Boolean): Seq[Int] = parseSS(value, bigEndian).map(shortToUnsignedInt)
  def parseFL(value: ByteString, bigEndian: Boolean): Seq[Float] = split(value, 4).map(bytesToFloat(_, bigEndian))
  def parseFD(value: ByteString, bigEndian: Boolean): Seq[Double] = split(value, 8).map(bytesToDouble(_, bigEndian))
  def parseDS(value: ByteString): Seq[Double] = split(value.utf8String).map(trim).flatMap(s => try Option(java.lang.Double.parseDouble(s)) catch {
    case _: Throwable => None
  })
  def parseIS(value: ByteString): Seq[Long] = split(value.utf8String).map(trim).flatMap(s => try Option(java.lang.Long.parseLong(s)) catch {
    case _: Throwable => None
  })
  def parseDA(value: ByteString): Seq[LocalDate] = split(value.utf8String).flatMap(parseDate)
  def parseTM(value: ByteString): Seq[LocalTime] = split(value.utf8String).flatMap(parseTime)
  def parseDT(value: ByteString, zoneOffset: ZoneOffset): Seq[ZonedDateTime] = split(value.utf8String).flatMap(parseDateTime(_, zoneOffset))
  def parsePN(string: String): Seq[PatientName] = split(string).map(trimPadding(_, VR.PN.paddingByte)).flatMap(parsePatientName)
  def parsePN(value: ByteString, characterSets: CharacterSets): Seq[PatientName] = parsePN(characterSets.decode(VR.PN, value))
  def parseUR(string: String): Option[URI] = parseURI(string)
  def parseUR(value: ByteString): Option[URI] = parseUR(trimPadding(value.utf8String, VR.UR.paddingByte))

  // parsing of strings to more specific types

  final val dateFormat = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 4, SignStyle.EXCEEDS_PAD)
    .appendPattern("['.']MM['.']dd")
    .toFormatter

  final val timeFormat = new DateTimeFormatterBuilder()
    .appendValue(HOUR_OF_DAY, 2, 2, SignStyle.EXCEEDS_PAD)
    .appendPattern("[[':']mm[[':']ss[")
    .appendFraction(MICRO_OF_SECOND, 1, 6, true)
    .appendPattern("]]]")
    .parseDefaulting(MINUTE_OF_HOUR, 0)
    .parseDefaulting(SECOND_OF_MINUTE, 0)
    .parseDefaulting(MICRO_OF_SECOND, 0)
    .toFormatter

  final val dateTimeFormat = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 4, SignStyle.EXCEEDS_PAD)
    .appendPattern("[MM[dd[HH[mm[ss[")
    .appendFraction(MICRO_OF_SECOND, 1, 6, true)
    .appendPattern("]]]]]]")
    .parseDefaulting(MONTH_OF_YEAR, 1)
    .parseDefaulting(DAY_OF_MONTH, 1)
    .parseDefaulting(HOUR_OF_DAY, 0)
    .parseDefaulting(MINUTE_OF_HOUR, 0)
    .parseDefaulting(SECOND_OF_MINUTE, 0)
    .parseDefaulting(MICRO_OF_SECOND, 0)
    .toFormatter

  final val dateTimeZoneFormat = new DateTimeFormatterBuilder()
    .append(dateTimeFormat)
    .appendPattern("[Z]")
    .toFormatter

  final val dateFormatForEncoding = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 4, SignStyle.EXCEEDS_PAD)
    .appendPattern("MMdd")
    .toFormatter

  final val timeFormatForEncoding = new DateTimeFormatterBuilder()
    .appendValue(HOUR_OF_DAY, 2, 2, SignStyle.EXCEEDS_PAD)
    .appendPattern("mmss")
    .toFormatter

  val defaultCharacterSet: CharacterSets = CharacterSets.defaultOnly

  def formatDate(date: LocalDate): String = date.format(dateFormatForEncoding)

  def formatTime(time: LocalTime): String = time.format(timeFormatForEncoding)

  def formatDateTime(dateTime: ZonedDateTime): String = dateTime.format(dateTimeZoneFormat)

  def parseDate(s: String): Option[LocalDate] =
    try Option(LocalDate.parse(s.trim, dateFormat)) catch {
      case _: Throwable => None
    }

  def parseTime(s: String): Option[LocalTime] =
    try Option(LocalTime.parse(s.trim, timeFormat)) catch {
      case _: Throwable => None
    }

  def parseDateTime(s: String, zoneOffset: ZoneOffset): Option[ZonedDateTime] = {
    val trimmed = s.trim
    try Option(ZonedDateTime.parse(trimmed, dateTimeZoneFormat)) catch {
      case _: Throwable => try Option(LocalDateTime.parse(trimmed, dateTimeFormat).atZone(zoneOffset)) catch {
        case _: Throwable => None
      }
    }
  }

  def parseZoneOffset(s: String): Option[ZoneOffset] =
    try Option(ZoneOffset.of(s)) catch {
      case _: Throwable => None
    }

  def parsePatientName(s: String): Option[PatientName] = {
    def ensureLength(ss: Seq[String], n: Int) = ss ++ Seq.fill(math.max(0, n - ss.length))("")

    val comps = ensureLength(s.split("""\^"""), 5)
      .map(s => ensureLength(s.split("="), 3).map(trim))
      .map(c => ComponentGroup(c.head, c(1), c(2)))

    Option(PatientName(comps.head, comps(1), comps(2), comps(3), comps(4)))
  }

  def parseURI(s: String): Option[URI] = try Option(new URI(s)) catch { case _: Throwable => None }

}

object DicomParsing extends DicomParsing
