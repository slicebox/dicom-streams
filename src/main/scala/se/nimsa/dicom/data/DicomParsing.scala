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

import java.time.format.{DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField._
import java.time.{LocalDate, LocalDateTime, ZoneOffset, ZonedDateTime}

import akka.util.ByteString
import se.nimsa.dicom.data.VR.VR
import se.nimsa.dicom.streams.DicomStreamException

/**
  * Helper methods for parsing binary DICOM data to dicom parts and other types.
  */
trait DicomParsing {

  val dicomPreambleLength = 132

  case class Info(bigEndian: Boolean, explicitVR: Boolean, hasFmi: Boolean) {
    /**
      * Best guess for transfer syntax.
      *
      * @return transfer syntax uid value
      */
    def assumedTransferSyntax: String = if (explicitVR) {
      if (bigEndian) {
        UID.ExplicitVRBigEndianRetired
      } else {
        UID.ExplicitVRLittleEndian
      }
    } else {
      UID.ImplicitVRLittleEndian
    }
  }

  case class Element(tag: Int, vr: VR, length: Long, value: ByteString)

  def dicomInfo(data: ByteString): Option[Info] =
    dicomInfo(data, assumeBigEndian = false)
      .orElse(dicomInfo(data, assumeBigEndian = true))

  private def dicomInfo(data: ByteString, assumeBigEndian: Boolean): Option[Info] = {
    val tag1 = bytesToTag(data, assumeBigEndian)
    val vr = Dictionary.vrOf(tag1)
    if (vr == VR.UN)
      None
    else {
      if (bytesToVR(data.drop(4)) == vr.code)
        Some(Info(
          bigEndian = assumeBigEndian,
          explicitVR = true,
          hasFmi = isFileMetaInformation(tag1)))
      else if (intToUnsignedLong(bytesToInt(data.drop(4), assumeBigEndian)) >= 0)
        if (assumeBigEndian)
          throw new DicomStreamException("Implicit VR Big Endian encoded DICOM Stream")
        else
          Some(Info(
            bigEndian = false,
            explicitVR = false,
            hasFmi = isFileMetaInformation(tag1)))
      else
        None
    }
  }

  // parse dicom UID element from buffer
  def parseUIDElement(data: ByteString, explicitVR: Boolean, assumeBigEndian: Boolean): Element = {
    def valueWithoutPadding(value: ByteString) =
      if (value.takeRight(1).contains(0.toByte)) {
        value.dropRight(1)
      } else {
        value
      }

    val maybeHeader = if (explicitVR) {
      readHeaderExplicitVR(data, assumeBigEndian)
    } else {
      readHeaderImplicitVR(data)
    }

    val (tag, vr, headerLength, length) = maybeHeader
      .getOrElse(throw new DicomStreamException("Could not parse DICOM data element from stream."))
    val value = data.drop(headerLength).take(length.toInt)
    Element(tag, vr, length, valueWithoutPadding(value))
  }

  def lengthToLong(length: Int): Long = if (length == -1) -1L else intToUnsignedLong(length)

  def readHeader(buffer: ByteString, assumeBigEndian: Boolean, explicitVR: Boolean): Option[(Int, VR, Int, Long)] = {
    if (explicitVR) {
      readHeaderExplicitVR(buffer, assumeBigEndian)
    } else {
      readHeaderImplicitVR(buffer)
    }
  }

  /**
    * Read header of data element for explicit VR
    *
    * @param buffer          current buffer
    * @param assumeBigEndian true if big endian, false otherwise
    * @return
    */
  def readHeaderExplicitVR(buffer: ByteString, assumeBigEndian: Boolean): Option[(Int, VR, Int, Long)] = {
    if (buffer.size >= 8) {
      val (tag, vr) = DicomParsing.tagVr(buffer, assumeBigEndian, explicitVr = true)
      if (vr == null) {
        // special case: sequences, length might be undefined '0xFFFFFFFF'
        val valueLength = bytesToInt(buffer.drop(4), assumeBigEndian)
        if (valueLength == -1) {
          // length of sequence undefined, not supported
          None
        } else {
          Some((tag, vr, 8, lengthToLong(bytesToInt(buffer.drop(4), assumeBigEndian))))
        }
      } else if (vr.headerLength == 8) {
        Some((tag, vr, 8, lengthToLong(bytesToUShort(buffer.drop(6), assumeBigEndian))))
      } else {
        if (buffer.size >= 12) {
          Some((tag, vr, 12, lengthToLong(bytesToInt(buffer.drop(8), assumeBigEndian))))
        } else {
          None
        }
      }
    } else {
      None
    }
  }

  /**
    * Read header of data element for implicit VR.
    *
    * @param buffer current buffer
    * @return
    */
  def readHeaderImplicitVR(buffer: ByteString): Option[(Int, VR, Int, Long)] =
    if (buffer.size >= 8) {
      val assumeBigEndian = false // implicit VR
      val tag = bytesToTag(buffer, assumeBigEndian)
      val vr = Dictionary.vrOf(tag)
      val valueLength = bytesToInt(buffer.drop(4), assumeBigEndian)
      if (tag == 0xFFFEE000 || tag == 0xFFFEE00D || tag == 0xFFFEE0DD)
        if (valueLength == -1)
          None // special case: sequences, with undefined length '0xFFFFFFFF' not supported
        else
          Some((tag, null, 8, lengthToLong(valueLength)))
      else
        Some((tag, vr, 8, lengthToLong(valueLength)))
    } else
      None

  def tagVr(data: ByteString, bigEndian: Boolean, explicitVr: Boolean): (Int, VR) = {
    val tag = bytesToTag(data, bigEndian)
    if (tag == 0xFFFEE000 || tag == 0xFFFEE00D || tag == 0xFFFEE0DD)
      (tag, null)
    else if (explicitVr)
      (tag, VR.valueOf(bytesToVR(data.drop(4))))
    else
      (tag, VR.UN)
  }

  def isPreamble(data: ByteString): Boolean = data.length >= dicomPreambleLength && data.slice(dicomPreambleLength - 4, dicomPreambleLength) == ByteString('D', 'I', 'C', 'M')
  def isHeader(data: ByteString): Boolean = dicomInfo(data).isDefined

  def isSequenceDelimiter(tag: Int): Boolean = groupNumber(tag) == 0xFFFE
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
  def parseDT(value: ByteString, zoneOffset: ZoneOffset): Seq[ZonedDateTime] = split(value.utf8String).flatMap(parseDateTime(_, zoneOffset))
  def parsePN(value: ByteString, characterSets: CharacterSets): Seq[PatientName] = split(characterSets.decode(VR.PN, value)).map(trimPadding(_, VR.PN.paddingByte)).flatMap(parsePatientName)


  // parsing of strings to more specific types

  final val dateFormat = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 4, SignStyle.EXCEEDS_PAD)
    .appendPattern("['.']MM['.']dd")
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

  def systemZone: ZoneOffset = ZonedDateTime.now().getOffset

  def parseDate(s: String): Option[LocalDate] =
    try Option(LocalDate.parse(s.trim, dateFormat)) catch {
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

  def parsePatientName(s: String): Option[PatientName] = {
    def ensureLength(ss: Seq[String], n: Int) = ss ++ Seq.fill(math.max(0, n - ss.length))("")

    val comps = ensureLength(s.split("""\^"""), 5)
      .map(s => ensureLength(s.split("="), 3).map(trim))
      .map(c => ComponentGroup(c.head, c(1), c(2)))

    Option(PatientName(comps.head, comps(1), comps(2), comps(3), comps(4)))
  }

}

object DicomParsing extends DicomParsing
