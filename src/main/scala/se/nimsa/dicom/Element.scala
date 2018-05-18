package se.nimsa.dicom

import java.time._
import java.time.format.{DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField._

import akka.util.ByteString
import se.nimsa.dicom.VR._
import se.nimsa.dicom.DicomParsing.isFileMetaInformation
import DicomParts.DicomHeader

/**
  * This class describes a DICOM data element (or attribute).
  *
  * @param tag        the tag number of the element
  * @param bigEndian  `true` if this element is using big endian encoding
  * @param vr         the value representation
  * @param explicitVR `true` if this element is encoded using explicit VR
  * @param length     the length of this element's value in bytes
  * @param value      the binary value of this element, possibly consisting of multiple components
  */
case class Element private[dicom](tag: Int, bigEndian: Boolean, vr: VR, explicitVR: Boolean, length: Long, value: ByteString) {

  import Element._

  /**
    * For each component value of this element's value, return it's string representation
    *
    * @param characterSets Character sets used for string decoding
    * @return a sequence of strings, one for each component of the value
    */
  def toStrings(characterSets: CharacterSets = CharacterSets.defaultOnly): Seq[String] = if (value.isEmpty) Seq.empty else
    vr match {
      case AT => parseAT.map(tagToString)
      case FL => parseFL.map(_.toString)
      case FD => parseFD.map(_.toString)
      case SL => parseSL.map(_.toString)
      case SS => parseSS.map(_.toString)
      case UL => parseSL.map(Integer.toUnsignedString)
      case US => parseSS.map(java.lang.Short.toUnsignedInt).map(_.toString)
      case OB => Seq(value.map(byteToHexString).mkString(" "))
      case OW => Seq(split(value, 2).map(bytesToShort(_, bigEndian)).map(shortToHexString).mkString(" "))
      case OF => Seq(parseFL.mkString(" "))
      case OD => Seq(parseFD.mkString(" "))
      case ST | LT | UT => Seq(trimPadding(characterSets.decode(vr, value), vr.paddingByte))
      case _ => split(characterSets.decode(vr, value)).map(trim)
    }

  /**
    * Get the string representation of each component of this value, joined with the bachslash character as separator
    *
    * @param characterSets Character sets used for string decoding
    * @return a string repreentation of all components of this value
    */
  def toSingleString(characterSets: CharacterSets = CharacterSets.defaultOnly): String =
    toStrings(characterSets).mkString(multiValueDelimiter)

  /**
    * @return the value of this element as a sequence of shorts. Casting is performed if necessary. If the value has no
    *         short representation, an empty sequence is returned.
    */
  def toShorts: Seq[Short] = vr match {
    case FL => parseFL.map(_.toShort)
    case FD => parseFD.map(_.toShort)
    case SL => parseSL.map(_.toShort)
    case SS => parseSS
    case UL => parseUL.map(_.toShort)
    case US => parseUS.map(_.toShort)
    case DS => parseDS.map(_.toShort)
    case IS => parseIS.map(_.toShort)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of int. Casting is performed if necessary. If the value has no
    *         int representation, an empty sequence is returned.
    */
  def toInts: Seq[Int] = vr match {
    case AT => parseAT
    case FL => parseFL.map(_.toInt)
    case FD => parseFD.map(_.toInt)
    case SL => parseSL
    case SS => parseSS.map(_.toInt)
    case UL => parseUL.map(_.toInt)
    case US => parseUS
    case DS => parseDS.map(_.toInt)
    case IS => parseIS.map(_.toInt)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of longs. Casting is performed if necessary. If the value has no
    *         long representation, an empty sequence is returned.
    */
  def toLongs: Seq[Long] = vr match {
    case FL => parseFL.map(_.toLong)
    case FD => parseFD.map(_.toLong)
    case SL => parseSL.map(_.toLong)
    case SS => parseSS.map(_.toLong)
    case UL => parseUL
    case US => parseUS.map(_.toLong)
    case DS => parseDS.map(_.toLong)
    case IS => parseIS
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of floats. Casting is performed if necessary. If the value has no
    *         float representation, an empty sequence is returned.
    */
  def toFloats: Seq[Float] = vr match {
    case FL => parseFL
    case FD => parseFD.map(_.toFloat)
    case SL => parseSL.map(_.toFloat)
    case SS => parseSS.map(_.toFloat)
    case UL => parseUL.map(_.toFloat)
    case US => parseUS.map(_.toFloat)
    case DS => parseDS.map(_.toFloat)
    case IS => parseIS.map(_.toFloat)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of doubles. Casting is performed if necessary. If the value has no
    *         double representation, an empty sequence is returned.
    */
  def toDoubles: Seq[Double] = vr match {
    case FL => parseFL.map(_.toDouble)
    case FD => parseFD
    case SL => parseSL.map(_.toDouble)
    case SS => parseSS.map(_.toDouble)
    case UL => parseUL.map(_.toDouble)
    case US => parseUS.map(_.toDouble)
    case DS => parseDS
    case IS => parseIS.map(_.toDouble)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of `LocalDate`s. Casting is performed if necessary. If the value has no
    *         `LocalDate` representation, an empty sequence is returned.
    */
  def toDates: Seq[LocalDate] = vr match {
    case DA => parseDA
    case DT => parseDT(systemZone).map(_.toLocalDate)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of `ZonedDateTime`s. Casting is performed if necessary. If the value has no
    *         `ZonedDateTime` representation, an empty sequence is returned.
    */
  def toDateTimes(zoneOffset: ZoneOffset = systemZone): Seq[ZonedDateTime] = vr match {
    case DA => parseDA.map(_.atStartOfDay(zoneOffset))
    case DT => parseDT(zoneOffset)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of 'PatientName`s. Casting is performed if necessary. If the value has no
    *         `PatientName`` representation, an empty sequence is returned.
    */
  def toPatientNames(characterSets: CharacterSets = CharacterSets.defaultOnly): Seq[PatientName] = vr match {
    case PN => parsePN(characterSets)
    case _ => Seq.empty
  }

  /**
    * @return the first short representation of this value, if any
    */
  def toShort: Option[Short] = toShorts.headOption

  /**
    * @return the first int representation of this value, if any
    */
  def toInt: Option[Int] = toInts.headOption

  /**
    * @return the first long representation of this value, if any
    */
  def toLong: Option[Long] = toLongs.headOption

  /**
    * @return the first float representation of this value, if any
    */
  def toFloat: Option[Float] = toFloats.headOption

  /**
    * @return the first double representation of this value, if any
    */
  def toDouble: Option[Double] = toDoubles.headOption

  /**
    * @return the first `LocalDate` representation of this value, if any
    */
  def toDate: Option[LocalDate] = toDates.headOption

  /**
    * @return the first `ZonedDateTime` representation of this value, if any
    */
  def toDateTime(zoneOffset: ZoneOffset = systemZone): Option[ZonedDateTime] = toDateTimes(zoneOffset).headOption

  /**
    * @return the first `PatientName` representation of this value, if any
    */
  def toPatientName(characterSets: CharacterSets = CharacterSets.defaultOnly): Option[PatientName] = toPatientNames(characterSets).headOption

  private def parseAT: Seq[Int] = split(value, 4).map(b => bytesToTag(b, bigEndian))

  private def parseSL: Seq[Int] = split(value, 4).map(bytesToInt(_, bigEndian))

  private def parseUL: Seq[Long] = parseSL.map(intToUnsignedLong)

  private def parseSS: Seq[Short] = split(value, 2).map(bytesToShort(_, bigEndian))

  private def parseUS: Seq[Int] = parseSS.map(shortToUnsignedInt)

  private def parseFL: Seq[Float] = split(value, 4).map(bytesToFloat(_, bigEndian))

  private def parseFD: Seq[Double] = split(value, 8).map(bytesToDouble(_, bigEndian))

  private def parseDS: Seq[Double] = split(value.utf8String).map(trim).flatMap(s => try Option(java.lang.Double.parseDouble(s)) catch {
    case _: Throwable => None
  })

  private def parseIS: Seq[Long] = split(value.utf8String).map(trim).flatMap(s => try Option(java.lang.Long.parseLong(s)) catch {
    case _: Throwable => None
  })

  private def parseDA: Seq[LocalDate] = split(value.utf8String).flatMap(parseDate)

  private def parseDT(zoneOffset: ZoneOffset): Seq[ZonedDateTime] = split(value.utf8String).flatMap(parseDateTime(_, zoneOffset))

  private def parsePN(characterSets: CharacterSets): Seq[PatientName] = split(characterSets.decode(VR.PN, value)).map(trimPadding(_, vr.paddingByte)).flatMap(parsePatientName)

  /**
    * Return a copy of this element with its value updated. Length will be updated automatically.
    *
    * @param newValue the new value
    * @return a new Element instance
    */
  def withUpdatedValue(newValue: ByteString): Element = {
    val paddedValue = padToEvenLength(newValue, vr)
    copy(length = paddedValue.length, value = paddedValue)
  }

  /**
    * The `DicomHeader` representation of header data in this `Element`
    */
  def header: DicomHeader = DicomHeader(tag, vr, length, isFileMetaInformation(tag), bigEndian, explicitVR)

  /**
    * @return The DICOM byte array representation of this element
    */
  def toBytes: ByteString = header.bytes ++ value

  override def toString: String =
    s"Element [${tagToString(tag)} $vr $length (${if (bigEndian) "big endian" else "little endian"}/${if (explicitVR) "explicit" else "implicit"}) ${toSingleString()}]"
}

object Element {

  final lazy val multiValueDelimiter = """\"""
  final lazy val multiValueDelimiterRegex = """\\"""

  final private lazy val dateFormat = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 4, SignStyle.EXCEEDS_PAD)
    .appendPattern("['.']MM['.']dd")
    .toFormatter

  final private lazy val dateTimeFormat = new DateTimeFormatterBuilder()
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

  final private lazy val dateTimeZoneFormat = new DateTimeFormatterBuilder()
    .append(dateTimeFormat)
    .appendPattern("[Z]")
    .toFormatter

  private def systemZone: ZoneOffset = ZonedDateTime.now().getOffset

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

  case class ComponentGroup(alphabetic: String, ideographic: String, phonetic: String) {
    override def toString: String = s"$alphabetic=$ideographic=$phonetic".replaceAll("=+$", "")
  }

  case class PatientName(familyName: ComponentGroup, givenName: ComponentGroup, middleName: ComponentGroup, prefix: ComponentGroup, suffix: ComponentGroup) {
    override def toString: String = s"$familyName^$givenName^$middleName^$prefix^$suffix".replaceAll("\\^+$", "")
  }

  def parsePatientName(value: String): Option[PatientName] = {
    def ensureLength(s: Seq[String], n: Int) = s ++ Seq.fill(math.max(0, n - s.length))("")

    val comps = ensureLength(value.split("""\^"""), 5)
      .map(s => ensureLength(s.split("="), 3).map(trim))
      .map(c => ComponentGroup(c.head, c(1), c(2)))

    Option(PatientName(comps.head, comps(1), comps(2), comps(3), comps(4)))
  }

  private def split(bytes: ByteString, size: Int): Seq[ByteString] = bytes.grouped(size).filter(_.length == size).toSeq
  private def split(s: String): Seq[String] = s.split(multiValueDelimiterRegex)

  private def trim(s: String): String = s.trim
  private def trimPadding(s: String, paddingByte: Byte): String = {
    var index = s.length - 1
    while (index >= 0 && s(index) <= paddingByte)
      index -= 1
    val n = s.length - 1 - index
    if (n > 0) s.dropRight(n) else s
  }

  private def combine(vr: VR, values: Seq[ByteString]): ByteString = vr match {
    case AT | FL | FD | SL | SS | UL | US | OB | OW | OF | OD => values.reduce(_ ++ _)
    case _ => if (values.isEmpty) ByteString.empty else values.tail.foldLeft(values.head)((bytes, b) => bytes ++ ByteString('\\') ++ b)
  }

  def empty(tag: Int, vr: VR, bigEndian: Boolean, explicitVR: Boolean) =
    Element(tag, bigEndian, vr, explicitVR, 0, ByteString.empty)

  def apply(tag: Int, vr: VR, value: ByteString, bigEndian: Boolean, explicitVR: Boolean): Element = {
    val paddedValue = padToEvenLength(value, vr)
    Element(tag, bigEndian, vr, explicitVR, paddedValue.length, paddedValue)
  }

  def apply(tag: Int, value: ByteString, bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    apply(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  private def stringBytes(vr: VR, value: String, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(Integer.parseInt(value, 16), bigEndian)
    case FL => floatToBytes(java.lang.Float.parseFloat(value), bigEndian)
    case FD => doubleToBytes(java.lang.Double.parseDouble(value), bigEndian)
    case SL => intToBytes(Integer.parseInt(value), bigEndian)
    case SS => shortToBytes(java.lang.Short.parseShort(value), bigEndian)
    case UL => truncate(4, longToBytes(java.lang.Long.parseUnsignedLong(value), bigEndian), bigEndian)
    case US => truncate(2, intToBytes(java.lang.Integer.parseUnsignedInt(value), bigEndian), bigEndian)
    case OB | OW | OF | OD => throw new IllegalArgumentException("Cannot create binary array from string")
    case _ => ByteString(value)
  }
  def fromString(tag: Int, vr: VR, value: String, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, stringBytes(vr, value, bigEndian), bigEndian, explicitVR)
  def fromString(tag: Int, value: String, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromString(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromStrings(tag: Int, vr: VR, values: Seq[String], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(stringBytes(vr, _, bigEndian))), bigEndian, explicitVR)

  private def shortBytes(vr: VR, value: Short, bigEndian: Boolean): ByteString = vr match {
    case AT => throw new IllegalArgumentException("Cannot create tag from short")
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value, bigEndian)
    case UL => intToBytes(java.lang.Short.toUnsignedInt(value), bigEndian)
    case US => truncate(2, intToBytes(java.lang.Short.toUnsignedInt(value), bigEndian), bigEndian)
    case OB | OW | OF | OD => throw new IllegalArgumentException("Cannot create binary array from short")
    case _ => ByteString(value.toString)
  }
  def fromShort(tag: Int, vr: VR, value: Short, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, shortBytes(vr, value, bigEndian), bigEndian, explicitVR)
  def fromShort(tag: Int, value: Short, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromShort(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromShorts(tag: Int, vr: VR, values: Seq[Short], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(shortBytes(vr, _, bigEndian))), bigEndian, explicitVR)

  private def intBytes(vr: VR, value: Int, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value, bigEndian)
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(Integer.toUnsignedLong(value), bigEndian), bigEndian)
    case US => truncate(6, longToBytes(Integer.toUnsignedLong(value), bigEndian), bigEndian)
    case OB | OW | OF | OD => throw new IllegalArgumentException("Cannot create binary array from int")
    case _ => ByteString(value.toString)
  }
  def fromInt(tag: Int, vr: VR, value: Int, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, intBytes(vr, value, bigEndian), bigEndian, explicitVR)
  def fromInt(tag: Int, value: Int, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromInt(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromInts(tag: Int, vr: VR, values: Seq[Int], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(intBytes(vr, _, bigEndian))), bigEndian, explicitVR)

  def fromLong(tag: Int, value: Long, bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    apply(tag, longToBytes(value, bigEndian), bigEndian, explicitVR)

  def fromLongs(tag: Int, values: Seq[Long], bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    apply(tag, values.map(longToBytes(_, bigEndian)).reduce(_ ++ _), bigEndian, explicitVR)

  def fromFloat(tag: Int, value: Long, bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    apply(tag, floatToBytes(value, bigEndian), bigEndian, explicitVR)

  def fromFloats(tag: Int, values: Seq[Float], bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    apply(tag, values.map(floatToBytes(_, bigEndian)).reduce(_ ++ _), bigEndian, explicitVR)

  def fromDouble(tag: Int, value: Long, bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    apply(tag, doubleToBytes(value, bigEndian), bigEndian, explicitVR)

  def fromDoubles(tag: Int, values: Seq[Double], bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    apply(tag, values.map(doubleToBytes(_, bigEndian)).reduce(_ ++ _), bigEndian, explicitVR)

  def fromDate(tag: Int, value: LocalDate, bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    fromString(tag, value.format(dateFormat), bigEndian, explicitVR)

  def fromDates(tag: Int, values: Seq[LocalDate], bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    fromString(tag, values.map(_.format(dateFormat)).mkString(multiValueDelimiter), bigEndian, explicitVR)

  def fromDateTime(tag: Int, value: ZonedDateTime, bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    fromString(tag, value.format(dateTimeZoneFormat), bigEndian, explicitVR)

  def fromDateTimes(tag: Int, values: Seq[ZonedDateTime], bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    fromString(tag, values.map(_.format(dateTimeZoneFormat)).mkString(multiValueDelimiter), bigEndian, explicitVR)

  def fromPatientName(tag: Int, value: PatientName, bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    fromString(tag, value.toString, bigEndian, explicitVR)

  def fromPatientNames(tag: Int, values: Seq[PatientName], bigEndian: Boolean = false, explicitVR: Boolean = true): Element =
    fromString(tag, values.map(_.toString).mkString(multiValueDelimiter), bigEndian, explicitVR)
}