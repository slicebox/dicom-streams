package se.nimsa.dicom.data

import java.time._
import java.time.format.{DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField._

import akka.util.ByteString
import se.nimsa.dicom.data.DicomParsing.isFileMetaInformation
import se.nimsa.dicom.data.DicomParts.HeaderPart
import se.nimsa.dicom.data.VR._
import se.nimsa.dicom.data.DicomParsing._

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

  /**
    * For each component value of this element's value, return it's string representation
    *
    * @param characterSets Character sets used for string decoding
    * @return a sequence of strings, one for each component of the value
    */
  def toStrings(characterSets: CharacterSets = CharacterSets.defaultOnly): Seq[String] = if (value.isEmpty) Seq.empty else
    vr match {
      case AT => parseAT(value, bigEndian).map(tagToString)
      case FL => parseFL(value, bigEndian).map(_.toString)
      case FD => parseFD(value, bigEndian).map(_.toString)
      case SL => parseSL(value, bigEndian).map(_.toString)
      case SS => parseSS(value, bigEndian).map(_.toString)
      case UL => parseSL(value, bigEndian).map(Integer.toUnsignedString)
      case US => parseSS(value, bigEndian).map(java.lang.Short.toUnsignedInt).map(_.toString)
      case OB => Seq(value.map(byteToHexString).mkString(" "))
      case OW => Seq(split(value, 2).map(bytesToShort(_, bigEndian)).map(shortToHexString).mkString(" "))
      case OF => Seq(parseFL(value, bigEndian).mkString(" "))
      case OD => Seq(parseFD(value, bigEndian).mkString(" "))
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
    case FL => parseFL(value, bigEndian).map(_.toShort)
    case FD => parseFD(value, bigEndian).map(_.toShort)
    case SL => parseSL(value, bigEndian).map(_.toShort)
    case SS => parseSS(value, bigEndian)
    case UL => parseUL(value, bigEndian).map(_.toShort)
    case US => parseUS(value, bigEndian).map(_.toShort)
    case DS => parseDS(value).map(_.toShort)
    case IS => parseIS(value).map(_.toShort)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of int. Casting is performed if necessary. If the value has no
    *         int representation, an empty sequence is returned.
    */
  def toInts: Seq[Int] = vr match {
    case AT => parseAT(value, bigEndian)
    case FL => parseFL(value, bigEndian).map(_.toInt)
    case FD => parseFD(value, bigEndian).map(_.toInt)
    case SL => parseSL(value, bigEndian)
    case SS => parseSS(value, bigEndian).map(_.toInt)
    case UL => parseUL(value, bigEndian).map(_.toInt)
    case US => parseUS(value, bigEndian)
    case DS => parseDS(value).map(_.toInt)
    case IS => parseIS(value).map(_.toInt)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of longs. Casting is performed if necessary. If the value has no
    *         long representation, an empty sequence is returned.
    */
  def toLongs: Seq[Long] = vr match {
    case FL => parseFL(value, bigEndian).map(_.toLong)
    case FD => parseFD(value, bigEndian).map(_.toLong)
    case SL => parseSL(value, bigEndian).map(_.toLong)
    case SS => parseSS(value, bigEndian).map(_.toLong)
    case UL => parseUL(value, bigEndian)
    case US => parseUS(value, bigEndian).map(_.toLong)
    case DS => parseDS(value).map(_.toLong)
    case IS => parseIS(value)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of floats. Casting is performed if necessary. If the value has no
    *         float representation, an empty sequence is returned.
    */
  def toFloats: Seq[Float] = vr match {
    case FL => parseFL(value, bigEndian)
    case FD => parseFD(value, bigEndian).map(_.toFloat)
    case SL => parseSL(value, bigEndian).map(_.toFloat)
    case SS => parseSS(value, bigEndian).map(_.toFloat)
    case UL => parseUL(value, bigEndian).map(_.toFloat)
    case US => parseUS(value, bigEndian).map(_.toFloat)
    case DS => parseDS(value).map(_.toFloat)
    case IS => parseIS(value).map(_.toFloat)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of doubles. Casting is performed if necessary. If the value has no
    *         double representation, an empty sequence is returned.
    */
  def toDoubles: Seq[Double] = vr match {
    case FL => parseFL(value, bigEndian).map(_.toDouble)
    case FD => parseFD(value, bigEndian)
    case SL => parseSL(value, bigEndian).map(_.toDouble)
    case SS => parseSS(value, bigEndian).map(_.toDouble)
    case UL => parseUL(value, bigEndian).map(_.toDouble)
    case US => parseUS(value, bigEndian).map(_.toDouble)
    case DS => parseDS(value)
    case IS => parseIS(value).map(_.toDouble)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of `LocalDate`s. Casting is performed if necessary. If the value has no
    *         `LocalDate` representation, an empty sequence is returned.
    */
  def toDates: Seq[LocalDate] = vr match {
    case DA => parseDA(value)
    case DT => parseDT(value, systemZone).map(_.toLocalDate)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of `ZonedDateTime`s. Casting is performed if necessary. If the value has no
    *         `ZonedDateTime` representation, an empty sequence is returned.
    */
  def toDateTimes(zoneOffset: ZoneOffset = systemZone): Seq[ZonedDateTime] = vr match {
    case DA => parseDA(value).map(_.atStartOfDay(zoneOffset))
    case DT => parseDT(value, zoneOffset)
    case _ => Seq.empty
  }

  /**
    * @return the value of this element as a sequence of 'PatientName`s. Casting is performed if necessary. If the value has no
    *         `PatientName`` representation, an empty sequence is returned.
    */
  def toPatientNames(characterSets: CharacterSets = CharacterSets.defaultOnly): Seq[PatientName] = vr match {
    case PN => parsePN(value, characterSets)
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
  def header: HeaderPart = HeaderPart(tag, vr, length, isFileMetaInformation(tag), bigEndian, explicitVR)

  /**
    * @return The DICOM byte array representation of this element
    */
  def toBytes: ByteString = header.bytes ++ value

  override def toString: String =
    s"Element [${tagToString(tag)} $vr $length (${if (bigEndian) "big endian" else "little endian"}/${if (explicitVR) "explicit" else "implicit"}) ${toSingleString()}]"
}

object Element {

  final private lazy val dateFormatForEncoding = new DateTimeFormatterBuilder()
    .appendValue(YEAR, 4, 4, SignStyle.EXCEEDS_PAD)
    .appendPattern("MMdd")
    .toFormatter


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
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value, bigEndian)
    case UL => intToBytes(java.lang.Short.toUnsignedInt(value), bigEndian)
    case US => truncate(2, intToBytes(java.lang.Short.toUnsignedInt(value), bigEndian), bigEndian)
    case OB | OW | OF | OD | AT => throw new IllegalArgumentException(s"Cannot create value of VR $vr from short")
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
    case OB | OW | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from int")
    case _ => ByteString(value.toString)
  }
  def fromInt(tag: Int, vr: VR, value: Int, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, intBytes(vr, value, bigEndian), bigEndian, explicitVR)
  def fromInt(tag: Int, value: Int, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromInt(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromInts(tag: Int, vr: VR, values: Seq[Int], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(intBytes(vr, _, bigEndian))), bigEndian, explicitVR)

  private def longBytes(vr: VR, value: Long, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value.toInt, bigEndian)
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(value, bigEndian), bigEndian)
    case US => truncate(6, longToBytes(value, bigEndian), bigEndian)
    case OB | OW | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from long")
    case _ => ByteString(value.toString)
  }
  def fromLong(tag: Int, vr: VR, value: Long, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, longBytes(vr, value, bigEndian), bigEndian, explicitVR)
  def fromLong(tag: Int, value: Long, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromLong(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromLongs(tag: Int, vr: VR, values: Seq[Long], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(longBytes(vr, _, bigEndian))), bigEndian, explicitVR)

  private def floatBytes(vr: VR, value: Float, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value.toInt, bigEndian)
    case FL => floatToBytes(value, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(value.toLong, bigEndian), bigEndian)
    case US => truncate(6, longToBytes(value.toLong, bigEndian), bigEndian)
    case OB | OW | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from float")
    case _ => ByteString(value.toString)
  }
  def fromFloat(tag: Int, vr: VR, value: Float, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, floatBytes(vr, value, bigEndian), bigEndian, explicitVR)
  def fromFloat(tag: Int, value: Float, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromFloat(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromFloats(tag: Int, vr: VR, values: Seq[Float], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(floatBytes(vr, _, bigEndian))), bigEndian, explicitVR)

  private def doubleBytes(vr: VR, value: Double, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value.toInt, bigEndian)
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(value.toLong, bigEndian), bigEndian)
    case US => truncate(6, longToBytes(value.toLong, bigEndian), bigEndian)
    case OB | OW | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from double")
    case _ => ByteString(value.toString)
  }
  def fromDouble(tag: Int, vr: VR, value: Double, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, doubleBytes(vr, value, bigEndian), bigEndian, explicitVR)
  def fromDouble(tag: Int, value: Double, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromDouble(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromDoubles(tag: Int, vr: VR, values: Seq[Double], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(doubleBytes(vr, _, bigEndian))), bigEndian, explicitVR)

  private def dateBytes(vr: VR, value: LocalDate): ByteString = vr match {
    case AT | FL | FD | SL | SS | UL | US | OB | OW | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from date")
    case _ => ByteString(value.format(dateFormatForEncoding))
  }
  def fromDate(tag: Int, vr: VR, value: LocalDate, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, dateBytes(vr, value), bigEndian, explicitVR)
  def fromDate(tag: Int, value: LocalDate, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromDate(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromDates(tag: Int, vr: VR, values: Seq[LocalDate], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(dateBytes(vr, _))), bigEndian, explicitVR)

  private def dateTimeBytes(vr: VR, value: ZonedDateTime): ByteString = vr match {
    case AT | FL | FD | SL | SS | UL | US | OB | OW | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from date-time")
    case _ => ByteString(value.format(dateTimeZoneFormat))
  }
  def fromDateTime(tag: Int, vr: VR, value: ZonedDateTime, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, dateTimeBytes(vr, value), bigEndian, explicitVR)
  def fromDateTime(tag: Int, value: ZonedDateTime, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromDateTime(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromDateTimes(tag: Int, vr: VR, values: Seq[ZonedDateTime], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(dateTimeBytes(vr, _))), bigEndian, explicitVR)

  private def patientNameBytes(vr: VR, value: PatientName): ByteString = vr match {
    case PN => ByteString(value.toString)
    case _  => throw new IllegalArgumentException(s"Cannot create value of VR $vr from date-time")
  }
  def fromPatientName(tag: Int, vr: VR, value: PatientName, bigEndian: Boolean, explicitVR: Boolean): Element = apply(tag, vr, patientNameBytes(vr, value), bigEndian, explicitVR)
  def fromPatientName(tag: Int, value: PatientName, bigEndian: Boolean = false, explicitVR: Boolean = true): Element = fromPatientName(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
  def fromPatientNames(tag: Int, vr: VR, values: Seq[PatientName], bigEndian: Boolean = false, explicitVR: Boolean = true): Element = apply(tag, vr, combine(vr, values.map(patientNameBytes(vr, _))), bigEndian, explicitVR)
}