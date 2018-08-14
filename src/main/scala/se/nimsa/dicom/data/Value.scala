package se.nimsa.dicom.data

import java.net.URI
import java.time.{LocalDate, ZoneOffset, ZonedDateTime}

import akka.util.ByteString
import se.nimsa.dicom.data.DicomParsing._
import se.nimsa.dicom.data.VR._

/**
  * This class describes a DICOM data value
  *
  * @param bytes the binary data of this value
  */
case class Value private[data](bytes: ByteString) {

  val length: Int = bytes.length

  /**
    * For each component value of this value, return it's string representation
    *
    * @param characterSets Character sets used for string decoding
    * @return a sequence of strings, one for each component of this value
    */
  def toStrings(vr: VR, bigEndian: Boolean = false, characterSets: CharacterSets = defaultCharacterSet): Seq[String] = if (bytes.isEmpty) Seq.empty else
    vr match {
      case AT => parseAT(bytes, bigEndian).map(tagToString)
      case FL => parseFL(bytes, bigEndian).map(_.toString)
      case FD => parseFD(bytes, bigEndian).map(_.toString)
      case SL => parseSL(bytes, bigEndian).map(_.toString)
      case SS => parseSS(bytes, bigEndian).map(_.toString)
      case UL => parseSL(bytes, bigEndian).map(Integer.toUnsignedString)
      case US => parseSS(bytes, bigEndian).map(java.lang.Short.toUnsignedInt).map(_.toString)
      case OB => Seq(bytes.map(byteToHexString).mkString(" "))
      case OW => Seq(split(bytes, 2).map(bytesToShort(_, bigEndian)).map(shortToHexString).mkString(" "))
      case OF => Seq(parseFL(bytes, bigEndian).mkString(" "))
      case OD => Seq(parseFD(bytes, bigEndian).mkString(" "))
      case ST | LT | UT | UR => Seq(trimPadding(characterSets.decode(vr, bytes), vr.paddingByte))
      case UC => split(trimPadding(characterSets.decode(vr, bytes), vr.paddingByte))
      case _ => split(characterSets.decode(vr, bytes)).map(trim)
    }

  /**
    * @return this value as a sequence of shorts. Casting is performed if necessary. If the value has no
    *         short representation, an empty sequence is returned.
    */
  def toShorts(vr: VR, bigEndian: Boolean = false): Seq[Short] = vr match {
    case FL => parseFL(bytes, bigEndian).map(_.toShort)
    case FD => parseFD(bytes, bigEndian).map(_.toShort)
    case SL => parseSL(bytes, bigEndian).map(_.toShort)
    case SS => parseSS(bytes, bigEndian)
    case UL => parseUL(bytes, bigEndian).map(_.toShort)
    case US => parseUS(bytes, bigEndian).map(_.toShort)
    case DS => parseDS(bytes).map(_.toShort)
    case IS => parseIS(bytes).map(_.toShort)
    case _ => Seq.empty
  }

  /**
    * @return this value as a sequence of int. Casting is performed if necessary. If the value has no
    *         int representation, an empty sequence is returned.
    */
  def toInts(vr: VR, bigEndian: Boolean = false): Seq[Int] = vr match {
    case AT => parseAT(bytes, bigEndian)
    case FL => parseFL(bytes, bigEndian).map(_.toInt)
    case FD => parseFD(bytes, bigEndian).map(_.toInt)
    case SL => parseSL(bytes, bigEndian)
    case SS => parseSS(bytes, bigEndian).map(_.toInt)
    case UL => parseUL(bytes, bigEndian).map(_.toInt)
    case US => parseUS(bytes, bigEndian)
    case DS => parseDS(bytes).map(_.toInt)
    case IS => parseIS(bytes).map(_.toInt)
    case _ => Seq.empty
  }

  /**
    * @return this value as a sequence of longs. Casting is performed if necessary. If the value has no
    *         long representation, an empty sequence is returned.
    */
  def toLongs(vr: VR, bigEndian: Boolean = false): Seq[Long] = vr match {
    case FL => parseFL(bytes, bigEndian).map(_.toLong)
    case FD => parseFD(bytes, bigEndian).map(_.toLong)
    case SL => parseSL(bytes, bigEndian).map(_.toLong)
    case SS => parseSS(bytes, bigEndian).map(_.toLong)
    case UL => parseUL(bytes, bigEndian)
    case US => parseUS(bytes, bigEndian).map(_.toLong)
    case DS => parseDS(bytes).map(_.toLong)
    case IS => parseIS(bytes)
    case _ => Seq.empty
  }

  /**
    * @return this value as a sequence of floats. Casting is performed if necessary. If the value has no
    *         float representation, an empty sequence is returned.
    */
  def toFloats(vr: VR, bigEndian: Boolean = false): Seq[Float] = vr match {
    case FL => parseFL(bytes, bigEndian)
    case FD => parseFD(bytes, bigEndian).map(_.toFloat)
    case SL => parseSL(bytes, bigEndian).map(_.toFloat)
    case SS => parseSS(bytes, bigEndian).map(_.toFloat)
    case UL => parseUL(bytes, bigEndian).map(_.toFloat)
    case US => parseUS(bytes, bigEndian).map(_.toFloat)
    case DS => parseDS(bytes).map(_.toFloat)
    case IS => parseIS(bytes).map(_.toFloat)
    case _ => Seq.empty
  }

  /**
    * @return this value as a sequence of doubles. Casting is performed if necessary. If the value has no
    *         double representation, an empty sequence is returned.
    */
  def toDoubles(vr: VR, bigEndian: Boolean = false): Seq[Double] = vr match {
    case FL => parseFL(bytes, bigEndian).map(_.toDouble)
    case FD => parseFD(bytes, bigEndian)
    case SL => parseSL(bytes, bigEndian).map(_.toDouble)
    case SS => parseSS(bytes, bigEndian).map(_.toDouble)
    case UL => parseUL(bytes, bigEndian).map(_.toDouble)
    case US => parseUS(bytes, bigEndian).map(_.toDouble)
    case DS => parseDS(bytes)
    case IS => parseIS(bytes).map(_.toDouble)
    case _ => Seq.empty
  }

  /**
    * @return this value as a sequence of `LocalDate`s. Casting is performed if necessary. If the value has no
    *         `LocalDate` representation, an empty sequence is returned.
    */
  def toDates(vr: VR = VR.DA): Seq[LocalDate] = vr match {
    case DA => parseDA(bytes)
    case DT => parseDT(bytes, systemZone).map(_.toLocalDate)
    case _ => Seq.empty
  }

  /**
    * @return this value as a sequence of `ZonedDateTime`s. Casting is performed if necessary. If the value has no
    *         `ZonedDateTime` representation, an empty sequence is returned.
    */
  def toDateTimes(vr: VR = VR.DT, zoneOffset: ZoneOffset = systemZone): Seq[ZonedDateTime] = vr match {
    case DA => parseDA(bytes).map(_.atStartOfDay(zoneOffset))
    case DT => parseDT(bytes, zoneOffset)
    case _ => Seq.empty
  }

  /**
    * @return this value as a sequence of 'PatientName`s. Casting is performed if necessary. If the value has no
    *         `PatientName`` representation, an empty sequence is returned.
    */
  def toPatientNames(vr: VR = VR.PN, characterSets: CharacterSets = defaultCharacterSet): Seq[PatientName] = vr match {
    case PN => parsePN(bytes, characterSets)
    case _ => Seq.empty
  }

  /**
    * @return this value as an option of a `URI`. If the value has no `URI` representation, an empty option is
    *         returned.
    */
  def toURI(vr: VR = VR.UR): Option[URI] = vr match {
    case UR => parseUR(bytes)
    case _ => None
  }

  /**
    * @return this value as a byte array with data in big endian order. Endianness is ensured for values of value
    *         representations OW. For OB the byte array is returned as-is. Other representations are not
    *         supported and an empty array will be returned.
    */
  def toByteArray(vr: VR, bigEndian: Boolean = false): Array[Byte] = vr match {
    case OW if !bigEndian => bytes.grouped(2).flatMap(_.reverse).toArray
    case OB | OW => bytes.toArray
    case _ => Array.empty
  }

  // TODO short, int, long, float, double arrays. Unsigned? BitSets? Incorporate bitsStored?

  /**
    * @return the first string representation of this value, if any
    */
  def toString(vr: VR, bigEndian: Boolean = false, characterSets: CharacterSets = defaultCharacterSet): Option[String] =
    toStrings(vr, bigEndian, characterSets).headOption

  /**
    * Get the string representation of each component of this value, joined with the backslash character as separator
    *
    * @param characterSets Character sets used for string decoding
    * @return a string repreentation of all components of this value, if any
    */
  def toSingleString(vr: VR, bigEndian: Boolean = false, characterSets: CharacterSets = defaultCharacterSet): Option[String] = {
    val strings = toStrings(vr, bigEndian, characterSets)
    if (strings.isEmpty) None else Option(strings.mkString(multiValueDelimiter))
  }

  /**
    * @return a direct decoding to UTF8 from the bytes of this value
    */
  def toUtf8String: String = bytes.utf8String

  /**
    * @return the first short representation of this value, if any
    */
  def toShort(vr: VR, bigEndian: Boolean = false): Option[Short] = toShorts(vr, bigEndian).headOption

  /**
    * @return the first int representation of this value, if any
    */
  def toInt(vr: VR, bigEndian: Boolean = false): Option[Int] = toInts(vr, bigEndian).headOption

  /**
    * @return the first long representation of this value, if any
    */
  def toLong(vr: VR, bigEndian: Boolean = false): Option[Long] = toLongs(vr, bigEndian).headOption

  /**
    * @return the first float representation of this value, if any
    */
  def toFloat(vr: VR, bigEndian: Boolean = false): Option[Float] = toFloats(vr, bigEndian).headOption

  /**
    * @return the first double representation of this value, if any
    */
  def toDouble(vr: VR, bigEndian: Boolean = false): Option[Double] = toDoubles(vr, bigEndian).headOption

  /**
    * @return the first `LocalDate` representation of this value, if any
    */
  def toDate(vr: VR = VR.DA): Option[LocalDate] = toDates(vr).headOption

  /**
    * @return the first `ZonedDateTime` representation of this value, if any
    */
  def toDateTime(vr: VR = VR.DT, zoneOffset: ZoneOffset = systemZone): Option[ZonedDateTime] = toDateTimes(vr, zoneOffset).headOption

  /**
    * @return the first `PatientName` representation of this value, if any
    */
  def toPatientName(vr: VR = VR.PN, characterSets: CharacterSets = defaultCharacterSet): Option[PatientName] = toPatientNames(vr, characterSets).headOption

  override def toString: String =
    s"Value [${bytes.length} bytes]"

  /**
    * @param bytes additional bytes
    * @return a new Value with bytes added
    */
  def ++(bytes: ByteString): Value = copy(bytes = this.bytes ++ bytes)

  /**
    * @return a new Value guaranteed to have even length, padded if necessary with the correct padding byte
    */
  def ensurePadding(vr: VR): Value = copy(bytes = padToEvenLength(bytes, vr))
}

object Value {

  private def combine(vr: VR, values: Seq[ByteString]): ByteString = vr match {
    case AT | FL | FD | SL | SS | UL | US | OB | OW | OL | OF | OD => values.reduce(_ ++ _)
    case _ => if (values.isEmpty) ByteString.empty else values.tail.foldLeft(values.head)((bytes, b) => bytes ++ ByteString('\\') ++ b)
  }

  /**
    * A Value with empty value
    */
  val empty: Value = Value(ByteString.empty)

  /**
    * Create a new Value, padding the input if necessary to ensure even length
    *
    * @param bytes value bytes
    * @return a new Value
    */
  def apply(vr: VR, bytes: ByteString): Value = Value(padToEvenLength(bytes, vr))

  private def stringBytes(vr: VR, value: String, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(Integer.parseInt(value, 16), bigEndian)
    case FL => floatToBytes(java.lang.Float.parseFloat(value), bigEndian)
    case FD => doubleToBytes(java.lang.Double.parseDouble(value), bigEndian)
    case SL => intToBytes(Integer.parseInt(value), bigEndian)
    case SS => shortToBytes(java.lang.Short.parseShort(value), bigEndian)
    case UL => truncate(4, longToBytes(java.lang.Long.parseUnsignedLong(value), bigEndian), bigEndian)
    case US => truncate(2, intToBytes(java.lang.Integer.parseUnsignedInt(value), bigEndian), bigEndian)
    case OB | OW | OL | OF | OD => throw new IllegalArgumentException("Cannot create binary array from string")
    case _ => ByteString(value)
  }
  def fromString(vr: VR, value: String, bigEndian: Boolean = false): Value = apply(vr, stringBytes(vr, value, bigEndian))
  def fromStrings(vr: VR, values: Seq[String], bigEndian: Boolean = false): Value = apply(vr, combine(vr, values.map(stringBytes(vr, _, bigEndian))))

  private def shortBytes(vr: VR, value: Short, bigEndian: Boolean): ByteString = vr match {
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value, bigEndian)
    case UL => intToBytes(java.lang.Short.toUnsignedInt(value), bigEndian)
    case US => truncate(2, intToBytes(java.lang.Short.toUnsignedInt(value), bigEndian), bigEndian)
    case OB | OW | OL | OF | OD | AT => throw new IllegalArgumentException(s"Cannot create value of VR $vr from short")
    case _ => ByteString(value.toString)
  }
  def fromShort(vr: VR, value: Short, bigEndian: Boolean = false): Value = apply(vr, shortBytes(vr, value, bigEndian))
  def fromShorts(vr: VR, values: Seq[Short], bigEndian: Boolean = false): Value = apply(vr, combine(vr, values.map(shortBytes(vr, _, bigEndian))))

  private def intBytes(vr: VR, value: Int, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value, bigEndian)
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(Integer.toUnsignedLong(value), bigEndian), bigEndian)
    case US => truncate(6, longToBytes(Integer.toUnsignedLong(value), bigEndian), bigEndian)
    case OB | OW | OL | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from int")
    case _ => ByteString(value.toString)
  }
  def fromInt(vr: VR, value: Int, bigEndian: Boolean = false): Value = apply(vr, intBytes(vr, value, bigEndian))
  def fromInts(vr: VR, values: Seq[Int], bigEndian: Boolean = false): Value = apply(vr, combine(vr, values.map(intBytes(vr, _, bigEndian))))

  private def longBytes(vr: VR, value: Long, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value.toInt, bigEndian)
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(value, bigEndian), bigEndian)
    case US => truncate(6, longToBytes(value, bigEndian), bigEndian)
    case OB | OW | OL | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from long")
    case _ => ByteString(value.toString)
  }
  def fromLong(vr: VR, value: Long, bigEndian: Boolean = false): Value = apply(vr, longBytes(vr, value, bigEndian))
  def fromLongs(vr: VR, values: Seq[Long], bigEndian: Boolean = false): Value = apply(vr, combine(vr, values.map(longBytes(vr, _, bigEndian))))

  private def floatBytes(vr: VR, value: Float, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value.toInt, bigEndian)
    case FL => floatToBytes(value, bigEndian)
    case FD => doubleToBytes(value.toDouble, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(value.toLong, bigEndian), bigEndian)
    case US => truncate(6, longToBytes(value.toLong, bigEndian), bigEndian)
    case OB | OW | OL | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from float")
    case _ => ByteString(value.toString)
  }
  def fromFloat(vr: VR, value: Float, bigEndian: Boolean = false): Value = apply(vr, floatBytes(vr, value, bigEndian))
  def fromFloats(vr: VR, values: Seq[Float], bigEndian: Boolean = false): Value = apply(vr, combine(vr, values.map(floatBytes(vr, _, bigEndian))))

  private def doubleBytes(vr: VR, value: Double, bigEndian: Boolean): ByteString = vr match {
    case AT => tagToBytes(value.toInt, bigEndian)
    case FL => floatToBytes(value.toFloat, bigEndian)
    case FD => doubleToBytes(value, bigEndian)
    case SL => intToBytes(value.toInt, bigEndian)
    case SS => shortToBytes(value.toShort, bigEndian)
    case UL => truncate(4, longToBytes(value.toLong, bigEndian), bigEndian)
    case US => truncate(6, longToBytes(value.toLong, bigEndian), bigEndian)
    case OB | OW | OL | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from double")
    case _ => ByteString(value.toString)
  }
  def fromDouble(vr: VR, value: Double, bigEndian: Boolean = false): Value = apply(vr, doubleBytes(vr, value, bigEndian))
  def fromDoubles(vr: VR, values: Seq[Double], bigEndian: Boolean = false): Value = apply(vr, combine(vr, values.map(doubleBytes(vr, _, bigEndian))))

  private def dateBytes(vr: VR, value: LocalDate): ByteString = vr match {
    case AT | FL | FD | SL | SS | UL | US | OB | OW | OL | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from date")
    case _ => ByteString(formatDate(value))
  }
  def fromDate(vr: VR, value: LocalDate): Value = apply(vr, dateBytes(vr, value))
  def fromDates(vr: VR, values: Seq[LocalDate]): Value = apply(vr, combine(vr, values.map(dateBytes(vr, _))))

  private def dateTimeBytes(vr: VR, value: ZonedDateTime): ByteString = vr match {
    case AT | FL | FD | SL | SS | UL | US | OB | OW | OL | OF | OD => throw new IllegalArgumentException(s"Cannot create value of VR $vr from date-time")
    case _ => ByteString(formatDateTime(value))
  }
  def fromDateTime(vr: VR, value: ZonedDateTime): Value = apply(vr, dateTimeBytes(vr, value))
  def fromDateTimes(vr: VR, values: Seq[ZonedDateTime]): Value = apply(vr, combine(vr, values.map(dateTimeBytes(vr, _))))

  private def patientNameBytes(vr: VR, value: PatientName): ByteString = vr match {
    case PN => ByteString(value.toString)
    case _ => throw new IllegalArgumentException(s"Cannot create value of VR $vr from date-time")
  }
  def fromPatientName(vr: VR, value: PatientName): Value = apply(vr, patientNameBytes(vr, value))
  def fromPatientNames(vr: VR, values: Seq[PatientName]): Value = apply(vr, combine(vr, values.map(patientNameBytes(vr, _))))

  private def uriBytes(vr: VR, value: URI): ByteString = vr match {
    case UR => ByteString(value.toString)
    case _ => throw new IllegalArgumentException(s"Cannot create value of VR $vr from URI")
  }
  def fromURI(vr: VR, value: URI): Value = apply(vr, uriBytes(vr, value))

  /**
    * Create a value from a byte array where values are encoded in big endian order (representation OW, all
    * other representations use the byte array as-is).
    *
    * @return a new `Value`
    */
  def fromByteArray(vr: VR, array: Array[Byte], bigEndian: Boolean = false): Value = vr match {
    case OW if !bigEndian => apply(vr, ByteString(array.grouped(2).flatMap(_.reverse).toArray))
    case _ => apply(vr, ByteString(array))
  }

}
