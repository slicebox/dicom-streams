package se.nimsa.dicom

import java.time._
import java.time.format.{DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.ChronoField._

import akka.util.ByteString
import se.nimsa.dicom.VR._

case class Element(tagPath: TagPath, bigEndian: Boolean, vr: VR, explicitVR: Boolean, length: Long, value: ByteString) {

  import Element._

  def toStrings(characterSets: CharacterSets = CharacterSets.defaultOnly): Seq[String] = if (value.isEmpty) Seq.empty else
    vr match {
      case AT => parseAT.map(tagToString)
      case FL => parseFL.map(_.toString)
      case FD => parseFD.map(_.toString)
      case SL => parseSL.map(_.toString)
      case SS => parseSS.map(_.toString)
      case UL => parseSL.map(Integer.toUnsignedString)
      case US => parseSS.map(java.lang.Short.toUnsignedInt).map(_.toString)
      case OB => Seq(trimToLength(value, length).map(byteToHexString).mkString(" "))
      case OW => Seq(split(value, 2).map(bytesToShort(_, bigEndian)).map(shortToHexString).mkString(" "))
      case OF => Seq(parseFL.mkString(" "))
      case OD => Seq(parseFD.mkString(" "))
      case ST | LT | UT => Seq(trimPadding(characterSets.decode(vr, value), vr.paddingByte))
      case _ => split(characterSets.decode(vr, value)).map(trim)
    }

  def toSingleString(characterSets: CharacterSets = CharacterSets.defaultOnly): String =
    toStrings(characterSets).mkString(multiValueDelimiter)

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

  def toDates: Seq[LocalDate] = vr match {
    case DA => parseDA
    case DT => parseDT(systemZone).map(_.toLocalDate)
    case _ => Seq.empty
  }

  def toDateTimes(zoneOffset: ZoneOffset = systemZone): Seq[ZonedDateTime] = vr match {
    case DA => parseDA.map(_.atStartOfDay(zoneOffset))
    case DT => parseDT(zoneOffset)
    case _ => Seq.empty
  }

  def toPatientNames(characterSets: CharacterSets = CharacterSets.defaultOnly): Seq[PatientName] = vr match {
    case PN => parsePN(characterSets)
    case _ => Seq.empty
  }

  def toShort: Option[Short] = toShorts.headOption
  def toInt: Option[Int] = toInts.headOption
  def toLong: Option[Long] = toLongs.headOption
  def toFloat: Option[Float] = toFloats.headOption
  def toDouble: Option[Double] = toDoubles.headOption
  def toDate: Option[LocalDate] = toDates.headOption
  def toDateTime(zoneOffset: ZoneOffset = systemZone): Option[ZonedDateTime] = toDateTimes(zoneOffset).headOption
  def toPatientName(characterSets: CharacterSets = CharacterSets.defaultOnly): Option[PatientName] = toPatientNames(characterSets).headOption

  private def parseAT: Seq[Int] = split(value, 4).map(b => bytesToTag(b, bigEndian))
  private def parseSL: Seq[Int] = split(value, 4).map(bytesToInt(_, bigEndian))
  private def parseUL: Seq[Long] = parseSL.map(intToUnsignedLong)
  private def parseSS: Seq[Short] = split(value, 2).map(bytesToShort(_, bigEndian))
  private def parseUS: Seq[Int] = parseSS.map(shortToUnsignedInt)
  private def parseFL: Seq[Float] = split(value, 4).map(bytesToFloat(_, bigEndian))
  private def parseFD: Seq[Double] = split(value, 8).map(bytesToDouble(_, bigEndian))
  private def parseDS: Seq[Double] = split(value.utf8String).map(trim).flatMap(s => try Option(java.lang.Double.parseDouble(s)) catch { case _: Throwable => None })
  private def parseIS: Seq[Long] = split(value.utf8String).map(trim).flatMap(s => try Option(java.lang.Long.parseLong(s)) catch { case _: Throwable => None })
  private def parseDA: Seq[LocalDate] = split(value.utf8String).flatMap(parseDate)
  private def parseDT(zoneOffset: ZoneOffset): Seq[ZonedDateTime] = split(value.utf8String).flatMap(parseDateTime(_, zoneOffset))
  private def parsePN(characterSets: CharacterSets): Seq[PatientName] = split(characterSets.decode(VR.PN, value)).map(trimPadding(_, vr.paddingByte)).flatMap(parsePatientName)
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
    try Option(LocalDate.parse(s.trim, dateFormat)) catch { case _: Throwable => None }

  def parseDateTime(s: String, zoneOffset: ZoneOffset): Option[ZonedDateTime] = {
    val trimmed = s.trim
    try Option(ZonedDateTime.parse(trimmed, dateTimeZoneFormat)) catch {
      case _: Throwable => try Option(LocalDateTime.parse(trimmed, dateTimeFormat).atZone(zoneOffset)) catch {
        case _: Throwable => None
      }
    }
  }

  case class ComponentGroup(alphabetic: String, ideographic: String, phonetic: String)
  case class PatientName(familyName: ComponentGroup, givenName: ComponentGroup, middleName: ComponentGroup, prefix: ComponentGroup, suffix: ComponentGroup)

  def parsePatientName(value: String): Option[PatientName] = try {
    def ensureLength(s: Seq[String], n: Int) = s ++ Seq.fill(math.max(0, n - s.length))("")

    val comps = ensureLength(value.split("""\^"""), 5)
      .map(s => ensureLength(s.split("="), 3).map(trim))
      .map(c => ComponentGroup(c.head, c(1), c(2)))

    Option(PatientName(comps.head, comps(1), comps(2), comps(3), comps(4)))
  } catch {
    case _: Throwable => None
  }

  private def split(bytes: ByteString, size: Int): Seq[ByteString] = bytes.grouped(size).filter(_.length == size).toSeq
  private def split(s: String): Seq[String] = s.split(multiValueDelimiterRegex)

  private def trimToLength(bytes: ByteString, length: Long): ByteString = if (bytes.length < length) bytes.take(length.toInt) else bytes
  private def trim(s: String): String = s.trim
  private def trimPadding(s: String, paddingByte: Byte): String = {
    var index = s.length - 1
    while (index >= 0 && s(index) <= paddingByte)
      index -= 1
    val n = s.length - 1 - index
    if (n > 0) s.dropRight(n) else s
  }

}