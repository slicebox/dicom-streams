package se.nimsa.dicom.data

import java.time.{LocalDate, ZoneOffset, ZonedDateTime}

import akka.util.ByteString
import se.nimsa.dicom.data.DicomParsing.{Element => _, _}
import se.nimsa.dicom.data.DicomParts._
import se.nimsa.dicom.data.Elements.{ValueElement, _}
import se.nimsa.dicom.data.TagPath.{EmptyTagPath, TagPathSequenceItem, TagPathTag, TagPathTrunk}
import se.nimsa.dicom.data.VR.VR

import scala.collection.mutable.ArrayBuffer

/**
  * Representation of a group of `Element`s, each paired with the `TagPath` that describes their position within a
  * dataset. Representation is immutable so methods for inserting, updating and removing elements return a new instance.
  * Also specifies the character sets that should be used for decoding the values of textual elements.
  *
  * @param characterSets The character sets used for decoding text values
  * @param data          the `Map`ping of `TagPath` to `Element`
  */
case class Elements(characterSets: CharacterSets, zoneOffset: ZoneOffset, data: Vector[ElementSet]) {

  /**
    * Get a single element, if present
    *
    * @param tag tag number the element, referring to the root dataset
    * @return optional Element
    */
  def apply(tag: Int): Option[ElementSet] = data.find(_.tag == tag)

  /**
    * Find a element in this Elements described by a tag path
    *
    * @param tagPath input tag path
    * @return element, if present
    */
  def apply(tagPath: TagPathTag): Option[ElementSet] = tagPath.previous match {
    case tp: TagPathSequenceItem => getNested(tp).flatMap(_.apply(tagPath.tag))
    case EmptyTagPath => apply(tagPath.tag)
    case _ => throw new IllegalArgumentException("Unsupported tag path type")
  }

  def get[A](tag: Int, f: ValueElement => Option[A]): Option[A] = apply(tag).flatMap {
    case e: ValueElement => f(e)
    case _ => None
  }

  def getAll[A](tag: Int, f: ValueElement => Seq[A]): Seq[A] = apply(tag).map {
    case e: ValueElement => f(e)
    case _ => Seq.empty
  }.getOrElse(Seq.empty)

  def getElement(tag: Int): Option[ValueElement] = get(tag, Option.apply)
  def getValue(tag: Int): Option[Value] = getElement(tag).map(_.value)
  def getBytes(tag: Int): Option[ByteString] = getValue(tag).map(_.bytes)
  def getStrings(tag: Int): Seq[String] = getAll(tag, v => v.value.toStrings(v.vr, v.bigEndian, characterSets))
  def getSingleString(tag: Int): Option[String] = get(tag, v => v.value.toSingleString(v.vr, v.bigEndian, characterSets))
  def getString(tag: Int): Option[String] = get(tag, v => v.value.toString(v.vr, v.bigEndian, characterSets))
  def getShorts(tag: Int): Seq[Short] = getAll(tag, v => v.value.toShorts(v.vr, v.bigEndian))
  def getShort(tag: Int): Option[Short] = get(tag, v => v.value.toShort(v.vr, v.bigEndian))
  def getInts(tag: Int): Seq[Int] = getAll(tag, v => v.value.toInts(v.vr, v.bigEndian))
  def getInt(tag: Int): Option[Int] = get(tag, v => v.value.toInt(v.vr, v.bigEndian))
  def getLongs(tag: Int): Seq[Long] = getAll(tag, v => v.value.toLongs(v.vr, v.bigEndian))
  def getLong(tag: Int): Option[Long] = get(tag, v => v.value.toLong(v.vr, v.bigEndian))
  def getFloats(tag: Int): Seq[Float] = getAll(tag, v => v.value.toFloats(v.vr, v.bigEndian))
  def getFloat(tag: Int): Option[Float] = get(tag, v => v.value.toFloat(v.vr, v.bigEndian))
  def getDoubles(tag: Int): Seq[Double] = getAll(tag, v => v.value.toDoubles(v.vr, v.bigEndian))
  def getDouble(tag: Int): Option[Double] = get(tag, v => v.value.toDouble(v.vr, v.bigEndian))
  def getDates(tag: Int): Seq[LocalDate] = getAll(tag, v => v.value.toDates(v.vr))
  def getDate(tag: Int): Option[LocalDate] = get(tag, v => v.value.toDate(v.vr))
  def getDateTimes(tag: Int): Seq[ZonedDateTime] = getAll(tag, v => v.value.toDateTimes(v.vr, zoneOffset))
  def getDateTime(tag: Int): Option[ZonedDateTime] = get(tag, v => v.value.toDateTime(v.vr, zoneOffset))
  def getPatientNames(tag: Int): Seq[PatientName] = getAll(tag, v => v.value.toPatientNames(v.vr, characterSets))
  def getPatientName(tag: Int): Option[PatientName] = get(tag, v => v.value.toPatientName(v.vr, characterSets))

  def getSequence(tag: Int): Option[Sequence] = apply(tag).flatMap {
    case e: Sequence => Some(e)
    case _ => None
  }
  def getItem(tag: Int, item: Int): Option[Item] = getSequence(tag).flatMap(_.item(item))
  def getNested(tag: Int, item: Int): Option[Elements] = getItem(tag, item).map(_.elements)
  def getNested(tagPath: TagPathSequenceItem): Option[Elements] = {
    def find(elems: Option[Elements], tagPath: TagPathTrunk): Option[Elements] = {
      if (tagPath.isEmpty)
        elems
      else
        tagPath match {
          case tp: TagPathSequenceItem => find(elems, tagPath.previous).flatMap(_.getNested(tp.tag, tp.item))
          case _ => throw new IllegalArgumentException("Unsupported tag path type")
        }
    }

    find(Some(this), tagPath.previous).flatMap(_.getNested(tagPath.tag, tagPath.item))
  }

  def getFragments(tag: Int): Option[Fragments] = apply(tag).flatMap {
    case e: Fragments => Some(e)
    case _ => None
  }

  private def insertOrdered(element: ElementSet): Vector[ElementSet] = {
    if (isEmpty) Vector(element) else {
      val b = Vector.newBuilder[ElementSet]
      var isBelow = true
      data.foreach { e =>
        if (isBelow && e.tag > element.tag) {
          b += element
          isBelow = false
        }
        if (e.tag == element.tag) {
          b += element
          isBelow = false
        } else
          b += e
      }
      if (isBelow) b += element
      b.result()
    }
  }

  /**
    * Insert or update element in the root dataset with the specified tag number. If the element is Specific Character
    * Set or Timezone OFfset From UTC, this information will be updated accordingly.
    *
    * @param element element to insert or update
    * @return a new Elements containing the updated element
    */
  def set(element: ElementSet): Elements = element match {
    case e: ValueElement if e.tag == Tag.SpecificCharacterSet =>
      copy(characterSets = CharacterSets(e), data = insertOrdered(e))
    case e: ValueElement if e.tag == Tag.TimezoneOffsetFromUTC =>
      copy(zoneOffset = parseZoneOffset(e.value.toUtf8String).getOrElse(zoneOffset), data = insertOrdered(e))
    case e => copy(data = insertOrdered(e))
  }

  def setAll(elementSets: Seq[ElementSet]): Elements = elementSets.foldLeft(this)(_.set(_))

  def setNested(sequenceTag: Int, itemIndex: Int, setFunction: Elements => Elements): Option[Elements] =
    for {
      s1 <- getSequence(sequenceTag)
      i1 <- s1.item(itemIndex)
    } yield {
      val e1 = i1.elements
      val e2 = setFunction(e1)
      val i2 = i1.setElements(e2)
      val s2 = s1.setItem(itemIndex, i2)
      set(s2)
    }

  def setNested(tagPath: TagPathSequenceItem, element: ElementSet): Elements = {
    def find(elems: Elements, tagPath: List[TagPath]): Elements = {
      if (tagPath.isEmpty)
        elems.set(element)
      else
        tagPath.head match {
          case tp: TagPathSequenceItem => elems.setNested(tp.tag, tp.item, e => find(e, tagPath.tail)).getOrElse(elems)
          case _ => throw new IllegalArgumentException("Unsupported tag path type")
        }
    }

    find(this, tagPath.toList)
  }

  /**
    * Set the character sets used to decode string data in this Elements
    *
    * @param characterSets character sets to use in new Elements
    * @return a new Elements with the specified character sets
    */
  def setCharacterSets(characterSets: CharacterSets): Elements = copy(characterSets = characterSets)

  /**
    * Set the time zone offset used for date-time elements in this Elements
    *
    * @param zoneOffset time zone offset to be used for date-times without time zone specified
    * @return a new Elements with the specified time zone offset
    */
  def setZoneOffset(zoneOffset: ZoneOffset): Elements = copy(zoneOffset = zoneOffset)

  def setValue(tag: Int, vr: VR, value: Value, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    set(ValueElement(tag, vr, value, bigEndian, explicitVR))
  def setBytes(tag: Int, vr: VR, value: ByteString, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setValue(tag, vr, Value(value), bigEndian, explicitVR)

  def setStrings(tag: Int, vr: VR, values: Seq[String], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromStrings(vr, values, bigEndian), bigEndian, explicitVR)
  def setStrings(tag: Int, values: Seq[String], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setStrings(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setString(tag: Int, vr: VR, value: String, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromString(vr, value, bigEndian), bigEndian, explicitVR)
  def setString(tag: Int, value: String, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setString(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setShorts(tag: Int, vr: VR, values: Seq[Short], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromShorts(vr, values, bigEndian), bigEndian, explicitVR)
  def setShorts(tag: Int, values: Seq[Short], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setShorts(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setShort(tag: Int, vr: VR, value: Short, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromShort(vr, value, bigEndian), bigEndian, explicitVR)
  def setShort(tag: Int, value: Short, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setShort(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setInts(tag: Int, vr: VR, values: Seq[Int], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromInts(vr, values, bigEndian), bigEndian, explicitVR)
  def setInts(tag: Int, values: Seq[Int], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setInts(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setInt(tag: Int, vr: VR, value: Int, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromInt(vr, value, bigEndian), bigEndian, explicitVR)
  def setInt(tag: Int, value: Int, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setInt(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setLongs(tag: Int, vr: VR, values: Seq[Long], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromLongs(vr, values, bigEndian), bigEndian, explicitVR)
  def setLongs(tag: Int, values: Seq[Long], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setLongs(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setLong(tag: Int, vr: VR, value: Long, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromLong(vr, value, bigEndian), bigEndian, explicitVR)
  def setLong(tag: Int, value: Long, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setLong(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setFloats(tag: Int, vr: VR, values: Seq[Float], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromFloats(vr, values, bigEndian), bigEndian, explicitVR)
  def setFloats(tag: Int, values: Seq[Float], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setFloats(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setFloat(tag: Int, vr: VR, value: Float, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromFloat(vr, value, bigEndian), bigEndian, explicitVR)
  def setFloat(tag: Int, value: Float, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setFloat(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setDoubles(tag: Int, vr: VR, values: Seq[Double], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromDoubles(vr, values, bigEndian), bigEndian, explicitVR)
  def setDoubles(tag: Int, values: Seq[Double], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setDoubles(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setDouble(tag: Int, vr: VR, value: Double, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromDouble(vr, value, bigEndian), bigEndian, explicitVR)
  def setDouble(tag: Int, value: Double, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setDouble(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setDates(tag: Int, vr: VR, values: Seq[LocalDate], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromDates(vr, values), bigEndian, explicitVR)
  def setDates(tag: Int, values: Seq[LocalDate], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setDates(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setDate(tag: Int, vr: VR, value: LocalDate, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromDate(vr, value), bigEndian, explicitVR)
  def setDate(tag: Int, value: LocalDate, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setDate(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setDateTimes(tag: Int, vr: VR, values: Seq[ZonedDateTime], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromDateTimes(vr, values), bigEndian, explicitVR)
  def setDateTimes(tag: Int, values: Seq[ZonedDateTime], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setDateTimes(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setDateTime(tag: Int, vr: VR, value: ZonedDateTime, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromDateTime(vr, value), bigEndian, explicitVR)
  def setDateTime(tag: Int, value: ZonedDateTime, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setDateTime(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  def setPatientNames(tag: Int, vr: VR, values: Seq[PatientName], bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromPatientNames(vr, values), bigEndian, explicitVR)
  def setPatientNames(tag: Int, values: Seq[PatientName], bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setPatientNames(tag, Dictionary.vrOf(tag), values, bigEndian, explicitVR)
  def setPatientName(tag: Int, vr: VR, value: PatientName, bigEndian: Boolean, explicitVR: Boolean): Elements =
    setValue(tag, vr, Value.fromPatientName(vr, value), bigEndian, explicitVR)
  def setPatientName(tag: Int, value: PatientName, bigEndian: Boolean = false, explicitVR: Boolean = true): Elements =
    setPatientName(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)

  /**
    * Remove element with the specified tag, if present
    *
    * @param tag tag for element to be removed
    * @return a new Elements
    */
  def remove(tag: Int): Elements = filter(_.tag != tag)

  /**
    * Keep only elements for which the supplied filter function returns `true`
    *
    * @param f filter function
    * @return a new Elements
    */
  def filter(f: ElementSet => Boolean): Elements = copy(data = data.filter(f))

  /**
    * @return the first element in this Elements
    */
  def head: ElementSet = data.head

  /**
    * @return the number of elements in this Elements
    */
  def size: Int = data.size

  /**
    * @return `true` if this Elements contains no data
    */
  def isEmpty: Boolean = data.isEmpty

  /**
    * @return 'true' if this Elements contains one or more elements
    */
  def nonEmpty: Boolean = !isEmpty

  /**
    * @param tag tag to test
    * @return `true` if these elements contains an element with the input tag number
    */
  def hasElement(tag: Int): Boolean = data.map(_.tag).contains(tag)

  /**
    * @return a new Elements sorted by tag number. If already sorted, this function returns a copy
    */
  def sorted(): Elements = copy(data = data.sortBy(_.tag))

  /**
    * @return data as a list of `ElementSet`s
    */
  def toList: List[ElementSet] = data.toList

  /**
    * @return data as a list of `Element`s
    */
  def toElements: List[Element] = toList.flatMap(_.toElements)

  /**
    * @return data as a list of `DicomPart`s
    */
  def toParts: List[DicomPart] = toElements.flatMap(_.toParts)

  /**
    * @return a DICOM byte array representation of this Elements. This representation may be different from one used to
    *         produce this Elements as items and sequences are always of indeterminate length here.
    */
  def toBytes: ByteString = data.map(_.toBytes).foldLeft(ByteString.empty)(_ ++ _)

  private def toStrings(indent: String): Vector[String] = {
    def space1(description: String): String = " " * Math.max(0, 40 - description.length)

    def space2(length: Long): String = " " * Math.max(0, 4 - length.toString.length)

    data.flatMap {
      case e: ValueElement =>
        val strings = e.value.toStrings(e.vr, e.bigEndian, characterSets)
        val s = strings.mkString(multiValueDelimiter)
        val vm = strings.length.toString
        s"$indent${tagToString(e.tag)} ${e.vr} [$s] ${space1(s)} # ${space2(e.length)} ${e.length}, $vm ${Keyword.valueOf(e.tag)}" :: Nil

      case s: Sequence =>
        val heading = {
          val description = if (s.length == indeterminateLength) "Sequence with indeterminate length" else s"Sequence with explicit length ${s.length}"
          s"$indent${tagToString(s.tag)} SQ ($description) ${space1(description)} # ${space2(s.length)} ${s.length}, 1 ${Keyword.valueOf(s.tag)}"
        }
        val items = s.items.flatMap { i =>
          val heading = {
            val description = if (i.indeterminate) "Item with indeterminate length" else s"Item with explicit length ${i.length}"
            s"$indent  ${tagToString(Tag.Item)} na ($description) ${space1(description)} # ${space2(i.length)} ${i.length}, 1 Item"
          }
          val elems = i.elements.toStrings(indent + "    ").toList
          val delimitation = s"$indent  ${tagToString(Tag.ItemDelimitationItem)} na ${" " * 43} #     0, 0 ItemDelimitationItem${if (i.indeterminate) "" else " (marker)"}"
          heading :: elems ::: delimitation :: Nil
        }
        val delimitation = s"$indent${tagToString(Tag.SequenceDelimitationItem)} na ${" " * 43} #     0, 0 SequenceDelimitationItem${if (s.indeterminate) "" else " (marker)"}"
        heading :: items ::: delimitation :: Nil

      case f: Fragments =>
        val heading = {
          val description = s"Fragments with ${f.size} fragment(s)"
          s"$indent${tagToString(f.tag)} ${f.vr} ($description) ${space1(description)} #    na, 1 ${Keyword.valueOf(f.tag)}"
        }
        val offsets = f.offsets.map { o =>
          val description = s"Offsets table with ${o.length} offset(s)"
          s"$indent  ${tagToString(Tag.Item)} na ($description) ${space1(description)} # ${space2(o.length * 4)} ${o.length * 4}, 1 Item"
        }.getOrElse("")
        val fragments = f.fragments.map { f =>
          val description = s"Fragment with length ${f.length}"
          s"$indent  ${tagToString(Tag.Item)} na ($description) ${space1(description)} # ${space2(f.length)} ${f.length}, 1 Item"
        }
        val delimitation = s"$indent${tagToString(Tag.SequenceDelimitationItem)} na ${" " * 43} #     0, 0 SequenceDelimitationItem"
        heading :: offsets :: fragments ::: delimitation :: Nil

      case _ => Nil
    }
  }

  override def toString: String = toStrings("").mkString(System.lineSeparator)

}

object Elements {

  /**
    * @return an Elements with no data and default character set only
    */
  def empty(characterSets: CharacterSets = defaultCharacterSet, zoneOffset: ZoneOffset = systemZone) =
    Elements(characterSets, zoneOffset, Vector.empty)

  def newBuilder(characterSets: CharacterSets = CharacterSets.defaultOnly, zoneOffset: ZoneOffset = systemZone) =
    new ElementsBuilder(characterSets, zoneOffset)

  def fileMetaInformationElements(sopInstanceUID: String, sopClassUID: String, transferSyntax: String): List[ValueElement] = {
    val fmiElements = List(
      ValueElement.fromBytes(Tag.FileMetaInformationVersion, ByteString(0, 1)),
      ValueElement.fromString(Tag.MediaStorageSOPClassUID, sopClassUID),
      ValueElement.fromString(Tag.MediaStorageSOPInstanceUID, sopInstanceUID),
      ValueElement.fromString(Tag.TransferSyntaxUID, transferSyntax),
      ValueElement.fromString(Tag.ImplementationClassUID, Implementation.classUid),
      ValueElement.fromString(Tag.ImplementationVersionName, Implementation.versionName))
    val groupLength = ValueElement.fromBytes(Tag.FileMetaInformationGroupLength, intToBytesLE(fmiElements.map(_.toBytes.length).sum))
    groupLength :: fmiElements
  }

  trait Element {
    val bigEndian: Boolean
    def toBytes: ByteString
    def toParts: List[DicomPart]
  }

  sealed trait ElementSet {
    val tag: Int
    val vr: VR
    val bigEndian: Boolean
    val explicitVR: Boolean
    def toBytes: ByteString
    def toElements: List[Element]
  }

  case object PreambleElement extends Element {
    override val bigEndian: Boolean = false
    override def toBytes: ByteString = ByteString.fromArray(new Array[Byte](128)) ++ ByteString("DICM")
    override def toString: String = "PreambleElement(0, ..., 0, D, I, C, M)"
    override def toParts: List[DicomPart] = PreamblePart(toBytes) :: Nil
  }

  case class ValueElement(tag: Int, vr: VR, value: Value, bigEndian: Boolean, explicitVR: Boolean) extends Element with ElementSet {
    val length: Int = value.length
    /**
      * Return a copy of this element with its value updated.
      *
      * @param value the new value
      * @return a new Element instance
      */
    def setValue(value: Value): ValueElement = copy(value = value.ensurePadding(vr))
    /**
      * @return The DICOM byte array representation of this element
      */
    override def toBytes: ByteString = toParts.map(_.bytes).reduce(_ ++ _)
    override def toParts: List[DicomPart] =
      HeaderPart(tag, vr, length, isFileMetaInformation(tag), bigEndian, explicitVR) :: ValueChunk(bigEndian, value.bytes, last = true) :: Nil
    override def toElements: List[Element] = this :: Nil
    override def toString: String = {
      val strings = value.toStrings(vr, bigEndian, CharacterSets.defaultOnly)
      val s = strings.mkString(multiValueDelimiter)
      val vm = strings.length.toString
      s"ValueElement(${tagToString(tag)} $vr [$s] # $length, $vm ${Keyword.valueOf(tag)})"
    }
  }

  object ValueElement {
    def apply(tag: Int, value: Value, bigEndian: Boolean = false, explicitVR: Boolean = true): ValueElement =
      ValueElement(tag, Dictionary.vrOf(tag), value, bigEndian, explicitVR)
    def fromBytes(tag: Int, bytes: ByteString, bigEndian: Boolean = false, explicitVR: Boolean = true): ValueElement =
      apply(tag, Value(bytes), bigEndian, explicitVR)
    def fromString(tag: Int, string: String, bigEndian: Boolean = false, explicitVR: Boolean = true): ValueElement =
      apply(tag, Value(ByteString(string)), bigEndian, explicitVR)
    def empty(tag: Int, vr: VR, bigEndian: Boolean = false, explicitVR: Boolean = true): ValueElement =
      ValueElement(tag, vr, Value.empty, bigEndian, explicitVR)
  }

  case class SequenceElement(tag: Int, length: Long, bigEndian: Boolean = false, explicitVR: Boolean = true) extends Element {
    override def toBytes: ByteString = HeaderPart(tag, VR.SQ, length, isFmi = false, bigEndian, explicitVR).bytes
    override def toParts: List[DicomPart] = SequencePart(tag, length, bigEndian, explicitVR, toBytes) :: Nil
    override def toString: String = s"SequenceElement(${tagToString(tag)} SQ # $length ${Keyword.valueOf(tag)})"
  }

  case class FragmentsElement(tag: Int, vr: VR, bigEndian: Boolean = false, explicitVR: Boolean = true) extends Element {
    override def toBytes: ByteString = toParts.head.bytes
    override def toParts: List[DicomPart] = HeaderPart(tag, vr, indeterminateLength, isFmi = false, bigEndian, explicitVR) :: Nil
    override def toString: String = s"FragmentsElement(${tagToString(tag)} $vr # ${Keyword.valueOf(tag)})"
  }

  case class FragmentElement(index: Int, length: Long, value: Value, bigEndian: Boolean = false) extends Element {
    override def toBytes: ByteString = toParts.map(_.bytes).reduce(_ ++ _)
    override def toParts: List[DicomPart] = ItemElement(index, value.length, bigEndian).toParts ::: ValueChunk(bigEndian, value.bytes, last = true) :: Nil
    override def toString: String = s"FragmentElement(index = $index, length = $length)"
  }

  object FragmentElement {
    def empty(index: Int, length: Long, bigEndian: Boolean = false) = FragmentElement(index, length, Value.empty, bigEndian)
  }

  case class ItemElement(index: Int, length: Long, bigEndian: Boolean = false) extends Element {
    override def toBytes: ByteString = tagToBytes(Tag.Item, bigEndian) ++ intToBytes(length.toInt, bigEndian)
    override def toParts: List[DicomPart] = ItemPart(index, length, bigEndian, toBytes) :: Nil
    override def toString: String = s"ItemElement(index = $index, length = $length)"
  }

  case class ItemDelimitationElement(index: Int, marker: Boolean = false, bigEndian: Boolean = false) extends Element {
    override def toBytes: ByteString = tagToBytes(Tag.ItemDelimitationItem, bigEndian) ++ ByteString(0, 0, 0, 0)
    override def toParts: List[DicomPart] = if (marker) Nil else ItemDelimitationPart(index, bigEndian, toBytes) :: Nil
    override def toString: String = s"ItemDelimitationElement(index = $index, marker = $marker)"
  }

  case class SequenceDelimitationElement(marker: Boolean = false, bigEndian: Boolean = false) extends Element {
    override def toBytes: ByteString = tagToBytes(Tag.SequenceDelimitationItem, bigEndian) ++ ByteString(0, 0, 0, 0)
    override def toParts: List[DicomPart] = if (marker) Nil else SequenceDelimitationPart(bigEndian, toBytes) :: Nil
    override def toString: String = s"SequenceDelimitationElement(marker = $marker)"
  }


  case class Sequence(tag: Int, length: Long, items: List[Item], bigEndian: Boolean = false, explicitVR: Boolean = true) extends ElementSet {
    val vr: VR = VR.SQ
    val indeterminate: Boolean = length == indeterminateLength
    def item(index: Int): Option[Item] = try Option(items(index - 1)) catch { case _: Throwable => None }
    def +(item: Item): Sequence = copy(items = items :+ item)
    override def toBytes: ByteString = toElements.map(_.toBytes).reduce(_ ++ _)
    override def toElements: List[Element] = SequenceElement(tag, length, bigEndian, explicitVR) ::
      items.zipWithIndex.flatMap { case (item, index) => item.toElements(index + 1) } :::
      SequenceDelimitationElement(marker = !indeterminate, bigEndian) :: Nil
    def size: Int = items.length
    def setItem(index: Int, item: Item): Sequence = copy(items = items.updated(index - 1, item))
    override def toString: String = s"Sequence(${tagToString(tag)} SQ # $length ${items.length} ${Keyword.valueOf(tag)})"
  }

  object Sequence {
    def empty(tag: Int, length: Long, bigEndian: Boolean = false, explicitVR: Boolean = true): Sequence = Sequence(tag, length, Nil, bigEndian, explicitVR)
    def empty(element: SequenceElement): Sequence = empty(element.tag, element.length, element.bigEndian, element.explicitVR)
  }


  case class Item(length: Long, elements: Elements, bigEndian: Boolean = false) {
    val indeterminate: Boolean = length == indeterminateLength
    def withElements(elements: Elements): Item = copy(elements = elements)
    def toBytes(index: Int): ByteString = toElements(index).map(_.toBytes).reduce(_ ++ _)
    def toElements(index: Int): List[Element] = ItemElement(index, length, bigEndian) :: elements.toElements :::
      ItemDelimitationElement(index, marker = !indeterminate, bigEndian) :: Nil
    def setElements(elements: Elements): Item = copy(elements = elements)
    override def toString: String = s"Item(length = $length, elements size = ${elements.size})"
  }

  object Item {
    def empty(length: Long, bigEndian: Boolean = false): Item = Item(length, Elements.empty(), bigEndian)
    def empty(element: ItemElement): Item = empty(element.length, element.bigEndian)
  }


  case class Fragment(length: Long, value: Value, bigEndian: Boolean = false) {
    def toBytes(index: Int): ByteString = FragmentElement(index, length, value, bigEndian).toBytes
    def toElement(index: Int): FragmentElement = FragmentElement(index, length, value, bigEndian)
    def setValue(value: Value): Fragment = copy(value = value.ensurePadding(VR.OW))
    override def toString: String = s"Fragment(length = $length, value length = ${value.length})"
  }

  object Fragment {
    def fromElement(fragmentElement: FragmentElement): Fragment = Fragment(fragmentElement.length, fragmentElement.value, fragmentElement.bigEndian)
  }


  case class Fragments(tag: Int, vr: VR, offsets: Option[List[Int]], fragments: List[Fragment], bigEndian: Boolean = false, explicitVR: Boolean = true) extends ElementSet {
    def fragment(index: Int): Option[Fragment] = try Option(fragments(index - 1)) catch { case _: Throwable => None }
    def +(fragment: Fragment): Fragments =
      if (fragments.isEmpty && offsets.isEmpty)
        copy(offsets = Option(fragment.value.bytes.grouped(4).map(bytesToInt(_, fragment.bigEndian)).toList))
      else
        copy(fragments = fragments :+ fragment)
    def toBytes: ByteString = toElements.map(_.toBytes).reduce(_ ++ _)
    def size: Int = fragments.length
    override def toElements: List[Element] = FragmentsElement(tag, vr, bigEndian, explicitVR) ::
      offsets.map(o => FragmentElement(1, 4L * o.length, Value(o.map(intToBytes(_, bigEndian)).foldLeft(ByteString.empty)(_ ++ _)), bigEndian) :: Nil).getOrElse(Nil) :::
      fragments.zipWithIndex.map { case (fragment, index) => fragment.toElement(index + 2) } :::
      SequenceDelimitationElement(bigEndian) :: Nil
    def setFragment(index: Int, fragment: Fragment): Fragments = copy(fragments = fragments.updated(index - 1, fragment))
    override def toString: String = s"Fragments(${tagToString(tag)} $vr # ${fragments.length} ${Keyword.valueOf(tag)})"
  }

  object Fragments {
    def empty(tag: Int, vr: VR, bigEndian: Boolean = false, explicitVR: Boolean = true): Fragments = Fragments(tag, vr, None, Nil, bigEndian, explicitVR)
    def empty(element: FragmentsElement): Fragments = empty(element.tag, element.vr, element.bigEndian, element.explicitVR)
  }

  class ElementsBuilder private[Elements](var characterSets: CharacterSets = CharacterSets.defaultOnly, var zoneOffset: ZoneOffset = systemZone) {
    val data: ArrayBuffer[ElementSet] = ArrayBuffer.empty
    def +=(element: ElementSet): ElementsBuilder = {
      element match {
        case e: ValueElement if e.tag == Tag.SpecificCharacterSet =>
          characterSets = CharacterSets(e)
        case e: ValueElement if e.tag == Tag.TimezoneOffsetFromUTC =>
          zoneOffset = parseZoneOffset(e.value.toUtf8String).getOrElse(zoneOffset)
        case _ =>
      }
      data += element
      this
    }
    def result(): Elements = Elements(characterSets, zoneOffset, data.toVector)
    override def toString: String = s"ElementsBuilder(characterSets = $characterSets, zoneOffset = $zoneOffset, size = ${data.size})"
  }

}

