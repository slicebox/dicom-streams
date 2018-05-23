package se.nimsa.dicom.data

import akka.util.ByteString
import se.nimsa.dicom.data.DicomElements.Elements
import se.nimsa.dicom.data.VR.VR

object DicomParts {

  /**
    * The most general representation of a DICOM stream chunk
    */
  trait DicomPart {
    def bigEndian: Boolean
    def bytes: ByteString
  }

  /**
    * A `DicomPart` with a tag number (e.g. `DicomHeader`, `DicomSequence`, 'DicomFragments')
    */
  trait TagPart extends DicomPart {
    def tag: Int
  }

  /**
    * A 'DicomPart' with a length attribute (e.g. `DicomHeader`, `DicomSequence`, 'DicomItem')
    */
  trait LengthPart extends DicomPart {
    def length: Long
    def hasLength: Boolean = length >= 0
  }

  case class PreamblePart(bytes: ByteString) extends DicomPart {
    def bigEndian = false
    override def toString = s"${getClass.getSimpleName} (${bytes.length} bytes)"
  }

  case class HeaderPart(tag: Int, vr: VR, length: Long, isFmi: Boolean, bigEndian: Boolean, explicitVR: Boolean, bytes: ByteString) extends DicomPart with TagPart with LengthPart {

    def withUpdatedLength(newLength: Long): HeaderPart =
      if (newLength == length)
        this
      else {
        val updated = if ((bytes.size >= 8) && explicitVR && (vr.headerLength == 8)) { //explicit vr
          bytes.take(6) ++ shortToBytes(newLength.toShort, bigEndian)
        } else if ((bytes.size >= 12) && explicitVR && (vr.headerLength == 12)) { //explicit vr
          bytes.take(8) ++ intToBytes(newLength.toInt, bigEndian)
        } else { //implicit vr
          bytes.take(4) ++ intToBytes(newLength.toInt, bigEndian)
        }

        HeaderPart(tag, vr, newLength, isFmi, bigEndian, explicitVR, updated)
      }

    override def toString = s"${getClass.getSimpleName} ${tagToString(tag)} ${if (isFmi) "(meta) " else ""}$vr ${if (!explicitVR) "(implicit) " else ""}length = ${bytes.length} value length = $length ${if (bigEndian) "(big endian) " else ""}$bytes"
  }

  object HeaderPart {
    def apply(tag: Int, vr: VR, length: Long, isFmi: Boolean, bigEndian: Boolean, explicitVR: Boolean): HeaderPart = {
      val headerBytes = if (explicitVR)
        if (vr.headerLength == 8)
          tagToBytes(tag, bigEndian) ++ ByteString(vr.toString) ++ shortToBytes(length.toShort, bigEndian)
        else
          tagToBytes(tag, bigEndian) ++ ByteString(vr.toString) ++ ByteString(0, 0) ++ intToBytes(length.toInt, bigEndian)
      else
        tagToBytes(tag, bigEndian) ++ intToBytes(length.toInt, bigEndian)
      HeaderPart(tag, vr, length, isFmi, bigEndian, explicitVR, headerBytes)
    }
  }

  case class ValueChunk(bigEndian: Boolean, bytes: ByteString, last: Boolean) extends DicomPart {
    override def toString = s"${getClass.getSimpleName} ${if (last) "(last) " else ""}length = ${bytes.length} ${if (bigEndian) "(big endian) " else ""}${if (bytes.length <= 64) "ASCII = " + bytes.utf8String else ""}"
  }

  case class DeflatedChunk(bigEndian: Boolean, bytes: ByteString) extends DicomPart

  trait ItemPart extends LengthPart {
    def index: Int
    def length: Long
  }

  case class SequenceItemPart(index: Int, length: Long, bigEndian: Boolean, bytes: ByteString) extends ItemPart {
    override def toString = s"${getClass.getSimpleName} index = $index length = $length ${if (bigEndian) "(big endian) " else ""}$bytes"
  }

  // FIXME
  case class FragmentsItemPart(index: Int, length: Long, bigEndian: Boolean, bytes: ByteString) extends ItemPart {
    override def toString = s"${getClass.getSimpleName} index = $index length = $length ${if (bigEndian) "(big endian) " else ""}$bytes"
  }

  case class ItemDelimitationPart(index: Int, bigEndian: Boolean, bytes: ByteString) extends DicomPart

  case class SequencePart(tag: Int, length: Long, bigEndian: Boolean, explicitVR: Boolean, bytes: ByteString) extends DicomPart with TagPart with LengthPart {
    override def toString = s"${getClass.getSimpleName} ${tagToString(tag)} length = $length ${if (bigEndian) "(big endian) " else ""}${if (!explicitVR) "(implicit) " else ""}$bytes"
  }

  case class SequenceDelimitationPart(bigEndian: Boolean, bytes: ByteString) extends DicomPart

  case class FragmentsPart(tag: Int, length: Long, vr: VR, bigEndian: Boolean, bytes: ByteString) extends DicomPart with TagPart with LengthPart {
    override def toString = s"${getClass.getSimpleName} ${tagToString(tag)} $vr ${if (bigEndian) "(big endian) " else ""}$bytes"
  }

  // FIXME
  case class FragmentsDelimitationPart(bigEndian: Boolean, bytes: ByteString) extends DicomPart

  case class UnknownPart(bigEndian: Boolean, bytes: ByteString) extends DicomPart

  class ElementsPart(val label: String, characterSets: CharacterSets, data: Map[TagPath, Element]) extends Elements(characterSets, data) with DicomPart {
    override def bigEndian: Boolean = false
    override def bytes: ByteString = elements.foldLeft(ByteString.empty)(_ ++ _.toBytes)
  }
}
