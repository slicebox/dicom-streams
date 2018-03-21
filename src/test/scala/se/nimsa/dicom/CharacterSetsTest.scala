package se.nimsa.dicom

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

import scala.concurrent.ExecutionContextExecutor

class CharacterSetsTest extends TestKit(ActorSystem("CharacterSetsSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  /*
  @BeforeClass def readFiles(): Unit = {
    arab = readFile("SCSARAB")
    french = readFile("SCSFREN")
    german = readFile("SCSGERM")
    greek = readFile("SCSGREEK")
    japan1 = readFile("SCSH31")
    japan2 = readFile("SCSH32")
    hebrew = readFile("SCSHBRW")
    korea = readFile("SCSI2")
    russian = readFile("SCSRUSS")
    china1 = readFile("SCSX1")
    china2 = readFile("SCSX2")
  }
*/

  "Parsing a DICOM file" should "parse an Arab name correctly" in {
    val nameCodePoints = Array[Int]('ق', 'ب', 'ا', 'ن', 'ي', '^', 'ل', 'ن', 'ز', 'ا', 'ر')
    val nameBytes = ByteString(0xE2, 0xC8, 0xC7, 0xE6, 0xEA, 0x5E, 0xE4, 0xE6, 0xD2, 0xC7, 0xD1)
    val charsets = new CharacterSets(Seq("ISO_IR 127"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a French name correctly" in {
    val nameCodePoints = Array[Int]('B', 'u', 'c', '^', 'J', 'é', 'r', 'ô', 'm', 'e')
    val nameBytes = ByteString(0x42, 0x75, 0x63, 0x5E, 0x4A, 0xE9, 0x72, 0xF4, 0x6D, 0x65)
    val charsets = new CharacterSets(Seq("ISO_IR 100"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a German name correctly" in {
    val nameCodePoints = Array[Int]('Ä', 'n', 'e', 'a', 's', '^', 'R', 'ü', 'd', 'i', 'g', 'e', 'r')
    val nameBytes = ByteString(0xC4, 0x6E, 0x65, 0x61, 0x73, 0x5E, 0x52, 0xFC, 0x64, 0x69, 0x67, 0x65, 0x72)
    val charsets = new CharacterSets(Seq("ISO_IR 100"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Greek name correctly" in {
    val nameCodePoints = Array[Int]('Δ', 'ι', 'ο', 'ν', 'υ', 'σ', 'ι', 'ο', 'ς')
    val nameBytes = ByteString(0xC4, 0xE9, 0xEF, 0xED, 0xF5, 0xF3, 0xE9, 0xEF, 0xF2)
    val charsets = new CharacterSets(Seq("ISO_IR 126"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Japanese name correctly (1)" in {
    val nameCodePoints = Array[Int]('Y', 'a', 'm', 'a', 'd', 'a', '^', 'T', 'a', 'r', 'o', 'u', '=', '山', '田', '^', '太', '郎', '=', 'や', 'ま', 'だ', '^', 'た', 'ろ', 'う')
    val nameBytes = ByteString(0x59, 0x61, 0x6D, 0x61, 0x64, 0x61, 0x5E, 0x54, 0x61, 0x72, 0x6F, 0x75, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28, 0x42, 0x5E, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x42, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64, 0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x42, 0x5E, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B, 0x28, 0x42)
    val charsets = new CharacterSets(Seq("","ISO 2022 IR 87"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Japanese name correctly (2)" in {
    val nameCodePoints = Array[Int]('ﾔ', 'ﾏ', 'ﾀ', 'ﾞ', '^', 'ﾀ', 'ﾛ', 'ｳ', '=', '山', '田', '^', '太', '郎', '=', 'や', 'ま', 'だ', '^', 'た', 'ろ', 'う')
    val nameBytes = ByteString(0xD4, 0xCF, 0xC0, 0xDE, 0x5E, 0xC0, 0xDB, 0xB3, 0x3D, 0x1B, 0x24, 0x42, 0x3B, 0x33, 0x45, 0x44, 0x1B, 0x28, 0x4A, 0x5E, 0x1B, 0x24, 0x42, 0x42, 0x40, 0x4F, 0x3A, 0x1B, 0x28, 0x4A, 0x3D, 0x1B, 0x24, 0x42, 0x24, 0x64, 0x24, 0x5E, 0x24, 0x40, 0x1B, 0x28, 0x4A, 0x5E, 0x1B, 0x24, 0x42, 0x24, 0x3F, 0x24, 0x6D, 0x24, 0x26, 0x1B, 0x28, 0x4A)
    val charsets = new CharacterSets(Seq("ISO 2022 IR 13", "ISO 2022 IR 87"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Hebrew name correctly" in {
    val nameCodePoints = Array[Int]('ש', 'ר', 'ו', 'ן', '^', 'ד', 'ב', 'ו', 'ר', 'ה')
    val nameBytes = ByteString(0xF9, 0xF8, 0xE5, 0xEF, 0x5E, 0xE3, 0xE1, 0xE5, 0xF8, 0xE4)
    val charsets = new CharacterSets(Seq("ISO_IR 138"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Korean name correctly" in {
    val nameCodePoints = Array[Int]('H', 'o', 'n', 'g', '^', 'G', 'i', 'l', 'd', 'o', 'n', 'g', '=', '洪', '^', '吉', '洞', '=', '홍', '^', '길', '동')
    val nameBytes = ByteString(0x48, 0x6F, 0x6E, 0x67, 0x5E, 0x47, 0x69, 0x6C, 0x64, 0x6F, 0x6E, 0x67, 0x3D, 0x1B, 0x24, 0x29, 0x43, 0xFB, 0xF3, 0x5E, 0x1B, 0x24, 0x29, 0x43, 0xD1, 0xCE, 0xD4, 0xD7, 0x3D, 0x1B, 0x24, 0x29, 0x43, 0xC8, 0xAB, 0x5E, 0x1B, 0x24, 0x29, 0x43, 0xB1, 0xE6, 0xB5, 0xBF)
    val charsets = new CharacterSets(Seq("", "ISO 2022 IR 149"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Russian name correctly" in {
    val nameCodePoints = Array[Int]('Л', 'ю', 'к', 'c', 'e', 'м', 'б', 'y', 'p', 'г')
    val nameBytes = ByteString(0xBB, 0xEE, 0xDA, 0x63, 0x65, 0xDC, 0xD1, 0x79, 0x70, 0xD3)
    val charsets = new CharacterSets(Seq("ISO_IR 144"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Chinese name correctly (1)" in {
    val nameCodePoints = Array[Int]('W', 'a', 'n', 'g', '^', 'X', 'i', 'a', 'o', 'D', 'o', 'n', 'g', '=', '王', '^', '小', '東', '=')
    val nameBytes = ByteString(0x57, 0x61, 0x6E, 0x67, 0x5E, 0x58, 0x69, 0x61, 0x6F, 0x44, 0x6F, 0x6E, 0x67, 0x3D, 0xE7, 0x8E, 0x8B, 0x5E, 0xE5, 0xB0, 0x8F, 0xE6, 0x9D, 0xB1, 0x3D)
    val charsets = new CharacterSets(Seq("ISO_IR 192"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  it should "parse a Chinese name correctly (2)" in {
    val nameCodePoints = Array[Int]('W', 'a', 'n', 'g', '^', 'X', 'i', 'a', 'o', 'D', 'o', 'n', 'g', '=', '王', '^', '小', '东', '=')
    val nameBytes = ByteString(0x57, 0x61, 0x6E, 0x67, 0x5E, 0x58, 0x69, 0x61, 0x6F, 0x44, 0x6F, 0x6E, 0x67, 0x3D, 0xCD, 0xF5, 0x5E, 0xD0, 0xA1, 0xB6, 0xAB, 0x3D)
    val charsets = new CharacterSets(Seq("GB18030"))
    val name = charsets.decode(VR.PN, nameBytes)
    checkPatientName(name, nameCodePoints)
  }

  private def checkPatientName(name: String, expectedCodePoints: Array[Int]): Unit = {
    val codePoints = new Array[Int](name.codePointCount(0, name.length))
    val length = name.length
    var i = 0
    var offset = 0
    while (offset < length) {
      val codepoint = name.codePointAt(offset)
      codePoints(i) = codepoint
      i += 1
      offset += Character.charCount(codepoint)
    }
    codePoints shouldBe expectedCodePoints
  }

}
