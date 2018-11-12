package se.nimsa.dicom.data

import org.scalatest.{FlatSpec, Matchers}
import se.nimsa.dicom.data.TagPath.EmptyTagPath

class TagPathTest extends FlatSpec with Matchers {

  import TagPath.TagPathTag

  "The tag path depth" should "be 1 when pointing to a tag in the root dataset" in {
    val path = TagPath.fromTag(Tag.PatientID)
    path.depth shouldBe 1
  }

  it should "be 0 for empty tag paths" in {
    EmptyTagPath.depth shouldBe 0
  }

  it should "be 4 when pointing to a tag in three levels of sequences" in {
    val path = TagPath.fromItem(Tag.DerivationCodeSequence, 1).thenItem(Tag.DerivationCodeSequence, 3).thenItem(Tag.DerivationCodeSequence, 5).thenTag(Tag.PatientID)
    path.depth shouldBe 4
  }

  "A tag path" should "have a legible string representation" in {
    val path = TagPath.fromItem(Tag.DerivationCodeSequence, 4).thenItem(Tag.DerivationCodeSequence, 3).thenItem(Tag.DerivationCodeSequence, 2).thenTag(Tag.PatientID)
    path.toString(lookup = false) shouldBe "(0008,9215)[4].(0008,9215)[3].(0008,9215)[2].(0010,0020)"
  }

  it should "support string representations with keywords instead of tag numbers where possible" in {
    val path = TagPath.fromItem(Tag.DerivationCodeSequence, 1).thenItem(0x11110100, 3).thenItem(Tag.DetectorInformationSequence, 3).thenTag(Tag.PatientID)
    path.toString(lookup = true) shouldBe "DerivationCodeSequence[1].(1111,0100)[3].DetectorInformationSequence[3].PatientID"
  }

  it should "be root when pointing to root dataset" in {
    val path = TagPath.fromTag(Tag.PatientID)
    path.isRoot shouldBe true
  }

  it should "not be root when pointing to a tag in a sequence" in {
    val path = TagPath.fromItem(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientID)
    path.isRoot shouldBe false
  }

  "A list representation of tag path tags" should "contain a single entry for a tag in the root dataset" in {
    val path = TagPath.fromTag(Tag.PatientID)
    path.toList shouldBe path :: Nil
  }

  it should "contain four entries for a path of depth 3" in {
    val path = TagPath.fromItem(Tag.DerivationCodeSequence, 1).thenItem(Tag.DerivationCodeSequence, 3).thenItem(Tag.DerivationCodeSequence, 2).thenTag(Tag.PatientID)
    path.toList shouldBe path.previous.previous.previous :: path.previous.previous :: path.previous :: path :: Nil
  }

  "Comparing two tag paths" should "return false when comparing a tag path to itself" in {
    val path = TagPath.fromTag(1)
    path < path shouldBe false
  }

  it should "return false when comparing two equivalent tag paths" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(1, 3).thenItem(1, 1).thenTag(2)
    val bPath = TagPath.fromItem(1, 1).thenItem(1, 3).thenItem(1, 1).thenTag(2)
    aPath < bPath shouldBe false
  }

  it should "sort them by tag number for depth 0 paths" in {
    val aPath = TagPath.fromTag(1)
    val bPath = TagPath.fromTag(2)
    aPath < bPath shouldBe true
  }

  it should "sort them by tag number for depth 1 paths" in {
    val aPath = TagPath.fromItem(1, 1).thenTag(2)
    val bPath = TagPath.fromItem(1, 1).thenTag(1)
    aPath < bPath shouldBe false
  }

  it should "sort by earliest difference in deep paths" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(1, 2).thenTag(1)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenTag(1)
    aPath < bPath shouldBe true
  }

  it should "sort longer lists after shorter lists when otherwise equivalent" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3)
    aPath < bPath shouldBe false
  }

  it should "sort by item number in otherwise equivalent paths" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(1, 2).thenItem(1, 2).thenTag(2)
    val bPath = TagPath.fromItem(1, 1).thenItem(1, 3).thenItem(1, 2).thenTag(2)
    aPath < bPath shouldBe true
  }

  it should "not provide a particular ordering of two empty tag paths" in {
    EmptyTagPath < EmptyTagPath shouldBe false
  }

  it should "sort empty paths as less than any other path" in {
    EmptyTagPath < TagPath.fromTag(0) shouldBe true
    EmptyTagPath < TagPath.fromItem(0, 1) shouldBe true
  }

  it should "sort non-empty tag paths after empty paths" in {
    TagPath.fromTag(0) < EmptyTagPath shouldBe false
    TagPath.fromItem(0, 1) < EmptyTagPath shouldBe false

  }

  "Two tag paths" should "be equal if they point to the same path" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    aPath shouldBe bPath
  }

  it should "not be equal if item indices do not match" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(2, 1).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    aPath should not be bPath
  }

  it should "not be equal if they point to different tags" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(5)
    aPath should not be bPath
  }

  it should "not be equal if they have different depths" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    aPath should not be bPath
  }

  it should "be equal if both are empty" in {
    EmptyTagPath == EmptyTagPath shouldBe true
  }

  "The startsWith test" should "return true for equal paths" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    aPath.startsWith(bPath) shouldBe true
  }

  it should "return true for two empty paths" in {
    EmptyTagPath.startsWith(EmptyTagPath) shouldBe true
  }

  it should "return true when any path starts with empty path" in {
    val aPath = TagPath.fromTag(1)
    aPath.startsWith(EmptyTagPath) shouldBe true
  }

  it should "return false when empty path starts with non-empty path" in {
    val aPath = TagPath.fromTag(1)
    EmptyTagPath.startsWith(aPath) shouldBe false
  }

  it should "return false when subject path is longer than path" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    aPath.startsWith(bPath) shouldBe false
  }

  it should "return true when paths involving item indices are equal" in {
    val aPath = TagPath.fromItem(1, 4).thenItem(2, 2).thenItem(3, 2).thenTag(4)
    val bPath = TagPath.fromItem(1, 4).thenItem(2, 2).thenItem(3, 2).thenTag(4)
    aPath.startsWith(bPath) shouldBe true
  }

  it should "return true when subject path is subset of path" in {
    val aPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3)
    aPath.startsWith(bPath) shouldBe true
  }

  "The endsWith test" should "return true when a longer tag ends with a shorter" in {
    val aPath = TagPath.fromItem(1, 3).thenTag(2)
    val bPath = TagPath.fromTag(2)
    aPath.endsWith(bPath) shouldBe true
  }

  it should "return true for two empty paths" in {
    EmptyTagPath.endsWith(EmptyTagPath) shouldBe true
  }

  it should "return false when checking if non-empty path ends with empty path" in {
    val aPath = TagPath.fromTag(1)
    aPath.endsWith(EmptyTagPath) shouldBe false
  }

  it should "return false when empty path starts with non-empty path" in {
    val aPath = TagPath.fromTag(1)
    EmptyTagPath.endsWith(aPath) shouldBe false
  }

  it should "return false when a shorter tag is compared to a longer" in {
    val aPath = TagPath.fromItem(1, 3).thenTag(2)
    val bPath = TagPath.fromTag(2)
    bPath.endsWith(aPath) shouldBe false
  }

  it should "return false when tag numbers do not match" in {
    val aPath = TagPath.fromItem(1, 3).thenTag(2)
    val bPath = TagPath.fromTag(4)
    aPath.endsWith(bPath) shouldBe false
  }

  it should "work also with deep sequences" in {
    val aPath = TagPath.fromItem(1, 3).thenItem(2, 4).thenItem(3, 5).thenTag(6)
    val bPath = TagPath.fromItem(2, 4).thenItem(3, 5).thenTag(6)
    aPath.endsWith(bPath) shouldBe true
  }

  "Parsing a tag path" should "work for well-formed depth 0 tag paths" in {
    TagPathTag.parse("(0010,0010)") shouldBe TagPath.fromTag(Tag.PatientName)
  }

  it should "work for deep tag paths" in {
    TagPathTag.parse("(0008,9215)[1].(0008,9215)[666].(0010,0010)") shouldBe TagPath.fromItem(Tag.DerivationCodeSequence, 1).thenItem(Tag.DerivationCodeSequence, 666).thenTag(Tag.PatientName)
  }

  it should "throw an exception for malformed strings" in {
    intercept[IllegalArgumentException] {
      TagPathTag.parse("abc")
    }
  }

  it should "throw an exception for empty strings" in {
    intercept[IllegalArgumentException] {
      TagPathTag.parse("")
    }
  }

  it should "accept both tag numbers and keywords" in {
    val ref = TagPath.fromItem(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientName)
    TagPathTag.parse("(0008,9215)[1].(0010,0010)") shouldBe ref
    TagPathTag.parse("DerivationCodeSequence[1].(0010,0010)") shouldBe ref
    TagPathTag.parse("(0008,9215)[1].PatientName") shouldBe ref
    TagPathTag.parse("DerivationCodeSequence[1].PatientName") shouldBe ref
  }

  "The contains test" should "return for any tag number on the tag path" in {
    val path = TagPath.fromItem(1, 1).thenItem(2, 2).thenTag(3)
    path.contains(1) shouldBe true
    path.contains(2) shouldBe true
    path.contains(3) shouldBe true
    path.contains(4) shouldBe false
  }

  "The take operation" should "preserve elements from the left" in {
    val path = TagPath.fromItem(1, 1).thenItem(2, 1).thenItem(3, 3).thenTag(4)
    path.take(-100) shouldBe EmptyTagPath
    path.take(0) shouldBe EmptyTagPath
    path.take(1) shouldBe TagPath.fromItem(1, 1)
    path.take(2) shouldBe TagPath.fromItem(1, 1).thenItem(2, 1)
    path.take(3) shouldBe TagPath.fromItem(1, 1).thenItem(2, 1).thenItem(3, 3)
    path.take(4) shouldBe path
    path.take(100) shouldBe path
  }

  "The drop operation" should "remove elements from the left" in {
    val path = TagPath.fromItem(1, 1).thenItem(2, 1).thenItem(3, 3).thenTag(4)
    path.drop(-100) shouldBe path
    path.drop(0) shouldBe path
    path.drop(1) shouldBe TagPath.fromItem(2, 1).thenItem(3, 3).thenTag(4)
    path.drop(2) shouldBe TagPath.fromItem(3, 3).thenTag(4)
    path.drop(3) shouldBe TagPath.fromTag(4)
    path.drop(4) shouldBe EmptyTagPath
    path.drop(100) shouldBe EmptyTagPath
  }

  "The head of a tag path" should "be the root element of the path" in {
    TagPath.fromTag(1).head shouldBe TagPath.fromTag(1)
    TagPath.fromItem(1, 1).head shouldBe TagPath.fromItem(1, 1)
    TagPath.fromItem(1, 1).thenTag(2).head shouldBe TagPath.fromItem(1, 1)
    TagPath.fromItem(1, 1).thenItem(2, 2).thenTag(3).head shouldBe TagPath.fromItem(1, 1)
  }

  "The tail of a tag path" should "be the whole part except the root element" in {
    TagPath.fromTag(1).tail shouldBe EmptyTagPath
    TagPath.fromItem(1, 1).tail shouldBe EmptyTagPath
    TagPath.fromItem(1, 1).thenTag(2).tail shouldBe TagPath.fromTag(2)
    TagPath.fromItem(1, 1).thenItem(2, 1).thenTag(3).tail shouldBe TagPath.fromItem(2, 1).thenTag(3)
  }
}
