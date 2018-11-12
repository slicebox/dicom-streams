package se.nimsa.dicom.data

import org.scalatest.{FlatSpec, Matchers}
import se.nimsa.dicom.data.TagPath.EmptyTagPath
import se.nimsa.dicom.data.TagTree.EmptyTagTree

class TagTreeTest extends FlatSpec with Matchers {

  "A tag query" should "have a legible string representation" in {
    val path = TagTree.fromAnyItem(Tag.DerivationCodeSequence).thenItem(Tag.DerivationCodeSequence, 3).thenAnyItem(Tag.DerivationCodeSequence).thenTag(Tag.PatientID)
    path.toString(lookup = false) shouldBe "(0008,9215)[*].(0008,9215)[3].(0008,9215)[*].(0010,0020)"
  }

  it should "support string representations with keywords instead of tag numbers where possible" in {
    val path = TagTree.fromAnyItem(Tag.DerivationCodeSequence).thenItem(0x11110100, 3).thenAnyItem(Tag.DetectorInformationSequence).thenTag(Tag.PatientID)
    path.toString(lookup = true) shouldBe "DerivationCodeSequence[*].(1111,0100)[3].DetectorInformationSequence[*].PatientID"
  }

  it should "be root when pointing to root dataset" in {
    val path = TagTree.fromTag(Tag.PatientID)
    path.isRoot shouldBe true
  }

  it should "not be root when pointing to a tag in a sequence" in {
    val path = TagTree.fromAnyItem(Tag.DerivationCodeSequence).thenTag(Tag.PatientID)
    path.isRoot shouldBe false
  }

  "Two tag queries" should "be equal if they point to the same path" in {
    val aPath = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    val bPath = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    aPath shouldBe bPath
  }

  it should "not be equal if item indices do not match" in {
    val aPath = TagTree.fromAnyItem(1).thenItem(2, 1).thenAnyItem(3).thenTag(4)
    val bPath = TagTree.fromAnyItem(1).thenItem(2, 2).thenAnyItem(3).thenTag(4)
    aPath should not be bPath
  }

  it should "not be equal if they point to different tags" in {
    val aPath = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    val bPath = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(5)
    aPath should not be bPath
  }

  it should "not be equal if they have different depths" in {
    val aPath = TagTree.fromAnyItem(1).thenAnyItem(3).thenTag(4)
    val bPath = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    aPath should not be bPath
  }

  it should "not be equal if one points to all indices of a sequence and the other points to a specific index" in {
    val aPath = TagTree.fromAnyItem(1)
    val bPath = TagTree.fromItem(1, 1)
    aPath should not be bPath
  }

  it should "be equal if both are empty" in {
    EmptyTagTree == EmptyTagTree shouldBe true
  }

  "The contains test" should "return true for equal paths" in {
    val aPath = TagTree.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    aPath.hasTrunk(bPath) shouldBe true
  }

  it should "return true for two empty paths" in {
    EmptyTagTree.hasTrunk(EmptyTagPath) shouldBe true
  }

  it should "return true when any path starts with empty path" in {
    val aPath = TagTree.fromTag(1)
    aPath.hasTrunk(EmptyTagPath) shouldBe true
  }

  it should "return false when empty path starts with non-empty path" in {
    val aPath = TagPath.fromTag(1)
    EmptyTagTree.hasTrunk(aPath) shouldBe false
  }

  it should "return false when subject path is longer than path" in {
    val aPath = TagTree.fromItem(1, 1).thenItem(2, 2).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    aPath.hasTrunk(bPath) shouldBe false
  }

  it should "return true when paths involving item indices are equal" in {
    val aPath = TagTree.fromItem(1, 4).thenItem(2, 2).thenItem(3, 2).thenTag(4)
    val bPath = TagPath.fromItem(1, 4).thenItem(2, 2).thenItem(3, 2).thenTag(4)
    aPath.hasTrunk(bPath) shouldBe true
  }

  it should "return false when a path with wildcards starts with a path with item indices" in {
    val aPath = TagTree.fromAnyItem(2).thenAnyItem(3).thenTag(4)
    val bPath = TagPath.fromItem(2, 4).thenItem(3, 66).thenTag(4)
    aPath.hasTrunk(bPath) shouldBe false
  }

  it should "return true when subject path is subset of path" in {
    val aPath = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    val bPath = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 1)
    aPath.hasTrunk(bPath) shouldBe true
  }

  "The endsWith test" should "return true when a longer tag ends with a shorter" in {
    val aPath = TagTree.fromItem(1, 3).thenTag(2)
    val bPath = TagPath.fromTag(2)
    aPath.hasTwig(bPath) shouldBe true
  }

  it should "return true for two empty paths" in {
    EmptyTagTree.hasTwig(EmptyTagPath) shouldBe true
  }

  it should "return false when checking if non-empty path ends with empty path" in {
    val aPath = TagTree.fromTag(1)
    aPath.hasTwig(EmptyTagPath) shouldBe false
  }

  it should "return false when empty path starts with non-empty path" in {
    val aPath = TagPath.fromTag(1)
    EmptyTagTree.hasTwig(aPath) shouldBe false
  }

  it should "return false when a shorter tag is compared to a longer" in {
    val aPath = TagPath.fromItem(1, 3).thenTag(2)
    val bPath = TagTree.fromTag(2)
    bPath.hasTwig(aPath) shouldBe false
  }

  it should "return false when tag numbers do not match" in {
    val aPath = TagTree.fromItem(1, 3).thenTag(2)
    val bPath = TagPath.fromTag(4)
    aPath.hasTwig(bPath) shouldBe false
  }

  it should "work also with deep sequences" in {
    val aPath = TagTree.fromItem(1, 3).thenItem(2, 4).thenItem(3, 5).thenTag(6)
    val bPath = TagPath.fromItem(2, 4).thenItem(3, 5).thenTag(6)
    aPath.hasTwig(bPath) shouldBe true
  }

  "Parsing a tag query" should "work for well-formed depth 0 tag queries" in {
    TagTree.parse("(0010,0010)") shouldBe TagTree.fromTag(Tag.PatientName)
  }

  it should "work for deep tag paths" in {
    TagTree.parse("(0008,9215)[*].(0008,9215)[666].(0010,0010)") shouldBe TagTree.fromAnyItem(Tag.DerivationCodeSequence).thenItem(Tag.DerivationCodeSequence, 666).thenTag(Tag.PatientName)
  }

  it should "throw an exception for malformed strings" in {
    intercept[IllegalArgumentException] {
      TagTree.parse("abc")
    }
  }

  it should "throw an exception for empty strings" in {
    intercept[IllegalArgumentException] {
      TagTree.parse("")
    }
  }

  it should "accept both tag numbers and keywords" in {
    val ref = TagTree.fromItem(Tag.DerivationCodeSequence, 1).thenTag(Tag.PatientName)
    TagTree.parse("(0008,9215)[1].(0010,0010)") shouldBe ref
    TagTree.parse("DerivationCodeSequence[1].(0010,0010)") shouldBe ref
    TagTree.parse("(0008,9215)[1].PatientName") shouldBe ref
    TagTree.parse("DerivationCodeSequence[1].PatientName") shouldBe ref
  }

  "The drop operation" should "remove elements from the left" in {
    val path = TagTree.fromAnyItem(1).thenItem(2, 1).thenAnyItem(3).thenTag(4)
    path.drop(-100) shouldBe path
    path.drop(0) shouldBe path
    path.drop(1) shouldBe TagTree.fromItem(2, 1).thenAnyItem(3).thenTag(4)
    path.drop(2) shouldBe TagTree.fromAnyItem(3).thenTag(4)
    path.drop(3) shouldBe TagTree.fromTag(4)
    path.drop(4) shouldBe EmptyTagTree
    path.drop(100) shouldBe EmptyTagTree
  }
}
