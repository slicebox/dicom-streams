package se.nimsa.dicom.data

import org.scalatest.{FlatSpec, Matchers}
import se.nimsa.dicom.data.TagPath.EmptyTagPath
import se.nimsa.dicom.data.TagTree.EmptyTagTree

class TagTreeTest extends FlatSpec with Matchers {

  "A tag tree" should "have a legible string representation" in {
    val tree = TagTree.fromAnyItem(Tag.DerivationCodeSequence).thenItem(Tag.DerivationCodeSequence, 3).thenAnyItem(Tag.DerivationCodeSequence).thenTag(Tag.PatientID)
    tree.toString(lookup = false) shouldBe "(0008,9215)[*].(0008,9215)[3].(0008,9215)[*].(0010,0020)"
  }

  it should "support string representations with keywords instead of tag numbers where possible" in {
    val tree = TagTree.fromAnyItem(Tag.DerivationCodeSequence).thenItem(0x11110100, 3).thenAnyItem(Tag.DetectorInformationSequence).thenTag(Tag.PatientID)
    tree.toString(lookup = true) shouldBe "DerivationCodeSequence[*].(1111,0100)[3].DetectorInformationSequence[*].PatientID"
  }

  it should "be root when pointing to root dataset" in {
    val tree = TagTree.fromTag(Tag.PatientID)
    tree.isRoot shouldBe true
  }

  it should "not be root when pointing to a tag in a sequence" in {
    val tree = TagTree.fromAnyItem(Tag.DerivationCodeSequence).thenTag(Tag.PatientID)
    tree.isRoot shouldBe false
  }

  "Two tag trees" should "be equal if they point to the same tree" in {
    val aTree = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    val bTree = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    aTree shouldBe bTree
  }

  it should "not be equal if item indices do not match" in {
    val aTree = TagTree.fromAnyItem(1).thenItem(2, 1).thenAnyItem(3).thenTag(4)
    val bTree = TagTree.fromAnyItem(1).thenItem(2, 2).thenAnyItem(3).thenTag(4)
    aTree should not be bTree
  }

  it should "not be equal if they point to different tags" in {
    val aTree = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    val bTree = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(5)
    aTree should not be bTree
  }

  it should "not be equal if they have different depths" in {
    val aTree = TagTree.fromAnyItem(1).thenAnyItem(3).thenTag(4)
    val bTree = TagTree.fromAnyItem(1).thenAnyItem(2).thenAnyItem(3).thenTag(4)
    aTree should not be bTree
  }

  it should "not be equal if one points to all indices of a sequence and the other points to a specific index" in {
    val aTree = TagTree.fromAnyItem(1)
    val bTree = TagTree.fromItem(1, 1)
    aTree should not be bTree
  }

  it should "be equal if both are empty" in {
    EmptyTagTree == EmptyTagTree shouldBe true
  }

  "The hasTrunk test" should "return true for equally shaped tree and path" in {
    val tree = TagTree.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    val path = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    tree.hasTrunk(path) shouldBe true
  }

  it should "return true for two empty structures" in {
    EmptyTagTree.hasTrunk(EmptyTagPath) shouldBe true
  }

  it should "return true when subject path is empty" in {
    val aTree = TagTree.fromTag(1)
    aTree.hasTrunk(EmptyTagPath) shouldBe true
  }

  it should "return false when empty tree starts with non-empty path" in {
    val path = TagPath.fromTag(1)
    EmptyTagTree.hasTrunk(path) shouldBe false
  }

  it should "return false when subject path is longer than tree" in {
    val tree = TagTree.fromItem(1, 1).thenItem(2, 2).thenTag(4)
    val path = TagPath.fromItem(1, 1).thenItem(2, 2).thenItem(3, 3).thenTag(4)
    tree.hasTrunk(path) shouldBe false
  }

  it should "return true when a tree with wildcards is compared to a path with item indices" in {
    val tree = TagTree.fromAnyItem(2).thenAnyItem(3).thenTag(4)
    val path = TagPath.fromItem(2, 4).thenItem(3, 66).thenTag(4)
    tree.hasTrunk(path) shouldBe true
  }

  "The hasTwig test" should "return true when a longer tree ends with a shorter path" in {
    val tree = TagTree.fromItem(1, 3).thenTag(2)
    val path = TagPath.fromTag(2)
    tree.hasTwig(path) shouldBe true
  }

  it should "return true for empty tree and path" in {
    EmptyTagTree.hasTwig(EmptyTagPath) shouldBe true
  }

  it should "return false when checking if non-empty tree ends with empty path" in {
    val aTree = TagTree.fromTag(1)
    aTree.hasTwig(EmptyTagPath) shouldBe false
  }

  it should "return false when empty tree starts with non-empty path" in {
    val path = TagPath.fromTag(1)
    EmptyTagTree.hasTwig(path) shouldBe false
  }

  it should "return false when a shorter tree is compared to a longer path" in {
    val path = TagPath.fromItem(1, 3).thenTag(2)
    val tree = TagTree.fromTag(2)
    tree.hasTwig(path) shouldBe false
  }

  it should "return false when tag numbers do not match" in {
    val tree = TagTree.fromItem(1, 3).thenTag(2)
    val path = TagPath.fromTag(4)
    tree.hasTwig(path) shouldBe false
  }

  it should "work also with deep structures" in {
    val tree = TagTree.fromItem(1, 3).thenItem(2, 4).thenItem(3, 5).thenTag(6)
    val path = TagPath.fromItem(2, 4).thenItem(3, 5).thenTag(6)
    tree.hasTwig(path) shouldBe true
  }

  "Parsing a tag tree" should "work for well-formed depth 0 tag trees" in {
    TagTree.parse("(0010,0010)") shouldBe TagTree.fromTag(Tag.PatientName)
  }

  it should "work for deep tag trees" in {
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
    val tree = TagTree.fromAnyItem(1).thenItem(2, 1).thenAnyItem(3).thenTag(4)
    tree.drop(-100) shouldBe tree
    tree.drop(0) shouldBe tree
    tree.drop(1) shouldBe TagTree.fromItem(2, 1).thenAnyItem(3).thenTag(4)
    tree.drop(2) shouldBe TagTree.fromAnyItem(3).thenTag(4)
    tree.drop(3) shouldBe TagTree.fromTag(4)
    tree.drop(4) shouldBe EmptyTagTree
    tree.drop(100) shouldBe EmptyTagTree
  }
}
