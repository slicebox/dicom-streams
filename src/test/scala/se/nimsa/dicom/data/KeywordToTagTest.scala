package se.nimsa.dicom.data

import org.scalatest.{FlatSpec, Matchers}


class KeywordToTagTest extends FlatSpec with Matchers {

  "A keyword" should "be correctly mapped to its corresponding tag" in {
    val realTag = Tag.PatientName
    val mappedTag = KeywordToTag.tagOf("PatientName")
    mappedTag shouldBe realTag
  }

  it should "map to -1 for unknown keywords" in {
    KeywordToTag.tagOf("oups") shouldBe -1
  }
}
