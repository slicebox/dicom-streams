package se.nimsa.dicom.data

import se.nimsa.dicom.data.VR.VR

object Dictionary {

  def vrOf(tag: Int): VR = TagToVR.vrOf(tag)

  def vmOf(tag: Int): Multiplicity = TagToVM.vmOf(tag)

  def keywordOf(tag: Int): String = TagToKeyword.keywordOf(tag)

  def tagOf(keyword: String): Int = KeywordToTag.tagOf(keyword)

}
