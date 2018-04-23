package se.nimsa.dicom

import se.nimsa.dicom.TagPath.TagPathSequence

case class Elements(characterSets: CharacterSets, elements: List[Element]) {
  def apply(tagPath: TagPath): Option[Element] = elements.find(_.tagPath == tagPath)
  def apply(tag: Int): Option[Element] = apply(TagPath.fromTag(tag))
  def sequence(tagPath: TagPathSequence): Elements =
    Elements(characterSets, elements.filter(_.tagPath.startsWithSuperPath(tagPath)))
  def remove(tagPathCondition: TagPath => Boolean): Elements = Elements(characterSets, elements.filterNot(e => tagPathCondition(e.tagPath)))
  def insert(element: Element): Elements = {
    val replace = elements.count(_.tagPath == element.tagPath)
    val from = elements.zipWithIndex.find(element.tagPath < _._1.tagPath).map(_._2).getOrElse(elements.length)
    val updated = elements.patch(from - replace, List(element), replace)
    Elements(characterSets, updated)
  }
}

object Elements {
  def empty: Elements = Elements(CharacterSets.defaultOnly, Nil)
}

