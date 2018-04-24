package se.nimsa.dicom

import se.nimsa.dicom.TagPath.TagPathSequence

case class Elements(characterSets: CharacterSets, elements: Map[TagPath, Element]) {
  def apply(tagPath: TagPath): Option[Element] = elements.get(tagPath)
  def apply(tag: Int): Option[Element] = apply(TagPath.fromTag(tag))
  def apply(tagPathCondition: TagPath => Boolean): Elements =
    Elements(characterSets, elements.filterKeys(tp => tagPathCondition(tp)))
  def sequence(tagPath: TagPathSequence): Elements =
    apply(_.startsWithSuperPath(tagPath))
  def remove(tagPathCondition: TagPath => Boolean): Elements =
    Elements(characterSets, elements.filterKeys(tp => !tagPathCondition(tp)))
  def update(tagPath: TagPath, element: Element): Elements =
    Elements(characterSets, elements + (tagPath -> element))
  def updateCharacterSets(characterSets: CharacterSets): Elements =
    Elements(characterSets, elements)
}

object Elements {
  def empty: Elements = Elements(CharacterSets.defaultOnly, Map.empty)
}

