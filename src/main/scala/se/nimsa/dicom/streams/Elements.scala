package se.nimsa.dicom.streams

import se.nimsa.dicom.TagPath.TagPathSequence
import se.nimsa.dicom._

case class Elements(characterSets: CharacterSets, elements: List[Element]) {
  def apply(tagPath: TagPath): Option[Element] = elements.find(_.tagPath == tagPath)
  def apply(tag: Int): Option[Element] = apply(TagPath.fromTag(tag))
  def sequence(tagPath: TagPathSequence): Elements =
    Elements(characterSets, elements.filter(_.tagPath.startsWithSuperPath(tagPath)))
  def insert(element: Element): Elements =
    Elements(characterSets, (element :: elements).sortWith((a, b) => a.tagPath < b.tagPath))
  def insert(sequence: List[Element]): Elements =
    Elements(characterSets, (sequence ::: elements).sortWith((a, b) => a.tagPath < b.tagPath))
}

object Elements {

}

