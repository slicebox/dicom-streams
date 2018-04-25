package se.nimsa.dicom

import akka.util.ByteString
import se.nimsa.dicom.TagPath.TagPathSequence
import se.nimsa.dicom.streams.ElementFolds.TpElement

/**
  * Representation of a group of `Element`s, each paired with the `TagPath` that describes their position within a
  * dataset. Representation is immutable so methods for inserting, updating and removing elements return a new instance.
  * Also specifies the character sets that should be used for decoding the values of textual elements.
  *
  * @param characterSets The character sets used for decoding text values
  * @param data          the `Map`ping of `TagPath` to `Element`
  */
case class Elements(characterSets: CharacterSets, data: Map[TagPath, Element]) {
  def apply(tagPath: TagPath): Option[Element] = data.get(tagPath)
  def apply(tag: Int): Option[Element] = apply(TagPath.fromTag(tag))
  def apply(tagPathCondition: TagPath => Boolean): Elements =
    Elements(characterSets, data.filterKeys(tp => tagPathCondition(tp)))
  def sequence(tagPath: TagPathSequence): Elements =
    apply(_.startsWithSuperPath(tagPath))
  def remove(tagPathCondition: TagPath => Boolean): Elements =
    Elements(characterSets, data.filterKeys(tp => !tagPathCondition(tp)))
  def update(tagPath: TagPath, element: Element): Elements =
    Elements(characterSets, data + (tagPath -> element))
  def updateCharacterSets(characterSets: CharacterSets): Elements =
    Elements(characterSets, data)
  def toList: List[TpElement] =
    data.map(e => (TpElement.apply _).tupled(e)).toList.sortWith(_.tagPath < _.tagPath)
  def elements: List[Element] = toList.map(_.element)
  def tagPaths: List[TagPath] = toList.map(_.tagPath)
  def bytes: ByteString = elements.map(_.bytes).reduce(_ ++ _)
}

object Elements {
  def empty: Elements = Elements(CharacterSets.defaultOnly, Map.empty)
}

