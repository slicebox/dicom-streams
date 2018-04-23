/*
 * Copyright 2018 Lars Edenbrandt
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package se.nimsa.dicom.streams

import akka.NotUsed
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import se.nimsa.dicom.DicomParsing.isFileMetaInformation
import se.nimsa.dicom.DicomParts._
import se.nimsa.dicom.TagPath.{TagPathSequence, TagPathTag}
import se.nimsa.dicom.{Dictionary, TagPath, VR}

object ModifyFlow {

  /**
    * Class used to specify modifications to individual elements of a dataset
    *
    * @param tagPath      tag path
    * @param matches      function used to determine if another tag path is matching the tag path of this modification
    *                     (may mean e.g. equals, starts with or ends with)
    * @param modification a modification function
    * @param insert       if tag is absent in dataset it will be created and inserted when `true`
    */
  case class TagModification(tagPath: TagPathTag, matches: TagPath => Boolean, modification: ByteString => ByteString, insert: Boolean)

  object TagModification {

    /**
      * Modification that will modify dataset elements contained within its tag path. "Contained" means equals unless
      * item wildcards are used. E.g. if the modification tag path is (0008,9215)[*}.(0010,0010) the path
      * (0008,9215)[1].(0010,0010) will be contained in that path along with any other item indices. Useful for changing
      * specific elements in a dataset.
      */
    def contains(tagPath: TagPathTag, modification: ByteString => ByteString, insert: Boolean) =
      TagModification(tagPath, tagPath.hasSubPath, modification, insert)

    /**
      * Modification that will modify dataset elements where the corresponding tag path ends with the tag path of this
      * modification. E.g. both the dataset elements (0010,0010) and (0008,9215)[1].(0010,0010) will be modified if
      * this tag path is (0010,0010). Useful for changing all instances of a certain element.
      */
    def endsWith(tagPath: TagPathTag, modification: ByteString => ByteString, insert: Boolean) =
      TagModification(tagPath, _.endsWith(tagPath), modification, insert)
  }

  /**
    * Modification flow for inserting or overwriting the values of specified elements. When inserting a new element,
    * the corresponding modification function will be called with an empty `ByteString`. A modification is specified by
    * a tag path, a modification function from current value bytes to their replacement, and a flag specifying whether
    * the element should be inserted if not present. Elements are inserted only if they point to existing datasets.
    * That means that sequences are never inserted, only modified. Insertion works according to the `TagPathTag#contains`
    * method, meaning that if sequence wildcards are used in modifications they will apply to all items in a sequence.
    *
    * Note that modified DICOM data may not be valid. This function does not ensure values are padded to even length and
    * changing an element may lead to invalid group length elements such as MediaStorageSOPInstanceUID. There are
    * utility functions in `se.nimsa.ddcm4che.streams` for padding values and flows for adjusting group lengths.
    *
    * @param modifications Any number of `TagModification`s each specifying a tag path, a modification function, and
    *                      a Boolean indicating whether absent values will be inserted or skipped.
    * @return the modified flow of DICOM parts
    */
  def modifyFlow(modifications: TagModification*): Flow[DicomPart, DicomPart, NotUsed] =
    DicomFlowFactory.create(new DeferToPartFlow[DicomPart] with EndEvent[DicomPart] with TagPathTracking[DicomPart] {

      val sortedModifications: List[TagModification] = modifications.toList.sortWith((a, b) => a.tagPath < b.tagPath)

      var currentModification: Option[TagModification] = None // current modification
      var currentHeader: Option[DicomHeader] = None // header of current element being modified
      var latestTagPath: Option[TagPath] = None // last seen new tag path
      var value: ByteString = ByteString.empty // value of current element being modified
      var bigEndian = false // endianness of current element
      var explicitVR = true // VR representation of current element

      def updateSyntax(header: DicomHeader): Unit = {
        bigEndian = header.bigEndian
        explicitVR = header.explicitVR
      }

      def headerAndValueParts(tagPath: TagPath, modification: ByteString => ByteString): List[DicomPart] = {
        val valueBytes = modification(ByteString.empty)
        val vr = Dictionary.vrOf(tagPath.tag)
        if (vr == VR.UN) throw new IllegalArgumentException("Tag is not present in dictionary, cannot determine value representation")
        if (vr == VR.SQ) throw new IllegalArgumentException("Cannot insert sequences")
        val isFmi = isFileMetaInformation(tagPath.tag)
        val header = DicomHeader(tagPath.tag, vr, valueBytes.length, isFmi, bigEndian, explicitVR)
        val value = DicomValueChunk(bigEndian, valueBytes, last = true)
        header :: value :: Nil
      }

      def isBetween(tagToTest: TagPath, upperTag: TagPath, lowerTagMaybe: Option[TagPath]): Boolean =
        tagToTest < upperTag && lowerTagMaybe.forall(_ < tagToTest)

      def isInDataset(tagToTest: TagPath, sequenceMaybe: Option[TagPathSequence]): Boolean =
        sequenceMaybe.map(tagToTest.startsWithSubPath).getOrElse(tagToTest.isRoot)

      def findInsertParts: List[DicomPart] = sortedModifications
        .filter(_.insert)
        .filter(m => tagPath.exists(tp => isBetween(m.tagPath, tp, latestTagPath)))
        .filter(m => isInDataset(m.tagPath, tagPath.flatMap(_.previous)))
        .flatMap(m => headerAndValueParts(m.tagPath, m.modification))

      def findModifyPart(header: DicomHeader): List[DicomPart] = sortedModifications
        .find(m => tagPath.exists(m.matches))
        .map { tagModification =>
          if (header.length > 0) {
            currentHeader = Some(header)
            currentModification = Some(tagModification)
            value = ByteString.empty
            Nil
          } else {
            val newValue = tagModification.modification(ByteString.empty)
            val newHeader = header.withUpdatedLength(newValue.length)
            newHeader :: DicomValueChunk(bigEndian, newValue, last = true) :: Nil
          }
        }
        .getOrElse(header :: Nil)

      override def onPart(part: DicomPart): List[DicomPart] =
        part match {
          case header: DicomHeader =>
            updateSyntax(header)
            val insertParts = findInsertParts
            val modifyPart = findModifyPart(header)
            latestTagPath = tagPath
            insertParts ::: modifyPart

          case sequence: DicomSequence =>
            val insertParts = findInsertParts
            latestTagPath = tagPath
            insertParts ::: sequence :: Nil

          case chunk: DicomValueChunk =>
            if (currentModification.isDefined && currentHeader.isDefined) {
              value = value ++ chunk.bytes
              if (chunk.last) {
                val newValue = currentModification.get.modification(value)
                val newHeader = currentHeader.get.withUpdatedLength(newValue.length)
                currentModification = None
                currentHeader = None
                newHeader :: DicomValueChunk(bigEndian, newValue, last = true) :: Nil
              } else
                Nil
            } else
              chunk :: Nil

          case otherPart => otherPart :: Nil
        }

      override def onEnd(): List[DicomPart] =
        sortedModifications
          .filter(_.insert)
          .filter(_.tagPath.isRoot)
          .filter(m => latestTagPath.exists(_ < m.tagPath))
          .flatMap(m => headerAndValueParts(m.tagPath, m.modification))
    })

}
