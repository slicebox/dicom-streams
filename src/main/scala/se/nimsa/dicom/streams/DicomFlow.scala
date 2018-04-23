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
import akka.stream.scaladsl.{Flow, Source}
import akka.util.ByteString
import se.nimsa.dicom.TagPath
import se.nimsa.dicom.TagPath._
import se.nimsa.dicom.DicomParts._

/**
  * This class defines events for modular construction of DICOM flows. Events correspond to the DICOM parts commonly
  * encountered in a stream of DICOM data. Subclasses and/or traits add functionality by overriding and implementing
  * events. The stream entering the stage can be modified as well as the mapping from DICOM part to event.
  */
abstract class DicomFlow[Out] {

  def onPreamble(part: DicomPreamble): List[Out]
  def onHeader(part: DicomHeader): List[Out]
  def onValueChunk(part: DicomValueChunk): List[Out]
  def onSequenceStart(part: DicomSequence): List[Out]
  def onSequenceEnd(part: DicomSequenceDelimitation): List[Out]
  def onFragmentsStart(part: DicomFragments): List[Out]
  def onFragmentsEnd(part: DicomFragmentsDelimitation): List[Out]
  def onSequenceItemStart(part: DicomSequenceItem): List[Out]
  def onSequenceItemEnd(part: DicomSequenceItemDelimitation): List[Out]
  def onFragmentsItemStart(part: DicomFragmentsItem): List[Out]
  def onDeflatedChunk(part: DicomDeflatedChunk): List[Out]
  def onUnknownPart(part: DicomUnknownPart): List[Out]
  def onPart(part: DicomPart): List[Out]

  /**
    * @return the flow of `DicomPart`s entering the stage
    */
  def baseFlow: Flow[DicomPart, DicomPart, NotUsed] = Flow[DicomPart]

  /**
    * This method defines the mapping from `DicomPart` to event used to create the stage, see 'DicomFlowFactory.create'.
    *
    * @param dicomPart the incoming 'DicomPart'
    * @return the `List` of `DicomPart`s
    */
  def handlePart(dicomPart: DicomPart): List[Out] = dicomPart match {
    case part: DicomPreamble => onPreamble(part)
    case part: DicomHeader => onHeader(part)
    case part: DicomValueChunk => onValueChunk(part)
    case part: DicomSequence => onSequenceStart(part)
    case part: DicomSequenceDelimitation => onSequenceEnd(part)
    case part: DicomFragments => onFragmentsStart(part)
    case part: DicomFragmentsDelimitation => onFragmentsEnd(part)
    case part: DicomSequenceItem => onSequenceItemStart(part)
    case part: DicomSequenceItemDelimitation => onSequenceItemEnd(part)
    case part: DicomFragmentsItem => onFragmentsItemStart(part)
    case part: DicomDeflatedChunk => onDeflatedChunk(part)
    case part: DicomUnknownPart => onUnknownPart(part)
    case part => onPart(part)
  }
}

/**
  * Basic implementation of events where DICOM parts are simply passed on downstream. When implementing new flows, this
  * class can be overridden for the appropriate events.
  */
class IdentityFlow extends DicomFlow[DicomPart] {
  def onPreamble(part: DicomPreamble): List[DicomPart] = part :: Nil
  def onHeader(part: DicomHeader): List[DicomPart] = part :: Nil
  def onValueChunk(part: DicomValueChunk): List[DicomPart] = part :: Nil
  def onSequenceStart(part: DicomSequence): List[DicomPart] = part :: Nil
  def onSequenceEnd(part: DicomSequenceDelimitation): List[DicomPart] = part :: Nil
  def onFragmentsStart(part: DicomFragments): List[DicomPart] = part :: Nil
  def onFragmentsEnd(part: DicomFragmentsDelimitation): List[DicomPart] = part :: Nil
  def onDeflatedChunk(part: DicomDeflatedChunk): List[DicomPart] = part :: Nil
  def onUnknownPart(part: DicomUnknownPart): List[DicomPart] = part :: Nil
  def onPart(part: DicomPart): List[DicomPart] = part :: Nil
  def onSequenceItemStart(part: DicomSequenceItem): List[DicomPart] = part :: Nil
  def onSequenceItemEnd(part: DicomSequenceItemDelimitation): List[DicomPart] = part :: Nil
  def onFragmentsItemStart(part: DicomFragmentsItem): List[DicomPart] = part :: Nil
}

/**
  * Similar implementation to `IdentityFlow` with the difference that all events forward to the `onPart` event. Useful for
  * simple filters which implement a common behavior for all DICOM parts. This implementation is then provided in the
  * `onPart` method.
  */
abstract class DeferToPartFlow[Out] extends DicomFlow[Out] {
  def onPreamble(part: DicomPreamble): List[Out] = onPart(part)
  def onHeader(part: DicomHeader): List[Out] = onPart(part)
  def onValueChunk(part: DicomValueChunk): List[Out] = onPart(part)
  def onSequenceStart(part: DicomSequence): List[Out] = onPart(part)
  def onSequenceEnd(part: DicomSequenceDelimitation): List[Out] = onPart(part)
  def onFragmentsStart(part: DicomFragments): List[Out] = onPart(part)
  def onFragmentsEnd(part: DicomFragmentsDelimitation): List[Out] = onPart(part)
  def onDeflatedChunk(part: DicomDeflatedChunk): List[Out] = onPart(part)
  def onUnknownPart(part: DicomUnknownPart): List[Out] = onPart(part)
  def onSequenceItemStart(part: DicomSequenceItem): List[Out] = onPart(part)
  def onSequenceItemEnd(part: DicomSequenceItemDelimitation): List[Out] = onPart(part)
  def onFragmentsItemStart(part: DicomFragmentsItem): List[Out] = onPart(part)
  def onPart(part: DicomPart): List[Out]
}

case object DicomStartMarker extends DicomPart {
  def bigEndian: Boolean = false
  def bytes: ByteString = ByteString.empty
}

/**
  * This mixin adds an event marking the start of the DICOM stream. It does not add DICOM parts to the stream.
  */
trait StartEvent[Out] extends DicomFlow[Out] {
  def onStart(): List[Out] = Nil

  override def baseFlow: Flow[DicomPart, DicomPart, NotUsed] =
    super.baseFlow.prepend(Source.single(DicomStartMarker)) // add marker to start of stream

  override def handlePart(dicomPart: DicomPart): List[Out] = dicomPart match {
    case DicomStartMarker => onStart() // call event, do not emit marker
    case part => super.handlePart(part)
  }
}

case object DicomEndMarker extends DicomPart {
  def bigEndian: Boolean = false
  def bytes: ByteString = ByteString.empty
}

/**
  * This mixin adds an event marking the end of the DICOM stream. It does not add DICOM parts to the stream.
  */
trait EndEvent[Out] extends DicomFlow[Out] {
  def onEnd(): List[Out] = Nil

  override def baseFlow: Flow[DicomPart, DicomPart, NotUsed] =
    super.baseFlow.concat(Source.single(DicomEndMarker)) // add marker to end of stream

  override def handlePart(dicomPart: DicomPart): List[Out] = dicomPart match {
    case DicomEndMarker => onEnd() // call event, do not emit marker
    case part => super.handlePart(part)
  }
}

object DicomValueChunkMarker extends DicomValueChunk(bigEndian = false, ByteString.empty, last = true)

/**
  * This mixin makes sure the `onValueChunk` event is called also for empty elements. This special case requires
  * special handling since empty elements consist of a `DicomHeader`, but is not followed by a `DicomValueChunk`.
  */
trait GuaranteedValueEvent[Out] extends DicomFlow[Out] {

  abstract override def onHeader(part: DicomHeader): List[Out] =
    if (part.length == 0)
      super.onHeader(part) ::: onValueChunk(DicomValueChunkMarker)
    else
      super.onHeader(part)

  abstract override def onFragmentsItemStart(part: DicomFragmentsItem): List[Out] =
    if (part.length == 0)
      super.onFragmentsItemStart(part) ::: onValueChunk(DicomValueChunkMarker)
    else
      super.onFragmentsItemStart(part)

  abstract override def onValueChunk(part: DicomValueChunk): List[Out] =
    super.onValueChunk(part).filterNot(_ == DicomValueChunkMarker)
}

object DicomSequenceDelimitationMarker extends DicomSequenceDelimitation(bigEndian = false, ByteString.empty)

class DicomSequenceItemDelimitationMarker(item: Int) extends DicomSequenceItemDelimitation(item, bigEndian = false, ByteString.empty)

/**
  * By mixing in this trait, sequences and items with determinate length will be concluded by delimitation events, just
  * as is the case with sequences and items with indeterminate length and which are concluded by delimitation parts. This
  * makes it easier to create DICOM flows that react to the beginning and ending of sequences and items, as no special
  * care has to be taken for DICOM data with determinate length sequences and items.
  */
trait GuaranteedDelimitationEvents[Out] extends DicomFlow[Out] {
  var partStack: List[(LengthPart, Long)] = Nil

  def subtractLength(part: DicomPart): List[(LengthPart, Long)] =
    partStack.map {
      case (item: DicomSequenceItem, bytesLeft) => (item, bytesLeft - part.bytes.length)
      case (sequence, bytesLeft) => (sequence, bytesLeft - part.bytes.length)
    }

  def maybeDelimit(): List[Out] = {
    val delimits: List[DicomPart] = partStack
      .filter(_._2 <= 0) // find items and sequences that have ended
      .map { // create delimiters for those
      case (item: DicomSequenceItem, _) => new DicomSequenceItemDelimitationMarker(item.index)
      case (_, _) => DicomSequenceDelimitationMarker
    }
    partStack = partStack.filter(_._2 > 0) // only keep items and sequences with bytes left to subtract
    delimits.flatMap { // call events, any items will be inserted in stream
      case d: DicomSequenceDelimitation => onSequenceEnd(d)
      case d: DicomSequenceItemDelimitation => onSequenceItemEnd(d)
    }
  }

  def subtractAndEmit[A <: DicomPart](part: A, handle: A => List[Out]): List[Out] = {
    partStack = subtractLength(part)
    handle(part) ::: maybeDelimit()
  }

  abstract override def onSequenceStart(part: DicomSequence): List[Out] =
    if (part.hasLength) {
      partStack = (part, part.length) +: subtractLength(part)
      super.onSequenceStart(part) ::: maybeDelimit()
    } else subtractAndEmit(part, super.onSequenceStart)

  abstract override def onSequenceItemStart(part: DicomSequenceItem): List[Out] =
    if (part.hasLength) {
      partStack = (part, part.length) +: subtractLength(part)
      super.onSequenceItemStart(part) ::: maybeDelimit()
    } else subtractAndEmit(part, super.onSequenceItemStart)

  abstract override def onSequenceEnd(part: DicomSequenceDelimitation): List[Out] =
    subtractAndEmit(part, (p: DicomSequenceDelimitation) =>
      super.onSequenceEnd(p).filterNot(_ == DicomSequenceDelimitationMarker))

  abstract override def onSequenceItemEnd(part: DicomSequenceItemDelimitation): List[Out] =
    subtractAndEmit(part, (p: DicomSequenceItemDelimitation) =>
      super.onSequenceItemEnd(p).filterNot(_.isInstanceOf[DicomSequenceItemDelimitationMarker]))

  abstract override def onHeader(part: DicomHeader): List[Out] = subtractAndEmit(part, super.onHeader)
  abstract override def onValueChunk(part: DicomValueChunk): List[Out] = subtractAndEmit(part, super.onValueChunk)
  abstract override def onFragmentsStart(part: DicomFragments): List[Out] = subtractAndEmit(part, super.onFragmentsStart)
  abstract override def onFragmentsItemStart(part: DicomFragmentsItem): List[Out] = subtractAndEmit(part, super.onFragmentsItemStart)
  abstract override def onFragmentsEnd(part: DicomFragmentsDelimitation): List[Out] = subtractAndEmit(part, super.onFragmentsEnd)
}

/**
  * This mixin keeps track of the current tag path as the stream moves through attributes, sequences and fragments. The
  * tag path state is updated in the event callbacks. This means that implementations of events using this mixin must
  * remember to call the corresponding super method for the tag path to update.
  */
trait TagPathTracking[Out] extends DicomFlow[Out] with GuaranteedValueEvent[Out] with GuaranteedDelimitationEvents[Out] {

  protected var tagPath: Option[TagPath] = None
  protected var inFragments = false

  abstract override def onHeader(part: DicomHeader): List[Out] = {
    tagPath = tagPath.map {
      case t: TagPathSequenceItem => t.thenTag(part.tag)
      case t => t.previous.map(_.thenTag(part.tag)).getOrElse(TagPath.fromTag(part.tag))
    }.orElse(Some(TagPath.fromTag(part.tag)))
    super.onHeader(part)
  }

  abstract override def onFragmentsStart(part: DicomFragments): List[Out] = {
    inFragments = true
    tagPath = tagPath.map {
      case t: TagPathSequenceItem => t.thenTag(part.tag)
      case t => t.previous.map(_.thenTag(part.tag)).getOrElse(TagPath.fromTag(part.tag))
    }.orElse(Some(TagPath.fromTag(part.tag)))
    super.onFragmentsStart(part)
  }

  abstract override def onFragmentsEnd(part: DicomFragmentsDelimitation): List[Out] = {
    inFragments = false
    super.onFragmentsEnd(part)
  }

  abstract override def onSequenceStart(part: DicomSequence): List[Out] = {
    tagPath = tagPath.map {
      case t: TagPathSequenceItem => t.thenSequence(t.tag)
      case t => t.previous.map(_.thenSequence(part.tag)).getOrElse(TagPath.fromSequence(part.tag))
    }.orElse(Some(TagPath.fromSequence(part.tag)))
    super.onSequenceStart(part)
  }

  abstract override def onSequenceEnd(part: DicomSequenceDelimitation): List[Out] = {
    tagPath = tagPath.map(t => t.previous.map(_.thenSequence(t.tag)).getOrElse(TagPath.fromSequence(t.tag)))
    super.onSequenceEnd(part)
  }

  abstract override def onSequenceItemStart(part: DicomSequenceItem): List[Out] = {
    tagPath = tagPath.map(t => t.previous.map(_.thenSequence(t.tag, part.index)).getOrElse(TagPath.fromSequence(t.tag, part.index)))
    super.onSequenceItemStart(part)
  }

  abstract override def onSequenceItemEnd(part: DicomSequenceItemDelimitation): List[Out] = {
    tagPath = tagPath.flatMap(_.previous)
    super.onSequenceItemEnd(part)
  }

}

/**
  * Provides factory methods for creating `DicomFlow`s
  */
object DicomFlowFactory {

  def create[Out](flow: DicomFlow[Out]): Flow[DicomPart, Out, NotUsed] = flow.baseFlow.mapConcat(flow.handlePart)

}
