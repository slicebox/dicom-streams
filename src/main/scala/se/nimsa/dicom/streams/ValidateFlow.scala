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

import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape, Inlet, Outlet}
import akka.util.ByteString
import se.nimsa.dicom.data.DicomParsing._

/**
  * A flow which passes on the input bytes unchanged, fails for non-DICOM files, validates for DICOM files with supported
  * Media Storage SOP Class UID, Transfer Syntax UID combination passed as context.
  */
class ValidateFlow(drainIncoming: Boolean) extends GraphStage[FlowShape[ByteString, ByteString]] {
  val in: Inlet[ByteString] = Inlet[ByteString]("ValidateFlow.in")
  val out: Outlet[ByteString] = Outlet[ByteString]("ValidateFlow.out")
  override val shape: FlowShape[ByteString, ByteString] = FlowShape.of(in, out)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    var buffer: ByteString = ByteString.empty
    var isValidated: Option[Boolean] = None
    var failException: Option[Throwable] = None

    val zeroBytes = ByteString(0, 0, 0, 0, 0, 0, 0, 0)
    val maxBufferLength: Int = 140

    setHandlers(in, out, new InHandler with OutHandler {

      override def onPull(): Unit =
        pull(in)

      override def onPush(): Unit = {
        val chunk = grab(in)
        isValidated match {
          case None =>
            buffer = buffer ++ chunk

            if (buffer.length >= 8 && buffer.take(8) != zeroBytes && isHeader(buffer))
              setValidated()
            else if (buffer.length >= maxBufferLength)
              if (isPreamble(buffer))
                if (isHeader(buffer.drop(dicomPreambleLength)))
                  setValidated()
                else
                  setFailed(new DicomStreamException("Not a DICOM stream"), upstreamHasFinished = false)
              else if (isHeader(buffer))
                  setValidated()
              else
                setFailed(new DicomStreamException("Not a DICOM stream"), upstreamHasFinished = false)
            else
              pull(in)
          case Some(true) =>
            push(out, chunk)
          case Some(false) =>
            pull(in)
        }
      }

      override def onUpstreamFinish(): Unit = {
        isValidated match {
          case None =>
            if (buffer.length == dicomPreambleLength && isPreamble(buffer))
              setValidated()
            else if (buffer.length >= 8 && isHeader(buffer))
              setValidated()
            else
              setFailed(new DicomStreamException("Not a DICOM stream"), upstreamHasFinished = true)
            completeStage()
          case Some(true) =>
            completeStage()
          case Some(false) =>
            failStage(failException.get)
        }
      }

      def setValidated(): Unit = {
        isValidated = Some(true)
        push(out, buffer)
      }

      def setFailed(e: Throwable, upstreamHasFinished: Boolean): Unit = {
        isValidated = Some(false)
        failException = Some(e)
        if (!upstreamHasFinished && drainIncoming)
          pull(in)
        else
          failStage(failException.get)
      }

    })
  }
}
