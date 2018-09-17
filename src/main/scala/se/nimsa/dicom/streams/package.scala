package se.nimsa.dicom

import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import se.nimsa.dicom.data.Elements

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}

package object streams {

  import ElementFlows.elementFlow
  import ElementSink.elementSink
  import ParseFlow.parseFlow

  def toElements(source: Source[ByteString, Any])(implicit ec: ExecutionContext, mat: ActorMaterializer): Future[Elements] =
    source
      .via(parseFlow)
      .via(elementFlow)
      .runWith(elementSink)

  def toElementsBlocking(source: Source[ByteString, Any], d: FiniteDuration = 10.seconds)(implicit ec: ExecutionContext, mat: ActorMaterializer): Elements =
    Await.result(toElements(source), d)

}
