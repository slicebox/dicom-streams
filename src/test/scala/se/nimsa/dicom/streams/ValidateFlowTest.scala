package se.nimsa.dicom.streams

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import akka.util.ByteString
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}

class ValidateFlowTest extends TestKit(ActorSystem("ValidateFlowSpec")) with FlatSpecLike with Matchers with BeforeAndAfterAll {

  import se.nimsa.dicom.data.TestData._
  import se.nimsa.dicom.streams.DicomFlows._

  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  override def afterAll(): Unit = system.terminate()

  "The DICOM validation flow" should "accept a file consisting of a preamble only" in {
    val bytes = preamble

    val source = Source.single(bytes)
      .via(new Chunker(1000))
      .via(validateFlow())

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectNext(preamble)
      .expectComplete()
  }

  it should "accept a file consisting of a preamble only when it arrives in very small chunks" in {
    val bytes = preamble

    val source = Source.single(bytes)
      .via(new Chunker(1))
      .via(validateFlow())

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectNext(preamble)
      .expectComplete()
  }

  it should "accept a file with no preamble and which starts with an element header" in {
    val bytes = patientNameJohnDoe()

    val source = Source.single(bytes)
      .via(validateFlow())

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectNext(patientNameJohnDoe())
      .expectComplete()
  }

  it should "not accept a file with a preamble followed by a corrupt element header" in {
    val bytes = preamble ++ ByteString(1, 2, 3, 4, 5, 6, 7, 8, 9)

    val source = Source.single(bytes)
      .via(validateFlow())

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectError()
  }


  it should "accept any file that starts like a DICOM file" in {
    val bytes = preamble ++ fmiGroupLength(transferSyntaxUID()) ++ ByteString.fromArray(new Array[Byte](1024))

    val source = Source.single(bytes)
      .via(new Chunker(10))
      .via(validateFlow())

    source.runWith(TestSink.probe[ByteString])
      .request(1)
      .expectNext(bytes.take(140))
      .request(1)
      .expectNext(ByteString(28, 0, 0, 0, 0, 0, 0, 0, 0, 0))  // group length value (28, 0) + 8 zeros
      .request(1)
      .expectNext(ByteString(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  it should "stop requesting data once validation has failed if asked to not drain incoming data" in {
    var nItems = 0

    val bytesSource = Source.fromIterator(() => (1 to 10000)
      .map(_.toByte).iterator)
      .grouped(1000)
      .map(bytes => ByteString(bytes.toArray))
      .map(bs => {
        nItems += 1
        bs
      })

    val f = bytesSource
      .via(validateFlow())
      .runWith(Sink.ignore)

    Await.ready(f, 5.seconds)

    nItems shouldBe 1
  }

  it should "keep requesting data until finished after validation failed, but not emit more data when asked to drain incoming data" in {
    var nItemsRequested = 0
    var nItemsEmitted = 0

    val bytesSource = Source.fromIterator(() => (1 to 10000)
      .map(_.toByte).iterator)
      .grouped(1000)
      .map(bytes => ByteString(bytes.toArray))
      .map(bs => {
        nItemsRequested += 1
        bs
      })

    val f = bytesSource
      .via(validateFlow(drainIncoming = true))
      .map(bs => {
        nItemsEmitted += 1
        bs
      })
      .runWith(Sink.ignore)

    Await.ready(f, 5.seconds)

    nItemsRequested shouldBe 10
    nItemsEmitted shouldBe 0
  }

}
