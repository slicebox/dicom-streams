package se.nimsa.dicom.data

import akka.util.ByteString
import org.scalatest.{FlatSpec, Matchers}

class dataTest extends FlatSpec with Matchers {

  "Converting an int to a hex string" should "convert 1194684 to 123ABC" in {
    intToHexString(1194684) shouldBe "00123ABC"
  }

  "Creating a UID" should "create a random UID" in {
    val uid = createUID()
    uid.startsWith("2.25") shouldBe true
    uid.matches("""([0-9]+\.)+[0-9]+""") shouldBe true
    uid should not be createUID()
  }

  it should "create a random UID with specified root" in {
    val uid = createUID("6.66.666")
    uid.startsWith("6.66.666") shouldBe true
  }

  it should "create a name based UID" in {
    val uid1 = createNameBasedUID(ByteString("name"))
    val uid2 = createNameBasedUID(ByteString("name"))
    uid1.startsWith("2.25") shouldBe true
    uid1 shouldBe uid2
  }

  it should "create a name based UID with specified root" in {
    val uid1 = createNameBasedUID(ByteString("name"), "6.66.666")
    val uid2 = createNameBasedUID(ByteString("name"), "6.66.666")
    uid1.startsWith("6.66.666") shouldBe true
    uid1 shouldBe uid2
  }
}
