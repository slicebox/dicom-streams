package se.nimsa.dicom.data

import se.nimsa.dicom.BuildInfo

object Implementation {

  val uidPrefix: String = "1.2.826.0.1.3680043.9.7634"
  val name: String = BuildInfo.name
  val version: String = BuildInfo.version
  val versionName: String = s"${name}_$version"
  val classUid: String = s"$uidPrefix.1.${version.replaceAll("""[^0-9\.]""", "")}"
}
