import sbt.IO
import DicomSourceGenerators._

name := "dicom-streams"
version := "0.1-SNAPSHOT"
organization := "se.nimsa"
scalaVersion := "2.12.6"
scalacOptions := Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature", "-target:jvm-1.8")
scalacOptions in (Compile, doc) ++= Seq(
  "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
)

// repos

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

// deps

libraryDependencies ++= {
  val akkaVersion = "2.5.13"
  Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.1.0",
    "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "org.slf4j" % "slf4j-simple" % "1.7.25",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % "test"
  )
}

updateOptions := updateOptions.value.withCachedResolution(true)

sourceGenerators in Compile += Def.task {
  val tagFile = (sourceManaged in Compile).value / "se" / "nimsa" / "dicom" / "data" / "Tag.scala"
  val keywordFile = (sourceManaged in Compile).value / "se" / "nimsa" / "dicom" / "data" / "Keyword.scala"
  val uidFile = (sourceManaged in Compile).value / "se" / "nimsa" / "dicom" / "data" / "UID.scala"
  val dictionaryFile = (sourceManaged in Compile).value / "se" / "nimsa" / "dicom" / "data" / "Dictionary.scala"
  IO.write(tagFile, generateTag())
  IO.write(keywordFile, generateKeyword())
  IO.write(uidFile, generateUID())
  IO.write(dictionaryFile, generateDictionary())
  Seq(tagFile, keywordFile, uidFile, dictionaryFile)
}.taskValue

// for automatic license stub generation

organizationName := "Lars Edenbrandt"
startYear := Some(2018)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

// coverage

coverageExcludedPackages := ".*\\.Tag.*;.*\\.UID.*;.*\\.Dictionary.*;.*\\.Keyword.*"

// publish
publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra :=
  <url>https://github.com/slicebox/dicom-streams</url>
  <developers>
    <developer>
      <id>KarlSjostrand</id>
      <name>Karl Sjöstrand</name>
      <url>https://github.com/KarlSjostrand</url>
    </developer>
  </developers>