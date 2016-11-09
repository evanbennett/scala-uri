name := "scala-uri"
organization := "com.netaporter"
version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.0"
crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0")
scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature")
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"

lazy val updatePublicSuffixes = taskKey[Unit]("Updates the public suffix Trie at com.netaporter.uri.internet.PublicSuffixes")
updatePublicSuffixes := UpdatePublicSuffixTrie.generate()

def coverageAllowed(scalaVersion: String) = !scalaVersion.startsWith("2.10")
coverageOutputXML := coverageAllowed(scalaVersion.value)
coverageOutputCobertura := coverageAllowed(scalaVersion.value)
coverageHighlighting := coverageAllowed(scalaVersion.value)

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "io.spray" %%  "spray-json" % "1.3.2",
  "org.parboiled" %% "parboiled" % "2.1.3",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

logBuffered in Test := false
//parallelExecution in Test := false

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}
pomExtra := (
  <url>https://github.com/net-a-porter/scala-uri</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:net-a-porter/scala-uri.git</url>
    <connection>scm:git@github.com:net-a-porter/scala-uri.git</connection>
  </scm>
  <developers>
    <developer>
      <id>theon</id>
      <name>Ian Forsey</name>
      <url>http://theon.github.io</url>
    </developer>
  </developers>
)
