ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "com.propensive"
ThisBuild / organizationName := "Propensive OÜ"
ThisBuild / organizationHomepage := Some(url("https://propensive.com/"))
ThisBuild / version := "0.3.0"
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/propensive/escritoire"),
    "scm:git@github.com:propensive/escritoire.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "propensive",
    name  = "Jon Pretty",
    email = "jon.pretty@propensive.com",
    url   = url("https://twitter.com/propensive")
  )
)

ThisBuild / description := "A small library for writing tables"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/propensive/escritoire"))

ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

lazy val core = (project in file("."))
  .settings(
    name := "escritoire-core",
    Compile / scalaSource := baseDirectory.value / "src" / "core"
  )
