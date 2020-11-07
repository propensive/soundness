ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "com.propensive"
ThisBuild / organizationName := "Propensive OÜ"
ThisBuild / organizationHomepage := Some(url("https://propensive.com/"))
ThisBuild / version := "0.4.0"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/propensive/gastronomy"),
    "scm:git@github.com:propensive/gastronomy.git"
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

ThisBuild / description := "Fast, easy and transparent typeclass derivation for Scala 2"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/propensive/gastronomy"))

ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

lazy val core = (project in file(".core"))
  .settings(
    name := "gastronomy-core",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "core",
    libraryDependencies += "com.propensive" %% "magnolia" % "0.17.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val test = (project in file(".test"))
  .dependsOn(core)
  .settings(
    name := "gastronomy-test",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "test",
    libraryDependencies += "com.propensive" %% "probably-cli" % "0.5.0"
  )
