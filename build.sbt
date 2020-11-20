ThisBuild / scalaVersion := "2.12.12"
ThisBuild / organization := "com.propensive"
ThisBuild / organizationName := "Propensive OÜ"
ThisBuild / organizationHomepage := Some(url("https://propensive.com/"))
ThisBuild / version := "0.19.0"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/propensive/adversaria"),
    "scm:git@github.com:propensive/adversaria.git"
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

ThisBuild / description := "Providing typeclass interfaces to user-defined Scala annotations"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/propensive/adversaria"))

ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

lazy val core = (project in file(".sbt/core"))
  .settings(
    name := "adversaria-core",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    Compile / scalaSource := baseDirectory.value / ".." / ".." / "src" / "core",
  )

lazy val test = (project in file(".sbt/test"))
  .dependsOn(core)
  .settings(
    name := "adversaria-test",
    Compile / scalaSource := baseDirectory.value / ".." / ".." / "src" / "test",
    libraryDependencies += "com.propensive" %% "probably-cli" % "0.5.0",
    libraryDependencies += "com.propensive" %% "contextual-examples" % "2.0.0"
  )
