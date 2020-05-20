ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "com.propensive"
ThisBuild / organizationName := "Propensive OÜ"
ThisBuild / organizationHomepage := Some(url("https://propensive.com/"))
ThisBuild / version := "0.4.0"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/propensive/probably"),
    "scm:git@github.com:propensive/probably.git"
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

ThisBuild / description := "To probe what we can't prove so the unprovable may become probable"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/propensive/probably"))

ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

lazy val core = (project in file(".core"))
  .settings(
    name := "probably-core",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "core",
    libraryDependencies += "com.propensive" %% "gastronomy-core" % "0.3.0"
  )

lazy val property = (project in file(".property"))
  .settings(
    name := "probably-property",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "property",
    libraryDependencies += "com.propensive" %% "magnolia" % "0.16.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.10" % Provided
  )

lazy val cli = (project in file(".cli"))
  .dependsOn(core, property)
  .settings(
    name := "probably-cli",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "cli",
    libraryDependencies += "com.propensive" %% "escritoire-core" % "0.3.0"
  )
