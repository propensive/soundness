ThisBuild / scalaVersion := "2.12.12"
ThisBuild / organization := "com.propensive"
ThisBuild / organizationName := "Propensive OÜ"
ThisBuild / organizationHomepage := Some(url("https://propensive.com/"))
ThisBuild / version := "0.8.0"

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
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "core"
  )

lazy val property = (project in file(".property"))
  .settings(
    name := "probably-property",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "property",
    libraryDependencies += "com.propensive" %% "magnolia" % "0.17.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.12" % Provided
  )

lazy val cli = (project in file(".cli"))
  .dependsOn(core)
  .settings(
    name := "probably-cli",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "cli",
    libraryDependencies += "com.propensive" %% "escritoire-core" % "0.3.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.12" % Provided,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.12" % Provided
  )

lazy val test = (project in file(".test"))
  .dependsOn(cli, property)
  .settings(
    name := "probably-test",
    Compile / scalaSource := baseDirectory.value / ".." / "src" / "test"
  )
