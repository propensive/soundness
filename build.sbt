ThisBuild / version := "0.15.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"

ThisBuild / scalacOptions ++= Seq(
  "-experimental",
  "-new-syntax",
  "-feature",
  "-deprecation",
  "-Wunused:imports",
  "-Wimplausible-patterns",
  "-Wsafe-init",
  "-Yrequire-targetName",
  "-Ycc-new",
  "-Yexplicit-nulls",
  "-Ycheck-all-patmat",
  "-language:experimental.clauseInterleaving",
  "-language:experimental.modularity",
  "-language:experimental.genericNumberLiterals",
  "-language:experimental.fewerBraces",
  "-language:experimental.into",
  "-language:experimental.erasedDefinitions",
  "-language:experimental.saferExceptions",
  "-language:experimental.namedTypeArguments",
  "-language:implicitConversions"
)

lazy val core = (project in file("src/core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
        "dev.soundness" % "contingency-core" % "0.15.0",
    ),
    Compile / baseDirectory := baseDirectory.value,
  )

lazy val test = (project in file("src/test"))
  .settings(
    name := "test"
  ).dependsOn(core)
