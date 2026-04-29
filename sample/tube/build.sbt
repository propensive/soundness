import sbtassembly.AssemblyPlugin.autoImport._

val scala3Version = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Terminal Tube",
    version := "0.1.0-SNAPSHOT",
    Compile / scalaSource := baseDirectory.value / "src" / "core",
    Compile / resourceDirectory := baseDirectory.value / "res",
    assembly / assemblyJarName := "tube.jar",
    assembly / mainClass := Some("tube.terminal.app"),
    assembly / assemblyOption ~= { _.withIncludeScala(true).withIncludeDependency(true) },
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },
    libraryDependencies += "dev.soundness" % "ethereal-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "escritoire-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "exoskeleton-completions" % "0.20.0",
    libraryDependencies += "dev.soundness" % "telekinesis-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "caesura-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "jacinta-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "abacist-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "nomenclature-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "zeppelin-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "camouflage-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "geodesy-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "mosquito-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "aviation-core" % "0.20.0",
    libraryDependencies += "dev.soundness" % "hallucination-core" % "0.20.0",
    libraryDependencies += "org.scala-lang" %% "scala3-library" % "3.5.1",
    libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.14",
    scalaVersion := scala3Version,
    scalacOptions += "-experimental",
    scalacOptions += "-Xprint-suspension",
    scalacOptions += "-language:experimental.modularity",
    scalacOptions += "-language:experimental.clauseInterleaving",
    scalacOptions += "-language:experimental.genericNumberLiterals",
    scalacOptions += "-language:experimental.saferExceptions",
    scalacOptions += "-language:experimental.erasedDefinitions",
    scalacOptions += "-language:experimental.namedTypeArguments",
    scalacOptions += "-language:experimental.namedTuples",
    scalacOptions ++= List("-Xmax-inlines", "100")
  )
