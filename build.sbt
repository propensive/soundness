lazy val core = (project in file("core"))
  .settings(
    name := "caesura",
    libraryDependencies += "com.propensive" %% "magnolia" % "0.10.0"
  )

lazy val tests = (project in file("tests"))
  .settings(
    moduleName := "caesura-tests",
    libraryDependencies += "com.propensive" %% "probation" % "1.0.3" from "file://lib/probation.jar" // FIXME
  )
  .dependsOn(core)
