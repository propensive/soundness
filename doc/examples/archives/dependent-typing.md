# Only a JAR's handle has a manifest

A JAR is a ZIP with a manifest. The format an archive is opened as is a type, and it decides
what the handle inside the block can do: opened as `Jar`, `zipfile` has the manifest's
attributes, and opened as `Zip`, it has no such member.

```scala fuel
compile
frame
  label Opened as a JAR
  highlight zipfile.manifest  the main attributes, parsed
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, logging.silentLogging, strategies.throwUnsafely

      def describe(library: Path on Local): Text =
        library.open[Jar]():
          zipfile.manifest.inspect
frame
  label Opened as a ZIP
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, logging.silentLogging, strategies.throwUnsafely

      def describe(library: Path on Local): Text =
        library.open[Zip]():
          zipfile.manifest.inspect
##
```

The example is compiled, and is not run, since it reads a file.
[ZIP](../../soundness/modules/archives.md#zip) describes the manifest.
