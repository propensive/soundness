# An archive which is open for one block

Opening an archive makes its entries available for the length of a block, and closes the file
beneath it when the block ends. Inside, `zipfile` is the open archive. Outside, there is none.

```scala fuel
compile
frame
  label The handle, where nothing is open
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, logging.silentLogging, strategies.throwUnsafely

      def titles(folio: Path on Local): List[Text] =
        zipfile.entries.map(_.ref.encode)
frame
  label The handle, inside its scope
  focus folio.open[Zip]():
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, logging.silentLogging, strategies.throwUnsafely

      def titles(folio: Path on Local): List[Text] =
        folio.open[Zip]():
          zipfile.entries.map(_.ref.encode)
##
```

The example is compiled, and is not run, since it reads a file.
[ZIP](../../soundness/modules/archives.md#zip) opens a JAR in the same way, and
[delimited scopes](../../philosophy/delimited-scopes.md) describes the shape.
