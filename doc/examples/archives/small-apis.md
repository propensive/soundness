# One method archives a directory in any format

There is no `TarBuilder` and no `ZipOutputStream`. `archive` takes the format as a type, and
its result is the archive as a value, to stream, compress or write.

```scala fuel
compile
alternative Tar
  highlight archive[Tar]()  a `Tarfile`
  code
      import filesystemBackends.javaBaseFilesystem, filesystemOptions.preserveSymlinks
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import logging.silentLogging, strategies.throwUnsafely

      def bundle(folio: Path on Local): Tarfile = folio.archive[Tar]()
alternative ZIP
  highlight archive[Zip]()  a `Zipfile`
  code
      import filesystemBackends.javaBaseFilesystem, filesystemOptions.preserveSymlinks
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import logging.silentLogging, strategies.throwUnsafely

      def bundle(folio: Path on Local): Zipfile = folio.archive[Zip]()
alternative Ar
  highlight archive[Ar]()  an `Arfile`
  code
      import filesystemBackends.javaBaseFilesystem, filesystemOptions.preserveSymlinks
      import pathInterfaces.pathOnLocal, systems.javaBaseSystem
      import logging.silentLogging, strategies.throwUnsafely

      def bundle(folio: Path on Local): Arfile = folio.archive[Ar]()
##
```

The example is compiled, and is not run, since it reads a directory.
[Archiving a directory](../../soundness/modules/archives.md#archiving-a-directory) says what
each format can store.
