# An archive is read while it is open

Opening a ZIP archive makes its entries available for a block, and closes the file beneath them
when the block ends. A function which would look up an entry later still needs the open archive,
so the compiler keeps it inside. The text of an entry, once read, is an ordinary value.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A lookup kept for after the archive has closed
  errors show
  highlight zipfile.entry..read[Text]  would run after the archive had closed
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely, charsets.utf8Charset, textSanitizers.skipSanitizer

      val folio = temporaryDirectory[Path on Local] / "folio.zip"

      def play() = folio.open[Zip]():
        () => zipfile.entry(t"hamlet.txt".as[Path on Zip]).read[Text]
frame
  label The entry, read inside the block
  highlight zipfile.entry
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely, charsets.utf8Charset, textSanitizers.skipSanitizer

      val folio = temporaryDirectory[Path on Local] / "folio.zip"

      def play(): Text = folio.open[Zip]():
        zipfile.entry(t"hamlet.txt".as[Path on Zip]).read[Text]
##
```

The example is compiled, and is not run. [ZIP](../../soundness/modules/archives.md#zip) describes
reading, writing and opening an archive.
