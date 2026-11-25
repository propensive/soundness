# A compressor on one side of the disk, a decompressor on the other

A compressed file is written by pumping a stream through a compressing stage into the file's
intake, and read by placing the decompressing stage after the file. Neither the plain bytes nor
the compressed ones are ever held whole: each block is transformed and passed on.

```scala fuel
compile
frame
  label Writing: the stream is compressed on its way into the file
  highlight compress[Gzip].pump
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely, codepages.utf8Codepage, charsets.utf8Charset
      import textSanitizers.skipSanitizer

      val folio = temporaryDirectory[Path on Local] / "globe" / "folio.txt.gz"

      def store(play: Data): Unit = folio.create[File](CreateFlag.Parents): handle ?=>
        play.stream.compress[Gzip].pump(handle.intake())
frame
  label Reading: the file is decompressed on its way out
  highlight decompress[Gzip].read[Text]
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely, codepages.utf8Codepage, charsets.utf8Charset
      import textSanitizers.skipSanitizer

      val folio = temporaryDirectory[Path on Local] / "globe" / "folio.txt.gz"

      def store(play: Data): Unit = folio.create[File](CreateFlag.Parents): handle ?=>
        play.stream.compress[Gzip].pump(handle.intake())

      def restore(): Text = folio.open[File]()(file.stream.decompress[Gzip].read[Text])
##
```

The example is compiled, and is not run.
[Compressing a stream](../../soundness/modules/compression.md#compressing-a-stream) describes
how the whole-value and streaming forms interoperate.
