# A file's handle stays inside the block which opened it

`open` lends a handle to a block and closes the file when the block ends. A function which still
held the handle afterwards would read from a closed descriptor, so the compiler does not let one
out. What the block read into memory is an ordinary value, and leaves freely.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A function which would read after the file had closed
  errors show
  highlight handle.stream.read[Text]  would read when the function is called
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely, charsets.utf8Charset, textSanitizers.strictSanitizer

      val script = temporaryDirectory[Path on Local] / "globe" / "prompt.txt"

      val later = script.open[File](): handle ?=>
        () => handle.stream.read[Text]
frame
  label The text, read while the file is open
  highlight handle.stream.read[Text]
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely, charsets.utf8Charset, textSanitizers.strictSanitizer

      val script = temporaryDirectory[Path on Local] / "globe" / "prompt.txt"

      def prompt(): Text = script.open[File](): handle ?=>
        handle.stream.read[Text]
##
```

The example is compiled, and is not run.
[Reading and writing](../../soundness/modules/filesystem.md#reading-and-writing) describes
handles and their grants, and [delimited scopes](../../philosophy/delimited-scopes.md) the shape
which every `open` has.
