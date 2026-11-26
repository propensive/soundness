# A read which may fail says so

A file may be missing, or may not be readable. `read` raises an `Io.Error`, and a method
which reads a file declares the error, or handles it, before it compiles.

```scala fuel
compile
frame
  label A method which does not say that it can fail
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import charsets.utf8Charset, textSanitizers.skipSanitizer

      def script(path: Path on Local): Text = path.read[Text]
frame
  label The failure, in the return type
  highlight raises..Io.Error  the way it can fail
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import charsets.utf8Charset, textSanitizers.skipSanitizer

      def script(path: Path on Local): Text raises Io.Error = path.read[Text]
frame
  label Or an empty script, where none can be read
  highlight safely  a failure becomes an absent value
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem
      import charsets.utf8Charset, textSanitizers.skipSanitizer

      def script(path: Path on Local): Text = safely(path.read[Text]).or(t"")
##
```

The example is compiled, and is not run, since it has no file to read.
[On the filesystem](../../soundness/modules/filesystem.md#on-the-filesystem) says what the
error holds, and [error handling](../../philosophy/error-handling.md) why it is in the type.
