# One method reads a file as any type

A whole file is read with `read`, which takes the type that is wanted. Text is decoded with
the character set in scope, bytes are taken as they are, and JSON is parsed, all by the one
method.

```scala fuel
compile
frame
  label As text
  highlight read[Text]  decoded with the character set in scope
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import charsets.utf8Charset, textSanitizers.strictSanitizer

      def script(path: Path on Local): Text = path.read[Text]
frame
  label As bytes
  highlight read[Data]  the bytes, as they are
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import charsets.utf8Charset, textSanitizers.strictSanitizer

      def script(path: Path on Local): Text = path.read[Text]
      def bytes(path: Path on Local): Data = path.read[Data]
frame
  label As JSON
  highlight read[Json]  parsed
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import charsets.utf8Charset, textSanitizers.strictSanitizer

      def script(path: Path on Local): Text = path.read[Text]
      def bytes(path: Path on Local): Data = path.read[Data]
      def cast(path: Path on Local): Json = path.read[Json]
##
```

The example is compiled, and is not run, since it has no file to read.
[Reading and writing](../../soundness/modules/filesystem.md#reading-and-writing) has the
handles for larger files, and [small APIs](../../philosophy/small-apis.md) the reasons for
one name.
