# A file, read as a value

A path, a JSON document and a case class come from three libraries. A file is read as JSON by
the `read` which reads it as text, and the JSON is decoded by the `as` which decodes anything,
so a file becomes a value in one line.

```scala fuel
compile
frame
  label The file, as JSON
  highlight read[Json]  parsed as it is read
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import charsets.utf8Charset, textSanitizers.strictSanitizer

      case class Play(title: Text, acts: Int)

      def load(path: Path on Local): Json = path.read[Json]
frame
  label The JSON, as a play
  highlight as[Play]  decoded field by field
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import charsets.utf8Charset, textSanitizers.strictSanitizer

      case class Play(title: Text, acts: Int)

      def load(path: Path on Local): Play = path.read[Json].as[Play]
##
```

The example is compiled, and is not run, since it has no file to read.
[Reading and writing](../../soundness/modules/filesystem.md#reading-and-writing) has `read`,
[JSON](../../soundness/modules/json.md) the format, and
[composability](../../philosophy/composability.md) the reason they meet.
