# One method writes text or bytes

A whole file is written with `write`, whatever is written to it. Text is encoded under the
codepage in scope, bytes are written as they are, and anything else which can become a stream
of bytes is written by the same method.

```scala fuel
compile
frame
  label Text
  highlight write  encoded under the codepage in scope
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage

      def sign(script: Path on Local): Unit = script.write(t"Exeunt")
frame
  label Bytes
  highlight write(bytes)  the same method
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage

      def sign(script: Path on Local): Unit = script.write(t"Exeunt")
      def seal(script: Path on Local, bytes: Data): Unit = script.write(bytes)
##
```

The example is compiled, and is not run, since it would write a file.
[Reading and writing](../../soundness/modules/filesystem.md#reading-and-writing) has the
handles for larger files, and [small APIs](../../philosophy/small-apis.md) the reasons for
one name.
