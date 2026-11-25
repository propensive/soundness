# A file opened for reading cannot be written

The mode a file is opened in is part of the type of its handle. A handle which was opened to
read grants reading and nothing more, so writing through it is not an error at runtime: it
does not compile.

```scala fuel
compile
frame
  label Opened to read, and written to
  errors show
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage

      def conclude(script: Path on Local): Unit =
        script.open[File](): handle ?=>
          handle.write(Chain(t"Exeunt".in[Data]))
frame
  label Opened to write
  highlight Write  the grant which writing needs
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage

      def conclude(script: Path on Local): Unit =
        script.open[File](Write): handle ?=>
          handle.write(Chain(t"Exeunt".in[Data]))
##
```

The example is compiled, and is not run, since it would write a file.
[Reading and writing](../../soundness/modules/filesystem.md#reading-and-writing) describes the
grants, and [impossible states](../../philosophy/impossible-states.md) the principle.
