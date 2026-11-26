# A file which lasts as long as its block

A scratch file is created when its block begins, and removed when the block ends, whether it
ends with a value or with an error. The handle is the block's own, so nothing can reach the
file once it has gone.

```scala fuel
compile
frame
  label A scratch file, written
  highlight open[Scratch.File]  created for this block
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.strictSanitizer

      def rehearse(wings: Path on Local): Unit =
        wings.open[Scratch.File](Read & Write): file ?=>
          file.write(Chain(t"Enter Viola".in[Data]))
frame
  label And read back, before it is removed
  highlight file.path.read[Text]  the last use, inside the block
  code
      import filesystemBackends.javaBaseFilesystem, systems.javaBaseSystem, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.strictSanitizer

      def rehearse(wings: Path on Local): Text =
        wings.open[Scratch.File](Read & Write): file ?=>
          file.write(Chain(t"Enter Viola".in[Data]))
          file.path.read[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import temporaryDirectories.javaBaseTemporaryDirectory, pathInterfaces.pathOnLocal

Out.println(rehearse(temporaryDirectory[Path on Local]))
```

[Creating as a scope](../../soundness/modules/filesystem.md#creating-as-a-scope) has scratch
directories too, and [delimited scopes](../../philosophy/delimited-scopes.md) the shape.
