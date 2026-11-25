# A scratch directory cannot be kept

A directory opened as `Scratch` is created for a block and deleted when the block ends. Its handle
is the only way to reach anything inside it, and the handle cannot be returned, so no part of
the program is left holding a directory which is no longer there.

```scala fuel
compile
feature experimental.captureChecking
frame
  label The handle, returned from its block
  errors show
  highlight scratch
    line 9
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import filesystemOptions.{overwritePreexisting, createParents, dereferenceSymlinks}
      import strategies.throwUnsafely, charsets.utf8Charset, codepages.utf8Codepage

      val wings = temporaryDirectory[Path on Local] / "globe"

      val kept = wings.open[Scratch](Read & Write): scratch ?=>
        scratch
frame
  label The work, done while the directory exists
  highlight overwrite
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import filesystemOptions.{overwritePreexisting, createParents, dereferenceSymlinks}
      import strategies.throwUnsafely, charsets.utf8Charset, codepages.utf8Codepage

      val wings = temporaryDirectory[Path on Local] / "globe"

      def rehearse(): Unit = wings.open[Scratch](Read & Write): scratch ?=>
        (scratch / "cues.txt").overwrite(t"Exit, pursued by a bear")
##
```

The example is compiled, and is not run.
[Creating as a scope](../../soundness/modules/filesystem.md#creating-as-a-scope) describes
scratch files and directories.
