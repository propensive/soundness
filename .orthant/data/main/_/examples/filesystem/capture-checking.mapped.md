# A view of a mapped file ends with the mapping

`mapped` maps a file into memory and lends a view of it for the length of a block. A `slice` is a
narrower view of the same memory, with no copy, so it depends on the mapping as the whole view
does, and neither may leave the block. Bytes which are copied out with `read` are plain `Data`.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A slice of memory which will be unmapped
  errors show
  highlight view.slice..6L)
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely

      val folio = temporaryDirectory[Path on Local] / "globe" / "folio.bin"

      val tail = folio.mapped(view => view.slice(10L, 6L))
frame
  label Three bytes of it, copied out
  highlight .read(0L,..3)
  code
      import filesystemBackends.javaBaseFilesystem, pathInterfaces.pathOnLocal
      import systems.javaBaseSystem, temporaryDirectories.javaBaseTemporaryDirectory
      import strategies.throwUnsafely

      val folio = temporaryDirectory[Path on Local] / "globe" / "folio.bin"

      val tail: Data = folio.mapped(view => view.slice(10L, 6L).read(0L, 3))
##
```

The example is compiled, and is not run.
[Positional access](../../soundness/modules/filesystem.md#positional-access) describes what a
view can load.
