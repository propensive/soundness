# An image, built as a value

A container image is usually the output of a tool. Here it is three expressions: a file in a
tar archive, a layer made of the archive, and an image made of the layer. Nothing is run, and
each step is checked as it compiles.

```scala fuel
compile
frame
  label A layer
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      val file = Tar.Entry.File
        ( t"prologue.txt".as[Relative on Tar], UnixMode(), UnixUser(0), UnixGroup(0),
          0.bits.u32, Archive.Body(t"Two households\n".in[Data]) )

      val layer = Oci.Layer(Tarfile(List(file)))
frame
  label An image, with the command it runs
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      val file = Tar.Entry.File
        ( t"prologue.txt".as[Relative on Tar], UnixMode(), UnixUser(0), UnixGroup(0),
          0.bits.u32, Archive.Body(t"Two households\n".in[Data]) )

      val layer = Oci.Layer(Tarfile(List(file)))
      val config = Oci.ContainerConfig(Cmd = List(t"/bin/cat", t"prologue.txt"))
      val image = Oci.Image(List(layer), config = config)
frame
  label The archive which a runtime loads
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      val file = Tar.Entry.File
        ( t"prologue.txt".as[Relative on Tar], UnixMode(), UnixUser(0), UnixGroup(0),
          0.bits.u32, Archive.Body(t"Two households\n".in[Data]) )

      val layer = Oci.Layer(Tarfile(List(file)))
      val config = Oci.ContainerConfig(Cmd = List(t"/bin/cat", t"prologue.txt"))
      val image = Oci.Image(List(layer), config = config)
      val archive: Data = image.archive.source[Data].memoize
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(image.manifest.layers.map(_.mediaType.show).inspect)
```

[Building an image](../../soundness/modules/docker.md#building-an-image) names the parts, and
[direct style](../../philosophy/direct-style.md) explains why a build is ordinary code.
