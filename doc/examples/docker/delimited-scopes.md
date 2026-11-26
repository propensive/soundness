# An image is open for a block

An OCI archive is read by opening it. Its index, manifest and layers are reached through a
handle which exists inside the block, and what the block returns is an ordinary value which
outlives it.

```scala fuel
compile
frame
  label An archive, as bytes
  code
      import strategies.throwUnsafely, alphabets.hexLowerCase
      import codepages.utf8Codepage, formatting.compactJsonFormatting

      val image = Oci.Image.wasm(hex"0061736d01000000")
      val archive: Data = image.archive.source[Data].memoize
frame
  label Opened, and read
  highlight handle  exists only inside the block
  code
      import strategies.throwUnsafely, alphabets.hexLowerCase
      import codepages.utf8Codepage, formatting.compactJsonFormatting

      val image = Oci.Image.wasm(hex"0061736d01000000")
      val archive: Data = image.archive.source[Data].memoize

      val layers = archive.open[Oci.Image](): handle ?=>
        handle.manifest.layers.map(_.mediaType.show)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(layers.inspect)
```

[Reading an image](../../soundness/modules/docker.md#reading-an-image) has the ways to reach a
layer, and [delimited scopes](../../philosophy/delimited-scopes.md) the reason for the block.
