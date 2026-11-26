# A digest which nobody typed

Every part of an image is addressed by the digest of its bytes, and an image with one digest
wrong is silently invalid. Here an image is assembled from its content, and its manifest, with
the digest and size of each layer, is computed from that. There is no place to write a digest.

```scala fuel
compile
frame
  label A component
  code
      import alphabets.hexLowerCase, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      val component: Data = hex"0061736d01000000"
frame
  label An image of it
  highlight Oci.Image.wasm(component)
  code
      import alphabets.hexLowerCase, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      val component: Data = hex"0061736d01000000"
      val image = Oci.Image.wasm(component)
frame
  label The digests which its manifest declares
  highlight digest  computed from the bytes of the layer
  code
      import alphabets.hexLowerCase, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      val component: Data = hex"0061736d01000000"
      val image = Oci.Image.wasm(component)
      val digests = image.manifest.layers.map(_.digest)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(digests.inspect)
```

[What an image contains](../../soundness/modules/docker.md#what-an-image-contains) describes
the two digests of a compressed layer.
