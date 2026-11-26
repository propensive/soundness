# An image which describes itself, from a WIT world

A Wasm image records the interfaces which its component exports and imports, so that a host
can tell whether it can run it. They are already stated in the WIT world the component was
linked against, and the image takes them from there, so the contract is written once.

```scala fuel
compile
frame
  label A world
  code
      import strategies.throwUnsafely

      val wit = t"""package illyria:court;

      world herald {
        import wasi:io/streams@0.2.0;
        export wasi:http/incoming-handler@0.2.0;
      }"""

      val world = WitDialect.worlds(wit)(t"herald")
frame
  label An image which exports what the world exports
  highlight world.exports
  code
      import strategies.throwUnsafely, alphabets.hexLowerCase
      import codepages.utf8Codepage, formatting.compactJsonFormatting

      val wit = t"""package illyria:court;

      world herald {
        import wasi:io/streams@0.2.0;
        export wasi:http/incoming-handler@0.2.0;
      }"""

      val image = WitDialect.worlds(wit)(t"herald").let: world =>
        Oci.Image.wasm(hex"0061736d01000000", world.exports, world.imports)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(image.let(_.wasmConfig).let(_.component).let(_.exports).inspect)
```

[WebAssembly workloads](../../soundness/modules/docker.md#webassembly-workloads) describes the
artifact, and [composability](../../philosophy/composability.md) the way two libraries meet in
a plain value.
