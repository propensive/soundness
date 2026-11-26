# An archive says what it holds

An OCI archive may hold a filesystem to unpack or a WebAssembly component to instantiate. A
reader does not have to know which before it opens one: `config` gives one of two types, and a
match on it handles both.

```scala fuel
compile
frame
  label Either kind of configuration
  highlight Oci.Config  one of two types
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      def kind(archive: Data): Oci.Config = archive.open[Oci.Image](): handle ?=>
        handle.config
frame
  label Each kind, handled
  highlight handle.config  a component's configuration, or a filesystem's
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      def kind(archive: Data): Text = archive.open[Oci.Image](): handle ?=>
        handle.config match
          case config: Oci.WasmConfig   => t"a component for ${config.os}"
          case config: Oci.Image.Config => t"a filesystem for ${config.os}"
##
```

```scala fuel
run
##
import alphabets.hexLowerCase, formatting.compactJsonFormatting
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val image = Oci.Image.wasm(hex"0061736d01000000")
Out.println(kind(image.archive.source[Data].memoize))
```

[Reading an image](../../soundness/modules/docker.md#reading-an-image) describes `config`, and
[WebAssembly workloads](../../soundness/modules/docker.md#webassembly-workloads) the two kinds
of artifact.
