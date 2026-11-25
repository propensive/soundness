# An image is one kind or the other

An image holds either the configuration of a filesystem or that of a WebAssembly component.
Each is asked for as an optional value, so the one an image does not have is `Unset`, and not
an exception or an empty record.

```scala fuel
compile
frame
  label The configuration of a component
  highlight wasmConfig  present, for a Wasm artifact
  code
      import alphabets.hexLowerCase, codepages.utf8Codepage, formatting.compactJsonFormatting

      val artifact = Oci.Image.wasm(hex"0061736d01000000")
      val component: Optional[Text] = artifact.wasmConfig.let(_.os)
frame
  label And of a filesystem, which it has not
  highlight imageConfig  absent, for the same artifact
  code
      import alphabets.hexLowerCase, codepages.utf8Codepage, formatting.compactJsonFormatting

      val artifact = Oci.Image.wasm(hex"0061736d01000000")
      val component: Optional[Text] = artifact.wasmConfig.let(_.os)
      val filesystem: Optional[Text] = artifact.imageConfig.let(_.os)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(component.inspect)
Out.println(filesystem.inspect)
```

[WebAssembly workloads](../../soundness/modules/docker.md#webassembly-workloads) describes
the two kinds, and [optionality](../../philosophy/optionality.md) `let`.
