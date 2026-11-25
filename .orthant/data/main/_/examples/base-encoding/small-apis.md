# One method for every base

`serialize` writes bytes as text, and takes the base as a type. Hexadecimal, binary and a
quaternary alphabet of DNA's four nucleotides are the same call.

```scala fuel
compile
frame
  label Four bits to a character
  code
      import codepages.utf8Codepage
      import alphabets.hexLowerCase

      val bytes: Data = t"Puck".in[Data]
      val hex: Text = bytes.serialize[Hex]
frame
  label One bit to a character
  highlight serialize[Binary]  the same method
  code
      import codepages.utf8Codepage
      import alphabets.{hexLowerCase, binaryStandard}

      val bytes: Data = t"Puck".in[Data]
      val hex: Text = bytes.serialize[Hex]
      val bits: Text = bytes.serialize[Binary]
frame
  label Two bits to a character, as nucleotides
  highlight serialize[Quaternary]  the alphabet in scope is ATCG
  code
      import codepages.utf8Codepage
      import alphabets.{hexLowerCase, binaryStandard, quaternaryDnaNucleotide}

      val bytes: Data = t"Puck".in[Data]
      val hex: Text = bytes.serialize[Hex]
      val bits: Text = bytes.serialize[Binary]
      val genes: Text = bytes.serialize[Quaternary]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(hex)
Out.println(bits)
Out.println(genes)
```

[Other bases](../../soundness/modules/base-encoding.md#other-bases) tabulates the bases, and
[small APIs](../../philosophy/small-apis.md) explains the single name.
