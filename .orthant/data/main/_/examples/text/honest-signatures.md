# Text becomes bytes only through an encoding

The bytes of a piece of text depend on a character encoding, and no encoding is assumed.
Converting text to `Data` needs one in scope, so code which was not given an encoding cannot
quietly use the platform's default.

```scala fuel
compile
frame
  label No encoding has been named
  errors show
  code
      val bytes = t"Adélaïde".in[Data]
frame
  label UTF-8
  highlight codepages.utf8Codepage  the encoding, named
  code
      import codepages.utf8Codepage

      val bytes = t"Adélaïde".in[Data]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(bytes.length.show)
```

The eight characters are ten bytes in UTF-8. [Bytes](../../soundness/modules/text.md#bytes)
has the way back, and [honest signatures](../../philosophy/honest-signatures.md) the principle.
