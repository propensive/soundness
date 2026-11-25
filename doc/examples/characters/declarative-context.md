# The same text, in another encoding

How many bytes a piece of text becomes depends on the encoding, and the encoding is whichever
`Codepage` has been imported. The conversion is written once, and never names it.

```scala fuel
compile
frame
  label UTF-8, where an accented letter is two bytes
  highlight codepages.utf8Codepage  eight bytes
  code
      import codepages.utf8Codepage

      val bytes = t"Orléans".in[Data]
frame
  label ISO-8859-1, where every letter is one
  highlight codepages.iso88591Codepage  seven bytes
  code
      import codepages.iso88591Codepage

      val bytes = t"Orléans".in[Data]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(bytes.length.show)
```

The seven characters are seven bytes in ISO-8859-1, and would be eight in UTF-8.
[Encodings](../../soundness/modules/characters.md#encodings) lists the codepages, and
[declarative context](../../philosophy/declarative-context.md) explains the import.
