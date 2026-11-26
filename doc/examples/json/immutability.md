# An assignment which gives a new document

A `Json` value never changes. Assigning to a field of one is an expression whose value is
another document, and the original is as it was.

```scala fuel
compile
frame
  label A field added
  highlight feste.patron..t"Olivia"  a new document
  code
      import dynamicAccess.dynamicJson

      val feste: Json = j"""{"name": "Feste", "songs": 4}"""
      val kept: Json = feste.patron = t"Olivia"
frame
  label A field removed
  highlight kept.songs..Unset  another, without the field
  code
      import dynamicAccess.dynamicJson

      val feste: Json = j"""{"name": "Feste", "songs": 4}"""
      val kept: Json = feste.patron = t"Olivia"
      val silent: Json = kept.songs = Unset
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(feste.show)
Out.println(kept.show)
Out.println(silent.show)
```

[Updating](../../soundness/modules/json.md#updating) goes deeper with a lens, and
[immutability](../../philosophy/immutability.md) says what is gained.
