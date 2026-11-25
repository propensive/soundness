# A schema from a case class

A TEL schema says which keywords a document may have, and what their values are. The compiler
derives one from a case class, as it derives the codec, so the schema, the codec and the type
cannot disagree, and the schema guides a compact binary form of the same document.

```scala fuel
compile
frame
  label A document, from a derived codec
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      val viola: Tel = Player(t"Viola", 20).in[Tel]
frame
  label The schema of the same type
  highlight Tels.tels[Player]  derived, as the codec is
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      val viola: Tel = Player(t"Viola", 20).in[Tel]
      val schema: Tels = Tels.tels[Player](t"player")
frame
  label The document as BinTEL, under the schema
  highlight bintel(schema)  typed by the schema, not tagged
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      val viola: Tel = Player(t"Viola", 20).in[Tel]
      val schema: Tels = Tels.tels[Player](t"player")
      val binary: Data = viola.bintel(schema)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(viola.show)
Out.println(binary.length.show)
```

[Schemas](../../soundness/modules/tel.md#schemas) describes layers, validators and patterns,
and [BinTEL](../../soundness/modules/tel.md#bintel) the binary form.
