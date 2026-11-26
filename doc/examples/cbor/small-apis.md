# The same value, in fewer bytes

CBOR is written and read with the words which JSON uses. `in[Data over Json]` gives the bytes
of a value as JSON text, and changing one word of the type gives its bytes as CBOR.

```scala fuel
compile
frame
  label A player, as the bytes of JSON text
  highlight in[Data..Json]  25 bytes
  code
      import strategies.throwUnsafely
      import codepages.utf8Codepage, formatting.compactJsonFormatting

      case class Player(name: Text, age: Int)

      val bytes: Data = Player(t"Viola", 20).in[Data over Json]
      val viola: Player = bytes.read[Player in Json]
frame
  label The same player, as CBOR
  highlight in[Data..Cbor]  17 bytes
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      val bytes: Data = Player(t"Viola", 20).in[Data over Cbor]
      val viola: Player = bytes.read[Player in Cbor]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(bytes.length.show)
Out.println(viola.inspect)
```

The JSON is 25 bytes.
[Encoding and decoding](../../soundness/modules/cbor.md#encoding-and-decoding) has the steps
between, and [small APIs](../../philosophy/small-apis.md) the reason the words are shared.
