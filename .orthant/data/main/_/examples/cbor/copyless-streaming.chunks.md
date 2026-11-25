# A value which arrives in two pieces

Binary data comes off a network in chunks which know nothing of the values in them. A type with a
`Cbor.Parsable` instance is read straight from the chunks as they arrive, with no tree and
without the input being joined up first, so a value which is split between two chunks reads
exactly as one which arrives whole.

```scala fuel
compile
frame
  label The bytes of a value, whole
  code
      import strategies.throwUnsafely, breviloquence.Inlinable

      case class Player(name: Text, parts: Int)

      given playerParsable: (Player is Cbor.Parsable) = Inlinable.parsable[Player]

      val bytes: Data = Player(t"Burbage", 12).in[Data over Cbor]
      val player = Chain(bytes).read[Player in Cbor]
frame
  label The same bytes, cut after the ninth
  highlight bytes.keep(9)..skip(9)
  code
      import strategies.throwUnsafely, breviloquence.Inlinable

      case class Player(name: Text, parts: Int)

      given playerParsable: (Player is Cbor.Parsable) = Inlinable.parsable[Player]

      val bytes: Data = Player(t"Burbage", 12).in[Data over Cbor]
      val player = Chain(bytes.keep(9), bytes.skip(9)).read[Player in Cbor]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(player.inspect)
```

[Parsing directly](../../soundness/modules/cbor.md#parsing-directly) describes the derivation.
