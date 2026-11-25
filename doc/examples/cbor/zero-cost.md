# No tree between the bytes and the value

Reading a case class from CBOR ordinarily builds a tree of the document and then decodes it.
Under `codecs.strict`, a record is written and read by a codec composed for its own shape as
it compiles, and a type which could only go through the tree is a compile error.

```scala fuel
compile
frame
  label A record, written and read directly
  highlight codecs.strict  no tree, or no compilation
  code
      import strategies.throwUnsafely
      import codecs.strict

      case class Player(name: Text, age: Int)

      val bytes: Data = Player(t"Viola", 20).in[Data over Cbor]
      val viola: Player = bytes.read[Player in Cbor]
frame
  label A generic type, which has no direct codec
  errors show
  code
      import strategies.throwUnsafely
      import codecs.strict

      case class Sealed[letter](letter: letter)

      val bytes: Data = Sealed(t"Viola").in[Data over Cbor]
##
```

[Choosing the route for every type](../../soundness/modules/cbor.md#choosing-the-route-for-every-type)
compares the three modes.
