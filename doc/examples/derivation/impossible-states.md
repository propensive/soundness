# A field which cannot be encoded

A derived instance is built from the instances of the fields. Where one field has none, there
is nothing to build from, and the case class has no instance either: the gap is found as the
code compiles, and not when a value is first encoded.

```scala fuel
compile
frame
  label A case class of encodable fields
  highlight in[Json]  derived from Text and Int
  code
      case class Player(name: Text, age: Int)

      val json: Json = Player(t"Bottom", 35).in[Json]
frame
  label A field which JSON knows nothing about
  errors show
  code
      class Costume()

      case class Player(name: Text, costume: Costume)

      val json: Json = Player(t"Bottom", Costume()).in[Json]
##
```

[When derivation fails](../../soundness/modules/derivation.md#when-derivation-fails) lists the
common causes.
