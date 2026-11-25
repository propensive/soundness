# Paths, written as patterns

The path which a lens assigns to can also be the pattern of a `case`. `specifically` reads a
list of such cases, each a path from `root` and a value for the field at its end, and the
compiler checks every path against the type.

```scala fuel
compile
frame
  label An encoder for two fields, named by their paths
  highlight root.opponent.name()  a field of Duel, two levels down
  code
      case class Person(name: Text, age: Int)
      case class Duel(challenger: Person, opponent: Person)

      val shout: Text is Json.Encodable =
        Json.Encodable(() => Morphology.Str): text => Json(text.upper)

      val overrides: Duel is Specific over Json.Encodable =
        specifically:
          case root.opponent.name()   => shout
          case root.challenger.name() => shout
frame
  label A path which a duel does not have
  errors show
  code
      case class Person(name: Text, age: Int)
      case class Duel(challenger: Person, opponent: Person)

      val shout: Text is Json.Encodable =
        Json.Encodable(() => Morphology.Str): text => Json(text.upper)

      val overrides: Duel is Specific over Json.Encodable =
        specifically:
          case root.opponent.title() => shout
##
```

[Naming paths without traversing them](../../soundness/modules/optics.md#naming-paths-without-traversing-them)
describes what a `Specific` is for.
