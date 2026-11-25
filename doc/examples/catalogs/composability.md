# One field's JSON, overridden by its path

The path which addresses an entry of a catalog also names a field to a derived codec. A
`Specific` maps paths to instances, and the JSON encoder which is derived for `Play` consults
it at each field.

```scala fuel
compile
frame
  label The derived encoding
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      val play = Play(Person(t"Othello", 40), Person(t"Iago", 28))
      val json: Json = play.in[Json]
frame
  label An encoder for the villain's name alone
  highlight root.villain.name()  a path, checked against Play
  code
      case class Person(name: Text, age: Int)
      case class Play(hero: Person, villain: Person)

      val shout: Text is Json.Encodable =
        Json.Encodable(() => Morphology.Str): text => Json(text.upper)

      val play = Play(Person(t"Othello", 40), Person(t"Iago", 28))

      val json: Json =
        given (Play is Specific over Json.Encodable) =
          specifically:
            case root.villain.name() => shout

        play.in[Json]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(json.show)
```

[The same paths in derivation](../../soundness/modules/catalogs.md#the-same-paths-in-derivation)
has the rules for a path's value.
