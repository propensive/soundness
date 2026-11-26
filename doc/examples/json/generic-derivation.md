# A case class is already a JSON codec

A case class is read from JSON as it stands. Its decoder is made by the compiler from the
decoders of its fields, and changes when they do.

```scala fuel
compile
frame
  label A decoder derived from two fields
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val feste = t"""{"name": "Feste", "age": 30}""".read[Json].as[Player]
frame
  label A third field, which may be absent
  highlight Optional[Text]
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int, patron: Optional[Text])

      val feste = t"""{"name": "Feste", "age": 30}""".read[Json].as[Player]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(feste.inspect)
```

[Case classes and enumerations](../../soundness/modules/json.md#case-classes-and-enumerations)
covers encoding, enumerations and defaults.
