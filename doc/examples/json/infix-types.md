# Every route between text, JSON and a value

`read`, `as` and `in` each take a type which says where the value is going. `Player in Json`
is a player held in JSON, and `Text over Json` is text which carries JSON.

```scala fuel
compile
frame
  label Text to JSON, and JSON to a value
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val json: Json = t"""{"name": "Viola", "age": 20}""".read[Json]
      val viola: Player = json.as[Player]
frame
  label Both steps at once
  highlight read[Player..Json]  parsed and decoded
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val viola: Player = t"""{"name": "Viola", "age": 20}""".read[Player in Json]
frame
  label And back again
  highlight in[Text..Json]  encoded and rendered
  code
      import strategies.throwUnsafely, codepages.utf8Codepage
      import formatting.compactJsonFormatting

      case class Player(name: Text, age: Int)

      val viola: Player = t"""{"name": "Viola", "age": 20}""".read[Player in Json]
      val text: Text = viola.in[Text over Json]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(text)
```

[Case classes and enumerations](../../soundness/modules/json.md#case-classes-and-enumerations)
tabulates the routes, and [infix types](../../philosophy/infix-types.md) the words they use.
