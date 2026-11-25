# A record read straight from the text

`read[Player in Json]` ordinarily builds a tree of JSON values and then decodes it. With a
`Json.Parsable` instance, a parser composed for the one type takes the fields from the text as
they arrive, in the blocks in which they arrive, and no tree is ever built.

```scala fuel
compile
frame
  label A document which arrives in two blocks
  highlight "Bur..bage"  a string cut in the middle
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, parts: Int)

      val blocks = Chain(t"""{"name": "Bur""", t"""bage", "parts": 12}""")
frame
  label A parser made for the type
  highlight Json.Parsable.derived
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, parts: Int)

      given playerParsable: (Player is Json.Parsable) = Json.Parsable.derived

      val blocks = Chain(t"""{"name": "Bur""", t"""bage", "parts": 12}""")
frame
  label The record, with no tree between
  highlight read[Player..Json]
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, parts: Int)

      given playerParsable: (Player is Json.Parsable) = Json.Parsable.derived

      val blocks = Chain(t"""{"name": "Bur""", t"""bage", "parts": 12}""")
      val player = blocks.read[Player in Json]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(player.inspect)
```

[Skipping the tree](../../soundness/modules/json.md#skipping-the-tree) describes the direct
route, and the import which chooses it for every type.
