# The same words as JSON

A format adds a type and no methods. `read`, `as` and `in` do for YAML what they do for JSON,
so a case class which is read from one is read from the other, and a document is carried
between them by decoding and encoding again.

```scala fuel
compile
frame
  label A player, read from YAML
  highlight read[Player..Yaml]  YAML
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val viola = t"name: Viola\nage: 20".read[Player in Yaml]
frame
  label The same player, read from JSON
  highlight read[Player..Json]  JSON
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val viola = t"""{"name": "Viola", "age": 20}""".read[Player in Json]
frame
  label From one format to the other
  highlight in[Json]  encoded again
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Player(name: Text, age: Int)

      val viola = t"name: Viola\nage: 20".read[Player in Yaml]
      val json = viola.in[Json]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(json.show)
```

[Parsing and decoding](../../soundness/modules/yaml.md#parsing-and-decoding) has the YAML side,
and [small APIs](../../philosophy/small-apis.md) the reason there is one vocabulary.
