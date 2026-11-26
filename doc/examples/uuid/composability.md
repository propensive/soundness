# An identifier in a JSON document

Nothing in the JSON library mentions UUIDs. A `Uuid` can be decoded from text, and that is all
a field of a case class needs for the whole class to be read from JSON, with a malformed
identifier reported as the typed error it is.

```scala fuel
compile
frame
  label A field which is text
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Sonnet(id: Text, number: Int)

      val source = t"""{"id": "a0cb16f0-d41e-4c28-862f-bd6164bbcc8c", "number": 18}"""
      val sonnet = source.read[Json].as[Sonnet]
frame
  label The same field, as an identifier
  highlight Uuid  decoded from the text of the field
  code
      import strategies.throwUnsafely, codepages.utf8Codepage

      case class Sonnet(id: Uuid, number: Int)

      val source = t"""{"id": "a0cb16f0-d41e-4c28-862f-bd6164bbcc8c", "number": 18}"""
      val sonnet = source.read[Json].as[Sonnet]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(sonnet.id.msb.show)
Out.println(sonnet.number.show)
```

[Parsing](../../soundness/modules/uuid.md#parsing) mentions the decoder, and
[composability](../../philosophy/composability.md) why no adapter is needed.
