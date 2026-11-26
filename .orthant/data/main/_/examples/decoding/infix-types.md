# A new type which text can be read as

A type joins `as` by a given whose type is a sentence: a `Heading` is decodable in text. The
instance is the one function from text to the value, written as a pattern match.

```scala fuel
compile
frame
  label A type which as does not know
  errors show
  code
      import strategies.throwUnsafely

      enum Heading:
        case North, South

      val heading: Heading = t"north".as[Heading]
frame
  label A heading is decodable in text
  highlight Heading..Text  the type of the instance
    line 7
  code
      import strategies.throwUnsafely

      enum Heading:
        case North, South

      object Heading:
        given (Heading is Decodable in Text) =
          case t"north" => North
          case t"south" => South

      val heading: Heading = t"north".as[Heading]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(heading.inspect)
```

[Defining your own](../../soundness/modules/decoding.md#defining-your-own) has the other
typeclasses, and [infix types](../../philosophy/infix-types.md) explains `is` and `in`.
