# Text to a value, and back, in one step each

`read` and `in` take a type which says what is wanted and in what form. `Play in Xml` is a play
held in XML, and `Text over Xml` is text which carries XML, so parsing with decoding, and
encoding with writing, are each one expression.

```scala fuel
compile
frame
  label Parsed, then decoded
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Play(title: Text, year: Int)

      val xml: Xml = t"<Play><title>Hamlet</title><year>1603</year></Play>".read[Xml]
      val play: Play = xml.as[Play]
frame
  label Both at once
  highlight read[Play..Xml]  a play, held in XML
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Play(title: Text, year: Int)

      val play: Play = t"<Play><title>Hamlet</title><year>1603</year></Play>".read[Play in Xml]
frame
  label And back to text
  highlight in[Text..Xml]  text, carrying XML
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Play(title: Text, year: Int)

      val play: Play = t"<Play><title>Hamlet</title><year>1603</year></Play>".read[Play in Xml]
      val text: Text = play.in[Text over Xml]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(text)
```

[Case classes](../../soundness/modules/xml.md#case-classes) has the detail, and
[infix types](../../philosophy/infix-types.md) the vocabulary.
