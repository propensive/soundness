# A case class is an element

A case class is written as an element named for the type, with a child element for each field.
An annotation on a field makes it an attribute instead, and the decoder changes with the
encoder.

```scala fuel
compile
frame
  label A child element for each field
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Play(title: Text, year: Int)

      val hamlet: Xml = Play(t"Hamlet", 1603).in[Xml]
frame
  label One field as an attribute
  highlight @attribute  written in the opening tag
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Play(title: Text, @attribute year: Int)

      val hamlet: Xml = Play(t"Hamlet", 1603).in[Xml]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(hamlet.show)
Out.println(hamlet.as[Play].inspect)
```

[Case classes](../../soundness/modules/xml.md#case-classes) tabulates what each part of a type
becomes.
