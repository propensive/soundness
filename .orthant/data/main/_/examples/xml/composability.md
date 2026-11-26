# A case class assigned into a document

A lens comes from the optics library and an encoder from the derivation of a case class. Put
together, a value of the case class is assigned to a place in an XML document, and is encoded
as it lands.

```scala fuel
compile
frame
  label An element, replaced by XML
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema
      import optics.xmlLens, dynamicAccess.dynamicXml

      case class Player(name: Text, age: Int)

      val court: Xml = x"<court><Player><name>Yorick</name><age>7</age></Player></court>"
      val recast = court.lens(_.Player = x"<Player><name>Feste</name><age>30</age></Player>")
frame
  label The same element, replaced by a value
  highlight Player(t"Feste"..30)  encoded as it is assigned
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema
      import optics.xmlLens, dynamicAccess.dynamicXml

      case class Player(name: Text, age: Int)

      val court: Xml = x"<court><Player><name>Yorick</name><age>7</age></Player></court>"
      val recast = court.lens(_.Player = Player(t"Feste", 30))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(recast.show)
```

[Updating](../../soundness/modules/xml.md#updating) notes that no import is needed for the
encoding.
