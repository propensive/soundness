# The layout of XML is chosen by an import

Encoding a value as XML text is `in[Text over Xml]`, whatever the layout. The formatting in
scope decides whether the text is compact, or indented for a reader.

```scala fuel
compile
alternative Compact
  highlight compactXmlFormatting  no whitespace
  code
      import xmlSchemas.freeformXmlSchema
      import formatting.compactXmlFormatting

      case class Play(title: Text, year: Int)

      val text: Text = Play(t"Hamlet", 1603).in[Text over Xml]
alternative Indented
  highlight indentedXmlFormatting  an element to a line
  code
      import xmlSchemas.freeformXmlSchema
      import formatting.indentedXmlFormatting

      case class Play(title: Text, year: Int)

      val text: Text = Play(t"Hamlet", 1603).in[Text over Xml]
##
```

[Formatting](../../soundness/modules/xml.md#formatting) has the forms which write a large
document a block at a time.
