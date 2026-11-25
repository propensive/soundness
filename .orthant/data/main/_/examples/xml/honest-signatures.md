# Parsing asks which elements are allowed

What an XML parser accepts depends on a schema, even when the schema is "anything". Reading a
document therefore needs one in scope, and the choice to admit every element is made by an
import, where it can be seen.

```scala fuel
compile
frame
  label A document read against no schema
  errors show
  code
      import strategies.throwUnsafely

      val line: Xml = t"<line>Exit, pursued by a bear</line>".read[Xml]
frame
  label The schema which admits anything
  highlight freeformXmlSchema  any element, any attribute
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      val line: Xml = t"<line>Exit, pursued by a bear</line>".read[Xml]
##
```

[XML](../../soundness/modules/xml.md#on-xml) introduces the schema, and
[typed records](../../soundness/modules/xml.md#typed-records-from-an-xml-schema) puts an XSD in
its place.
