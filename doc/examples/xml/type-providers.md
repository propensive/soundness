# Fields from an XML Schema

An `Xml.Provider` reads an XSD as the code compiles. A document which matches it is read as a
record whose members are the schema's elements, each at the type the schema declares, with no
case class written to restate them.

```scala fuel
compile
substitute t"""<xs:schema..schema>"""  schema
frame
  label Members at the types of the schema
  highlight record.acts  an xs:int, so an Int
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      object Plays extends Xml.Provider(t"""<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:element name="play"><xs:complexType><xs:sequence>
          <xs:element name="title" type="xs:string"/>
          <xs:element name="acts" type="xs:int"/>
        </xs:sequence></xs:complexType></xs:element>
      </xs:schema>""")

      val record = Plays.record(x"<play><title>Hamlet</title><acts>5</acts></play>")
      val title: Text = record.title
      val acts: Int = record.acts
##
```

[Typed records from an XML Schema](../../soundness/modules/xml.md#typed-records-from-an-xml-schema)
says how each construct of a schema is read.
