# Content which may not be a number

The text of an element is read as a type with `as`, and not every text is a number. The
conversion raises an `Xml.Error`, and a method which makes it must handle the error or name it
in its type.

```scala fuel
compile
frame
  label A failure which is not accounted for
  errors show
  code
      import xmlSchemas.freeformXmlSchema

      def year(element: Xml): Int = element.as[Int]
frame
  label The failure, declared
  highlight raises..Error  part of the type
  code
      import xmlSchemas.freeformXmlSchema

      def year(element: Xml): Int raises Xml.Error = element.as[Int]
frame
  label The failure, as an absent value
  highlight safely  a failure becomes Unset
  code
      import xmlSchemas.freeformXmlSchema

      def year(element: Xml): Optional[Int] = safely(element.as[Int])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(year(x"<year>1603</year>").inspect)
Out.println(year(x"<year>sixteen-three</year>").inspect)
```

[Reading values](../../soundness/modules/xml.md#reading-values) shows the error, and
[error handling](../../philosophy/error-handling.md) explains `raises`.
