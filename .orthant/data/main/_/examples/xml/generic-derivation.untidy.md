# A decoder which reads untidy documents

Real XML has comments, stray text and elements in no particular order. The decoder derived for
a case class takes each field from the child of its name, wherever it is, gathers repeated
children into a list, and passes over the rest.

```scala fuel
compile
frame
  label A tidy document
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Fool(name: Text, songs: List[Text])

      val feste = x"<Fool><name>Feste</name><songs>Hey, Robin</songs></Fool>".as[Fool]
frame
  label Comments, prose and scattered elements
  highlight <!--..-->  ignored
  highlight <songs>O..songs>  joins the first, in document order
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Fool(name: Text, songs: List[Text])

      val feste = x"""<Fool><songs>Hey, Robin</songs>a fool<!--of Illyria-->
        <name>Feste</name><songs>O Mistress Mine</songs></Fool>""".as[Fool]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(feste.inspect)
```

[What decoding tolerates](../../soundness/modules/xml.md#what-decoding-tolerates) lists what
is passed over, and what is still an error.
