# Mathematics written and read like everything else

A formula has no methods of its own for XML. It is written with `in[Xml]` and read back with
`read[Math]`, as every other document is, and any number becomes one with `math`.

```scala fuel
compile
frame
  label A formula, written as XML
  highlight in[Xml]  the method which writes HTML and SVG
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      val area = ergo"(π r↗2)"
      val markup = area.in[Xml].show
frame
  label The XML, read as a formula
  highlight read[Math]  the method which reads JSON and images
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      val area = ergo"(π r↗2)"
      val markup = area.in[Xml].show
      val same = markup.read[Math] == area
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(markup)
Out.println(same.inspect)
```

[The MathML tree](../../soundness/modules/mathematical-markup.md#the-mathml-tree) describes
the round trip, and [small APIs](../../philosophy/small-apis.md) the reason for one name.
