# Every matching element, changed at once

An `Xml` value never changes. A lens writes an update as an assignment to a path through the
document, and the result is a new document. With `Each` in the path, the assignment is made to
every child.

```scala fuel
compile
frame
  label One element replaced
  highlight _.line..>  the first line
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema
      import optics.{xmlEachOptical, xmlLens}, dynamicAccess.dynamicXml

      val speech: Xml = x"<speech><line>To be</line><line>or not to be</line></speech>"
      val revised = speech.lens(_.line = x"<line>Words</line>")
frame
  label Every element replaced
  highlight _(Each)  every child
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema
      import optics.{xmlEachOptical, xmlLens}, dynamicAccess.dynamicXml

      val speech: Xml = x"<speech><line>To be</line><line>or not to be</line></speech>"
      val revised = speech.lens(_(Each) = x"<line>Words</line>")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(speech.show)
Out.println(revised.show)
```

[Updating](../../soundness/modules/xml.md#updating) has the rest, and
[optics](../../soundness/modules/optics.md) the lenses themselves.
