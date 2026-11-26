# An enumeration is a choice of elements

A value of an enumeration is written as an element named for its case, and the name of an
element chooses the case when one is read. `@name` gives a case another name in the document.

```scala fuel
compile
frame
  label An element named for the case
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      enum Exit:
        case Pursued(by: Text)
        case Alone(lines: Int)

      val exit: Exit = x"<Pursued><by>a bear</by></Pursued>".as[Exit]
frame
  label A case renamed for XML
  highlight @name[Xml](t"chased")  the name of the element
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      enum Exit:
        @name[Xml](t"chased") case Pursued(by: Text)
        case Alone(lines: Int)

      val exit: Exit = x"<chased><by>a bear</by></chased>".as[Exit]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(exit.inspect)
Out.println(exit.in[Xml].show)
```

[Case classes](../../soundness/modules/xml.md#case-classes) says what happens to a name which
matches no case.
