# An absent value is an absent element

XML has no null. A field of type `Optional` which is `Unset` writes no element at all, and a
document without the element reads back as `Unset`, so absence looks the same in the type and
in the text.

```scala fuel
compile
frame
  label A nickname which is there
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Knight(name: Text, nickname: Optional[Text])

      val knight: Xml = Knight(t"Falstaff", t"Jack").in[Xml]
frame
  label A nickname which is not
  highlight Unset  no element is written
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      case class Knight(name: Text, nickname: Optional[Text])

      val knight: Xml = Knight(t"Falstaff", Unset).in[Xml]
      val read: Knight = knight.as[Knight]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(knight.show)
Out.println(read.inspect)
```

[Case classes](../../soundness/modules/xml.md#case-classes) covers attributes and maps, and
[optionality](../../philosophy/optionality.md) the type.
