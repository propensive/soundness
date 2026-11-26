# A document which is its own pattern

A `tel"…"` literal builds a document from the values substituted into it. Written in a `case`,
the same literal takes a document apart, and binds a name where a value was.

```scala fuel
compile
frame
  label A value, written into a document
  highlight $name  encoded by its type
  code
      val name = t"Hamlet"
      val contact = tel"name $name"
frame
  label The same literal, as a pattern
  highlight $prince  bound as Tel
  code
      import strategies.throwUnsafely

      val name = t"Hamlet"
      val contact = tel"name $name"

      val found = contact match
        case tel"name $prince" => prince.as[Text]
        case _                 => t"anonymous"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(contact.show)
Out.println(found)
```

[Writing TEL literally](../../soundness/modules/tel.md#writing-tel-literally) has the rest.
