# A path which is checked before it is used

An `xp"…"` literal is an XPath which the compiler parses. A predicate which is never closed is
a compile error, so a query cannot fail at runtime for the way it was written.

```scala fuel
compile
frame
  label A predicate which is not closed
  errors show
  code
      val second = xp"/play/act[2/scene"
frame
  label A well-formed path
  code
      val second = xp"/play/act[2]/scene"
frame
  label A document answers it
  highlight selectText  the text of the first match
  code
      import strategies.throwUnsafely
      import xmlSchemas.freeformXmlSchema

      val second = xp"/play/act[2]/scene"
      val play = x"<play><act><scene>Elsinore</scene></act><act><scene>A room</scene></act></play>"
      val place = play.selectText(second)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(place.inspect)
```

[Paths and queries](../../soundness/modules/xml.md#paths-and-queries) has the axes, predicates
and functions a path may use.
