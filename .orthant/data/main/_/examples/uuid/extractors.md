# An identifier in a pattern

`Uuid` is an extractor as well as a type. A `case` which names it matches only text which is a
well-formed UUID, and binds the value already parsed, so there is no second step in which the
check and the conversion could disagree.

```scala fuel
compile
frame
  label Text, matched as text
  code
      def route(segment: Text): Text = segment match
        case t"new" => t"a new sonnet"
        case other  => t"the sonnet called $other"
frame
  label Text, matched as an identifier
  highlight Uuid(id)  `id` is a `Uuid`
  code
      def route(segment: Text): Text = segment match
        case t"new"   => t"a new sonnet"
        case Uuid(id) => t"the sonnet with the identifier ${id.lsb}"
        case other    => t"the sonnet called $other"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(route(t"new"))
Out.println(route(t"00000000-0000-0000-0000-00000000008a"))
Out.println(route(t"eighteen"))
```

[Parsing](../../soundness/modules/uuid.md#parsing) has the extractor and the method it is made
from.
