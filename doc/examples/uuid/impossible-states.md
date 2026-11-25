# Text is not an identifier

A method which takes a `Uuid` cannot be given text, however much the text looks like one. The
check which makes text into an identifier has to happen first, so the method never meets a
malformed one.

```scala fuel
compile
frame
  label Text, where an identifier is wanted
  errors show
  code
      def sonnet(id: Uuid): Text = t"the sonnet ${id.lsb}"

      val first = sonnet(t"00000000-0000-0000-0000-000000000012")
frame
  label An identifier, checked as the code compiles
  highlight uuid  one word, and the compiler reads the digits
  code
      def sonnet(id: Uuid): Text = t"the sonnet ${id.lsb}"

      val first = sonnet(uuid"00000000-0000-0000-0000-000000000012")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(first)
```

[On UUIDs](../../soundness/modules/uuid.md#on-uuids) explains why a string is an unreliable
home for one, and [impossible states](../../philosophy/impossible-states.md) the principle.
