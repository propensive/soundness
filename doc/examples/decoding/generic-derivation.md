# An enumeration, read by the names of its cases

The cases of an enumeration are known to the compiler, so nothing is written to read one from
text. `As[Direction]` matches the name of a case, and declines anything else.

```scala fuel
compile
frame
  label An enumeration
  code
      enum Direction:
        case North, South, East, West
frame
  label Extracted by name
  highlight As[Direction](direction)  matches the name of a case
  code
      enum Direction:
        case North, South, East, West

      def direction(text: Text): Optional[Direction] = text match
        case As[Direction](direction) => direction
        case _                        => Unset
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(direction(t"North").inspect)
Out.println(direction(t"Up").inspect)
```

[Enumerations by name and position](../../soundness/modules/decoding.md#enumerations-by-name-and-position)
describes the derived instance beneath it.
