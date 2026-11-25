# A flag from another enumeration

A `Flags[Color]` has one bit for each case of `Color`, and a bit is named only by a case. A raw
mask would test whichever bit had the same position, but a case of another enumeration is not
a flag of this set at all.

```scala fuel
compile
frame
  label A weekday is not a color
  errors show
  code
      enum Color:
        case Red, Orange, Yellow, Green

      enum Weekday:
        case Monday, Tuesday, Wednesday

      val warm = Flags[Color]()(Color.Orange) = true
      val monday = warm(Weekday.Monday)
frame
  label A color is
  highlight Color.Orange  a case of the right enumeration
    occurrence 2
  code
      enum Color:
        case Red, Orange, Yellow, Green

      enum Weekday:
        case Monday, Tuesday, Wednesday

      val warm = Flags[Color]()(Color.Orange) = true
      val orange = warm(Color.Orange)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(orange.inspect)
```

[What the compiler rejects](../../soundness/modules/flags.md#what-the-compiler-rejects) has the
case in full, and [impossible states](../../philosophy/impossible-states.md) the principle.
