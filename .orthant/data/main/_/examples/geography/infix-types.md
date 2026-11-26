# A compass of one's own

A bearing is given on whichever rose is asked for, and a rose is any type which says how an
angle becomes one of its values. An airman reads a bearing from a clock face, and
`Hour is Directional` is all that a clock face needs.

```scala fuel
compile
frame
  label Twelve directions
  code
      case class Hour(value: Int)
frame
  label How an angle becomes one
  highlight clock:..Directional  read as a sentence
  code
      case class Hour(value: Int)

      given clock: Hour is Directional =
        angle => Hour((angle.principal.degrees/30 + 0.5).toInt%12)
frame
  label A bearing, on the clock
  highlight bearing[Hour]  the rose is a type argument
  code
      case class Hour(value: Int)

      given clock: Hour is Directional =
        angle => Hour((angle.principal.degrees/30 + 0.5).toInt%12)

      val elsinore = Location(56.04.deg, 12.62.deg)
      val verona = Location(45.44.deg, 10.99.deg)
      val course: Hour = elsinore.bearing[Hour](verona)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(course.value.show)
```

Verona is at six o'clock from Elsinore.
[Compass points](../../soundness/modules/geography.md#compass-points) has the roses which are
provided, and [infix types](../../philosophy/infix-types.md) the words such types are made of.
