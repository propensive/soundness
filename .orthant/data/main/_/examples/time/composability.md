# A speed from two instants

The difference between two instants is a quantity of seconds, the same type the library of
physical units works in. A distance divided by it is a speed, with nothing converted by hand.

```scala fuel
compile
frame
  label Two instants
  code
      import chronometries.unixChronometry

      val start: Instant over Unix = Instant(0L)
      val finish: Instant over Unix = Instant(9_580L)
frame
  label The time between them
  highlight finish..start  a quantity of seconds
    line 6
  code
      import chronometries.unixChronometry

      val start: Instant over Unix = Instant(0L)
      val finish: Instant over Unix = Instant(9_580L)

      val elapsed = finish - start
frame
  label A distance, divided by it
  highlight 100*Metre/elapsed  meters per second
  code
      import chronometries.unixChronometry

      val start: Instant over Unix = Instant(0L)
      val finish: Instant over Unix = Instant(9_580L)

      val elapsed = finish - start
      val speed = 100*Metre/elapsed
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(elapsed.show)
Out.println(speed.show)
```

[Durations and timespans](../../soundness/modules/time.md#durations-and-timespans) says why a
duration is a quantity and a number of months is not, and
[quantities](../../soundness/modules/quantities.md#arithmetic-and-dimensions) how the units
combine.
