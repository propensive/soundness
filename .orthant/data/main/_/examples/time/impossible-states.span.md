# A month is not a number of seconds

A physical instant moves by physical time, in hours and seconds. A month is a span on a
calendar, of no fixed length, so it cannot be added to an instant, though it can to a date.

```scala fuel
compile
frame
  label An hour later
  code
      import chronometries.unixChronometry

      val curtain: Instant over Unix = Instant(0L)
      val interval = curtain + 1*Hour
frame
  label A month later
  errors show
  code
      import chronometries.unixChronometry

      val curtain: Instant over Unix = Instant(0L)
      val revival = curtain + 1*Month
##
```

[Durations and timespans](../../soundness/modules/time.md#durations-and-timespans) draws the
line between the two kinds of elapsed time.
