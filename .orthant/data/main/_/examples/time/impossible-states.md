# Two timelines which cannot be mixed

Unix time ignores leap seconds, and atomic time counts them, so the two disagree by a number
of seconds which grows. The timeline is part of an instant's type, and a difference between
instants on two timelines does not compile.

```scala fuel
compile
frame
  label Two instants on one timeline
  highlight landing..launch  a quantity of seconds
    line 6
  code
      import chronometries.unixChronometry

      val launch = Instant(1_497_484_800_000L)
      val landing = Instant(1_497_488_400_000L)

      val flight = landing - launch
frame
  label One of them, moved to atomic time
  errors show
  code
      import chronometries.unixChronometry

      val launch = Instant(1_497_484_800_000L)
      val landing = Instant(1_497_488_400_000L).over[Tai]

      val flight = landing - launch
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(flight.show)
```

[Leap seconds and atomic time](../../soundness/modules/time.md#leap-seconds-and-atomic-time)
explains what the conversion accounts for.
