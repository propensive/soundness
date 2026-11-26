# Today depends on where, and by which calendar

The current date is not a fact about the machine. It needs a clock, a time zone to read the
clock in, and a calendar to name the day, and `today()` asks for all three. A test supplies a
clock which stands still.

```scala fuel
compile
frame
  label A date, with nothing to compute it from
  errors show
  code
      val opening = today()
frame
  label A calendar, a zone and a clock
  highlight Clock.fixed  the first of January 1970, always
  code
      import calendars.gregorianCalendar, chronometries.unixChronometry

      given zone: Timezone = tz"Europe/London"
      given clock: Clock = Clock.fixed(Instant(0L))

      val opening = today()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import dateFormats.iso8601DateFormat

Out.println(opening.show)
```

[Instants](../../soundness/modules/time.md#instants) lists what each reading of the time
needs, and [honest signatures](../../philosophy/honest-signatures.md) why it is asked for.
