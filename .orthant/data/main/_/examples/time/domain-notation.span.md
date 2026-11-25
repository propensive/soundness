# A span of the calendar, as a sum

A length of calendar time is a number multiplied by a unit, and several of them added
together. The result is a `Timespan`, whose parts are read back by name.

```scala fuel
compile
frame
  label Two years
  code
      val lease = 2*Year
frame
  label And three months
  highlight 3*Month  another unit, added
  code
      val lease = 2*Year + 3*Month
frame
  label And a fortnight
  highlight 14*Day  and another
  code
      val lease = 2*Year + 3*Month + 14*Day
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(lease.years.show)
Out.println(lease.months.show)
Out.println(lease.days.show)
Out.println(lease.encode)
```

The last line is the span in its ISO 8601 form.
[Durations and timespans](../../soundness/modules/time.md#durations-and-timespans) explains
why a span is not a number of seconds.
