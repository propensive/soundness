# The last Friday of every month

A recurrence which follows the calendar is described by naming what selects its days. The
rule reads as the phrase it stands for, and its occurrences are a lazy chain of dates.

```scala fuel
compile
frame
  label Every month, from a start
  code
      import calendars.gregorianCalendar, dateFormats.iso8601DateFormat

      val paydays = Rrule(2024-Jan-1, Frequency.Monthly, count = 3)
frame
  label On its last Friday
  highlight WeekdayOrdinal(Fri,..-1)  counted back from the end
  code
      import calendars.gregorianCalendar, dateFormats.iso8601DateFormat

      val paydays = Rrule(2024-Jan-1, Frequency.Monthly, count = 3,
          byDay = List(WeekdayOrdinal(Fri, -1)))
frame
  label As dates
  highlight occurrences  computed as they are asked for
  code
      import calendars.gregorianCalendar, dateFormats.iso8601DateFormat

      val paydays = Rrule(2024-Jan-1, Frequency.Monthly, count = 3,
          byDay = List(WeekdayOrdinal(Fri, -1)))

      val dates: Text = paydays.occurrences.map(_.show).to[List].join(t", ")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(dates)
```

[Recurrence rules](../../soundness/modules/time.md#recurrence-rules) has the table of fields.
