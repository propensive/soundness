# A month after the thirty-first of January

Adding days to a date always has an answer. Adding a month does not: there is no thirty-first
of February. No answer is picked silently, so the sum does not compile until a policy for the
end of a month is in scope.

```scala fuel
compile
frame
  label Days are regular
  code
      import calendars.gregorianCalendar

      val due = 2024-Jan-31 + 30*Day
frame
  label Months are not
  errors show
  code
      import calendars.gregorianCalendar

      val due = 2024-Jan-31 + 1*Month
frame
  label The last day which exists
  highlight clampMonthEnd  the last day of February
  code
      import calendars.gregorianCalendar, monthEnds.clampMonthEnd

      val due = 2024-Jan-31 + 1*Month
frame
  label Or the days left over, in the next month
  highlight overflowMonthEnd  spilling into March
  code
      import calendars.gregorianCalendar, monthEnds.overflowMonthEnd

      val due = 2024-Jan-31 + 1*Month
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import dateFormats.iso8601DateFormat

Out.println(due.show)
```

[Arithmetic on dates](../../soundness/modules/time.md#arithmetic-on-dates) has working days
too, and [total transitions](../../philosophy/total-transitions.md) the principle.
