# Five working days, by the week and holidays in scope

Which days are worked depends on the country and the year. Arithmetic in working days takes
the shape of the week and the list of holidays from its scope, and does not compile without
them.

```scala fuel
compile
frame
  label Working days, with no week to count in
  errors show
  code
      import calendars.gregorianCalendar

      val delivery = 2025-Dec-24 + WorkingDays(1)
frame
  label A week which runs from Monday to Friday
  highlight europeanHebdomad  Saturday and Sunday are not worked
  code
      import calendars.gregorianCalendar, hebdomads.europeanHebdomad

      given holidays: Holidays = Holidays(List())

      val delivery = 2025-Dec-24 + WorkingDays(1)
frame
  label And two holidays
  highlight Holiday(2025-Dec-25,..Day")  skipped, like a weekend
  code
      import calendars.gregorianCalendar, hebdomads.europeanHebdomad

      given holidays: Holidays = Holidays(List
        ( Holiday(2025-Dec-25, t"Christmas Day"),
          Holiday(2025-Dec-26, t"Boxing Day") ))

      val delivery = 2025-Dec-24 + WorkingDays(1)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import dateFormats.iso8601DateFormat

Out.println(delivery.show)
```

[Arithmetic on dates](../../soundness/modules/time.md#arithmetic-on-dates) describes the two
givens.
