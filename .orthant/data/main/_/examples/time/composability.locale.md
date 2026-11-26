# A recurrence which describes itself

A recurrence rule can say what it means in words, and the words belong to a language. The
description takes its month and weekday names from the locale in scope, and does not compile
without one.

```scala fuel
compile
frame
  label The fourth Thursday of November, with no language
  errors show
  code
      import calendars.gregorianCalendar

      val feast = Rrule(2024-Jan-1, Frequency.Yearly, byMonth = List(Nov),
          byDay = List(WeekdayOrdinal(Thu, 4)))

      val described: Text = feast.show
frame
  label In English
  highlight Locale(en)  the language of the description
  code
      import calendars.gregorianCalendar
      import monthFormats.englishMonths, weekdays.englishWeekdays

      given locale: Locale[en] = Locale(en)

      val feast = Rrule(2024-Jan-1, Frequency.Yearly, byMonth = List(Nov),
          byDay = List(WeekdayOrdinal(Thu, 4)))

      val described: Text = feast.show
frame
  label In French
  highlight Locale(fr)  another language, the same rule
  code
      import calendars.gregorianCalendar
      import monthFormats.frenchMonths, weekdays.frenchWeekdays

      given locale: Locale[fr] = Locale(fr)

      val feast = Rrule(2024-Jan-1, Frequency.Yearly, byMonth = List(Nov),
          byDay = List(WeekdayOrdinal(Thu, 4)))

      val described: Text = feast.show
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(described)
```

[Describing recurrences](../../soundness/modules/time.md#describing-recurrences) has more, and
[internationalization](../../soundness/modules/internationalization.md) describes locales.
