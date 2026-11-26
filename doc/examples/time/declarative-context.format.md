# One date, written three ways

A date has no text form of its own. The format is an import, which fixes the convention for
every date shown in its scope.

```scala fuel
compile
alternative ISO 8601
  highlight iso8601DateFormat  year, month, day
  code
      import calendars.gregorianCalendar
      import dateFormats.iso8601DateFormat

      val opening: Text = (2025-Apr-7).show
alternative American
  highlight americanDateFormat  month, day, year
  code
      import calendars.gregorianCalendar
      import dateFormats.americanDateFormat

      val opening: Text = (2025-Apr-7).show
alternative European
  highlight europeanDateFormat  day, month, year
  code
      import calendars.gregorianCalendar
      import dateFormats.europeanDateFormat

      val opening: Text = (2025-Apr-7).show
##
```

[Formatting dates and times](../../soundness/modules/time.md#formatting-dates-and-times) shows
each result, and composes a format from smaller choices.
