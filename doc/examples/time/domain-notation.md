# A date the calendar does not have

A date is written as it would be on paper, and is an ordinary Scala expression which the
compiler checks against the calendar. The year 2011 had no 29th of February.

```scala fuel
compile
frame
  label Not a leap year
  errors show
  code
      import calendars.gregorianCalendar

      val leapDay = 2011-Feb-29
frame
  label A leap year
  highlight 2012-Feb-29  a `Date`, not text to be parsed
  code
      import calendars.gregorianCalendar

      val leapDay = 2012-Feb-29
##
```

The calendar is a choice, made by the import.
[Dates](../../soundness/modules/time.md#dates) describes the rest of the notation.
