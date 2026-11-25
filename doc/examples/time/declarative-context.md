# The same date, in another calendar

Whether a date exists depends on the calendar, and the calendar is chosen by an import. The
year 1900 was a leap year in the Julian calendar, and was not one in the Gregorian.

```scala fuel
compile
frame
  label The Gregorian calendar has no such day
  errors show
  code
      import calendars.gregorianCalendar

      val leapDay = 1900-Feb-29
frame
  label The Julian calendar has
  highlight julianCalendar
  code
      import calendars.julianCalendar

      val leapDay = 1900-Feb-29
##
```

Nothing but the import changed. It decides for every date in its scope, as the compiler reads
it. [Declarative context](../../philosophy/declarative-context.md) explains why a choice like
this is not an argument.
