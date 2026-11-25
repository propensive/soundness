# An appointment, written as it is said

A time of day is a number with `am` or `pm` after it, and `on` joins it to a date. Each part
is an ordinary expression, and each is checked: there is no thirteenth hour before noon.

```scala fuel
compile
frame
  label A time of day
  code
      val curtain = 7.30.pm
frame
  label On a date
  highlight on  from clock to calendar
  code
      import calendars.gregorianCalendar

      val curtain = 7.30.pm on 2018-Aug-11
frame
  label An hour the clock does not have
  errors show
  code
      import calendars.gregorianCalendar

      val curtain = 13.30.pm on 2018-Aug-11
##
```

[Times of day](../../soundness/modules/time.md#times-of-day) and
[timestamps](../../soundness/modules/time.md#timestamps) describe the two halves.
