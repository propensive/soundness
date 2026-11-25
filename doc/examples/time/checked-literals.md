# A time zone which was never defined

The name of a time zone is checked against the time-zone database as the code compiles. A
misspelled zone is a compile error, and never a lookup which fails in production.

```scala fuel
compile
frame
  label A zone the database does not have
  errors show
  code
      val globe = tz"Europe/Londinium"
frame
  label A zone it has
  highlight tz"Europe/London"  in the tz database
  code
      val globe = tz"Europe/London"
frame
  label A wall clock there, as a physical instant
  highlight globe  the zone decides the offset
    line 5
  code
      import calendars.gregorianCalendar

      val globe = tz"Europe/London"

      val curtain = Moment(1599-Jun-12, Clockface(14, 0, 0), globe).instant
##
```

[Time zones and moments](../../soundness/modules/time.md#time-zones-and-moments) says how a
zone connects a clock to physical time.
