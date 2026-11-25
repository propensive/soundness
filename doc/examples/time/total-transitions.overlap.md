# An hour which happens twice

When the clocks go back, an hour of wall-clock readings names two instants. A moment does not
guess between them in secret: it is the first occurrence unless the second is asked for, and
both resolve to a definite instant.

```scala fuel
compile
frame
  label Half past one, the first time
  code
      import calendars.gregorianCalendar

      val night = 2024-Oct-27
      val first = Moment(night, Clockface(1, 30, 0), tz"Europe/London")
frame
  label Half past one, the second time
  highlight Occurrence.Second  the later of the two
  code
      import calendars.gregorianCalendar

      val night = 2024-Oct-27
      val first = Moment(night, Clockface(1, 30, 0), tz"Europe/London")
      val second = Moment(night, Clockface(1, 30, 0), tz"Europe/London", Occurrence.Second)
frame
  label The physical time between them
  highlight second.instant.long..first.instant.long  in milliseconds
  code
      import calendars.gregorianCalendar

      val night = 2024-Oct-27
      val first = Moment(night, Clockface(1, 30, 0), tz"Europe/London")
      val second = Moment(night, Clockface(1, 30, 0), tz"Europe/London", Occurrence.Second)
      val apart = second.instant.long - first.instant.long
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(apart.show)
```

[Daylight-saving transitions](../../soundness/modules/time.md#daylight-saving-transitions)
covers the hour which never happens.
