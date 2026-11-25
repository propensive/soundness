# Counting the points of a period

Walking a period at a step gives a lazy chain, and a chain which spans years costs nothing
until its points are demanded. Counting them walks every one, so the count does not compile
until an import acknowledges that it is linear.

```scala fuel
compile
frame
  label A point for each day of six years
  highlight by(1*Day)  nothing is computed yet
  code
      import calendars.gregorianCalendar

      val season = Period(2024-Jan-1, 2030-Jan-1).by(1*Day)
frame
  label Counting them
  errors show
  code
      import calendars.gregorianCalendar

      val season = Period(2024-Jan-1, 2030-Jan-1).by(1*Day)
      val nights = season.to[List].size
frame
  label The cost, acknowledged
  highlight linearSize  every point is visited
  code
      import calendars.gregorianCalendar, dysasymptotics.linearSize

      val season = Period(2024-Jan-1, 2030-Jan-1).by(1*Day)
      val nights = season.to[List].size
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(nights.show)
```

[Periods](../../soundness/modules/time.md#periods) describes `by` and `segments`.
