# An ISO 8601 timestamp, checked digit by digit

A timestamp copied from a log or a specification is written as a `ts` literal. The compiler
reads it against the calendar and the clock, so the thirtieth of February is rejected in this
form as it is in any other.

```scala fuel
compile
frame
  label A day February does not have
  errors show
  code
      import soundness.tsInterpolator
      import calendars.gregorianCalendar

      val premiere = ts"2024-02-30T19:30:00"
frame
  label A day it has
  highlight ts"2024-02-29T19:30:00"  a `Timestamp`, already parsed
  code
      import soundness.tsInterpolator
      import calendars.gregorianCalendar

      val premiere = ts"2024-02-29T19:30:00"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(premiere.inspect)
```

[Timestamps](../../soundness/modules/time.md#timestamps) shows the other ways to write one.
