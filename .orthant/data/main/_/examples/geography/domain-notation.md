# Degrees, minutes and seconds

A latitude is written as a navigator writes it: so many degrees, so many minutes of arc, so
many seconds. Each term is an `Angle`, and so is their sum.

```scala fuel
compile
frame
  label Whole degrees
  code
      val latitude: Angle = 56.0*Degree
frame
  label Minutes of arc
  highlight 30.0*ArcMinute  half a degree
  code
      val latitude: Angle = 56.0*Degree + 30.0*ArcMinute
frame
  label Seconds of arc
  highlight 36.0*ArcSecond  a hundredth of a degree
  code
      val latitude: Angle = 56.0*Degree + 30.0*ArcMinute + 36.0*ArcSecond
##
```

```scala fuel
run
##
import soundness.angleShowable
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(latitude.show)
Out.println((latitude*8.0).show)
```

The second line is eight times the latitude, which wraps past a full turn as every angle
does. [Angles](../../soundness/modules/geography.md#angles) has the two normal forms.
