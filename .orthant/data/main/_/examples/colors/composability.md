# A hue is turned by an angle

A hue is a point on the color wheel, so it is rotated by an `Angle`, the same type which
measures a bearing or a latitude. A bare number is not accepted, since it has no unit.

```scala fuel
compile
frame
  label A third of a turn, in no unit
  errors show
  code
      val tomato = WebColors.Tomato.to[Hsl]
      val triad = tomato.rotate(120.0)
frame
  label In degrees
  highlight 120.0.deg  an angle, as on a compass
  code
      val tomato = WebColors.Tomato.to[Hsl]
      val triad = tomato.rotate(120.0.deg)
frame
  label Past a full turn
  highlight 480.0.deg  a turn and a third
  code
      val tomato = WebColors.Tomato.to[Hsl]
      val triad = tomato.rotate(480.0.deg)
##
```

```scala fuel
run
##
import soundness.angleShowable
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(tomato.hue.show)
Out.println(triad.hue.show)
```

The rotation wraps, so both turns arrive at the same hue.
[Adjusting](../../soundness/modules/colors.md#adjusting) has the other adjustments, and
[Angles](../../soundness/modules/geography.md#angles) the type.
