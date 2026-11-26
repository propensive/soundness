# Progress which is not known yet

A job may not know its total when it starts. Its progress is an `Optional[Fraction]`, which is
drawn by the same call: every design says what to draw when there is no value, so the type does
not change when the total arrives, and an unmeasured job is never shown as an empty bar.

```scala fuel
compile
frame
  label Not measured
  highlight Unset  the bar sweeps
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Fraction

      val unknown: Optional[Fraction] = Unset
      val waiting = gaugeLine(unknown, 30)
frame
  label Measured
  highlight Fraction.of(3,..8)  the bar fills
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Fraction

      val unknown: Optional[Fraction] = Unset
      val waiting = gaugeLine(unknown, 30)

      val known: Optional[Fraction] = Fraction.of(3, 8)
      val working = gaugeLine(known, 30)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xtermTrueColorTermcap

Out.println(waiting)
Out.println(working)
```

[Statuses](../../soundness/modules/gauges.md#statuses) describes the lifting, and
[optionality](../../philosophy/optionality.md) the type.
