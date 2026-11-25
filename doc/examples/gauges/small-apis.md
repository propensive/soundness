# The type of a status chooses its gauge

There is one method to draw a gauge, `gaugeLine`, and no parameter which says what kind. A
proportion is drawn as a bar, a count as a count and an outcome as a mark, because each is a
different type. A bare number is none of them, and is not drawn.

```scala fuel
compile
frame
  label A proportion
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.{Fraction, Reckoning, Standing}

      val bar = gaugeLine(Fraction(0.42), 30)
frame
  label A count
  highlight Reckoning(17,..120)  seventeen done, of 120
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.{Fraction, Reckoning, Standing}

      val bar = gaugeLine(Fraction(0.42), 30)
      val count = gaugeLine(Reckoning(17, 120), 7)
frame
  label An outcome
  highlight Standing.Succeeded
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.{Fraction, Reckoning, Standing}

      val bar = gaugeLine(Fraction(0.42), 30)
      val count = gaugeLine(Reckoning(17, 120), 7)
      val mark = gaugeLine(Standing.Succeeded, 1)
frame
  label A number which says nothing of what it measures
  errors show
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.{Fraction, Reckoning, Standing}

      val bar = gaugeLine(0.42, 30)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xtermTrueColorTermcap

Out.println(bar)
Out.println(count)
Out.println(mark)
```

[Statuses](../../soundness/modules/gauges.md#statuses) lists the types, and says why a
proportion is not a `Double`.
