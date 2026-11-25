# A bar at every width, for every value

A `Fraction` is clamped as it is made, and a design draws itself in exactly the cells it is
given. There is therefore no proportion and no width for which a bar overruns its row: it says
less as it narrows, down to a single cell.

```scala fuel
compile
frame
  label More than everything
  highlight Fraction(1.7)  clamped to 1
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Fraction

      val overfull = gaugeLine(Fraction(1.7), 24)
frame
  label Narrower and narrower
  highlight width  the bar is drawn in this many cells
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Fraction

      val overfull = gaugeLine(Fraction(1.7), 24)

      val narrowing = List(24, 8, 4, 1).map: width =>
        gaugeLine(Fraction(0.42), width)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xtermTrueColorTermcap

Out.println(overfull)
narrowing.each(Out.println(_))
```

[Degradation](../../soundness/modules/gauges.md#degradation) describes what each design sheds,
and [total transitions](../../philosophy/total-transitions.md) the principle.
