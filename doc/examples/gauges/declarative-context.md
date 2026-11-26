# A design chosen by import

The line which draws a progress bar says what is measured and how wide to draw it. What the
bar looks like, and which characters it may use, are decided by imports, and each can change
without the others.

```scala fuel
compile
alternative The default
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Fraction

      val bar = gaugeLine(Fraction(0.42), 30)
alternative An arrowhead
  highlight bars.arrowheadBar  another design for a Fraction
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Fraction
      import bars.arrowheadBar

      val bar = gaugeLine(Fraction(0.42), 30)
alternative Seven-bit characters
  highlight gaugeGlyphs.asciiGlyphs  degrades every design at once
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Fraction
      import gaugeGlyphs.asciiGlyphs

      val bar = gaugeLine(Fraction(0.42), 30)
##
```

[Choosing a design](../../soundness/modules/gauges.md#choosing-a-design) and
[colors and glyphs](../../soundness/modules/gauges.md#colors-and-glyphs) show each of them
drawn, and [declarative context](../../philosophy/declarative-context.md) explains the imports.
