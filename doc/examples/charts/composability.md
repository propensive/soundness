# An axis titled by its unit

A series may hold quantities instead of bare numbers. The chart knows nothing of physics, and
the quantity nothing of charts, yet the axis is titled with what is measured and its unit,
both read from the type.

```scala fuel
compile
frame
  label A series of numbers
  code
      val tide = Series(t"Dover")((0.0, 1.2), (6.0, 6.3), (12.0, 1.1))

      val drawing = tide.chart(Lines()).in[Svg].in[Xml].show
frame
  label A series of lengths
  highlight *Metre  the ordinate is now a quantity of length
  code
      val tide = Series(t"Dover")((0.0, 1.2*Metre), (6.0, 6.3*Metre), (12.0, 1.1*Metre))

      val drawing = tide.chart(Lines()).in[Svg].in[Xml].show
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(drawing.contains(t"distance / m").inspect)
```

[Style](../../soundness/modules/charts.md#style) describes the titles of axes, and
[quantities](../../soundness/modules/quantities.md) the units themselves.
