# One method draws every kind of chart

There is no `barChart`, `lineChart` or `pieChart`. Data is charted with `chart`, which takes
the kind as a value, and the drawing is made with `in[Svg]`, as an SVG is written `in[Xml]`.

```scala fuel
compile
alternative Bars
  highlight Bars()
  code
      val deaths = Series(t"deaths")((t"Hamlet", 8.0), (t"Macbeth", 7.0), (t"Lear", 10.0))

      val drawing: Svg = deaths.chart(Bars()).in[Svg]
alternative A pie
  highlight Pie()
  code
      val deaths = Series(t"deaths")((t"Hamlet", 8.0), (t"Macbeth", 7.0), (t"Lear", 10.0))

      val drawing: Svg = deaths.chart(Pie()).in[Svg]
alternative A histogram
  highlight Histogram()  a kind which draws samples, not points
  code
      val deaths = Samples(t"deaths")(8.0, 7.0, 10.0, 6.0, 5.0)

      val drawing: Svg = deaths.chart(Histogram()).in[Svg]
##
```

[Chart kinds](../../soundness/modules/charts.md#chart-kinds) lists the kinds, and
[rendering](../../soundness/modules/charts.md#rendering) what the drawing holds.
