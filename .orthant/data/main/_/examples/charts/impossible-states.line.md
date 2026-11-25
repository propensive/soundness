# A line through names

Whether an axis is a run of labels or a run of numbers is decided by the type of the data. A
line needs a numeric abscissa, so one drawn through the names of plays is not a chart which
can be written.

```scala fuel
compile
frame
  label The abscissa is text
  errors show
  code
      val lines = Series(t"lines")((t"Hamlet", 4024.0), (t"Macbeth", 2477.0))

      val chart = lines.chart(Lines())
frame
  label Bars for categories
  highlight Bars()  draws a categorical abscissa
  code
      val lines = Series(t"lines")((t"Hamlet", 4024.0), (t"Macbeth", 2477.0))

      val chart = lines.chart(Bars())
frame
  label Or a line over years
  highlight 1601  a number has a position on an axis
  code
      val lines = Series(t"lines")((1601, 4024.0), (1606, 2477.0))

      val chart = lines.chart(Lines())
##
```

[Chart kinds](../../soundness/modules/charts.md#chart-kinds) explains the pairing of a kind
with the shape of its data.
