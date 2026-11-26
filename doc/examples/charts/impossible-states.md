# A pie chart of two series

A chart kind draws data of one shape. A pie is the parts of a single whole, so it takes one
series, and a list of two is refused by the compiler and not discovered on the page.

```scala fuel
compile
frame
  label Two series in one pie
  errors show
  code
      val comedies = Series(t"comedies")((t"verse", 61.0), (t"prose", 39.0))
      val tragedies = Series(t"tragedies")((t"verse", 78.0), (t"prose", 22.0))

      val chart = List(comedies, tragedies).chart(Pie())
frame
  label Two series, side by side
  highlight Bars()  a kind which draws any number of series
  code
      val comedies = Series(t"comedies")((t"verse", 61.0), (t"prose", 39.0))
      val tragedies = Series(t"tragedies")((t"verse", 78.0), (t"prose", 22.0))

      val chart = List(comedies, tragedies).chart(Bars())
frame
  label One series in a pie
  highlight comedies.chart(Pie())  one whole, and its parts
  code
      val comedies = Series(t"comedies")((t"verse", 61.0), (t"prose", 39.0))
      val tragedies = Series(t"tragedies")((t"verse", 78.0), (t"prose", 22.0))

      val chart = comedies.chart(Pie())
##
```

[Chart kinds](../../soundness/modules/charts.md#chart-kinds) has the table of kinds and shapes,
and [impossible states](../../philosophy/impossible-states.md) the principle.
