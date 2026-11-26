# A chart which is revised, not redrawn

A chart is a value, and new data does not change it. `revise` gives the next chart, together
with a list of the parts of the drawing which differ, so a page replaces only those.

```scala fuel
compile
frame
  label A chart
  code
      val audience = Series(t"Globe")((1.0, 1200.0), (3.0, 1900.0)).chart(Lines())
frame
  label Its data, with one more point
  highlight add  gives a new series
  code
      val audience = Series(t"Globe")((1.0, 1200.0), (3.0, 1900.0)).chart(Lines())
      val data = audience.data.add((2.0, 1500.0))
frame
  label The next chart, and what changed
  highlight revise  gives a new chart and its differences
  code
      val audience = Series(t"Globe")((1.0, 1200.0), (3.0, 1900.0)).chart(Lines())
      val data = audience.data.add((2.0, 1500.0))
      val (next, revisions) = audience.revise(data)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import dysasymptotics.linearSize

Out.println(revisions.size.show)
```

The new point lies within the axes, so the one revision replaces the series and nothing else.
[Revision](../../soundness/modules/charts.md#revision) describes when a whole chart is redrawn.
