## Charts

### About

A chart turns a run of numbers into a picture: bars for categories, a line for a trend, wedges for
the parts of a whole, a histogram for a distribution. Soundness draws charts as
[SVG](svg.md) from typed data, in four separable layers — the shape of the data, the kind of
chart, the fit of one to the other, and the style — so that each can be chosen without disturbing
the rest, and a chart can be revised as new data arrives.

### On charting

Charting libraries usually take a bag of numbers and a bag of options, and discover at runtime
that the two do not go together: a pie chart of three series, a line through category labels, a
logarithmic axis over a range that includes zero. The decisions that ought to be separate — what
the data is, how it is drawn, how its axes are chosen, what it looks like — arrive tangled in one
call.

Charts whose kind is chosen by the shape of the data, and whose axes and style are chosen apart from it, are [decoupling](../philosophy/decoupling.md) between meaning and rendering.

Soundness keeps the four decisions apart. The *shape* of the data is in its types: a series over
`Text` is categorical, a series over `Double` or a `Duration` is continuous. A chart *kind* is
compatible with some shapes and not others, and the compiler checks the pairing. The *fit* of the
data to the axes — linear or logarithmic, padded to round numbers or tight, anchored at zero —
is a calibration chosen per axis type or passed explicitly. Everything about the *style* — size,
colors, type, legend — is a separate value. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Series

A `Series` is a named run of points, each an abscissa paired with an ordinate:

```scala
val north = Series(t"north")((t"jan", 3.0), (t"feb", 5.0), (t"mar", 4.0))
val south = Series(t"south")((t"jan", 2.0), (t"feb", 1.0), (t"mar", 6.0))
```

The abscissa type decides the axis. A `Text` (or an enum, or a `Category`) is `Categorical`: its
values are distinct labels in first-appearance order. A number, a `Duration`, an `Instant` or any
`Quantity` is `Continuous`: its values have positions on a numeric axis. Either typeclass can be
given for a type of your own. An `Estimate` is a continuous value with the interval it lies in,
which a chart draws as an error bar or a band:

```scala
val timings = Series(t"sort")((1000, Estimate(2.1, 1.9, 2.4)), (10000, Estimate(24.0, 22.5, 26.1)))
```

A series' name, like a sample set's or a `Category`, is any showable value — text, an enum
case, a number — rendered once when it is made. A series is immutable, and `add` appends a point
in constant amortized time, which is what a chart
fed one measurement at a time needs. Several series are any collection of them — a `List`, a
`Sequence`, a `Set` — since compatibility is stated for anything traversable by series.

### Chart kinds

A chart kind is a value: `Bars`, `StackedBars`, `Lines`, `Scatter`, `Pie`, `Histogram` or
`Boxes`. The `chart` method pairs data with a kind, and compiles only where the kind can draw the
data's shape:

```scala
val sales = List(north, south).chart(Bars())
```

Bars and pies want a categorical abscissa; lines and scatter plots want both axes continuous; a
pie takes exactly one series; a histogram and a box plot take `Samples` — raw measurements —
rather than points. A pairing that makes no sense is a compile error rather than a runtime
surprise:

```scala
north.chart(Lines())       // does not compile: a categorical series has no numeric abscissa
List(north, south).chart(Pie())  // does not compile: a pie is one series
```

The pairing is a `Plottable` instance, and a new kind of chart is a new value with instances for
the shapes it can draw; nothing else has to change.

### Fitting

Fitting chooses each axis's range and gradations from the data. The chart kind decides what the
axis must show — a bar chart's ordinate is anchored at zero, a line chart's is not — and a
`Calibration` decides how: linear or logarithmic positions, padded outward to whole steps or tight
to the data. The default is linear. A calibration is keyed on the type of the values on the axis,
so a scoped given changes every axis of that type, and an import changes every axis:

```scala
val sizes = Series(t"sort")((100, 1.2), (1000, 14.0), (10000, 160.0), (100000, 1900.0))

val logged =
  given Int is Calibration = calibrations.logarithmicCalibration
  sizes.chart(Lines())

logged.fit.abscissa.transform   // Scale.Transform.Logarithmic
```

A calibration can also be passed to a chart kind for one axis, when both axes share a type:

```scala
val scaled = sizes.chart(Lines(ordinate = calibrations.adaptiveCalibration))
```

The adaptive calibration goes logarithmic when the range spans three orders of magnitude, which
is what benchmark timings over input sizes usually want. The `Scale` a fit produces is plain
data: its bounds, its transform and its gradations, which are spaced at multiples of one, two and
five for numbers, and at clock steps — seconds, half-minutes, hours — for durations and instants.

### Style

A `Chart.Style` holds everything about appearance that is not data: the size, stroke width and
marker radius, the typeface, how densely axes are graduated, where the legend goes and the axis
titles. Colors are a `ChartPalette`, named by role — the series ramp, the axes, the grid, the
lettering — so that one chart renders under any palette. Both have no-import defaults; a palette
is chosen by importing one, and a style by giving one:

```scala
import palettes.solarizedDarkChartPalette

given Chart.Style = Chart.Style(width = 800, height = 300, ordinateTitle = t"units sold")
```

An axis over a [quantity](quantities.md) is titled by what it measures and its unit, taken from
the type: a series of `Quantity[Metres[1]]` values has the ordinate title `distance / m`, and one
of `Metre/Second` values `velocity / m·s¯¹`, with the units rendered as they are everywhere
else in Soundness. A title given in the style replaces the name but keeps the unit:

```scala
val tides = Series(t"tide")((0.0, 1.2*Metre), (6.0, 4.6*Metre), (12.0, 1.1*Metre))
tides.chart(Lines()).svg.xml.show.contains(t"distance / m")   // true
```

A `Duration` axis is titled `time`, and its gradations carry their own units — `250ms`,
`1m30s` — since clock steps are not decimal.

Text is measured to lay out the axis labels and legend. Without a font in scope, an average width
per character is assumed; with a [font](fonts.md) loaded, its own glyph advances are used, and
the margins fit their labels exactly:

```scala
import fontMetrics.averageFontMetric
```

The face measured should be the one the page will render, since the SVG does not embed it.

### Rendering

A chart renders to a savagery `Svg`, which serializes to XML text like any other:

```scala
val drawing = sales.svg
drawing.xml.show.keep(60)   // <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 300" …
```

Every part of the drawing is a group with a stable identifier — `grid`, `abscissa`, `ordinate`,
`legend` and `series-0`, `series-1`, … — which is what makes the chart revisable.

### Revision

A chart is immutable. Given new data, `revise` yields the next chart and a list of what changed on
the page. If the fit still accommodates the new data, the axes hold still and only the parts that
differ are replaced; if a point falls outside them, the whole chart is redrawn:

```scala
val latency = Series(t"p99")((0.0, 12.0), (10.0, 14.0)).chart(Lines())
val (next, revisions) = latency.revise(latency.data.add((5.0, 13.0)))
revisions.length   // 1: a Chart.Revision.Replace of `series-0`
```

A page holding the SVG needs only two operations, replace an element by its identifier and
replace the whole drawing, so a chart can be kept current over a WebSocket by sending each
revision as it is produced.

### Distributions

Raw measurements are `Samples`. A `Histogram` cuts their range into equal bins — by Sturges' rule
unless a bin count is given — and counts the samples in each; `Boxes` summarizes each set of
samples as a box between its quartiles with whiskers to its extremes:

```scala
val runs = List(Samples(t"before")(21.0, 22.5, 23.1, 22.8, 24.0), Samples(t"after")(18.2, 18.9, 19.5, 18.1, 19.0))
runs.chart(Histogram()).fit.edges.size
runs.chart(Boxes()).fit.summaries.size   // 2
```

Both accept any collection of sample sets, as the other kinds accept collections of series.
