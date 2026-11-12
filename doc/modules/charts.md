## Charts

### About

A chart turns a run of numbers into a picture: bars for categories, a line for a trend, wedges
for the parts of a whole, a histogram for a distribution. Charts are drawn here as [SVG](svg.md)
from typed data, in four separable layers: the shape of the data, the kind of chart, the fit of
one to the other, and the style. Each can be chosen without disturbing the rest, and a chart can
be revised as new data arrives.

### On charting

Charting libraries usually take a bag of numbers and a bag of options, and discover at [runtime]
that the two do not go together: a pie chart of three series, a line through category labels, a
logarithmic axis over a range which includes zero. The decisions which ought to be separate,
which are what the data is, how it is drawn, how its axes are chosen and what it looks like,
arrive tangled in one call.

Here the four decisions are kept apart. The *shape* of the data is in its types. A series over
`Text` is categorical, and a series over `Double` or a `Duration` is continuous. A chart *kind*
is compatible with some shapes and not others, and the compiler checks the pairing. The *fit* of
the data to the axes, which is linear or logarithmic, padded to round numbers or tight, anchored
at zero or not, is a calibration chosen for each axis type, or passed explicitly. Everything
about the *style*, which is size, colors, type and legend, is a separate value. This is
[decoupling](../philosophy/decoupling.md) of meaning from rendering.

Everything comes from the `soundness` package. The second import chooses a [strategy] for
errors, and the third lets the samples print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Series

A `Series` is a named run of points, each an abscissa paired with an ordinate:

```scala
val north = Series(t"north")((t"jan", 3.0), (t"feb", 5.0), (t"mar", 4.0))
val south = Series(t"south")((t"jan", 2.0), (t"feb", 1.0), (t"mar", 6.0))
```

The type of the abscissa decides the axis. A `Text`, an [enumeration] or a `Category` is
`Categorical`: its values are distinct labels, in the order of their first appearance. A number,
a `Duration`, an `Instant` or any `Quantity` is `Continuous`: its values have positions on a
numeric axis. Either [typeclass] can be given for a type of your own.

A `Chart.Estimate` is a continuous value with the interval it lies in, which a chart draws as an
error bar or a band:

```scala
val timings =
  Series(t"sort")((1000, Chart.Estimate(2.1, 1.9, 2.4)), (10000, Chart.Estimate(24.0, 22.5, 26.1)))
```

A value may also carry a note, as `Noted(3.0, t"Paris")`, which a scatter plot or a line chart
sets beside the point. The name of a series, like that of a sample set or a `Category`, is any
showable value, such as text, an enumeration case or a number, rendered once when it is made.

A series is [immutable], and `add` appends a point in constant amortized time, which is what a
chart fed one measurement at a time needs. Several series are any collection of them, such as a
`List`, a `Sequence` or a `Set`, since compatibility is stated for anything traversable by
series.

### Chart kinds

A chart kind is a value, and each draws data of its own shape:

```tabular
column  Kind
column  Draws
column  Abscissa
column  Ordinate
column  Style
row  `Bars()`  a series, or a collection of series  categorical  continuous  `Bars.Style`
row  `StackedBars()`  a collection of series  categorical  continuous  `StackedBars.Style`
row  `Lines()`  a series, or a collection of series  continuous  continuous  `Lines.Style`
row  `Scatter()`  a series, or a collection of series  continuous  continuous  `Scatter.Style`
row  `Pie()`  exactly one series  categorical  continuous  `Pie.Style`
row  `Histogram()`  `Samples`, or a collection of them  continuous  counts  `Histogram.Style`
row  `Boxes()`  `Samples`, or a collection of them  one box per set  continuous  `Boxes.Style`
row  `Doughnut()`  a `Dag` of `Weighted`, `Progressive` nodes  none  none  `Doughnut.Style`
```

The `chart` method pairs data with a kind, and compiles only where the kind can draw the shape
of the data:

```scala
val sales = List(north, south).chart(Bars())
```

A histogram and a box plot take `Samples`, which are raw measurements, rather than points. A
pairing which makes no sense is a compile error, not a surprise at runtime. A categorical series
has no numeric abscissa for a line:

```scala fuel
compile
errors show
##
north.chart(Lines())
```

A pie is one series:

```scala fuel
compile
errors show
##
List(north, south).chart(Pie())
```

The pairing is a `Plottable` instance. A new kind of chart is a new value with instances for the
shapes it can draw, and nothing else has to change.

### Fitting

Fitting chooses the range and gradations of each axis from the data. The chart kind decides what
the axis must show. The ordinate of a bar chart is anchored at zero, and that of a line chart is
not. A `Calibration` decides how: linear or logarithmic positions, padded outward to whole steps
or tight to the data. The default is linear.

A calibration is keyed on the type of the values on the axis, so a scoped given changes every
axis of that type, and an import changes every axis:

```scala
val sizes = Series(t"sort")((100, 1.2), (1000, 14.0), (10000, 160.0), (100000, 1900.0))
```

```scala fuel
run
##
val logged =
  given Int is Calibration = calibrations.logarithmicCalibration
  sizes.chart(Lines())

Out.println(logged.fit.abscissa.transform.inspect)
Out.println(sizes.chart(Lines()).fit.abscissa.transform.inspect)
```

A calibration can also be passed to a chart kind for one axis, when both axes share a type:

```scala
val scaled = sizes.chart(Lines(ordinate = calibrations.adaptiveCalibration))
```

The calibrations are givens in the `calibrations` package:

```tabular
column  Calibration
column  Positions
column  Range
row  `linearCalibration`  linear  padded outward to whole gradations; the default
row  `logarithmicCalibration`  logarithmic, or linear when the data reaches zero  whole decades
row  `adaptiveCalibration`  logarithmic when the range spans three orders of magnitude, else linear  padded
row  `tightCalibration`  linear  exactly the extent of the data
```

The adaptive calibration is what benchmark timings over input sizes usually want. The `Scale`
which a fit produces is plain data: its bounds, its transform and its gradations. Gradations are
spaced at multiples of one, two and five for numbers, and at clock steps, such as seconds,
half-minutes and hours, for durations and instants.

### Style

Everything about the appearance of a chart which is not data is a *style*: a trait with a method
for each component the chart draws, such as a tick, a tick label, a bar, a wedge or a legend
swatch. Each method takes the geometry which the chart computed, and returns the figures to
draw. `Chart.Style` holds what every kind shares: the canvas, the typeface, the legend and how
text is set. `Chart.Cartesian` adds axes, gradations, grid, titles, markers and error bars. Each
kind adds its own: `Bars.Style` has `bar`, `Lines.Style` has `line` and `band`, and `Pie.Style`
has `wedge`. A chart asks for the style of its kind, so a bar chart compiles against
`Bars.Style`.

`Chart.Standard` implements all of them. It is a case class, so its parameters are named
arguments, and any component is an override:

```scala
given Chart.Standard = new Chart.Standard(width = 800.0, ordinateTitle = t"units sold"):
  override def bar(corner: Point, width: Double, height: Double, color: Color in Srgb, series: Int)
  :   List[Figure] =
    List(Rectangle(corner, width.toFloat, height.toFloat, style = Css.Style(fill = color.to[Srgb])))

  override def arrowhead(tip: Point, axis: Chart.Axis, color: Color in Srgb): List[Figure] =
    List(Polyline(List(tip, Point(tip.x - 6, tip.y - 3), Point(tip.x - 6, tip.y + 3)), closed = true))
```

The hooks cover the details on which a chart is usually judged:

```tabular
column  Hook
column  Draws
column  Defined on
row  `fontStyle`, `lettering`  the size and colour of text; every run of text  `Chart.Style`
row  `backdrop`  the canvas behind the chart  `Chart.Style`
row  `swatch`, `legendLabel`  a legend entry  `Chart.Style`
row  `leader`  the line from a moved label back to its point  `Chart.Style`
row  `axisLine`  the stroke of each axis  `Chart.Cartesian`
row  `axisBreak`  a break mark where a linear axis starts away from zero  `Chart.Cartesian`
row  `arrowhead`  the end of an axis  `Chart.Cartesian`
row  `tick`, `gridLine`  a gradation and its grid line  `Chart.Cartesian`
row  `tickLabel`  a tick's label, where it sits and whether it is rotated  `Chart.Cartesian`
row  `axisTitle`  an axis's title  `Chart.Cartesian`
row  `marker`, `errorBar`  a point and its interval  `Chart.Cartesian`
row  `pointLabel`  the note of a `Noted` point  `Chart.Cartesian`
row  `bar`  a bar  `Bars.Style`
row  `line`, `band`  a series line and an estimate's band  `Lines.Style`
row  `box`, `whisker`, `median`  a box plot's parts  `Boxes.Style`
row  `wedge`, `wedgeLabel`  a wedge and its percentage  `Pie.Style`
row  `shade`, `sector`, `remainder`  a sector's colour and its two parts  `Doughnut.Style`
```

`labelRoom` tells the layout how much space the tick labels take, so a style which rotates them
overrides it beside `tickLabel`.

Colors are a `ChartPalette`, named by role: the series ramp, the axes, the grid, the lettering.
One style therefore renders under any palette, and a palette is chosen by importing one:

```scala
import palettes.solarizedDarkChartPalette
```

The `font` of a style is a `Font in Web`: a face paired with a provision for its typeface, as
[fonts](fonts.md#providing-a-typeface) describes. A chart can therefore be set only in a
typeface which its SVG will carry a `@font-face` for, or in a generic family, which is the
default. A provision is declared once, and the font is chosen in the style:

```scala
given menloProvision: ((Typeface of "Menlo") is Typesettable in Web) = Web.local()

val monospaced = Chart.Standard(font = Web.font(Typeface["Menlo"].face))
```

Text is measured to lay out the axis labels and the legend. The glyph advances of an embedded
font are used, so the face measured is the face rendered, and the margins fit their labels
exactly. For a font without a file to hand, an average width for each character is assumed.
Either can be overridden by importing a metric:

```scala
import fontMetrics.averageFontMetric
```

An axis over a [quantity](quantities.md) is titled by what it measures and its unit, taken from
the type. A series of `Quantity[Metres[1]]` values has the ordinate title `distance / m`, and
one of `Metre/Second` values has `velocity / m·s¯¹`, with the units rendered as they are
everywhere else. A title given in the style replaces the name but keeps the unit. The style
given above titles the ordinate `units sold`, so this chart of heights in meters is titled
`units sold / m`:

```scala
val tides = Series(t"tide")((0.0, 1.2*Metre), (6.0, 4.6*Metre), (12.0, 1.1*Metre))
```

```scala fuel
run
##
Out.println(tides.chart(Lines()).in[Svg].in[Xml].show.contains(t"units sold / m").inspect)
```

A `Duration` axis is titled `time`, and its gradations carry their own units, such as `250ms`
and `1m30s`, since clock steps are not decimal.

### Placing labels

The text on a chart is placed by [label placement](label-placement.md), so labels do not
collide. Tick labels, axis titles and legend entries hold still, and a tick label which would
overlap its neighbor is left out. The note beside a `Noted` point, and the percentage in the
wedge of a pie, move. A point's note is set to its right when that side is clear of other
notes, markers and lines, on another side when it is not, and further off, with a leader line
back to the point, when every side is taken. A wedge's percentage sits in its wedge, or is moved
outward with a leader when the label of a neighboring wedge is in the way.

The `Arranger` in scope decides the arrangement. The default places each label in turn. For a
crowded chart, simulated annealing considers the labels together, and is chosen by importing
it:

```scala
import arrangers.annealingArranger
```

A style draws each label where the arrangement put it. `tickLabel`, `pointLabel` and
`wedgeLabel` take a `Chart.Anchoring`, which is the point and the alignment which sets the text
on the right side of it, and `leader` draws the line from a moved label back to its point.

### Rendering

A chart renders to an `Svg`, which serializes to XML text like any other:

```scala fuel
run
##
val drawing = sales.in[Svg]

Out.println(drawing.in[Xml].show.keep(70))
```

Every part of the drawing is a group with a stable identifier, which is what makes the chart
revisable:

```tabular
column  Identifier
column  Holds
column  In
row  `backdrop`  the canvas, in the palette's background colour  every chart
row  `grid`  the grid lines  cartesian charts
row  `abscissa`, `ordinate`  each axis, its gradations and its title  cartesian charts
row  `legend`  the legend  charts with a legend
row  `series-0`, `series-1`, …  each series  cartesian charts
row  `wedges`  every wedge and its label  `Pie`
row  `rank-0`, `rank-1`, …  each band  `Doughnut`
```

### Revision

A chart is immutable. Given new data, `revise` gives the next chart and a list of what changed
on the page. If the fit still accommodates the new data, the axes hold still, and only the parts
which differ are replaced. If a point falls outside them, the whole chart is redrawn:

```scala fuel
run
##
import dysasymptotics.linearSize

val latency = Series(t"p99")((0.0, 12.0), (10.0, 14.0)).chart(Lines())
val (next, revisions) = latency.revise(latency.data.add((5.0, 13.0)))

Out.println(revisions.size.show)
```

The one revision is a `Chart.Revision.Replace` of `series-0`. A page which holds the SVG needs
only two operations, which are to replace an element by its identifier and to replace the whole
drawing, so a chart can be kept current over a WebSocket by sending each revision as it is
produced.

### Distributions

Raw measurements are `Samples`. A `Histogram` cuts their range into equal bins, by Sturges' rule
unless a count of bins is given, and counts the samples in each. `Boxes` summarizes each set of
samples as a box between its quartiles, with whiskers to its extremes:

```scala fuel
run
##
import dysasymptotics.linearSize

val runs = List
  ( Samples(t"before")(21.0, 22.5, 23.1, 22.8, 24.0),
    Samples(t"after")(18.2, 18.9, 19.5, 18.1, 19.0) )

Out.println(runs.chart(Histogram()).fit.edges.size.show)
Out.println(runs.chart(Boxes()).fit.summaries.size.show)
```

Both accept any collection of sample sets, as the other kinds accept collections of series.

### Doughnuts

A `Doughnut` draws a dependency graph, a [`Dag`](graphs.md), as concentric bands. The nodes
which depend on nothing are in the innermost band, and each further band holds the nodes one
rank further out, so a build fills in from the center outwards. Each node is a sector of its
band, which sweeps an angle proportional to its size within the rank, and a band is as thick as
its rank's share of the total size.

The size of a node and its progress, from `0.0` to `1.0`, come from two typeclasses on the node
type, `Weighted` and `Progressive`. The sector fills clockwise as far as the node is complete,
with the remainder in a subdued tint of the sector's color. That color is the first color of the
palette, shaded by where the sector lies: its hue turns through thirty degrees from left to
right, and it is darker below the middle and lighter above. A doughnut has no labels and no
legend. Each band is a `rank-0`, `rank-1`, … group, each sector a `sector-0`, `sector-1`, …
path, and the unfinished part of a sector is `sector-0-rest`.

A node carries its progress, so a doughnut is revised like any chart, with new data. The fit
survives any graph of the same shape and sizes, and only the bands whose sectors have filled
further are replaced:

```scala
case class Step(name: Text, size: Double, done: Double)
given stepWeighted: (Step is Weighted) = _.size
given stepProgressive: (Step is Progressive) = _.done

val parse = Step(t"parse", 2.0, 1.0)
val typecheck = Step(t"typecheck", 3.0, 0.5)
val emit = Step(t"emit", 1.0, 0.0)
val doughnut = Dag(typecheck -> parse, emit -> typecheck).chart(Doughnut())
```

```scala fuel
run
##
import dysasymptotics.linearSize

Out.println(doughnut.fit.bands.size.show)

val typechecked = typecheck.copy(done = 1.0)
Out.println(doughnut.revise(Dag(typechecked -> parse, emit -> typechecked))(1).size.show)
```

There are three bands, one for each rank. Finishing `typecheck` replaces only `rank-1`.

`Chart.Standard(hole = 0.3)` hollows the center. `sectorGap` and `bandGap` are the widths of the
grout between sectors and between bands. `Doughnut.Style` has `shade` for the color of a sector
from its position, and `sector` and `remainder` for its two parts. The ranking is the one
[graphs](graphs.md#layering) describes: `import rankings.balancedRanking` draws each node as
close to its dependents as it can be, rather than as close to the center.

### Related

- [SVG](svg.md) describes the drawing which a chart renders to.
- [Label placement](label-placement.md) describes how the labels of a chart avoid each other.
- [Fonts](fonts.md) describes the typefaces a chart may be set in.
- [Quantities](quantities.md) describes the units which title an axis.
- [Graphs](graphs.md) describes the `Dag` which a doughnut draws.
- [Colors](colors.md) describes the palette's colors.
- [Time](time.md) describes the durations and instants which a continuous axis may hold.
- [Gauges](gauges.md) and [tables](tables.md) present values in the terminal, and [testing](testing.md) charts benchmark results.
