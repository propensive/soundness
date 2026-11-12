## SVG

### About

[SVG](https://en.wikipedia.org/wiki/SVG) drawings are built from typed values: shapes with
typed coordinates, paths which are assembled a step at a time, transforms and gradients.
They are composed into an `Svg` document, which is written as XML.

Drawing tools suit artwork which is made once. Generating SVG from a program, as a chart, a
diagram, or a figure for each point of data, wants values which compose, and typed points
and vectors instead of strings of numbers.

### On vector graphics

An SVG document is XML which is full of little languages. The `d` attribute of a path is a
program of coordinates. `transform` is a pipeline of functions. Colors and units have
syntaxes of their own. A program which generates SVG by joining strings must get all of
them right at once, and a misplaced coordinate produces a drawing which is silently wrong.

Here the structure is built from values. A `Point` is a position, and a `Delta` is a
displacement. They are distinct types, with vector arithmetic between them. Paths grow by
named steps, and each little language is written by the types which understand it. Figures
which are typed values, with coordinates which are numbers and not text, follow
[impossible states](../philosophy/impossible-states.md): a malformed drawing cannot be
written.

Everything comes from the `soundness` package. The last import lets the samples print their
results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Shapes

Every figure is written as one SVG element:

```tabular
column  Figure
column  Element
row  `Rectangle`  `<rect>`
row  `Circle`  `<circle>`
row  `Ellipse`  `<ellipse>`, or `<circle>` when its radii are equal
row  `Outline`  `<path>`
row  `Polyline`  `<polyline>`, or `<polygon>` when it is `closed`
row  `Group`  `<g>`
row  `Lettering`  `<text>`
```

Rectangles, circles and ellipses are values, positioned by points. A pair of numbers
converts to a point where one is expected. `in[Xml]` writes a figure as [XML](xml.md):

```scala fuel
run
##
Out.println(Rectangle((0, 0), 10, 5).in[Xml].show)
Out.println(Circle((0, 0), 5).in[Xml].show)
```

### Paths

An `Outline` is a path which is built a step at a time. Absolute steps have names which end
in `To`, and take `Point`s. Relative steps do not, and take `Delta`s. There are curves,
quadratic curves, and a `closed` end:

```tabular
column  Absolute
column  Relative
column  Path command
row  `moveTo(point)`  `move(delta)`  `M`, `m`
row  `lineTo(point)`  `line(delta)`  `L`, `l`
row  `curveTo(control1, control2, point)`  `curve(control1, control2, delta)`  `C`, `c`
row  `curveTo(control2, point)`  `curve(control2, delta)`  `S`, `s`
row  `quadCurveTo(control, point)`  `quadCurve(control, delta)`  `Q`, `q`
row  `quadCurveTo(point)`  `quadCurve(delta)`  `T`, `t`
row
  cell
  cell  `lineUp(n)`, `lineDown(n)`, `lineLeft(n)`, `lineRight(n)`
  cell  `l`
row
  cell
  cell  `moveUp(n)`, `moveDown(n)`, `moveLeft(n)`, `moveRight(n)`
  cell  `m`
row
  cell  `closed`
  cell
  cell  `Z`
```

```scala
val plus = Outline().moveTo((0, 0))
  . lineUp(2).lineLeft(2).lineUp(1).lineRight(2)
  . lineUp(2).lineRight(1).lineDown(2).lineRight(2)
  . lineDown(1).lineLeft(2).lineDown(2).closed
```

```scala fuel
run
##
Out.println(Outline().moveTo((0, 0)).curveTo((1, 1), (2, 1), (3, 0)).in[Xml].show)
Out.println(plus.in[Xml].show)
```

### Points and deltas

Positions and displacements are different things, and different types:

```tabular
column  Expression
column  Type
row  `Point + Delta`, `Point - Delta`  `Point`
row  `Point - Point`  `Delta`
row  `Delta + Delta`, `Delta - Delta`, `-Delta`  `Delta`
row  `Delta*n`, `n*Delta`  `Delta`
```

The named directions `Delta.Up`, `Delta.Down`, `Delta.Left` and `Delta.Right` make arithmetic on
displacements read plainly. The y-axis of SVG points down, so `Up` is a negative change in y:

```scala
val step: Delta = 7*Delta.Up + 3*Delta.Right
```

```scala fuel
run
##
Out.println(step.inspect)
```

Adding two positions has no meaning, and does not compile:

```scala fuel
compile
errors show
##
Point(1, 2) + Point(3, 4)
```

### Transforms

A shape is translated, scaled, rotated and skewed. Each gives a new figure, and angles are
typed [angles](geography.md):

```scala
val turned = Rectangle((0, 0), 1, 1).translate(Delta(5, 0)).rotate(Angle.degrees(45))
```

A transform is recorded on the figure. It is not applied to the figure's coordinates. The
geometry which a figure was defined with is therefore the geometry which it keeps, and the
element which is written has a `transform` attribute. That is what makes the output
editable in a drawing program afterwards:

```scala fuel
run
##
Out.println(Rectangle((0, 0), 10, 5).translate(Delta(3, 4)).in[Xml].show)
Out.println(turned.in[Xml].show)
```

Transforms compose in the order in which they are applied, and several may be supplied at
construction instead. A tuple may stand for a delta, with a unary `+` to read as a
displacement, as in `Rectangle((0, 0), 10, 5).translate(+(3, 4))`.

### Identifiers

A figure, or a definition, may have an `Svg.Id`. That is what a reference to a gradient, the
target of an animation, or a `<use>` element needs, to name it. The identifier is a distinct
type, not text, so a reference to a definition which does not exist is not silently
written:

```scala
val named = Outline(id = Svg.Id(t"plus")).moveTo((0, 0)).closed
```

### Groups, polylines and text

A `Group` is a `<g>` element: figures which move, are styled and are identified together.
The axes of a [chart](charts.md), or one series of it, are a group, so that the whole part
can be found and replaced, by its identifier, when it changes:

```scala
val axes = Group(List(Rectangle((0, 0), 100, 60)), id = Svg.Id(t"axes"))
```

A `Polyline` joins absolute points with straight segments, and becomes a `<polygon>` when it
is `closed`. A plotted line, and the filled area beneath it, are therefore the same figure,
with one flag different:

```scala
val line = Polyline(List(Point(0, 60), Point(50, 20), Point(100, 40)))
val area = Polyline(List(Point(0, 60), Point(50, 20), Point(100, 40), Point(100, 60)), closed = true)
```

```scala fuel
run
##
Out.println(line.in[Xml].show)
Out.println(area.in[Xml].show)
```

`Lettering` sets text at a position. It is named for what it is in a drawing, since `Text`
is the type of a string. Its anchor says whether the position is the start, the middle or
the end of the run, and its baseline says which line of the glyphs lies on the position:

```scala fuel
run
##
Out.println(Lettering((50, 70), t"time", Lettering.Anchor.Middle, Lettering.Baseline.Hanging).in[Xml].show)
```

A `Lettering` may also name its `font`. That is a `Font in Web`: a face, paired with a
provision for its typeface, which exists only where such a provision is in scope, as
[Fonts](fonts.md) describes. The font's declarations join the element's inline style. The
SVG writes a `<style>` of `@font-face` rules into its definitions, for every typeface which
its lettering names, so the drawing looks alike wherever it is shown:

```scala fuel
typecheck
##
given menlo: ((Typeface of "Menlo") is Typesettable in Web) = Web.local()

Lettering((50, 70), t"time", font = Font(Typeface["Menlo"].bold))
```

Every figure may have an inline `style`, which is a typed set of [CSS](css.md)
declarations, and an `id`:

```scala fuel
run
##
Out.println(Rectangle((0, 0), 10, 10, style = Css.Style(fill = Srgb(1, 0, 0)), id = Svg.Id(t"box")).in[Xml].show)
```

### Gradients and color

A linear gradient is a definition with typed stops. Each stop has an offset in `[0, 1]`,
and a [color](colors.md). The offset is a [bounded number](numbers.md#bounded-numbers), so
one outside the range does not compile:

```scala
val fade = Svg.LinearGradient(Svg.Id(t"fade"), Stop(0.0, WebColors.Red), Stop(1.0, WebColors.Blue))
```

```scala fuel
compile
errors show
##
Stop(1.5, WebColors.Red)
```

### Documents

Figures and definitions are assembled into an `Svg`, with a size. It is written as a
standalone document, with its XML header:

```scala
val drawing = Svg(50, 50, figures = List(plus))
```

```scala fuel
run
##
Out.println(Document(drawing, enc"UTF-8").show)
```

Parsing runs the other way. It reads SVG text back into typed figures and definitions, so a
drawing which was produced elsewhere can be inspected, measured or altered, and not merely
embedded. The XML parser beneath it is given the free-form schema, since SVG's own
vocabulary is validated by the types of the figures, so no `XmlSchema` is needed in scope:

```scala
import dysasymptotics.linearSize

val svg = t"""<svg width="50" height="50"><rect x="0" y="0" width="10" height="10"/></svg>"""
        . read[Svg]
```

```scala fuel
run
##
Out.println(svg.width.inspect)
Out.println(svg.height.inspect)
Out.println(svg.figures.size.show)
```

A file which was written by a drawing tool begins with an XML declaration, and often a
comment, before the `<svg>` element. `read[Svg]` accepts the declaration, and discards it,
so a file is read in the same way as a bare element. `load[Svg]` keeps it, and gives a
`Document[Svg]`, whose metadata has the encoding.

```scala
val saved = t"""<?xml version="1.0" encoding="UTF-8" standalone="no"?>
                <!-- created by a drawing tool -->
                <svg width="100" height="100"/>""".read[Svg]
```

The `width` and `height` of the root element may have a unit, as a drawing tool writes
them, and a `viewBox` names the rectangle of user space which the viewport shows. Both are
kept, and are written back as they were read:

```scala
val sized = t"""<svg width="48.5mm" height="20mm" viewBox="0 0 48.5 20"/>""".read[Svg]
```

```scala fuel
run
##
Out.println(saved.width.inspect)
Out.println(sized.width.inspect)
Out.println(sized.unit.inspect)
Out.println(sized.viewBox.inspect)
```

An `Svg` is an XML value underneath. A drawing is therefore embedded directly into an
[HTML](html.md) page, with no step of serialization between, and the same drawing serves as
a standalone `*.svg` file when it is wrapped in a `Document`.

### Related

- [Charts](charts.md) draws plots as SVG.
- [XML](xml.md) is the format beneath an SVG document.
- [Colors](colors.md) and [CSS](css.md) supply fills, strokes and styles.
- [Fonts](fonts.md) describes the typefaces which lettering names.
- [Images](images.md) covers raster pictures.
- [Label placement](label-placement.md) decides where the text in a drawing goes.
