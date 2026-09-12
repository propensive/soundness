## SVG

### About

[SVG](https://en.wikipedia.org/wiki/SVG) drawings are built from typed values: shapes with typed
coordinates, paths assembled step by step, transforms and gradients — composed into an `Svg`
document that renders to XML. Drawing tools suit one-off artwork; generating SVG from a program —
a chart, a diagram, a figure per data point — wants values that compose, and typed points and
vectors instead of strings of numbers.

### On vector graphics

An SVG document is XML full of little languages: the `d` attribute of a path is a coordinate
program, `transform` is a function pipeline, colors and units have their own syntaxes. Programs
that generate SVG by concatenating strings must get all of them right at once, and a misplaced
coordinate produces a drawing that is silently wrong.

Soundness builds the structure from values. A `Point` is a position and a `Delta` a displacement —
distinct types, with the vector arithmetic between them — paths grow by named steps, and the
little languages are rendered by the types that understand them. Everything comes from the
`soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

Figures as typed values, with coordinates that are quantities, follow [impossible states](../philosophy/impossible-states.md): a malformed drawing cannot be written.

### Shapes

Rectangles, circles and ellipses are values, positioned by points — a pair of numbers converts
where a point is expected — and rendered to XML with `xml`:

```scala
Rectangle((0, 0), 10, 5).xml.show
// <rect x="0.0" y="0.0" width="10.0" height="5.0"/>

Circle((0, 0), 5).xml.show
// <circle cx="0.0" cy="0.0" r="5.0"/>
```

### Paths

An `Outline` is a path built step by step — absolute steps suffixed `To`, relative ones without —
with curves, quadratics and a `closed` end:

```scala
Outline().moveTo((0, 0)).curveTo((1, 1), (2, 1), (3, 0)).xml.show
// <path d="M 0.0 0.0 C 1.0 1.0, 2.0 1.0, 3.0 0.0"/>

val plus = Outline().moveTo((0, 0))
  . lineUp(2).lineLeft(2).lineUp(1).lineRight(2)
  . lineUp(2).lineRight(1).lineDown(2).lineRight(2)
  . lineDown(1).lineLeft(2).lineDown(2).closed
```

### Points and deltas

Positions and displacements are different things, and different types: a `Point` plus a `Delta` is
a `Point`, two points subtract to a `Delta`, and deltas scale and add. The named directions `Up`,
`Down`, `Left` and `Right` make displacement arithmetic read plainly:

```scala
val step: Delta = 7*Up + 3*Right   // Delta(3.0, -7.0) — SVG's y-axis points down
```

### Transforms

A shape translates, scales, rotates and skews, each producing a new figure, with angles as the
typed angles of [geography](geography.md):

```scala
Rectangle((0, 0), 1, 1).translate(Delta(5, 0)).rotate(Angle.degrees(45))
```

A transform is recorded on the figure rather than applied to its coordinates, so the geometry a
figure was defined with is the geometry it keeps, and the rendered element carries a `transform`
attribute — which is what makes the output editable in a drawing program afterwards:

```scala
Rectangle((0, 0), 10, 5).translate(Delta(3, 4)).xml.show
// <rect x="0.0" y="0.0" width="10.0" height="5.0" transform="translate(3.0,4.0)"/>
```

Transforms compose in the order they are applied, and several may be supplied at construction
instead. A tuple may stand for a delta, with unary `+` reading as a displacement:

```scala
Rectangle((0, 0), 10, 5).translate(+(3, 4))
```

### Identifiers

A figure or definition may carry an `Svg.Id`, which is what a gradient reference, an animation
target or a `<use>` element needs to name it. The identifier is a distinct type rather than text,
so a reference to a definition that does not exist is not silently rendered:

```scala
Outline(id = Svg.Id(t"plus")).moveTo((0, 0)).closed
```

### Groups, polylines and text

A `Group` is a `<g>` element: figures that move, style and identify together. A chart's axes, or
one series of it, is a group, so that the whole part can be found and replaced by its identifier
when it changes:

```scala
val axes = Group(List(Rectangle((0, 0), 100, 60)), id = Svg.Id(t"axes"))
```

A `Polyline` joins absolute points with straight segments, and becomes a `<polygon>` when it is
`closed`, so a plotted line and the filled area beneath it are the same figure with one flag
different:

```scala
val line = Polyline(List(Point(0, 60), Point(50, 20), Point(100, 40)))
val area = Polyline(List(Point(0, 60), Point(50, 20), Point(100, 40), Point(100, 60)), closed = true)
```

`Lettering` sets text at a position — named for what it is in a drawing, since `Text` is the
string type. Its anchor says whether the position is the start, middle or end of the run, and its
baseline which line of the glyphs lies on the position:

```scala
Lettering((50, 70), t"time", Lettering.Anchor.Middle, Lettering.Baseline.Hanging)
```

A `Lettering` may also name its `font`: a `Font in Web`, which is a face paired with a provision
for its typeface (see [fonts](fonts.md)), and which exists only where such a provision is in
scope. The font's declarations join the element's inline style, and the SVG writes a `<style>` of
`@font-face` rules for every typeface its lettering names into its definitions, so the drawing
renders alike wherever it is shown:

<!-- doccheck: skip -->
```scala
given (Typeface of "Menlo") is Typesettable in Web = Web.local()

Lettering((50, 70), t"time", font = Font(Typeface["Menlo"].bold))
```

Every figure may carry an inline `style`, a typed [CSS](css.md) declaration set whose property
names and values are checked as the code compiles, and an `id`:

```scala
Rectangle((0, 0), 10, 10, style = Css.Style(fill = Srgb(1, 0, 0)), id = Svg.Id(t"box"))
```

### Gradients and color

A linear gradient is a definition with typed stops, each an offset in `[0, 1]` — a
[bounded number](numbers.md), so an offset outside the range does not compile — and a
[color](colors.md):

```scala
Svg.LinearGradient(Svg.Id(t"fade"), Stop(0.0, WebColors.Red), Stop(1.0, WebColors.Blue))
```

### Documents

Figures and definitions assemble into an `Svg` with a size, rendered as a standalone document with
its XML header, or parsed back from SVG text:

```scala
val drawing = Svg(50, 50, figures = List(plus))
Document(drawing, enc"UTF-8").show   // <?xml version="1.0" …?><svg …>
```

Parsing runs the other way, reading SVG text back into typed figures and definitions, so a drawing
produced elsewhere can be inspected, measured or altered rather than merely embedded. The
[XML](xml.md) parser beneath it takes a schema, and SVG's own vocabulary is validated by the
figure types, so the free-form schema is the one to use:

```scala
given XmlSchema = XmlSchema.Freeform

val svg = t"""<svg width="50" height="50"><rect x="0" y="0" width="10" height="10"/></svg>"""
        . read[Svg]

(svg.width, svg.height, svg.figures.size)   // (50.0f, 50.0f, 1)
```

Because an `Svg` is an [XML](xml.md) value underneath, a drawing embeds directly into an
[HTML](html.md) page with no serialization step between, and the same drawing serves as a
standalone `.svg` file when wrapped in a `Document`.
