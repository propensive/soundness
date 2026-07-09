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
```

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

### Gradients and color

A linear gradient is a definition with typed stops, each an offset in `[0, 1]` — a
[bounded number](numbers.md), so an offset outside the range does not compile — and a
[color](colors.md):

```scala
LinearGradient(SvgId(t"fade"), Stop(0.0, WebColors.Red), Stop(1.0, WebColors.Blue))
```

### Documents

Figures and definitions assemble into an `Svg` with a size, rendered as a standalone document with
its XML header, or parsed back from SVG text:

```scala
val drawing = Svg(50, 50, figures = List(plus))
Document(drawing, enc"UTF-8").show   // <?xml version="1.0" …?><svg …>
```
