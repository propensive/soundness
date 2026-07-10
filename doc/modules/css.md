## CSS

### About

[CSS](https://en.wikipedia.org/wiki/CSS) is parsed, validated and generated as typed values. A
stylesheet parses into a `Css` tree of rules and declarations, with every property checked against
its real grammar — the value-definition syntax of the
[MDN data](https://github.com/mdn/data) for over six hundred properties — so a misspelled property
or a malformed value is a structured error, not a style that silently fails in the browser. The
`css"…"` interpolator performs the same validation as the code compiles.

Selectors parse to the full [Selectors Level 4](https://www.w3.org/TR/selectors-4/) grammar,
lengths and angles are typed [quantities](quantities.md), and [colors](colors.md) substitute
directly, rendered to hex.

### On CSS

CSS is unusual among languages in that its consumers never reject it: a browser ignores what it
does not understand, so a typo in a property name or an invalid value fails by *doing nothing*,
discovered visually or not at all. Tooling has grown up around linting CSS text after the fact;
type systems can do better, by checking styles where they are written.

Soundness checks CSS the way it checks any other embedded language: literals at compiletime,
runtime input on arrival, both against the genuine grammar of each property. Everything comes from
the `soundness` package, with a formatting choice in scope for rendering:

```scala
import soundness.*
import strategies.throwUnsafely
import formatting.standardCssFormatting
```

### Writing CSS

The `css"…"` interpolator writes a stylesheet, checked as the code compiles, and substitutes typed
values — a color renders as hex, a length with its unit:

```scala
val red = Srgb(1.0, 0.0, 0.0)
val width = 4.0*Px

css"a { color: $red; width: $width }"
```

An unknown property, or a value that does not satisfy the property's grammar, is a compile error.
Bare declarations, without a selector, form a `Css.Style` for use as an inline `style` attribute,
and can equally be built from named arguments:

```scala
Css.Style(color = red, width = 4.0*Px).text   // t"color: #ff0000; width: 4px"
```

### Units

CSS's units are typed quantities — `Px`, `Rem`, `Vw`, `Pct`, `Deg`, `Fr` and the rest — so a length
is a length and cannot be confused with a number or an angle:

```scala
2.0*Rem
50.0*Pct
90.0*Deg
```

The physical units CSS shares with the real world — centimetres, points, seconds — are the same
quantities used everywhere else in Soundness.

### Parsing

CSS text parses with `read`, validating every declaration and accumulating *all* the problems
rather than stopping at the first — the shape a linter or a build step needs:

```scala
t"a { color: red }".read[Css]

capture[CssErrors](t"a { colour: red }".read[Css]).errors.head.reason
// CssError.Reason.UnknownProperty(t"colour")
```

Each `CssError` carries its line, column and reason — an unknown property, a bad value, a malformed
selector — so a stylesheet's faults are reported precisely.

### Rendering

A `Css` renders back to text through the formatting in scope — standard formatting indents for
reading, compact strips every inessential character for serving:

```scala
t"a { color: red }".read[Css].show
// a {
//   color: red;
// }

locally:
  import formatting.compactCssFormatting
  t"a { color: red }".read[Css].show   // t"a{color:red;}"
```

Stylesheets concatenate with `+`, and a stylesheet reports the classes and ids it defines — the
hook by which [HTML](html.md) class attributes are checked against the styles that exist.

### Selectors

Selectors are structural values covering the full modern grammar — combinators including the column
`||`, attribute tests, `:is`, `:not`, `:has`, `An+B` expressions and nesting `&` — so a selector can
be built, inspected and rendered rather than spliced together as text.
