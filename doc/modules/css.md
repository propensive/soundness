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
import formatting.indentedCssFormatting
```

Properties checked as the code compiles are [safety by construction](../philosophy/safety-by-construction.md) applied to a language usually validated in a browser.

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

The physical units CSS shares with the real world — centimeters, points, seconds — are the same
quantities used everywhere else in Soundness.

### Parsing

CSS text parses with `read`, validating every declaration and accumulating *all* the problems
rather than stopping at the first — the shape a linter or a build step needs:

```scala
import errorDiagnostics.stackTracesDiagnostics

t"a { color: red }".read[Css]

capture[Css.Errors](t"a { colour: red }".read[Css]).errors(0).reason
// Css.Error.Reason.UnknownProperty(t"colour")
```

Each `Css.Error` carries its line, column and reason — an unknown property, a bad value, a malformed
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

A parsed selector is a tree, not a string: a compound selector holds its simple parts, a complex
selector holds compounds joined by combinators, and a selector list holds alternatives. Each part
is a typed case, so a program can ask what a selector actually matches:

```scala
t"a > b".read[Css]        // a child combinator between two type selectors
t"a:hover".read[Css]      // a type selector with a pseudo-class
t"p::before".read[Css]    // a pseudo-element
```

Attribute selectors carry their matcher — presence, exact, prefix, suffix, substring, dash-match
and whitespace-match — and the case-sensitivity modifier where one is given, so `[href^="https" i]`
is understood rather than merely preserved.

`An+B` arguments to `:nth-child` and its siblings parse to their coefficients, with `odd` and
`even` normalized to `2n+1` and `2n`, and an `of` clause kept alongside. `:is`, `:not` and `:has`
carry selector lists as their arguments, so their contents are themselves inspectable.

Namespaced type selectors — `svg|rect`, `*|a` and the default-namespace `|a` — keep their prefix
distinctly, since the three mean different things.

### Property validation

Parsing checks property names against the known set, so a misspelling is an error rather than a
declaration that silently does nothing:

```scala
capture[Css.Errors](t"a { colour: red; width: wide }".read[Css]).errors.map(_.reason)
// List(Css.Error.Reason.UnknownProperty(t"colour"), Css.Error.Reason.BadValue(…))
```

Errors accumulate: a rule with several bad declarations reports all of them, so a stylesheet is
corrected in one pass. The error type is plural — `Css.Errors` — because reporting one fault at a
time from a document that has several is the wrong shape for the job.

### Checking class names against a stylesheet

The point of knowing which classes and ids a stylesheet defines is to check the markup that uses
them. Binding a stylesheet resource makes its names available at compiletime, and an
[HTML](html.md) attribute referring to a name resolves to `class` or `id` according to which the
stylesheet declares — so a misspelled class name in a template is a compile error, and the right
attribute is used without saying which:

```scala
import classloaders.threadContextClassloader

given (Styles at "/site.css") = Styles(cp"/site.css")
import cssBindings.checkedBinding
```

The resource is read as the code compiles, so the stylesheet must be on the compiler's
classpath, and the binding's type names the resource it was read from.
