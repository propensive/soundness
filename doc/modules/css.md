## CSS

### About

[CSS](https://en.wikipedia.org/wiki/CSS) is parsed, validated and generated as typed values.
A stylesheet parses into a `Css` tree of rules and declarations. Every property is checked
against its real grammar, which is the value-definition syntax of the
[MDN data](https://github.com/mdn/data), for more than six hundred properties. A misspelled
property, or a malformed value, is therefore a structured error, not a style which silently
fails in the browser. The `css"…"` [interpolator] writes a stylesheet in source code, with
typed values substituted into it.

Selectors parse to the full grammar of
[Selectors Level 4](https://www.w3.org/TR/selectors-4/). Lengths and angles are typed
[quantities](quantities.md), and [colors](colors.md) are substituted directly, written as
hex.

### On CSS

CSS is unusual among languages, in that its consumers never reject it. A browser ignores
what it does not understand. A typo in the name of a property, or an invalid value, fails by
*doing nothing*, and is discovered by eye, or not at all. Tools have grown up around
checking the text of CSS after the fact. A type system can do better, by checking styles
where they are written.

Here CSS is checked against the real grammar of each property, as it is parsed. That is
[safety by construction](../philosophy/safety-by-construction.md), applied to a language
which is usually validated in a browser.

Everything comes from the `soundness` package. A choice of formatting is in scope, for
writing CSS out, and the last import lets the samples print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import formatting.indentedCssFormatting
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Writing CSS

The `css"…"` interpolator writes a stylesheet. Typed values are substituted into it: a
color is written as hex, and a length with its unit.

```scala
val red = Srgb(1.0, 0.0, 0.0)
val width = 4.0*Px

val sheet = css"a { color: $red; width: $width }"
```

```scala fuel
run
##
Out.println(sheet.show)
```

Bare declarations, with no selector, form a `Css.Style`, for use as an inline `style`
attribute. One can equally be built from named arguments:

```scala fuel
run
##
Out.println(Css.Style(color = red, width = 4.0*Px).text)
```

### Units

The units of CSS are typed quantities, so a length is a length, and cannot be confused with
a number or an angle:

```scala
val margin = 2.0*Rem
val half = 50.0*Pct
val quarterTurn = 90.0*Deg
```

Each unit belongs to a dimension, and only units of the same dimension add or convert:

```tabular
column  Units
column  Measure
column  Converts to
row  `Px`  pixels  nothing else
row  `Rem`, `Ex`, `Ch`  the root font size, the x-height, the character width  nothing else
row  `Vw`, `Vh`, `Vmin`, `Vmax`  fractions of the viewport  nothing else
row  `Pct`  a percentage  nothing else
row  `Cm`, `Mm`, `Pt`, `Pc`  physical length  one another, and every other length in Soundness
row  `S`, `Ms`  time  every other time in Soundness
row  `Deg`, `Rad`, `Turn`  angle  nothing else, not even one another
row  `Fr`  a share of a grid's free space  nothing else
```

The physical units which CSS shares with the real world are the same
[quantities](quantities.md#mixing-units) which are used everywhere else.

### Parsing

CSS text is parsed with `read`:

```scala
import errorDiagnostics.stackTracesDiagnostics

val parsed = t"a { color: red }".read[Css]
```

Every declaration is validated, and *all* the problems are collected, instead of stopping at
the first, which is what a linter, or a step of a build, needs. The error type is plural,
`Css.Errors`, because reporting one fault at a time, from a document which has several, is
the wrong shape for the job:

```scala fuel
run
##
val faults = capture[Css.Errors](t"a { colour: red; width: wide }".read[Css]).errors

faults.each: fault =>
  Out.println(fault.reason.inspect)
```

Each `Css.Error` has its line, its column and its reason, such as an unknown property, a bad
value or a malformed selector, so the faults of a stylesheet are reported precisely.
[Errors](errors.md#accumulating-failures) describes accumulating errors in general.

### Rendering

A `Css` is written back to text through the formatting in scope. The standard formatting
indents, for reading. The compact formatting strips every inessential character, for
serving:

```scala fuel
run
##
Out.println(parsed.show)

locally:
  import formatting.compactCssFormatting
  Out.println(parsed.show)
```

Stylesheets are joined with `+`. A stylesheet reports the classes and the ids which it
defines, which is how the class attributes of [HTML](html.md#attributes) are checked against
the styles which exist.

### Selectors

Selectors are structural values, which cover the full modern grammar: combinators,
including the column combinator `||`, tests on attributes, `:is`, `:not`, `:has`, `An+B`
expressions, and the nesting selector `&`. A selector can therefore be built, inspected and
written out, and need not be spliced together as text.

A parsed selector is a tree, not a string. A compound selector holds its simple parts, a
complex selector holds compounds joined by combinators, and a selector list holds
alternatives. Each part is a typed case, so a program can ask what a selector actually
matches:

```scala
val child = t"a > b { color: red }".read[Css]
val hover = t"a:hover { color: red }".read[Css]
val before = t"p::before { color: red }".read[Css]
```

The three are a child combinator between two type selectors, a type selector with a
pseudo-class, and a pseudo-element.

Attribute selectors have their matcher, and the modifier for case-sensitivity where one is
given, so `[href^="https" i]` is understood, and not merely preserved:

```tabular
column  Syntax
column  Matcher
column  Matches an attribute whose value
row  `[a]`  none: presence  is anything
row  `[a=v]`  `AttributeMatcher.Exact`  is exactly `v`
row  `[a~=v]`  `AttributeMatcher.Includes`  has `v` among its whitespace-separated words
row  `[a|=v]`  `AttributeMatcher.DashMatch`  is `v`, or begins with `v-`
row  `[a^=v]`  `AttributeMatcher.Prefix`  begins with `v`
row  `[a$=v]`  `AttributeMatcher.Suffix`  ends with `v`
row  `[a*=v]`  `AttributeMatcher.Substring`  contains `v`
```

`An+B` arguments, to `:nth-child` and its siblings, are parsed to their coefficients. `odd`
and `even` are normalized to `2n+1` and `2n`, and an `of` clause is kept beside them. `:is`,
`:not` and `:has` have selector lists as their arguments, so their contents can themselves
be inspected.

Type selectors with a namespace, which are `svg|rect`, `*|a`, and `|a` for the default
namespace, keep their prefix distinctly, since the three mean different things.

### Checking class names against a stylesheet

The point of knowing which classes and ids a stylesheet defines is to check the markup which
uses them. Binding a stylesheet resource makes its names available at compiletime. An HTML
attribute which refers to a name then resolves to `class` or to `id`, according to which the
stylesheet declares. A misspelled class name in a template is therefore a compile error, and
the right attribute is used without saying which:

```scala fuel
typecheck
##
import classloaders.threadContextClassloader

given styles: (Styles at "/cataclysm/test.css") = Styles(cp"/cataclysm/test.css")
import cssBindings.checkedBinding
```

The resource is read as the code compiles, so the stylesheet must be on the compiler's
[classpath](classpath.md), and the type of the binding names the resource which it was read
from.

### Related

- [HTML](html.md) builds the elements which a stylesheet styles.
- [Colors](colors.md) and [Quantities](quantities.md) supply the typed values of properties.
- [Pages](pages.md) gathers the styles of a page's parts into one stylesheet.
- [Names](names.md#built-in-planes) describes the rules for a class name.
- [Fonts](fonts.md), [forms](forms.md), [SVG](svg.md) and [web automation](web-automation.md) use stylesheets and selectors.
