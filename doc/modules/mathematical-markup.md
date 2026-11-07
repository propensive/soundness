## Mathematical Markup

### About

Mathematics on a web page is [MathML](https://www.w3.org/TR/MathML-Core/): a presentation tree of
rows, fractions, scripts and roots that browsers render natively. Soundness models that tree as
typed values, reads it back from XML, and embeds it in [HTML](html.md) — and, because writing
MathML by hand is unbearable, provides *Ergo*, a one-line shorthand that parses to the same tree
and serializes back out of it.

### On writing mathematics

An equation is a tree, and MathML says so honestly — but it says so in perhaps ten times the
characters of the equation itself. `x²` is `<msup><mi>x</mi><mn>2</mn></msup>`, and a quadratic
formula runs to several lines that no one can read as mathematics. The alternatives are worse in a
different way: TeX is compact and expressive but needs a large renderer and produces something the
browser cannot introspect, and an image loses the content entirely.

Markup built from typed values rather than strings is [safety by construction](../philosophy/safety-by-construction.md) for a notation that is easy to get subtly wrong.

Ergo takes the middle path. It is a compact linear notation, one glyph per structural role, which
parses to MathML and serializes back — so the source stays readable, the output is a real
document tree, and the round trip is exact. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### The MathML tree

The elements are values named after the tags, and `Math` is the root. The ones Ergo writes are:

```tabular
column  Value
column  Element
column  Meaning
row  `Mi`  `<mi>`  identifier
row  `Mn`  `<mn>`  number
row  `Mo`  `<mo>`  operator
row  `Mrow`  `<mrow>`  group
row  `Mfrac`  `<mfrac>`  fraction
row  `Msqrt`  `<msqrt>`  square root
row  `Mroot`  `<mroot>`  nth root
row  `Msup`  `<msup>`  superscript
row  `Msub`  `<msub>`  subscript
row  `Msubsup`  `<msubsup>`  subscript and superscript
row  `Mover`  `<mover>`  overscript
row  `Munder`  `<munder>`  underscript
row  `Munderover`  `<munderover>`  underscript and overscript
row  `Mtable`, `Mtr`, `Mtd`  `<mtable>`, `<mtr>`, `<mtd>`  table, row, cell
```

The other elements — `Mtext`, `Mspace`, `Mstyle`, `Mpadded` and their kin — are modeled the same
way. `in[Xml]` renders a `Math`:

```scala
import Mathml.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely

Math(Msup(Mi(t"x"), Mn(t"2"))).in[Xml].show
// <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>x</mi><mn>2</mn></msup></math>
```

MathML text reads back with `read[Math]`, and a document round-trips unchanged — attributes the
model does not interpret are preserved rather than dropped. A root element that is not `<math>`
raises a `Mathml.Error` naming what was found instead. `html` embeds an expression as a foreign
element in an HTML tree, and `Mathml.Reader.read` extracts it again.

A `Math` also has a `display` — block or inline — which decides whether the browser sets it as a
displayed equation or within running text.

### Ergo

An Ergo expression is delimited by a bracket pair, and the **first character** chooses which pair
— `(`, `[`, `{` or `⟨` — acts as grouping throughout; every *other* bracket is then a literal
operator. A grouped run is one `<mrow>`, and the operands of an operator are uniformly "an atom or
a grouped unit".

```scala
Ergo.parse(t"(x = (-b ± √(b↗2 - 4 a c))/(2 a))")
```

Tokens follow three rules, and a space is a separator which renders as nothing:

```tabular
column  A run of
column  Becomes
column  Example
row  letters  one identifier, `<mi>`  `sin` is `<mi>sin</mi>`; `x y` means *x·y*
row  digits, with an interior `.` allowed  one number, `<mn>`  `3.14`
row  any other character  an operator, `<mo>`  `=`
```

### Structural glyphs

Scripts and limits attach to the atom on their left, and a fraction binds looser than the
scripts. An index atom immediately before `√`, with no space, makes an nth root. The three table
introducers are each self-delimiting, their body one group whose children are the elements.

```tabular
column  Glyph
column  Role
column  Element
column  Example
row  `↗`  superscript  `<msup>`  `x↗2` is *x²*
row  `↘`  subscript  `<msub>`  `x↘i` is *xᵢ*
row  `↘` and `↗`  both, on one base  `<msubsup>`  `x↘i↗2` is *xᵢ²*
row  `↑`  overscript or upper limit  `<mover>`
row  `↓`  underscript or lower limit  `<munder>`
row  `↓` and `↑`  both, on one base  `<munderover>`  `∑↓(i = 1)↑n`
row  `/`  fraction  `<mfrac>`  `a/b`
row  `√`  square root  `<msqrt>`  `√x`
row  `√` after an index  nth root  `<mroot>`  `3√x` is *∛x*
row  `⋱`  matrix  `<mtable>`  `⋱(((1)(2))((3)(4)))` is a 2×2 matrix
row  `⋯`  row vector  a single-row `<mtable>`
row  `⋮`  column vector  a single-column `<mtable>`
```

Large operators need no special syntax: `∑` is an ordinary operator, and `∑↓(i = 1)↑n` gives it
limits below and above.

An operator glyph with a missing operand degrades to a literal operator, so `(↗)` writes a literal
↗ rather than failing.

### Attribute directives

Each MathML Core presentation attribute has one directive glyph. Directives are *postfix* and bind
to the atom or bracketed group immediately to their left, and several simply juxtapose: `=◆⇿` sets
both `largeop` and `stretchy` on the `=`.

Enumerated and boolean attributes have one bare glyph per value and take no parameter — a boolean
is `⇿` for true and `↮` for false. Because they never take a group, `=◆(a)` is `=` with
`largeop="true"`, *times* `(a)`. Open-valued attributes — lengths, colors, integers — take their
value in the active grouping bracket, read verbatim: under `(` grouping, `x●(red)` sets
`mathcolor="red"`.

Grouping decides what a directive applies to: `(x↗2)●(red)` colors the whole superscript, while
`x↗2●(red)` colors only the `2`.

```tabular
column  Glyphs
column  Attribute
column  Meaning
row  `⧆` / `⧄`  `displaystyle`  display style, or inline/text style
row  `⌄[±n]`  `scriptlevel`  relative script size; `+n` shrinks
row  `◻` / `▭`  `display`  block or inline, on the root
row  `●[colour]`  `mathcolor`  foreground color
row  `▨[colour]`  `mathbackground`  background color
row  `⟑[length]`  `mathsize`  font size
row  `⦱`  `mathvariant`  upright, canceling automatic italicisation
row  `⊩` / `⫣`  `dir`  left-to-right or right-to-left
row  `⊰` / `⊹` / `⊱`  `form`  prefix, infix or postfix operator
row  `∥` / `∤`  `fence`  mark as a fence
row  `▮` / `▯`  `separator`  mark as a separator
row  `⇿` / `↮`  `stretchy`  allow stretching to surrounding content
row  `⋈` / `⋊`  `symmetric`  stretch symmetrically about the axis
row  `◆` / `◇`  `largeop`  treat as a large operator in display style
row  `⧳` / `⧯`  `movablelimits`  limits over/under in display, scripts inline
row  `⧔[length]` / `⧕[length]`  `lspace` / `rspace`  space either side of an operator
row  `⟰[length]` / `⟱[length]`  `maxsize` / `minsize`  stretch bounds
row  `↔[length]`  `width`  advance width of a box
row  `⍏[length]` / `⍖[length]`  `height` / `depth`  extent above and below the baseline
row  `↕[length]`  `voffset`  vertical shift of padded content
row  `═[length]`  `linethickness`  fraction bar thickness; `0` for none
row  `◠` / `⌢`  `accent`  treat an overscript as a tight accent
row  `◡` / `⌣`  `accentunder`  treat an underscript as a tight accent
row  `⚙[type]`  `actiontype`  legacy; undefined in Core
```

The scope is MathML Core, so the MathML 3 families it dropped have no glyphs: the table attributes
(`columnalign`, `rowspan`, `frame` and their kin — Core keeps the table *elements* but defines
none of these), `<menclose>` and its notations, `<ms>`'s quote attributes, and the elementary-maths
elements `<mstack>`, `<mlongdiv>` and the rest.

### Serializing back

`Ergo.serialize` turns a MathML tree back into Ergo, and the two are inverse over the Ergo subset:
a parsed expression serializes and re-parses to the same tree. An element outside that subset —
`<mtext>`, say — is rejected with an `Ergo.Error` rather than serialized approximately, so the
round trip is a guarantee rather than a hope.
