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

Ergo takes the middle path. It is a compact linear notation, one glyph per structural role, which
parses to MathML and serializes back — so the source stays readable, the output is a real
document tree, and the round trip is exact. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### The MathML tree

The elements are values named after the tags: `Mi` for an identifier, `Mn` for a number, `Mo` for
an operator, `Mrow` for a group, and `Msup`, `Msub`, `Mfrac`, `Msqrt` and the rest for the
structures. `Math` is the root, and `xml` renders it:

```scala
Math(Msup(Mi(t"x"), Mn(t"2"))).xml.show
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

Tokens follow three rules: a run of letters is one identifier, so `sin` is `<mi>sin</mi>` and a
space separates identifiers (`x y` means *x·y*); a run of digits, with an interior `.` allowed, is
one number; and any other character is an operator. A space is a separator and renders as nothing.

### Structural glyphs

Scripts and limits attach to the atom on their left:

- `↗` — superscript → `<msup>` — `x↗2` is *x²*
- `↘` — subscript → `<msub>` — `x↘i` is *xᵢ*
- `↑` — overscript or upper limit → `<mover>`
- `↓` — underscript or lower limit → `<munder>`

A base absorbing one `↘` and one `↗` becomes `<msubsup>`; one `↓` and one `↑` becomes
`<munderover>`. Large operators need no special syntax — `∑` is an ordinary operator, and
`∑↓(i = 1)↑n` gives it limits below and above.

Fractions and roots:

- `/` — fraction → `<mfrac>` — `a/b`, binding looser than the scripts
- `√` — square root → `<msqrt>` — `√x`
- an index atom immediately before `√`, with no space, gives an nth root → `<mroot>` — `3√x` is
  *∛x*

Three introducers build tables, each self-delimiting, its body one group whose children are the
elements:

- `⋱` — matrix → `<mtable>` — `⋱(((1)(2))((3)(4)))` is a 2×2 matrix
- `⋯` — row vector → a single-row `<mtable>`
- `⋮` — column vector → a single-column `<mtable>`

An operator glyph with a missing operand degrades to a literal operator, so `(↗)` writes a literal
↗ rather than failing.

### Attribute directives

Each MathML Core presentation attribute has one directive glyph. Directives are *postfix* and bind
to the atom or bracketed group immediately to their left, and several simply juxtapose: `=◆⇿` sets
both `largeop` and `stretchy` on the `=`.

Enumerated and boolean attributes have one bare glyph per value and take no parameter — a boolean
is `⇿` for true and `↮` for false. Because they never take a group, `=◆(a)` is `=` with
`largeop="true"`, *times* `(a)`. Open-valued attributes — lengths, colours, integers — take their
value in the active grouping bracket, read verbatim: under `(` grouping, `x●(red)` sets
`mathcolor="red"`.

Grouping decides what a directive applies to: `(x↗2)●(red)` colours the whole superscript, while
`x↗2●(red)` colours only the `2`.

| Glyphs | Attribute | Meaning |
|---|---|---|
| `⧆` / `⧄` | `displaystyle` | display style, or inline/text style |
| `⌄[±n]` | `scriptlevel` | relative script size; `+n` shrinks |
| `◻` / `▭` | `display` | block or inline, on the root |
| `●[colour]` | `mathcolor` | foreground colour |
| `▨[colour]` | `mathbackground` | background colour |
| `⟑[length]` | `mathsize` | font size |
| `⦱` | `mathvariant` | upright, cancelling automatic italicisation |
| `⊩` / `⫣` | `dir` | left-to-right or right-to-left |
| `⊰` / `⊹` / `⊱` | `form` | prefix, infix or postfix operator |
| `∥` / `∤` | `fence` | mark as a fence |
| `▮` / `▯` | `separator` | mark as a separator |
| `⇿` / `↮` | `stretchy` | allow stretching to surrounding content |
| `⋈` / `⋊` | `symmetric` | stretch symmetrically about the axis |
| `◆` / `◇` | `largeop` | treat as a large operator in display style |
| `⧳` / `⧯` | `movablelimits` | limits over/under in display, scripts inline |
| `⧔[length]` / `⧕[length]` | `lspace` / `rspace` | space either side of an operator |
| `⟰[length]` / `⟱[length]` | `maxsize` / `minsize` | stretch bounds |
| `↔[length]` | `width` | advance width of a box |
| `⍏[length]` / `⍖[length]` | `height` / `depth` | extent above and below the baseline |
| `↕[length]` | `voffset` | vertical shift of padded content |
| `═[length]` | `linethickness` | fraction bar thickness; `0` for none |
| `◠` / `⌢` | `accent` | treat an overscript as a tight accent |
| `◡` / `⌣` | `accentunder` | treat an underscript as a tight accent |
| `⚙[type]` | `actiontype` | legacy; undefined in Core |

The scope is MathML Core, so the MathML 3 families it dropped have no glyphs: the table attributes
(`columnalign`, `rowspan`, `frame` and their kin — Core keeps the table *elements* but defines
none of these), `<menclose>` and its notations, `<ms>`'s quote attributes, and the elementary-maths
elements `<mstack>`, `<mlongdiv>` and the rest.

### Serializing back

`Ergo.serialize` turns a MathML tree back into Ergo, and the two are inverse over the Ergo subset:
a parsed expression serializes and re-parses to the same tree. An element outside that subset —
`<mtext>`, say — is rejected with an `Ergo.Error` rather than serialized approximately, so the
round trip is a guarantee rather than a hope.
