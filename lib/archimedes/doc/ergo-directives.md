# Ergo glyph reference

Ergo is a one-line shorthand for Presentation MathML. An expression is delimited
by a bracket pair; the **first character** chooses which pair (`(`/`[`/`{`/`⟨`)
acts as grouping syntax throughout, and every _other_ bracket is a literal `<mo>`.
A grouped run is an `<mrow>` unit; operands of an operator are uniformly "an atom
or a grouped unit".

This file lists every glyph Ergo uses: the **structural** glyphs that build the
equation tree, then the **attribute directives** (MathML Core) that decorate it.

---

## Structure

### Scripts & limits

- `↗` — **superscript** → `<msup>` — `x↗2` = _x²_
- `↘` — **subscript** → `<msub>` — `x↘i` = _xᵢ_
- `↑` — **overscript / limit above** → `<mover>` (accent when the script is a single `<mo>`)
- `↓` — **underscript / limit below** → `<munder>` (accent when the script is a single `<mo>`)

A base absorbs one `↘` and one `↗` → `<msubsup>`; one `↓` and one `↑` →
`<munderover>`. Big operators need no special syntax: `∑` is a plain `<mo>`, and
`∑↓(i = 1)↑n` gives it under/over limits.

### Fraction & roots

- `/` — **fraction** → `<mfrac>` — `a/b`
- `√` — **square root** (prefix) → `<msqrt>` — `√x`
- `n√x` — **nth root** → `<mroot>` — an index atom immediately before `√` (no space): `3√x` = _∛x_

### Introducers (tables)

Self-delimiting; the body is one group whose child groups are the elements.

- `⋱` — **matrix** → `<mtable>` — `⋱(((1)(2))((3)(4)))` = a 2×2 matrix (rows of cells)
- `⋯` — **row vector** → `<mtable>` with one row — `⋯((1)(2)(3))`
- `⋮` — **column vector** → `<mtable>` with one column — `⋮((a)(b))`

### Tokens & spacing

- a **letter run** → one `<mi>` — `sin` = `<mi>sin</mi>`; a space splits identifiers (`x y` = _x·y_)
- a **digit run** (interior `.` allowed) → one `<mn>` — `3.14`
- any other character → `<mo>` — `+`, `=`, `∑`, `±`, …
- an **operator glyph with a missing operand** degrades to a literal `<mo>` — `(↗)` writes a literal ↗
- a **space** is a separator and is not rendered

---

## Attribute directives (MathML Core)

Each MathML Core presentation attribute is written as a single **directive glyph**.
The scope is MathML Core only, so the large MathML 3 table/elementary attribute
families are absent (see _Excluded from Core_ below).

**Conventions** _(as implemented in the parser)_

- Directives are **postfix** and bind to the primary (atom or bracketed group)
  immediately to their left; multiple directives simply **juxtapose**:
  `=◆⇿` sets both `largeop="true"` and `stretchy="true"` on the operator `=`.
- **Enumerated and boolean** attributes have **one bare glyph per value** — no
  parameter. `form` is `⊰`/`⊹`/`⊱` (prefix/infix/postfix); a boolean is `⇿`
  (true) or `↮` (false). Since these never take a group, `=◆(a)` is `=` with
  `largeop="true"` _times_ `(a)`, not `largeop="a"`.
- **Open-valued** attributes (length/colour/integer) take their value in the
  **active grouping bracket** (shown below as `[…]`), read verbatim: with `(`
  grouping, `x●(red)` sets `mathcolor="red"`; with `[` grouping it would be
  `x●[red]`. Values are lengths (`0.5em`, `2px`, `40%`), colours (`red`,
  `#3366cc`), or signed integers (`+1`, `-2`).
- To attach a directive to a whole sub-expression, group it: `(x↗2)●(red)` colours
  the `<msup>`, whereas `x↗2●(red)` colours only the `2`.

### Document & display level

- `⧆` / `⧄` — **displaystyle** = true / false — use display style (larger, limits over/under) vs inline/text style
- `⌄[±n]` — **scriptlevel** — relative script size level; `+n` shrinks, `-n` enlarges
- `◻` / `▭` — **display** = block / inline _(on `<math>`)_ — block equation vs inline in running text

### Colour

- `●[color]` — **mathcolor** — foreground (ink) colour of the content
- `▨[color]` — **mathbackground** — background colour behind the content

### Text size & style

- `⟑[length]` — **mathsize** — font size of the element
- `⦱` — **mathvariant** = normal — render upright, cancelling the automatic italicisation of a single-letter identifier
- `⊩` / `⫣` — **dir** = ltr / rtl — text/layout direction

### Operator role (`<mo>`)

- `⊰` / `⊹` / `⊱` — **form** = prefix / infix / postfix — which spacing/role form the operator takes
- `∥` / `∤` — **fence** = true / false — mark the operator as a fence (bracket/paren)
- `▮` / `▯` — **separator** = true / false — mark the operator as a separator (e.g. a comma)
- `⇿` / `↮` — **stretchy** = true / false — allow the operator to stretch to its surrounding content
- `⋈` / `⋊` — **symmetric** = true / false — stretch symmetrically about the maths axis
- `◆` / `◇` — **largeop** = true / false — treat as a large operator (e.g. `∑`, `∫`) in display style
- `⧳` / `⧯` — **movablelimits** = true / false — limits over/under in display style but as scripts inline

### Operator spacing & stretch (`<mo>`)

- `⧔[length]` — **lspace** — space to the left of the operator
- `⧕[length]` — **rspace** — space to the right of the operator
- `⟰[length]` — **maxsize** — maximum stretched size
- `⟱[length]` — **minsize** — minimum stretched size

### Box metrics (`<mspace>`, `<mpadded>`)

- `↔[length]` — **width** — advance width of the box
- `⍏[length]` — **height** — extent above the baseline
- `⍖[length]` — **depth** — extent below the baseline
- `↕[length]` — **voffset** _(on `<mpadded>`)_ — vertical shift of the content

### Fraction (`<mfrac>`)

- `═[length]` — **linethickness** — thickness of the fraction bar (`0` = no bar)

### Under/over accents (`<munder>`, `<mover>`, `<munderover>`)

- `◠` / `⌢` — **accent** = true / false — treat the overscript as a tight accent
- `◡` / `⌣` — **accentunder** = true / false — treat the underscript as a tight accent

### Legacy (present in Core but behaviour undefined)

- `⚙[type]` — **actiontype** _(on `<maction>`)_ — legacy action type; behaviour is undefined in MathML Core

---

## Excluded from Core (no glyph assigned)

These MathML 3 features are **not** in MathML Core, so Ergo assigns no glyph for
them; they would only be reachable through a generic named-element escape:

- **Table attributes** — `columnalign`, `rowalign`, `columnlines`, `rowlines`,
  `frame`, `framespacing`, `rowspan`, `columnspan`, `align`, `side`, … (Core keeps
  `<mtable>`/`<mtr>`/`<mtd>` as elements but defines none of these attributes)
- **`<menclose>`** and its `notation` values — not in Core
- **`<ms>` `lquote`/`rquote`** — removed in Core
- **Elementary math** — `<mstack>`, `<mlongdiv>`, `<msgroup>`, `<msrow>`,
  `<mscarries>`, `<mscarry>`, `<msline>` and their attributes — not in Core
- **`mathvariant`** values other than `normal` — Core requires the corresponding
  Unicode Mathematical Alphanumeric characters instead
- **Reserved** — `intent`, `arg`, `alttext` (declared but undefined in Core)
