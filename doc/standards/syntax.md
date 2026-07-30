# Syntax and Formatting

This document defines the syntactic and whitespace conventions enforced by
the Decorum compiler plugin. It is written to govern any project that adopts
it; a project fixes two parameters — the exact text of its licence header
(and hence the header's length in lines) and, optionally, the name of its
umbrella re-export package — and every other convention applies unchanged.
Examples are drawn verbatim from real code governed by this standard.

The style has a single organizing idea: **layout is a deterministic function
of the code**. Given a fragment of Scala, there is one correct way to lay it
out, and a reader can rely on that — every line break carries information,
because a break always means "this didn't fit". The conventions are not a
list of unrelated preferences; each one derives from one of eight named
principles, set out in Part I. Part II states every rule in full, grouped by
the principle it derives from, citing its checker identity in `[SN-nnn]`
form. Part III indexes every rule number. The appendices collect supporting
material: the operator precedence classes, the keyword-sequence grammar,
the whitespace-insignificant interpolators, and guidance on where to place
extension methods.

## Part I — The Principles

### P1 — The Frame

**One fixed canvas per file: the project's licence header first, `package`
on the line after it, then one blank line; 100 columns, two-space even
indentation, spaces only, no trailing whitespace.**

File geography should be muscle memory. When every file has the same shape —
the same header in the same place, the code starting at the same line, lines
never wider than the same limit, indentation always landing on the same
columns — the reader's attention is free to go entirely to content. Nothing
about the frame is ever a decision, so nothing about it is ever a signal;
all signal lives inside it.

Each project fixes its licence header once, as a block comment of some
exact, unvarying length — call it *H* lines. Every file in the project then
carries that header verbatim on lines 1 to *H*, the `package` declaration on
line *H* + 1, and a blank line at *H* + 2, so code always starts at the same
line. (In the project this document was written for, *H* is 32; the examples
below use those line numbers.)

```scala
package gossamer        // always the line after the header (here, line 33)
                        // always one blank line
import anticipation.*   // content always starts at the same line
```

Rules: [SN-799], [SN-131], [SN-658], [SN-230], [SN-135], [SN-926],
[SN-015], [SN-162].

### P2 — Anchoring

**Every substructure has an anchor: the leftmost character of the token
that opens it, extended leftward to the line's leading declaration keyword
or modifier. Subordinate keywords sit on the anchor's line or in its
column; bodies indent exactly anchor + 2.**

Structure reads down the left edge. A reader scanning the left margin sees
each construct's keywords stacked in one column and each body stepped
exactly two columns in, so the shape of the code is visible without
reading it.

```scala
val foo: Int = bar:
  baz                  // anchored to `val`, so indented to `val` + 2

if x > 0               // subordinate keywords in the anchor column
then x
else -x
```

Rules: [SN-473.8], [SN-473.1], [SN-473.9], [SN-833.1], [SN-833.2],
[SN-833.3], [SN-833.4], [SN-140], [SN-560], and the quote/splice layout
family [SN-473.2]–[SN-473.7].

### P3 — Density

**Fit decides form: the most compact compliant layout that fits is the only
correct one. A break always means "didn't fit", and cascades run forward
only.**

Vertical space is a scarce resource — every needless line pushes context off
the screen — and an *optional* break is worse than wasteful: it invites the
reader to look for a reason that isn't there. When compactness is
compulsory, a broken construct reliably signals size, and the layouts of two
equal-sized constructs are identical. Some forms are inherently vertical —
statement sequences, runs of `case` clauses, template bodies — and are
exempt: their verticality is their structure, not a response to width.

The choice among a construct's forms is likewise forced by fit, never by
taste: a definition is single-line if it fits, multi-line if only the
signature fits, heavy only when the signature itself does not fit; a lambda's
bracketing is decided by its shape ([SN-312]); a closing bracket never takes
a line of its own ([SN-811]); a keyword sequence keeps every keyword inline
when the whole fits ([SN-833]); import groups appear in one fixed order
([SN-302]).

```scala
def keys: Set[node] = edgeMap.keySet        // fits — must be one line

if x > 0 then x else -x                     // fits — must not be broken
```

Rules: [SN-247], [SN-312]; and the canonical break order (§"The canonical
break order" in Part II), which is normative but not yet checker-enforced.

### P4 — Continuation Marking

**A continuation line is distinguishable at its left margin.** Every line
that continues a construct rather than starting one carries a mark a reader
can see without parsing: the +2 indent step, a hard-spaced marker (`=>␣␣`
or `:␣␣␣` — marker plus pad spanning exactly two indent steps), the inner
space of a formal bracket block (`( arg )`), or a leading `. ` chain marker.

A corollary decides *which side of a break the connecting token goes on*:
the operator sits on whichever side makes the wrap unambiguous. No Scala
statement can *begin* with `.`, so a leading dot unambiguously marks a
continuation — dots lead. No Scala statement can *end* with an infix
operator, so a trailing operator unambiguously promises a continuation, a
syntactic ellipsis — symbolic operators trail.

```scala
source.lines
. filter(_.nonEmpty)    // dot leads: a `.`-led line can only be a continuation

left ++
  right                 // operator trails: a `++`-ended line cannot be complete
```

Rules: [SN-163], [SN-616], [SN-444]; the inner-space mark is shared with
[SN-811]/[SN-402] (P5), and the quote-body indent [SN-473.9] participates.

### P5 — Balance

**What is syntactically paired is typographically mirrored.** Brackets take
inner spaces on both sides or neither; an operator's two sides carry equal
spacing; a symbolic method name is spelled at its definition the way it is
spaced at its use.

One statement of this principle does a lot of work: **a line break on one
side of an infix token demands exactly one space on the other side.** A line
break is the largest gap there is, and the token's other flank must carry
its minimal analogue — one space — or the token would visually attach to one
operand. From this single sentence, two rules fall out:

- A trailing infix operator is preceded by exactly one space: `left ++⏎`,
  never `left++⏎` — the break after demands the space before ([SN-616.3]).
  This holds even for operators legally written tight (`a++b`) when inline.
- A leading chain dot is followed by exactly one space: `. method`, never
  `.method` — the break before demands the space after ([SN-163]).

This derivation is the clearest example of the style's design: neither
spelling is an arbitrary house quirk; both are the same principle applied to
the two legal sides of a break.

Rules: [SN-376], [SN-376.1], [SN-013], [SN-811], [SN-402]; it grounds
[SN-616.3] and the `. method` spelling of [SN-163], and the closer alignment
of [SN-560].

### P6 — Proximity

**Spacing encodes binding: gap size is monotone in binding looseness.** The
gap scale runs `none < space < newline < blank line < two blank lines`, and
a bigger gap always means a looser connection. Tighter-binding operators
never carry more space than looser ones; a comma hugs the item it follows;
a multi-line statement (a *chunk*) is set off from its siblings by blank
lines; at most one blank line appears anywhere, except that up to two may
surround a heavy-signature definition — the top of the gap scale marking the
heaviest construct the style recognises.

```scala
a + b*c            // `*` binds tighter than `+`, so it gets less space

def slug: Text = name.lower.tr(' ', '-')
                   // blank line: the next definition is a separate thought
def id: Text = slug
```

Rules: [SN-315], [SN-783], [SN-677], [SN-441], [SN-551], [SN-529]; the
precedence-monotone side of [SN-376] (P5) and the import-group separation
[SN-302.3] (P8) express the same scale.

### P7 — Tabulation

**Adjacent lines instancing one schema align corresponding tokens in
columns.** When consecutive lines are instances of the same shape — the
cases of a match, the parameters of a `using` clause, the generators of a
`for` — their corresponding tokens form vertical columns, so the eye can
scan one column to compare the parts that differ.

```scala
case CannotExecuteGit   => m"the `git` command could not be executed"
case CloneFailed        => m"the repository could not be cloned"
case InvalidRepoPath    => m"the repository path was not valid"
```

Rules: [SN-326], [SN-946], [SN-924], and the alignment relaxation of
[SN-529].

### P8 — Findability

**One name, one home.** Everything is findable by knowing its name: a type
lives in the file named after it; a companion object — the most-read API
surface — comes before its type; imports are grouped and alphabetised and
never aliased, so a name in code is the name at its definition; every public
name is re-exported to the project's umbrella package, so one import reaches
everything; and prose documentation lives in `doc/`, not in doc-comments, so
it has one home too.

Rules: [SN-302], [SN-847], [SN-398], [SN-742], [SN-742.1], [SN-162.2].

### Retained freedoms

Everything not listed here is deterministic. The complete list of author
discretion is:

1. **Single-character operator spacing.** `a+b` and `a + b` are both legal;
   the choice is expressive use of P6 (a tight spelling reads as tight
   binding), bounded by P5's symmetry, by same-precedence consistency, and
   by precedence monotonicity ([SN-376]).
2. **A single blank line between single-line siblings.** Two adjacent
   single-line definitions may sit flush or take one blank line between
   them — expressive grouping on the P6 gap scale ([SN-315], [SN-783]).
3. **Opt-in alignment padding.** Extra spaces after commas ([SN-529]) and
   before a run of aligned `=` defaults ([SN-376.1]) may build tabular
   columns, provided the alignment is consistent across the run (P7).

## Part II — Derived Rules

### P1 — The Frame

#### License header [SN-799]

Every file begins with the project's canonical license header — a block of
exactly *H* lines inside a single `/* … */` comment, identical in every
file. Line 1 opens the block comment with `/*` and line *H* closes it with
`*/`. Lines 1 to *H* are reserved for it. No exceptions.

#### Package declaration [SN-131] [SN-658]

The line after the header is `package <module>` — a single identifier
segment matching the module the file belongs to, with nothing else on the
line [SN-131]. It is followed by a single blank line [SN-658].

#### Line length [SN-230]

Hard limit: 100 columns. Lines that would exceed this must be broken; refer
to the canonical break order (P3) and the rules for heavy signatures (P3)
and chain continuation (P4). Interior lines of multi-line triple-quoted
strings are exempt: their text is string content, governed by [SN-560] for
the layout interpolators and significant data for the rest.

#### Indentation [SN-135] [SN-926]

Two spaces, no tabs, applied uniformly to every level of nesting and to
every continuation line. Tab characters are forbidden anywhere in the file,
including strings and comments [SN-135].

The leading indent of every code line is an even number of columns
[SN-926]. It is the **marker token** — the leading `.` of a chain
continuation, the `=>` of a given continuation, the `:` of a heavy
return-type line — that sits on the even-indent grid, not the padded
payload that follows it: in `. filter(…)` the `.` occupies the even column
and `filter` sits at odd column marker + 2; in `=>  body` the `=>` starts
on the grid. The rule suspends inside open `(…)` blocks, where continuation
rows align under names from the opener line and may need an odd number of
leading spaces.

Scala 3's indented (colon) syntax is used everywhere that a choice exists;
brace and paren lambda wrappers are prescribed by shape in [SN-312] (P3),
whose table is authoritative.

#### Trailing whitespace [SN-015]

No trailing whitespace at the end of any line. Interior and closing lines
of multi-line triple-quoted strings are exempt (their text is string
content), as are whitespace-only lines.

#### Block comments [SN-162]

`/* … */` block comments are reserved for the license header
[SN-162.1] — the frame owns the only block comment in the file. `/** … */`
doc comments are never permitted [SN-162.2]: prose documentation lives in
`doc/` markdown files (P8).

Line comments use `//` followed by exactly one space and then the comment
text — the `//␣` gap is the P6 space that separates marker from content. A
comment that pertains to a single line of code may sit at the end of that
line; otherwise it sits on its own line directly above the code it
describes, at the same indent.

### P2 — Anchoring

#### The anchor

An *indented scope* — the body of a colon-block (`recv:`), a `match`, a
lambda or `case` arrow (`=>`), or a definition's `=` — must be indented
exactly two columns beyond the scope's **anchor** [SN-473.8]. The anchor is
the column of the construct that opens the scope, except it **extends
leftwards** to the line's leading declaration keyword (`val`, `var`, `def`,
`case`, `given`, after any modifiers) when that keyword shares the opener's
line. The receiver's form is irrelevant: `bar:`, `bar():`, and
`bar(baz): quux =>` anchor identically. So

```scala
val foo: Int = bar:
  baz
```

is legal — the colon-block's anchor extends from `bar` to `val`, so `baz`
sits at the `val` column + 2.

A *parameter block* — a `(`/`[` argument or type-argument list following a
receiver — is not an indented scope; it anchors to its receiver
(§"Anchor of a heavy argument block"). So

```scala
val foo: Bar = Bar
  ( baz, quux )
```

is rejected in favour of

```scala
val foo: Bar =
  Bar
    ( baz, quux )
```

A tuple (a parenthesised group with no receiver, e.g. an `=` right-hand
side) is an indented value, not a parameter block.

The extension does not apply to keyword sequences (`if`/`then`/`else`,
etc.), which keep their own anchor (§"Keyword sequences"). So

```scala
val x = if pred
then foo
else bar
```

is rejected; the `if` sequence must be indented on its own lines:

```scala
val x =
  if pred
  then foo
  else bar
```

#### Maximum indent step [SN-473.1]

A non-blank code line cannot be indented more than two columns deeper than
the previous code line. This forbids "alignment" indents that line up under
a name on the previous line — when a continuation needs deeper indent, the
previous line should be split so the rule is satisfied by ordinary +2
stepping. Two exceptions allow +4 instead of +2: the previous line opened a
quote or splice context (`' {` or `$ {` as its trailing tokens), or the
previous line is a chain method-application opener — beginning with `.` and
ending with `:` or `=>` (e.g. `. within:` or `. map: x =>`). The rule
suspends inside unclosed brackets from an earlier line, inside multi-line
import selector lists, on lone `for`-filter rows (governed by [SN-924.3]),
and on the first body line of an indented scope (governed by [SN-473.8]).

#### Keyword sequences [SN-833.1] [SN-833.2]

A "keyword sequence" is a multi-word control-flow construct whose keywords
appear in a fixed order with bodies between them: `if … then … else …`,
`for … yield …`, `for … do …`, `while … do …`, and
`try … catch … finally …`. Each sequence has the shape
`K₁ B₁ K₂ B₂ … Kₙ Bₙ` — keywords interleaved with bodies. The full grammar,
including the `else if` bridge, is set out in Appendix B.

The **anchor point** of a sequence is the line and column of the first
character of K₁; when K₁ is preceded by modifiers (`inline`,
`transparent inline`), the anchor moves to the leftmost modifier, so all
alignment is measured from the visual start of the construct.

**Placement rule** [SN-833.1]. For a chain with anchor (line `L`, column
`C`), each subsequent keyword Kᵢ (i ≥ 2) is placed in exactly one of two
ways:

- **inline** — Kᵢ starts on line `L`; or
- **broken** — Kᵢ starts a new line, in column `C`.

Once any Kᵢ is broken, every later Kⱼ must also be broken. The chain has a
single break point: keywords before it sit on line `L`; keywords from it
onwards each sit on their own new line in column `C`. In particular, `then`
and `else` may share a line only if that line is also the anchor line —
once `then` is on its own line, `else` must be too. The compact form
(every Kᵢ inline) is required whenever it fits within the line length (P3).

**Body cascade** [SN-833.2]. Bodies Bᵢ for i ≥ 2 are placed in one of two
ways:

- **inline** — Bᵢ starts on the same line as Kᵢ; or
- **indented** — Bᵢ starts on a new line, indented past column `C`.

Once any Bᵢ (i ≥ 2) is indented, every later Bⱼ must also be indented. The
first body B₁ — the condition of `if`/`while`, the generators of `for`, the
body of `try` — is exempt; its layout does not constrain the later bodies.
For an `else if` bridge, the body whose layout is checked is the body after
the bridge's internal `then`, not the bridge's own condition.

Both cascades fire forward only — a break in Kᵢ does not retroactively
require K₁…Kᵢ₋₁ to break. Keyword and body cascades are independent.
Accepted and rejected examples are gathered in Appendix B.

#### Type-annotation anchor [SN-833.3]

A definition's type-annotation `:` must either sit on the same line as the
keyword that introduces the definition, or, when it breaks onto its own
line, sit in the same column as the first character of that keyword's
leading modifier (or the keyword itself if there is no modifier) — the `d`
of `def`, the `p` of `private val`, the `i` of `inline def`. The rule
applies uniformly to `val`, `var`, `lazy val`, `def`, and `given`
declarations. This is the anchor-point concept applied to the
return-type line of a heavy signature (P3), which is written
`:   ReturnType =` with the hard space of [SN-444] (P4).

#### Anchor of a heavy argument block [SN-833.4]

When a line begins (after indentation) with `(` or `[`, the **previous line
must be a tight expression**. A *tight expression* is one that does not
decompose into separate parts at top level: at bracket depth zero, it
consists of a single chain of references, member accesses, applications,
and type applications, optionally headed by one **expression-introducing
keyword** (`new`, `throw`, `return`, `yield`, `then`, `else`, `do`, `try`,
`catch`, `finally`, `extends`, `with`). Equivalently in source-layout
terms: a tight expression has no whitespace between top-level code tokens
except for a single space that may follow a leading expression-introducing
keyword.

Parenthesising any expression makes it tight: the content moves to
depth > 0 where the no-top-level-whitespace condition no longer applies.
(The one case that can't be parenthesised this way is an assignment —
`(x = y)` is tight by layout, but applying further arguments to it is
rejected by the type system, so the case doesn't arise in valid code.)

```scala
// accepted — anchor is a tight expression on its own line
recur
  ( arg1, arg2 )

new Exception
  ( reason )

foo.bar(baz).quux
  ( arg )

// rejected — the line containing the anchor also contains a top-level
// operator or assignment, so the heavy bracket appears to attach to a
// mid-line subexpression
head :: recur
  ( arg )

val foo = bar
  ( arg )

// accepted via parenthesisation
(if x then a else b)
  ( arg )

// accepted — multi-clause currying naturally satisfies the rule, since
// a whole-line `( ... )` is itself tight
f
  ( x )
  ( y )
```

Declaration signatures are not subject to this rule: the heavy `( … )` of
`def`/`val`/`given` is a parameter list, not a method application, and is
governed by the heavy-signature shape (P3). A line whose leading `(`/`[`
group is immediately followed by `=>` is a lambda parameter list, also
exempt.

#### Signature `=` placement [SN-473.9]

When a `def` or `given` signature spans more than one line, the
body-introducing `=` must be the last token on the final signature line, so
the body begins on a fresh line.

#### `given` continuation alignment [SN-140]

Continuation lines of a multi-line `given` signature that begin with `=>`
must align vertically with the leading modifier or `given` keyword on the
first line of the signature — the anchor column:

```scala
given addable: [dom        <: Dom,
                leftTopic  <: Label,
                rightTopic <: Label,
                left       <: Html of leftTopic in dom,
                right      <: Html of rightTopic in dom]
=>  left is Addable by right to (Fragment of leftTopic | rightTopic in dom) =

  (left, right) =>
    Fragment(List(left, right).nodes*).of[leftTopic | rightTopic].in[dom]
```

The `=>  ` at the start of each continuation carries the hard space of
[SN-444] (P4); the `<:` alignment across the type-parameter rows is P7
tabulation.

#### Multi-line interpolated strings [SN-560]

The interpolators whose leading and trailing whitespace is *insignificant* —
`m` (messages), `j` (JSON), `x` (XML), `y` (YAML) and `tel` (TEL); see
Appendix C — lay a multi-line `"""…"""` string out as a block anchored to
the opening prefix:

1. the opening quotes end their line — the content begins on the *next*
   line [SN-560.1];
2. the content is indented two columns beyond the opening prefix
   [SN-560.2];
3. no content line is indented less than the first content line
   [SN-560.3];
4. the closing `"""` sits alone on its line, in the same column as the
   opening prefix [SN-560.4] — the closer mirroring the opener's column is
   P5 balance.

```scala
def message =
  m"""
    This is the message.
    It spans two lines.
  """
```

A leading `( ` before the opening quotes and a trailing `,` / `)` after the
closing quotes are permitted — the string content simply must not share the
opener or closer line — so the heavy-argument form stays compliant:

```scala
extends Error
  ( m"""
      the table required a minimum width of $minimumWidth, but only $availableWidth was available
    """ )
```

This applies only to those five interpolators, because their whitespace does
not affect the result. Every other interpolator (`t`, `s`, `sh`, …) and raw
`"""` string carries significant whitespace, so the layout of their content
is left entirely to the author — and the line-length and trailing-whitespace
rules do not apply to the interior of any multi-line string.

#### Macro quotes and splices [SN-473.2]–[SN-473.7]

A quoted or spliced block is an indented scope like any other, and its
layout is the anchor discipline applied to the `'`/`$` opener. The rules of
this family are contractually frozen; their normative text and examples
follow unchanged.

Scala 3 macro quote (`'{ … }`, `'[ … ]`) and splice (`${ … }`) syntax
has two layouts:

- **Inline** — closer on the same source line as the opener. No space
  between `'` (or `$`) and the opening `{`/`[`, and no space
  immediately inside the opening or before the closing bracket
  [SN-473.7]:

  ```scala
  '{Quantity(left)}
  ${quantitative.internal.multiply('left, 'right)}
  ```

- **Multi-line** — closer on a different line from the opener. The
  layout is fixed:

  1. **Space between `'`/`$` and `{`.** The opener is two tokens,
     written `' {` (or `$ {`) [SN-473.3].
  2. **The opener pair is alone on its line.** Only indentation
     before `'`/`$`; only whitespace (or EOL) after `{` [SN-473.4].
  3. **Body indented to column `{`+2.** Every body line that begins
     a top-level statement of the quoted block sits two columns past
     the `{` column (equivalently four past the `'`/`$`). Sub-
     expressions within the body deepen further by the usual rules
     [SN-473.5].
  4. **`}` alone on its line at column `{`.** The closing brace
     occupies its own line and sits in the same column as the
     opening `{` [SN-473.2] [SN-473.6].

  ```scala
  ' {
      Multiplicable[multiplicand, multiplier, Quantity[result]]:
        (left, right) =>
          ${quantitative.internal.multiply('left, 'right).asExprOf[Quantity[result]]}
    }
  ```

  In the example above the `'` sits at column 3, the `{` at column
  5, body lines at column 7 (= `{`+2 = `'`+4), and the `}` at
  column 5.

The two layouts are determined by whether the closer ends up on the
opener's line. The inline layout is preferred when the content fits
within the line-length budget on a single line; otherwise the multi-
line layout applies.

Quoted references (`'identifier`) take no padding.

### P3 — Density

#### The necessity rule [SN-247]

A construct spread over several source lines whose one-line rendering would
fit within 100 columns is a violation: a break must always mean "didn't
fit". This is the principle's direct enforcement — where the other density
rules prescribe *which* compact form to use, [SN-247] forbids breaking at
all when no break is needed.

**Scope (v1).** Three high-confidence families of construct are checked:

- **keyword sequences** — `if`/`then`/`else`, `while`/`do`, `for`/`yield`,
  `for`/`do`, `try`/`finally`;
- **symbolic infix operator chains** — measured from the top of each chain
  (an `else if` link or the operand of a larger chain is never flagged on
  its own, since one-lining it alone would break the enclosing cascade);
- **lambdas** whose body starts on a later line than their parameters.

The one-line width is measured by collapsing every whitespace run outside
string literals to a single space, counting the code already to the left
and right of the construct on its first and last lines, and — for a
paren-wrapped named lambda — charging the two extra columns of the `{ … }`
form that [SN-312.1] would require. When an outer flagged construct
contains an inner one, only the outermost is reported.

**Bail-outs (deliberate conservatism).** A site is never flagged when its
one-line rendering cannot be *proved* to be a simple whitespace-collapsing
join. Comments, multi-line strings, colon-block interiors (a line whose
last semantic token is `:`), any `case` keyword or `match`, `end`-marker
lines, blank interior lines, multi-statement block bodies, `for`s whose
enumerator section itself spans lines (joining them would need `;`
separators), form feeds, and any scanner ambiguity all disqualify the
site. Overestimation runs the safe way: the collapsed width joins every
break with exactly one space even where a tighter spelling would be legal,
so a bail-out can suppress a finding but never invent one. The scope is
expected to widen in later versions as more join-forms are proved safe.

#### Definition shapes

A definition has three possible shapes; the shape is determined by fit,
never preference:

- **Single-line**: signature _and_ body fit on one line, including the `=`.
  ```scala
  def keys: Set[node] = edgeMap.keySet
  ```
- **Multi-line**: signature fits on one line ending with `=`, body wraps to
  the next line(s) indented 2 spaces.
  ```scala
  def map[node2](lambda: node => node2): Dag[node2] =
    Dag(edgeMap.map { (k, v) => (lambda(k), v.map(lambda)) })
  ```
- **Heavy-signature**: signature itself does not fit on one line. Use only
  when the single-line signature would exceed 100 columns; otherwise use
  single-line or multi-line.

A heavy signature has, in order:

1. The introducer line: `def name[typeParams]` (and the first parameter
   list if it fits there).
2. Each subsequent parameter list on its own line, indented 2 spaces, with
   a single space inside each parenthesis: `( name: Type, … )`
   ([SN-811], P5).
3. The return-type colon line, anchored to the column of the leftmost
   keyword that introduces the definition ([SN-833.3], P2), written
   `:   ReturnType =` with the three-space hard space ([SN-444], P4). The
   `=` ends the line ([SN-473.9], P2).
4. A blank line ([SN-677], P6).
5. The body, indented 2 spaces from the `def`.

Example:

```scala
inline def selectDynamic[variable](key: String)
  ( using environment:      Environment,
          reader:           Variable[key.type, variable],
          environmentError: Tactic[EnvironmentError] )
:   variable =

  environment.variable(reader.defaultName).let(reader.read(_)).or:
    raise(EnvironmentError(reader.defaultName)) yet reader.read(Text(""))
```

The same single/multi/heavy distinction applies to vals, vars, lazy vals,
type aliases and opaque types: keep on one line if it fits, break the
right-hand side onto a continuation line otherwise. Type intersections
beyond what fits on one line break after the `=`:

```scala
type ElectricalConductivity =
  Units[-3, Distance] & Units[-1, Mass] & Units[3, Time] & Units[2, Current]
```

A class, trait, object or enum header is likewise either _single-line_
(everything up to and including the body-introducing `:` fits on one line)
or _heavy_. When heavy, the components appear on separate lines:

```scala
class AnnotatedFields[operand <: StaticAnnotation, self, plane, limit]
  ( annotations0: Set[operand], fields0: Map[Text, Set[operand]] )
extends Fields:
```

- The type-parameter list stays on the declaration line.
- The constructor parameter list appears indented 2 spaces, with single
  spaces inside the parentheses.
- `extends …` is flush-left with the class keyword on its own line.
- The body-introducing `:` terminates the `extends` line.

`extension` declarations may sit at the top level in `_core.scala` files or
nested inside an object (see Appendix D for where an extension should live).
Methods inside an extension block follow the same single/multi/heavy rules
as ordinary `def`s.

An annotation appears on the line directly above the declaration it
annotates, flush-left with that declaration ([SN-551], P6):

```scala
@targetName("removeKey")
infix def - (key: node): Dag[node] = Dag(edgeMap - key)
```

#### Lambda forms [SN-312]

A lambda's bracketing is decided by its shape. The following table is
authoritative — where any prose elsewhere appears to permit a different
bracketing, this table governs:

| Lambda shape                                | Required form            | Rule       |
| ------------------------------------------- | ------------------------ | ---------- |
| Multi-line (body on later lines)            | colon-arg `f: x => …`    | [SN-312.3] |
| Anonymous (placeholder `_`)                 | parens `f(_.g)`          | [SN-312.4] |
| Named, single-line, last thing on its line  | colon-arg `f: x => …`    | [SN-312.2] |
| Named, single-line, mid-line                | braces `f { x => … }`    | [SN-312.1] |

So braces appear only for a short named-parameter lambda that is not the
last thing on its line, e.g. `{ key => (key, dependencies(key)) }`, with
single spaces inside the braces (a formal block, [SN-811]). The inline
arrow itself is written `x => …` or `(x, y) => …` with spaces around `=>`.
Multi-line lambdas use indented syntax:

```scala
input =>
  val root = Tag.root(content.reify.map(_.tt).to(Set))
  parse(input.iterator, root).of[content]
```

#### The canonical break order

When a line genuinely does not fit, it is broken at the **loosest-binding
seam first** — the same precedence ordering that governs spacing (P6)
governs breaking, since a line break is the top of the gap scale. The
order, from first-choice seam to last:

1. **Chain dot.** Break the method chain, moving `. method` continuations
   onto their own lines ([SN-163]).
2. **Infix operator, loosest precedence class first.** Break at the
   lowest-precedence operator present (Appendix A), the operator trailing
   ([SN-616]); only if the line still does not fit, break at the next
   class up.
3. **Argument block.** Move the parameter application to its own line as a
   heavy `( … )` block ([SN-833.4], [SN-811]).
4. **Heavy signature form.** For a declaration, fall back to the full
   heavy-signature shape.

For a method call, steps 1 and 3 compose in order:

```scala
foo.bar.baz(arg1, arg2, arg3)
```

becomes, first,

```scala
foo.bar
. baz(arg1, arg2, arg3)
```

and, if the call still doesn't fit,

```scala
foo.bar
. baz
    ( arg1, arg2, arg3 )
```

The arguments must be either *all on one line* or *all on different lines,
each indented to the same column*:

```scala
foo.bar
. baz
    ( arg1,
      arg2,
      arg3 )
```

The same `( ... )` form is used for any parenthesised block that lives on
its own line — heavy method signatures, anonymous given continuations,
multi-line type-parameter blocks. The closing bracket sits on the same line
as the last parameter; it is never alone on a line ([SN-811]).

The break order is normative, but it is stated with a caveat: it is the
part of this specification most likely to evolve as more constructs are
catalogued, and it is not yet checker-enforced. When it changes, existing
SN rules will not: they constrain what a broken form looks like, not which
seam was chosen.

### P4 — Continuation Marking

#### Chain continuation [SN-163]

When a chain of method calls wraps to a new line, the continuation begins
with `. method` — a leading dot followed by a single space (the P5 balance
of the break before it) — at the same indent as the receiver. Whether a
blank line precedes the dot is determined by the indent of the *immediately
preceding* code line:

- If the previous line is *more* indented than the `. method` line (i.e.
  the chain wrapped into a nested block), a blank line is required before
  the dot [SN-163.1]:

  ```scala
  edgeMap.flatMap:
    case (k, v) => lambda(k).edgeMap.map:
      case (h, w) => (h, (w ++ v.flatMap(lambda(_).keys)))

  . reduction
  ```

- If the previous line is at the *same* indent as the `. method` line
  (the chain stayed flat), no blank line is permitted [SN-163.2]:

  ```scala
  source.lines
  . filter(_.nonEmpty)
  . map(_.trim)
  ```

This applies to every wrapped chain.

#### Symbolic-operator continuation [SN-616]

When a symbolic infix operator (`++`, `&&`, `+`, `::`, `|`, …) joins two
operands that fall on different source lines, the operator must terminate
the first line [SN-616.1], preceded by exactly one space [SN-616.3] — the
line break on its right balanced by a single space on its left (P5) — and
the continuation must be indented two columns beyond the line on which the
left operand begins [SN-616.2]:

```scala
left ++
  right          // ok
```

The operator must not begin the continuation line, nor stand alone on its
own line, nor abut its left operand, nor may the continuation use any
indent other than +2:

```scala
left
++ right          // wrong — operator begins the continuation

left
  ++              // wrong — operator alone on its own line
  right

left++
  right          // wrong — trailing operator abuts its left operand

left ++
    right          // wrong — continuation indented +4, not +2
```

The one-space requirement of [SN-616.3] applies even to operators legally
written tight when inline: `a++b` is fine on one line, but the moment the
operator trails a break it must read `a ++⏎`.

A chain of same-precedence operators keeps every continuation at the same
+2 indent (it does not step deeper with each operator):

```scala
alpha ++
  beta ++
  gamma          // ok
```

This rule covers both symbolic value operators and symbolic infix *type*
operators (`A | B`, `A & B`, `A *: B`). It does **not** apply to word-named
infix operators (`is`, `raises`, `max`), to pattern alternatives
(`case A | B`), or to `.` method selection — wrapped method chains follow
[SN-163] above.

#### Hard spaces [SN-444]

When a short marker token starts a continuation line — `=>` in a given
continuation, or the return-type `:` of a heavy signature — it is followed
by a fixed pad of spaces such that **marker plus pad together span exactly
two indent steps** (four columns), placing the payload on the indent grid
two steps past the marker's column:

- `=>  ` (two trailing spaces): `=>` is 2 columns + 2 spaces = 4.
- `:   ` (three trailing spaces): `:` is 1 column + 3 spaces = 4.

The marker itself sits on the even-indent grid ([SN-926], P1); the payload
lands on the grid two steps later. The `:   Type =` form is the practical
one to remember: **three spaces between the leading `:` and the return type
on a heavy-signature return-type line.** (The general rule elsewhere is one
space after `:`.)

### P5 — Balance

#### Operator spacing [SN-376]

Every operator must have either zero spaces or exactly one space on each
side. Within those choices, four constraints apply:

1. **Symmetry.** The spacing on the left and right of an operator must
   match: `a + b` and `a+b` are both fine; `a +b` and `a+ b` are not.
2. **Single-character only for zero spaces.** Multi-character operators
   (`=>`, `->`, `<-`, `<:`, `>:`, `&&`, `||`, `==`, `!=`, `<=`, `>=`,
   `<<`, `>>`, `>>>`, `&~`, etc.) must always carry one space on each
   side.
3. **Equal-precedence consistency.** Within a single unparenthesised
   expression, all operators of equal precedence must use the same
   spacing.
4. **Precedence ordering.** Higher-precedence operators must never have
   more spacing than lower-precedence operators in the same
   unparenthesised expression. Tighter binding reads as tighter spacing
   (P6).

The precedence classes are listed in Appendix A.

Examples:

```scala
a + b               // ok
a+b                 // ok (single-char, symmetric)
a + b*c             // ok (* tighter than +, less spacing)
a + b*c + d         // ok (consistent + spacing)
a+b * c             // wrong (* has more spacing than higher-precedence +)
a + b - c           // ok (same-precedence, same spacing)
a + b-c             // wrong (mixed spacing within precedence 8)
a => b              // ok (multi-char, must be spaced)
a=>b                // wrong (multi-char must be spaced)
asRgb24Int(c)&255   // ok (single-char `&`, symmetric, no spaces)
```

Custom infix words (`is`, `of`, `in`, `by`, `to`, `under`, `on`, `raises`)
follow the same rule as letter-named operators (precedence 1): because they
are multi-character, they must always be spaced.

#### Assignment spacing [SN-376.1]

Assignment and mutation operators (`=`, `+=`, `-=`, …) require *at least*
one space before and *exactly* one space after, when the right-hand side
appears on the same line. The "at least one before" admits the P7
tabulation of `=`s in aligned parameter-default rows:

```scala
( mode:  UnixMode          = UnixMode(),
  user:  UnixUser          = UnixUser(0),
  group: UnixGroup         = UnixGroup(0) )
```

#### Symbolic method names [SN-013]

When a method's name is a symbolic operator (`+`, `-`, `*`, `/`, `++`,
etc.), a single space separates the operator from the following parameter
list or type-parameter list — the definition renders the name as it is
used, spaced:

```scala
infix def + (right: Double): Double = left + right
infix def * [rightMin <: Double, rightMax <: Double](right: rightMin ~ rightMax)
```

Alphabetic method names follow the usual rule (no space before the
parameter list).

#### Bracket interiors: both or neither [SN-811] [SN-402]

Bracket interiors are padded on both sides or on neither — mirrored,
because the brackets are paired.

A **formal block** — a `(…)`/`[…]` group functioning as a multi-line
parameter or argument block — pads its interior: a single space directly
inside both the opener and the closer, `( name: Type, … )` [SN-811]. Its
closing bracket shares a line with the last parameter; it never stands
alone on a line (P3 — the lone closer would spend a line on no content).

A **compact pair** — any single-line bracket group that is not a formal
block — takes no interior padding: `(a, b)`, `Map[K, V]` [SN-402]. (Tuples
on a fresh line after `=>` — lambda and match-case body tuples — may use
either tight or formal-style spacing.)

One space follows every comma; no space precedes one ([SN-529], P6).

### P6 — Proximity

#### The gap scale

Gap size is monotone in binding looseness: `none < space < newline <
blank line < two blank lines`. Every rule in this section places a
construct on that scale.

#### Comma spacing [SN-529]

No space is permitted before a comma [SN-529.1] — the comma binds to the
item it follows — and exactly one space is required after it [SN-529.2],
except where the extra spaces build a multi-row alignment column (P7,
§"Comma-column alignment").

#### Chunks [SN-315]

A **chunk** is a statement (or expression at statement position) that
spans two or more source lines. Chunks may contain blank lines in their
interior. Single-line statements are not chunks.

The chunk rule is the unifying blank-line principle:

> Every chunk must be **followed** by a blank line, and **preceded** by
> a blank line — except when it is the first thing in a newly opened
> indented scope (a class/trait/object body, a method body, a block, a
> match's case list, etc.), in which case the preceding-blank rule does
> not apply.

Two single-line statements may sit adjacent with no blank between them
(neither is a chunk, so the rule says nothing), or may take a single blank
line as expressive grouping (a retained freedom). The moment one of them
becomes a chunk, a blank line is required on either side.

This rule subsumes two older blank-line rules: the previous "multi-line
case must have a blank before it" rule, and the multi-line side of the
sibling-declaration padding rule.

The first statement inside a newly-opened indented scope (class body, def
body, match's first case, …) needs no blank line before it:

```scala
class Foo:
  def first =     // first member, no blank required before
    body
```

The exception remains when the enclosing scope's signature is *heavy*:
[SN-677] requires a blank line between the `:   ReturnType =` line and the
body, so the first member of a heavy scope is always preceded by a blank.

#### Maximum blank lines [SN-783]

At most **one** blank line is permitted anywhere, with a single exception:
a run of up to **two** blank lines may sit immediately before the first
line, or immediately after the last body line, of a definition with a
heavy signature (one whose signature includes a `:   ReturnType =` line).
The two-blank gap is the top of the gap scale, and it marks the heaviest
construct the style recognises — nothing else earns it.

#### Blank line after a heavy return type [SN-677]

The return-type line of a heavy signature — beginning `:` + three spaces
and ending with the body-introducing `=` — must be separated from the body
that follows it by a blank line. The signature and the body are distinct
in kind, and the blank line keeps that distinction visible.

#### Import separation [SN-441]

A blank line must separate the import region from the first declaration.
(One blank line also separates the `package` declaration from the first
import — that line is part of the frame, [SN-658].)

#### Annotation adjacency [SN-551]

An annotation sits directly above the declaration it annotates, with no
blank line between them [SN-551.2] — zero gap, because the annotation
binds to the declaration as tightly as a modifier does. An annotation
must be followed by a declaration at all [SN-551.1]. The annotation is
flush-left with the declaration it annotates — the same column, P2's
anchor extended to the line above.

### P7 — Tabulation

#### Case runs [SN-326]

`match` appears on the same line as the scrutinee; cases are at +2 indent:

```scala
queue match
  case Nil => None

  case (vertex, trace) :: tail =>
    …
```

Within a _run_ of cases whose bodies fit on one line — consecutive
single-line cases at the same indent with no intervening blank line —
patterns are right-padded with spaces so that every `=>` falls in the same
column. The alignment column is determined by the longest pattern in the
run:

```scala
case CannotExecuteGit   => m"the `git` command could not be executed"
case CloneFailed        => m"the repository could not be cloned"
case InvalidRepoPath    => m"the repository path was not valid"
…
case CannotSwitchBranch => m"the branch could not be changed"
```

A case whose body wraps onto additional lines ends the run. A blank line
separates the multi-line case from its neighbours ([SN-315]); cases that
follow a multi-line case start a fresh alignment run with their own
column. The rule holds everywhere, in existing code and new.

In a multi-line case, exactly one space precedes the `=>`. When the
*pattern* itself is heavy — `case Foo` with a `( a, b )` parameter block
on the next line — the `=>` trails the last pattern token on the same
line (never alone on its own line) and the body begins on a fresh line.
(A case whose *guard* spans multiple lines is exempt: the parser needs the
`=>` dedented below the indented guard.)

#### `using`-clause alignment [SN-946]

Inside a multi-line `( using … )` clause, each fresh parameter row begins
at the column of the clause's first parameter token — including any
per-parameter modifier such as `inline`. Parameter names are right-padded
so that their `:` characters align in a single column, and types are
left-padded so that they begin in a single column (see the heavy-signature
example under P3). A row is fresh iff the previous line ended with `,`,
`(`, or `using`; otherwise the line is a wrapped continuation of the
previous parameter's type, aligned intentionally to the type column.

#### For-comprehension alignment [SN-924]

Single-line forms are preferred when they fit (P3):

```scala
for left <- elements; right <- elements
do if element.compare(left, right) then map(left) += right
```

When the comprehension is split across multiple lines, two layouts are
acceptable:

- **Aligned-LHS style.** The first generator follows `for ` on the same
  line; subsequent generators are indented to put their LHS in the
  column of the first generator's LHS (4 columns past `for`). `yield`
  / `do` align with `for` ([SN-833.1], P2).

  ```scala
  for x  <- xs
      y  <- ys.filter(p)
      zs =  gather(y)
  yield x + y + zs.size
  ```

- **Indented-block style.** `for` sits alone on its line; generators
  follow on subsequent lines indented two spaces, and `yield` / `do`
  aligns with `for`.

  ```scala
  for
    x <- xs
    y <- ys
  yield x + y
  ```

In either layout, when more than one generator/binding/filter line
appears:

- All `<-` and `=` operators are vertically aligned [SN-924.1]. The LHS
  is right-padded with spaces as needed to make the columns match.
- All generator/binding LHSs sit in the same column [SN-924.2].
- An `if` filter on its own line is placed in the column of the `<-`/`=`
  operators, not in the LHS column [SN-924.3] — and is not separated
  from the preceding enumerator by a blank line [SN-924.4]. A filter
  sharing a line with its generator (`x <- xs if cond`) is exempt from
  the column rule.

#### Comma-column alignment [SN-529 relaxation]

When two or more consecutive lines align values into vertical columns by
adding extra spaces after commas, the extra spaces are permitted:

```scala
val H  = ChemicalElement(1,   t"H",  t"Hydrogen")
val He = ChemicalElement(2,   t"He", t"Helium")
val Li = ChemicalElement(3,   t"Li", t"Lithium")
```

The relaxation requires at least two adjacent lines exhibiting the
alignment pattern — a run — and the alignment must be consistent across
the run; an isolated line with extra spaces after a comma is still a
violation.

### P8 — Findability

#### Imports [SN-302]

Imports are grouped, with one blank line between groups ([SN-302.3], the
P6 gap scale applied to import regions) and alphabetical ordering within
each group ([SN-302.2]). The groups appear in this order:

1. `language.*` (and other `import language.…` directives)
2. `java.*` and `javax.*`
3. `scala.*`
4. compiler and JVM/JEE internals (`dotty.*`, `com.sun.*`, `sun.*`,
   `jakarta.*`)
5. project-family library imports (the project's own modules and the
   libraries that share this standard)

Wildcard imports (`import anticipation.*`) are the norm for library
imports. Within the project-family group, wildcard imports and named
imports (`import filesystemOptions.readAccess`, `import AsyncError.Reason`)
may interleave as the code requires.

Project-family imports must not introduce aliases [SN-302.1]. Both
`import x.y as z` (Scala 3) and `import x.{y => z}` (Scala 2 style) are
forbidden for group-5 imports — write the full path instead, so the name
in code is the name at its definition. Standard-library and JDK aliases
(`import scala.collection.mutable as scm`, `import java.util.concurrent
as juc`) remain an established convention, and aliasing inside a `using`
clause or a method body is unaffected.

#### File naming [SN-847]

A module's source files follow these patterns:

- `module.TypeName.scala` — a single class, trait, enum or object plus its
  companion object; the file must actually declare the named top-level
  type. The package given block for that type lives in the companion.
- `module_core.scala` — top-level extensions, package-level given blocks,
  and nested package blocks for the module.
- `<umbrella>_module_core.scala` — re-exports under the project's umbrella
  package. Contains only the umbrella `package` declaration and one or more
  `export` statements.
- `module.internal.scala`, `module.protointernal.scala`,
  `module.anteprotointernal.scala` — implementation-detail traits and
  objects layered to satisfy compile-time ordering of givens. The prefixes
  denote successively earlier layers in the resolution order.

#### Companion ordering [SN-398]

When a type and its companion appear in the same file, place the companion
`object` _before_ the type definition. Cross-companion references are
resolved at use-site, so this ordering keeps the more frequently-read API
surface at the top of the file.

#### Umbrella re-exports [SN-742] [SN-742.1]

A project may define an *umbrella package*: a single package that
re-exports every public name, so that one wildcard import reaches
everything. Where it does, every public module in a component — a top-level
definition living in its own `<component>.<Name>.scala` file, other than
`internal` modules — must be re-exported into the umbrella package by its
export surface [SN-742], and every public top-level extension method
likewise by its leaf name [SN-742.1] (unless marked `@unexported`).

#### Documentation [SN-162.2]

Prose documentation lives in `doc/` markdown files, not in `/** … */`
doc-comments. Documentation has one home, findable without opening source
files, and source files carry only code and `//` comments.

## Part III — Rule Index

Every rule enforced by the Decorum checker, its section in Part II, and
the principle it derives from. Sub-rules (`SN-nnn.e`) are documented with
their family.

| Rule       | Part II section                          | Principle |
| ---------- | ---------------------------------------- | --------- |
| [SN-013]   | Symbolic method names                    | P5 — Balance |
| [SN-015]   | Trailing whitespace                      | P1 — The Frame |
| [SN-131]   | Package declaration                      | P1 — The Frame |
| [SN-135]   | Indentation                              | P1 — The Frame |
| [SN-140]   | `given` continuation alignment           | P2 — Anchoring |
| [SN-162]   | Block comments; Documentation            | P1 — The Frame (162.2 also P8) |
| [SN-163]   | Chain continuation                       | P4 — Continuation Marking |
| [SN-230]   | Line length                              | P1 — The Frame |
| [SN-247]   | The necessity rule                       | P3 — Density |
| [SN-302]   | Imports                                  | P8 — Findability |
| [SN-312]   | Lambda forms                             | P3 — Density |
| [SN-315]   | Chunks                                   | P6 — Proximity |
| [SN-326]   | Case runs                                | P7 — Tabulation |
| [SN-376]   | Operator spacing                         | P5 — Balance |
| [SN-376.1] | Assignment spacing                       | P5 — Balance |
| [SN-398]   | Companion ordering                       | P8 — Findability |
| [SN-402]   | Bracket interiors: both or neither       | P5 — Balance |
| [SN-441]   | Import separation                        | P6 — Proximity |
| [SN-444]   | Hard spaces                              | P4 — Continuation Marking |
| [SN-473.1] | Maximum indent step                      | P2 — Anchoring |
| [SN-473.2]–[SN-473.7] | Macro quotes and splices      | P2 — Anchoring |
| [SN-473.8] | The anchor                               | P2 — Anchoring |
| [SN-473.9] | Signature `=` placement                  | P2 — Anchoring |
| [SN-529]   | Comma spacing; Comma-column alignment    | P6 — Proximity (relaxation P7) |
| [SN-551]   | Annotation adjacency                     | P6 — Proximity |
| [SN-560]   | Multi-line interpolated strings          | P2 — Anchoring |
| [SN-616]   | Symbolic-operator continuation           | P4 — Continuation Marking (616.3 grounded in P5) |
| [SN-658]   | Package declaration                      | P1 — The Frame |
| [SN-677]   | Blank line after a heavy return type     | P6 — Proximity |
| [SN-742]   | Umbrella re-exports                      | P8 — Findability |
| [SN-742.1] | Umbrella re-exports                      | P8 — Findability |
| [SN-783]   | Maximum blank lines                      | P6 — Proximity |
| [SN-799]   | License header                           | P1 — The Frame |
| [SN-811]   | Bracket interiors: both or neither       | P5 — Balance |
| [SN-833.1] | Keyword sequences                        | P2 — Anchoring |
| [SN-833.2] | Keyword sequences                        | P2 — Anchoring |
| [SN-833.3] | Type-annotation anchor                   | P2 — Anchoring |
| [SN-833.4] | Anchor of a heavy argument block         | P2 — Anchoring |
| [SN-847]   | File naming                              | P8 — Findability |
| [SN-924]   | For-comprehension alignment              | P7 — Tabulation |
| [SN-926]   | Indentation                              | P1 — The Frame |
| [SN-946]   | `using`-clause alignment                 | P7 — Tabulation |

## Appendix A — Operator precedence classes

Operator precedence (lowest to highest), classified by first character.
The same classes order both spacing ([SN-376], P6: looser binding, more
space) and breaking (the canonical break order, P3: looser binding, break
first).

1. letter-named operators (`is`, `of`, `in`, `by`, `to`, `raises`, …)
2. `|`
3. `^`
4. `&`
5. `=`, `!`
6. `<`, `>`
7. `:`
8. `+`, `-`
9. `*`, `/`, `%`
10. other special characters

## Appendix B — Keyword sequences and the `else if` bridge

The recognised keyword sequences are:

- `if … then … else …` (with `else` optional)
- `for … yield …` and `for … do …`
- `while … do …`
- `try … catch … finally …` (with one or both of `catch` and `finally`)

Each sequence has the shape `K₁ B₁ K₂ B₂ … Kₙ Bₙ` — keywords interleaved
with bodies. The placement and cascade rules are stated under P2 in
Part II ([SN-833.1], [SN-833.2]); this appendix defines the `else if`
bridge and gathers the examples.

### `else if` bridges

An `else` followed on the same line by `if` (optionally with modifiers
between them, as in `else inline if`) forms a single **`else if` bridge**.
The bridge is one chain element; its internal `if` and the `then` that
follows it are part of the bridge unit and are not separately subject to
the placement rule. The condition between `if` and `then` belongs to the
bridge.

If `else` appears on its own line and the next `if` is on a subsequent,
more deeply indented line, that inner `if` is **not** a bridge — it starts
a fresh chain with its own anchor, and the outer chain ends at `else`.

### Examples — accepted

```scala
if x > 0 then x else -x

if x > 0 then x       // inline up to `then`, broken at `else`
else -x

if x > 0              // broken at `then`, all later keywords broken
then x
else -x

if x > 0 then         // `then` inline; first inner body indented
  longBody            // forces `else`'s body to indent too
else
  other

if a then x else if b then y else z

if a                  // broken throughout; bridge sits in anchor column
then x
else if b
then y
else z

if x > 0 then         // bridge with indented bodies
  1
else if x < 0 then
  -1
else
  0

if a then x           // newline + indent between `else` and inner `if`:
else                  // outer chain ends at `else`; inner `if` is a
  if b then y         // fresh chain anchored at column 3
  else z
```

### Examples — rejected

```scala
if x > 0              // `then` broken from `if`, but `else` inline with
then x else -x        // `then` — cascade violated

if x > 0              // broken `then` not in anchor column
    then x
    else -x

if x > 0 then 1       // `else if` bridge not in anchor column
    else if x < 0 then -1
  else 0

while running()       // `do` broken but not aligned with `while`
    do step()

if x > 0 then         // first inner body indented but `else`'s body inline
  longBody
else other
```

The same rules apply to `try`/`catch`/`finally`:

```scala
try parse(s)
catch case e: Error => log(e)
finally close()
```

## Appendix C — Whitespace-insignificant interpolators

The string interpolators in general use are:

- `t"…"` for `Text`.
- `m"…"` for `Message`.
- `s"…"` and plain `"…"` only where a raw `String` is genuinely needed.

Five interpolators produce values on which the leading and trailing
whitespace of the literal has no effect:

| Prefix | Content  |
| ------ | -------- |
| `m`    | messages |
| `j`    | JSON     |
| `x`    | XML      |
| `y`    | YAML     |
| `tel`  | TEL      |

Because their whitespace is insignificant, their multi-line `"""…"""`
literals can be — and must be — laid out as indented blocks ([SN-560],
P2). Every other interpolator (`t`, `s`, `sh`, …) and raw `"""` string
carries significant whitespace, so the layout of its content is left
entirely to the author, and the line-length and trailing-whitespace rules
do not apply to the interior of any multi-line string.

## Appendix D — Where to put an extension so it resolves

A call `value.method` finds an extension `method` through **one of two**
scopes, and *where* the extension is declared decides which:

1. **Lexical scope** — the extension is declared at, or imported into, the
   current scope. This is how every top-level extension is reached: a consumer
   writes `import gossamer.*` (or the project's umbrella import, which
   re-exports member modules) and the package-level extension comes into scope.

2. **Implicit scope of the receiver type** — the extension is a member of an
   object that belongs to the receiver type's implicit scope, in which case it
   resolves **with no import at all**. The implicit scope of a type `T`
   includes the companion objects of: `T` itself; for an *opaque type*, the
   object in which it is defined; every base type of `T`; and every type
   *argument* of `T` (so an extension on `List[Foo]` may live in `object Foo`).

The practical consequence: **prefer the companion object** for an extension
whose receiver is a concrete type *you define in this module*. Placing
`extension (x: Foo) def bar` inside `object Foo` keeps `x.bar` resolvable
everywhere `Foo` is, without anyone importing it — and it keeps the package's
top level uncluttered. `dissonance.Diff`, `zephyrine.Cursor` and
`stratiform.Tel` follow this.

Keep an extension **at the top level** when:

- the receiver is a *foreign* type — a primitive (`Int`, `Double`), `String`,
  `StringContext` (every `t"…"`-style interpolator), a collection (`List`,
  `IArray`, `Stream`), a JDK type, or a type owned by *another* module. You
  cannot reopen a companion you don't own, so these have no companion anchor
  here and must rely on lexical import. (Likewise, a sibling sub-module cannot
  reopen a companion defined in another sub-module — e.g. `stratiform.binary`
  cannot add to `stratiform.core`'s `object Tel`.)
- the receiver is an unconstrained type parameter (`extension [value](x: value)`),
  which has no anchor at all.

Two traps when moving an extension into a companion:

- **Named umbrella re-exports.** `<umbrella>_<module>_core.scala` often lists the
  extension by name (`export module.{ Foo, bar }`). Once `bar` lives in
  `object Foo` it is no longer a top-level member, so drop it from that list —
  the exported *type* `Foo` already carries its companion's extensions through
  implicit scope.
- **`Dynamic` companions and member clashes.** Selecting a *no-such-member* name
  on a `Dynamic` **object** (`Foo.bar` where `object Foo extends Dynamic`) is
  rewritten to `applyDynamic` before a `Foo.type` extension is tried, so such
  static-style extensions must stay top-level or become plain companion
  `def`s (see `jacinta.Json.parseTracked`). A `Dynamic` **value class** does
  *not* have this problem — `tel.edited` still resolves to a companion
  extension even though `class Tel extends Dynamic`. Similarly, if the
  companion already declares a member with the extension's name, the member
  wins; give the extension a distinct name or make it a `def`.

## Summary checklist

- 2-space indent, spaces only, 100-column hard limit; even-column indent
  grid, marker tokens on the grid.
- License header (the project's fixed *H* lines) → `package` → blank →
  grouped imports → blank → code.
- Imports: fixed group order, alphabetical within group, no aliases for
  project-family libraries.
- Indented (colon) syntax; lambda bracketing by the SN-312 table.
- Anything that fits on one line goes on one line (SN-247); break at the
  loosest-binding seam first.
- Operator spacing: zero or one space, symmetric; zero only for
  single-character operators; same-precedence operators in one expression
  share spacing; higher-precedence operators have ≤ spacing than
  lower-precedence operators.
- Commas: one space after, none before, except in 2+-line alignment runs.
- Single-line / multi-line / heavy-signature definitions; choice forced by
  the 100-column limit.
- Heavy signature: parameter blocks indented 2 with internal spaces; return
  type `:   Type =` anchored to the declaration keyword; blank line before
  body.
- Match cases: `=>` aligned within every single-line run; multi-line cases
  set off by blank lines.
- Symbolic-operator method names take a space before the parameter list.
- Chain continuation: `. method` at receiver indent; blank-line-before iff
  the preceding line is more indented.
- Operator continuation: one space, then the operator, then the break
  (`left ++⏎`); continuation at +2, chains stay flat at +2.
- Multi-line method applications: `( arg, arg )` (or all-aligned
  multi-line) with a space inside each paren; the closing bracket sits
  with the last argument, never alone.
- Macro quotes/splices: inline `'{x}`/`${x}` unpadded; multi-line `' {`
  alone on its line, body at `{`+2, `}` alone at the column of `{`.
- Multi-line `m`/`j`/`x`/`y`/`tel` strings: opener ends its line, content
  +2 from the prefix, closer alone and aligned with the prefix. Other
  interpolators' and raw `"""` content is left alone.
- Keyword sequences: broken keywords align with K₁; forward-only keyword
  and body cascades, independently.
- For-comprehensions: aligned-LHS or indented-block layout; `<-`/`=`
  aligned, LHSs share a column, `if` filters in the operator column.
- Blank lines: chunks set off by one blank; at most one blank anywhere,
  except up to two around a heavy-signature definition.
- Companion `object` before the class/trait/enum it accompanies; every
  public name re-exported to the umbrella package.
