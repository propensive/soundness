## Tables

### About

Tabular data renders to the terminal through a layout engine that takes the hard part seriously:
fitting columns of uneven content into a fixed width. A table is defined either implicitly — a
sequence of case classes becomes a table with a column per field — or explicitly, naming each
column, its alignment, and its *sizing strategy*: wrap as a paragraph, truncate with an ellipsis,
collapse entirely when space runs short. The result draws with box characters in a style chosen in
scope.

### On tables

Most terminal tables are format strings: fixed widths guessed at development time, truncating or
overflowing when the data disagrees. Real layout is negotiation — some columns matter more than
others, prose columns can wrap where identifiers cannot, and below some width a column is better
dropped than mangled — and that negotiation is exactly what a layout engine can do and a format
string cannot.

Separating a table's scaffold from its data and its width is [decoupling](../philosophy/decoupling.md), and what lets one definition render at any width.

Soundness computes the layout per rendering: each column declares how it may shrink, and the
engine distributes the available width, wrapping with [hyphenation](hyphenation.md) where wrapping
is allowed. Everything comes from the `soundness` package, with a style, a metric, and an overflow
policy in scope:

```scala
import soundness.*
import tableStyles.thickTableStyle
import textMetrics.uniformMetric
import columnAttenuation.ignoreAttenuation
import hyphenations.englishHyphenation
import stdios.javaLangSystemStdio
import strategies.throwUnsafely
```

### A table from a case class

A sequence of case classes tabulates directly, a column per field, titled from the field names;
`grid` lays it out at a width and produces the lines:

```scala
case class Codebase(id: Text, name: Text, linesOfCode: Int, year: Int, description: Text)

val libraries = List
  ( Codebase(t"gossamer", t"Gossamer", 4200, 2019, t"Statically-checked text operations"),
    Codebase(t"jacinta", t"Jacinta", 3800, 2020, t"JSON parsing and serialization"),
    Codebase(t"escritoire", t"Escritoire", 1900, 2018, t"Tabular layout for the terminal") )
```

```scala
libraries.tabulation.grid(80).render.each(Out.println(_))
```

Numeric columns right-align by default, text left-aligns, and the whole table is `Printable`, so
printing it at the terminal's own width needs nothing more than `Out.println`.

Three types are at work, and knowing which is which explains where each decision is made. A
`Scaffold` says how a type is put into columns — their content, order and parameters — but holds
no data. A `Tabulation` is data arranged by a scaffold: an array of textual values, one per row
and column, no longer referring to the row type at all. A `Grid` is a tabulation fitted to a
particular width, which is the point at which a column may shrink or vanish. `tabulation` above
derived a scaffold for `Codebase` and applied it in one step; the sections below take the stages
separately, which is what gives control over both the layout and the width.

### Explicit columns

Columns defined by hand choose their titles, content, alignment and sizing. A `Paragraph` column
wraps; a `Collapsible` column vanishes when the layout falls below its threshold; a `Fixed` or
`Shortened` column truncates with an ellipsis:

```scala
val table = Scaffold[Codebase]
  ( Column(t"Name")(_.name),
    Column(t"Identifier", sizing = columnar.Collapsible(0.9))(_.id),
    Column(t"LoC", sizing = columnar.Collapsible(0.3))(_.linesOfCode),
    Column(t"Year", sizing = columnar.Collapsible(0.5))(_.year),
    Column(t"Description", textAlign = TextAlignment.Justify,
        sizing = columnar.Paragraph)(_.description) )

table.tabulate(libraries).grid(70).render
```

Each column is a title and a lambda from the row type to the cell value, and the lambdas here
return a mixture of `Text` and `Int` without a type annotation between them. That works because
the *title* fixes the textual type: a `Textual` instance is resolved for the title's type, that
instance names the typeclass — usually `Show` — that converts other values into it, and each
lambda's result type is shown through it. The table's own cell type is then the least upper bound
of its columns'. So `table` above is a `Scaffold[Codebase, Text]`, and writing the titles as
`e"Name"` instead of `t"Name"` would have made it a `Scaffold[Codebase, Teletype]`, with color and
style available in every cell.

The `Collapsible` thresholds are meaningful only relative to each other. They are compared against
the *slack* the layout is working at — the number the next section is about — so at a slack of
`0.6` both `LoC` and `Year` show, at `0.4` only `LoC` does, and at `0.2` neither does.

### Renderings at different widths

The same table, the same data, laid out at five widths. At 120 characters everything fits on one
line per row:

```mono
┏━━━━━━━━━━━━━━┯━━━━━━━━━━━━━━┯━━━━━━┯━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ Name         │ Identifier   │  LoC │ Year │ Description                                                     ┃
┠──────────────┼──────────────┼──────┼──────┼─────────────────────────────────────────────────────────────────┨
┃ Wisteria     │ wisteria     │  581 │ 2017 │ Simple, fast and transparant generic derivation for typeclasses ┃
┃ Quantitative │ quantitative │ 1271 │ 2023 │ Statically-checked physical units with seamless syntax          ┃
┃ Turbulence   │ turbulence   │ 1047 │ 2022 │ Simple tools for working with data streams                      ┃
┃ Escritoire   │ escritoire   │  494 │ 2018 │ A library for writing tables                                    ┃
┗━━━━━━━━━━━━━━┷━━━━━━━━━━━━━━┷━━━━━━┷━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

At 100, *Identifier* is gone — it is the collapsible column with the threshold closest to `1`, so
it is the first to go:

```mono
┏━━━━━━━━━━━━━━┯━━━━━━┯━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ Name         │  LoC │ Year │ Description                                                     ┃
┠──────────────┼──────┼──────┼─────────────────────────────────────────────────────────────────┨
┃ Wisteria     │  581 │ 2017 │ Simple, fast and transparant generic derivation for typeclasses ┃
┃ Quantitative │ 1271 │ 2023 │ Statically-checked physical units with seamless syntax          ┃
┃ Turbulence   │ 1047 │ 2022 │ Simple tools for working with data streams                      ┃
┃ Escritoire   │  494 │ 2018 │ A library for writing tables                                    ┃
┗━━━━━━━━━━━━━━┷━━━━━━┷━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

At 80 no further column need be dropped; the description wraps instead:

```mono
┏━━━━━━━━━━━━━━┯━━━━━━┯━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ Name         │  LoC │ Year │ Description                                     ┃
┠──────────────┼──────┼──────┼─────────────────────────────────────────────────┨
┃ Wisteria     │  581 │ 2017 │ Simple, fast and transparant generic derivation ┃
┃              │      │      │ for typeclasses                                 ┃
┃ Quantitative │ 1271 │ 2023 │ Statically-checked physical units with seamless ┃
┃              │      │      │ syntax                                          ┃
┃ Turbulence   │ 1047 │ 2022 │ Simple tools for working with data streams      ┃
┃ Escritoire   │  494 │ 2018 │ A library for writing tables                    ┃
┗━━━━━━━━━━━━━━┷━━━━━━┷━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

At 60, *Year* — the next-highest threshold — goes, and the justification of the description
becomes visible as the lines stretch to the full width:

```mono
┏━━━━━━━━━━━━━━┯━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ Name         │  LoC │ Description                        ┃
┠──────────────┼──────┼────────────────────────────────────┨
┃ Wisteria     │  581 │ Simple,   fast   and   transparant ┃
┃              │      │ generic derivation for typeclasses ┃
┃ Quantitative │ 1271 │ Statically-checked physical  units ┃
┃              │      │ with seamless syntax               ┃
┃ Turbulence   │ 1047 │ Simple tools for working with data ┃
┃              │      │ streams                            ┃
┃ Escritoire   │  494 │ A library for writing tables       ┃
┗━━━━━━━━━━━━━━┷━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

And at 40, only two columns remain and a description may run to four lines:

```mono
┏━━━━━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━┓
┃ Name         │ Description           ┃
┠──────────────┼───────────────────────┨
┃ Wisteria     │ Simple,   fast    and ┃
┃              │ transparant   generic ┃
┃              │ derivation        for ┃
┃              │ typeclasses           ┃
┃ Quantitative │ Statically-checked    ┃
┃              │ physical  units  with ┃
┃              │ seamless syntax       ┃
┃ Turbulence   │ Simple   tools    for ┃
┃              │ working   with   data ┃
┃              │ streams               ┃
┃ Escritoire   │ A library for writing ┃
┃              │ tables                ┃
┗━━━━━━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━┛
```

### The layout algorithm

The scaffold does not decide the widths; it delegates to each column, passing a `Double` between
`0` and `1` — the *slack* on that column, which is simply the opposite of pressure. Given a slack,
a column reports the width it would take, and a column under enough pressure may report that it
will not render at all. A prose column shrinks smoothly as its text reflows; an identifier column
cannot shrink past its content and either holds its width or disappears.

Summing those widths gives the table's width at that slack, and from there it is a search: raise
the slack where the table fits, lower it where it does not, hunting logarithmically for the
highest slack at which the table still fits the space available.

That maximum usually sits *just after* a column has been hidden, which tends to leave space
unused — the column that vanished freed more room than the survivors needed. So a second pass
raises the slack on the remaining columns, without reintroducing the ones already removed, letting
them spread into what is left. Experimentally this produces markedly better results than stopping
at the first fit.

The search rests on one requirement, which any custom `Columnar` must honor: a column must
respond *monotonically* to slack. Decreasing the slack may leave a column the same width, but it
must never make it wider. `Paragraph`, `Collapsible`, `Fixed` and `Shortened` are simply the
strategies provided; the concept is open, and a sizing strategy that responds to slack in some
other way is an ordinary implementation of the same interface.

### Styles

The box drawing is a `TableStyle` given: heavy outer rules with thin inner ones by default,
rounded corners, horizontal-only rules, or a minimal style with almost no furniture — a change of
import, not of table code:

```scala
import tableStyles.thinRoundedTableStyle
```

`thinRoundedTableStyle` draws light rules throughout, with rounded corners:

```mono
╭──────────────┬──────┬──────────────────────────╮
│ Name         │  LoC │ Description              │
├──────────────┼──────┼──────────────────────────┤
│ Wisteria     │  581 │ Simple,     fast     and │
│              │      │ transparant      generic │
│              │      │ derivation           for │
│              │      │ typeclasses              │
│ Quantitative │ 1271 │ Statically-checked       │
│              │      │ physical   units    with │
│              │      │ seamless syntax          │
╰──────────────┴──────┴──────────────────────────╯
```

`horizontalTableStyle` keeps the horizontal rules and drops the verticals:

```mono
╶────────────────────────────────────────────────╴
  Name            LoC   Description
╶────────────────────────────────────────────────╴
  Wisteria        581   Simple,     fast     and
                        transparant      generic
                        derivation           for
                        typeclasses
  Quantitative   1271   Statically-checked
                        physical   units    with
                        seamless syntax
╶────────────────────────────────────────────────╴
```

`verticalTableStyle` does the opposite:

```mono
╷              ╷      ╷                          ╷
│ Name         │  LoC │ Description              │
│              │      │                          │
│ Wisteria     │  581 │ Simple,     fast     and │
│              │      │ transparant      generic │
│              │      │ derivation           for │
│              │      │ typeclasses              │
│ Quantitative │ 1271 │ Statically-checked       │
│              │      │ physical   units    with │
│              │      │ seamless syntax          │
╵              ╵      ╵                          ╵
```

And `minimalTableStyle` leaves a single rule beneath the titles and nothing else:

```mono
  Name            LoC   Description
╶────────────────────────────────────────────────╴
  Wisteria        581   Simple,     fast     and
                        transparant      generic
                        derivation           for
                        typeclasses
  Quantitative   1271   Statically-checked
                        physical   units    with
                        seamless syntax
```

`midOnlyTableStyle` is the same, but reserves blank lines above and below where the outer rules
would be, so the table keeps its vertical spacing without drawing them. Styled
[terminal text](terminal.md) works as cell content in every style, so a table of
highlighted or colored values lays out by its visible width, not the length of its escape codes.

### Alignment

Each column states how its content sits within the width it is given. `Left` pads on the right,
`Right` pads on the left, and `Center` splits the padding, putting the odd column on the right
where it cannot be split evenly. `Justify` spreads the spaces between words so that every line
but the last reaches the full width — the newspaper setting, which suits a prose column and
nothing else.

Vertical alignment matters as soon as one cell wraps: a wrapped cell makes its whole row taller,
and the other cells in that row sit at the top, middle or bottom of the extra height according to
their `VerticalAlignment`.

### Titles

A derived table titles its columns from the field names, capitalized — `linesOfCode` becomes
`Lines Of Code`. Where that is not the wanted title, a `TableRelabelling` overrides it by name,
so the Scala field keeps its name and the table gets its own:

```scala
given TableRelabelling[Codebase]:
  def relabelling() = Map(t"linesOfCode" -> t"LoC")
```

An explicitly-defined column names itself, and `retitle` renames one after the fact, which is
useful where a table definition is shared and one caller wants a different heading.

### Reusing a column definition

A `Column` is parameterized by the row type it reads from, which would make a column defined for
one type useless for another. `contramap` adapts it, so a column defined once — with its
alignment, sizing and title — serves every type that can produce the value it displays:

```scala
val nameColumn = Column[Text, Text, Text](t"Name")(identity)
val codebaseName = nameColumn.contramap[Codebase](_.name)
```

### Tabulating something that is not a case class

A sequence of anything with a `Tabulable` instance tabulates, so a list of plain integers or of
text renders as a one-column table without a wrapper type. That instance is also the seam for a
type whose columns are not its fields — a record read from a schema, or a value whose display
columns are computed.

### When the table does not fit

Below some width, no arrangement of columns is satisfactory, and what should happen then is a
policy rather than a fixed behavior. The `columnAttenuation` given decides: `ignoreAttenuation`
renders anyway, at whatever quality the width allows, while `failAttenuation` raises a
`Table.Error` rather than producing something misleading.

The limit is not abstract. In the table above, the *Description* column can never be narrower
than the longest single word it contains — `Statically-checked` — so below about 36 characters no
slack will make it fit, however much the rest collapses.

`Table.Error` is a checked error like any other, carrying the minimum width the table needed and
the width it was given, so a handler can say something useful rather than merely failing:

```scala
import columnAttenuation.failAttenuation

recover:
  case Table.Error(minimum, available) =>
    Out.println(t"The table needs a width of at least $minimum to be shown.")
. protect:
    Out.println(table.tabulate(libraries).grid(30))
```

A report written to a file, where nobody will see that the columns were mangled, wants the
failure; an interactive display, where the user can widen the window, wants the rendering. And
because `ignoreAttenuation` cannot fail, choosing it requires no error handling at all.
