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

Soundness computes the layout per rendering: each column declares how it may shrink, and the
engine distributes the available width, wrapping with [hyphenation](hyphenation.md) where wrapping
is allowed. Everything comes from the `soundness` package, with a style, a metric, and an overflow
policy in scope:

```scala
import soundness.*
import tableStyles.defaultTableStyle
import textMetrics.uniformMetric
import columnAttenuation.ignoreAttenuation
import hyphenations.englishHyphenation
```

### A table from a case class

A sequence of case classes tabulates directly, a column per field, titled from the field names;
`grid` lays it out at a width and produces the lines:

```scala
case class Library(name: Text, linesOfCode: Int, description: Text)

libraries.tabulation.grid(80).render.each(Out.println(_))
```

Numeric columns right-align by default, text left-aligns, and the whole table is `Printable`, so
printing it at the terminal's own width needs nothing more than `Out.println`.

### Explicit columns

Columns defined by hand choose their titles, content, alignment and sizing. A `Paragraph` column
wraps; a `Collapsible` column vanishes when the layout falls below its threshold; a `Fixed` or
`Shortened` column truncates with an ellipsis:

```scala
val table = Scaffold[Library]
  ( Column(t"Name")(_.name),
    Column(t"LoC", sizing = columnar.Collapsible(0.3))(_.linesOfCode),
    Column(t"Description", textAlign = TextAlignment.Justify,
        sizing = columnar.Paragraph)(_.description) )

table.tabulate(libraries).grid(70).render
```

At seventy columns all three columns show, the description justified and wrapped; squeezed
further, the lines-of-code column — marked most collapsible — disappears first, and the rest of
the table remains readable rather than everything degrading together.

### Styles

The box drawing is a `TableStyle` given: heavy outer rules with thin inner ones by default,
rounded corners, horizontal-only rules, or a minimal style with almost no furniture — a change of
import, not of table code:

```scala
import tableStyles.thinRoundedTableStyle
```

Styled [terminal text](terminal.md) works as cell content, so a table of highlighted or colored
values lays out by its visible width, not the length of its escape codes.
