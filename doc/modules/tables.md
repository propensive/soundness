## Tables

### About

Tabular data is drawn in a terminal by a layout engine which takes the hard part seriously:
fitting columns of uneven content into a fixed width.

A table is defined in one of two ways. Implicitly, a sequence of case classes becomes a
table with a column for each field. Explicitly, each column is named, with its alignment and
its *sizing strategy*: wrap as a paragraph, truncate with an ellipsis, or collapse entirely
when space runs short. The result is drawn with box characters, in a style chosen in scope.

### On tables

Most tables in a terminal are format strings: fixed widths, guessed when the program was
written, which truncate or overflow when the data disagrees. Real layout is a negotiation.
Some columns matter more than others. A column of prose can wrap where a column of
identifiers cannot. Below some width, a column is better dropped than mangled. A layout
engine can conduct that negotiation, and a format string cannot.

Here the layout is computed each time a table is drawn. Each column declares how it may
shrink, and the engine distributes the width which is available, wrapping with
[hyphenation](hyphenation.md) where wrapping is allowed. Separating a table's columns from
its data, and from its width, is [decoupling](../philosophy/decoupling.md), and it is what
lets one definition be drawn at any width.

Everything comes from the `soundness` package. A style, a metric for the width of text, and
a policy for a table which does not fit are in scope:

```scala
import soundness.*
import tableStyles.thickTableStyle
import textMetrics.uniformMetric
import columnAttenuation.ignoreAttenuation
import hyphenations.englishHyphenation
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import strategies.throwUnsafely
```

### A table from a case class

A sequence of case classes is tabulated directly, with a column for each field, titled from
the names of the fields:

```scala
case class Codebase(id: Text, name: Text, linesOfCode: Int, year: Int, description: Text)

val libraries = List
  ( Codebase(t"wisteria", t"Wisteria", 581, 2017,
        t"Simple, fast and transparent generic derivation for typeclasses"),
    Codebase(t"quantitative", t"Quantitative", 1271, 2023,
        t"Statically-checked physical units with seamless syntax"),
    Codebase(t"turbulence", t"Turbulence", 1047, 2022,
        t"Simple tools for working with data streams"),
    Codebase(t"escritoire", t"Escritoire", 494, 2018, t"A library for writing tables") )
```

`grid` lays the table out at a width, and `render` gives its lines:

```scala fuel
run
##
libraries.tabulation.grid(100).render.each(Out.println(_))
```

Numeric columns are aligned to the right by default, and text to the left. The whole table
is `Printable`, so printing it at the terminal's own width needs nothing more than
`Out.println`.

Three types are at work, and knowing which is which explains where each decision is made:

```tabular
column  Type
column  Holds
column  Made with
row  `Scaffold`  how a type is put into columns — their content, order and parameters — but no data  `Scaffold[Row](columns…)`
row  `Tabulation`  data arranged by a scaffold: textual values, one per row and column, no longer referring to the row type  `scaffold.tabulate(rows)`
row  `Grid`  a tabulation fitted to a particular width, where a column may shrink or vanish  `tabulation.grid(width)`
```

`tabulation`, above, derived a scaffold for `Codebase` and applied it, in one step. The
sections below take the stages separately, which is what gives control over both the layout
and the width.

### Explicit columns

Columns which are defined by hand choose their titles, content, alignment and sizing. The
sizings are in the package `columnar`:

```tabular
column  Sizing
column  The column
row  `Paragraph`  wraps, with hyphenation, but no narrower than its longest word
row  `Collapsible(threshold)`  keeps its natural width, and vanishes when the slack falls below `threshold`
row  `Fixed(width)`  is always `width` wide, truncating its content with an ellipsis
row  `Shortened(width)`  shrinks from its natural width, truncating its content with an ellipsis
```

```scala
val table = Scaffold[Codebase]
  ( Column(t"Name")(_.name),
    Column(t"Identifier", sizing = columnar.Collapsible(0.9))(_.id),
    Column(t"LoC", sizing = columnar.Collapsible(0.3))(_.linesOfCode),
    Column(t"Year", sizing = columnar.Collapsible(0.5))(_.year),
    Column(t"Description", textAlign = TextAlignment.Justify,
        sizing = columnar.Paragraph)(_.description) )
```

Each column is a title, and a function from the type of a row to the value of a cell. The
functions here return a mixture of `Text` and `Int`, with no type annotation between them.
That works because the *title* fixes the textual type. A `Textual` instance is found for the
type of the title. That instance names the [typeclass], usually `Showable`, which converts
other values into it, and the result of each function is shown through it.

The table's own type of cell is then the least upper bound of its columns'. So `table`,
above, is a `Scaffold[Codebase, Text]`. Writing the titles as `e"Name"` instead of `t"Name"`
would have made it a `Scaffold[Codebase, Teletype]`, with color and style available in every
cell, as [Terminal](terminal.md#styled-text) describes.

The thresholds of the `Collapsible` columns are meaningful only relative to each other. They
are compared against the *slack* at which the layout is working, which is the number the
[layout algorithm](#the-layout-algorithm) searches for:

```tabular
column  Slack
column  Identifier (`0.9`)
column  Year (`0.5`)
column  LoC (`0.3`)
row  `0.6`  hidden  shown  shown
row  `0.4`  hidden  hidden  shown
row  `0.2`  hidden  hidden  hidden
```

### Renderings at different widths

Here is the same table, with the same data, laid out at four widths. At 100 characters,
*Identifier* is already gone. It is the collapsible column whose threshold is closest to
`1`, so it is the first to go:

```scala fuel
run
##
table.tabulate(libraries).grid(100).render.each(Out.println(_))
```

At 80, no further column need be dropped. The description wraps instead:

```scala fuel
run
##
table.tabulate(libraries).grid(80).render.each(Out.println(_))
```

At 60, *Year* goes, since it has the next-highest threshold, and the justification of the
description becomes visible, as the lines stretch to the full width:

```scala fuel
run
##
table.tabulate(libraries).grid(60).render.each(Out.println(_))
```

And at 40, only two columns remain, and a description may run to four lines:

```scala fuel
run
##
table.tabulate(libraries).grid(40).render.each(Out.println(_))
```

### The layout algorithm

The scaffold does not decide the widths. It delegates to each column, passing a `Double`
between `0` and `1`. That number is the *slack* on the column, which is simply the opposite
of pressure. Given a slack, a column reports the width which it would take, and a column
under enough pressure may report that it will not be drawn at all. A column of prose shrinks
smoothly as its text reflows. A column of identifiers cannot shrink past its content, and
either holds its width or disappears.

Adding up those widths gives the table's width at that slack, and from there it is a search.
The slack is raised where the table fits, and lowered where it does not, hunting for the
highest slack at which the table still fits the space available.

That maximum usually sits *just after* a column has been hidden, which tends to leave space
unused: the column which vanished freed more room than the survivors needed. A second pass
therefore raises the slack on the remaining columns, without bringing back the ones already
removed, and lets them spread into what is left. In practice this gives markedly better
results than stopping at the first fit.

The search rests on one requirement, which any custom `Columnar` must honor: a column must
respond *monotonically* to slack. Decreasing the slack may leave a column the same width,
but must never make it wider. `Paragraph`, `Collapsible`, `Fixed` and `Shortened` are simply
the strategies which are provided. The concept is open, and a sizing strategy which responds
to slack in some other way is an ordinary implementation of the same interface.

### Styles

The box drawing is a `TableStyle` given. The style is a change of import, not of the code
which defines the table. `thinRoundedTableStyle` draws light rules throughout, with rounded
corners:

```scala
val few = libraries.excerpt(0, 2)
```

```scala fuel
run
##
locally:
  import tableStyles.thinRoundedTableStyle
  table.tabulate(few).grid(50).render.each(Out.println(_))
```

`horizontalTableStyle` keeps the horizontal rules, and drops the vertical ones:

```scala fuel
run
##
locally:
  import tableStyles.horizontalTableStyle
  table.tabulate(few).grid(50).render.each(Out.println(_))
```

`verticalTableStyle` does the opposite:

```scala fuel
run
##
locally:
  import tableStyles.verticalTableStyle
  table.tabulate(few).grid(50).render.each(Out.println(_))
```

And `minimalTableStyle` leaves a single rule beneath the titles, and nothing else:

```scala fuel
run
##
locally:
  import tableStyles.minimalTableStyle
  table.tabulate(few).grid(50).render.each(Out.println(_))
```

`midOnlyTableStyle` is the same, but reserves blank lines above and below, where the outer
rules would be, so the table keeps its vertical spacing without drawing them.

Styled terminal text works as the content of a cell in every style. A table of highlighted
or colored values is laid out by its visible width, not by the length of its escape codes.

### Alignment

Each column states how its content sits within the width which it is given, as a
`TextAlignment`:

```tabular
column  Alignment
column  Pads
row  `Left`  on the right
row  `Right`  on the left
row  `Center`  on both sides, putting the odd column on the right where it cannot be split evenly
row  `Justify`  between words, so that every line but the last reaches the full width
```

`Justify` is the setting of a newspaper, which suits a column of prose and nothing else.

Vertical alignment matters as soon as one cell wraps. A wrapped cell makes its whole row
taller, and the other cells in that row sit at the top, the middle or the bottom of the extra
height, according to their `VerticalAlignment`: `Top`, `Middle` or `Bottom`.

### Titles

A derived table titles its columns from the names of the fields, capitalized, so
`linesOfCode` becomes `Lines of code`. Where that is not the title which is wanted, a
`TableRelabelling` overrides it by name. The Scala field keeps its name, and the table gets
its own:

```scala
given relabelling: TableRelabelling[Codebase]:
  def relabelling() = Map(t"linesOfCode" -> t"LoC")
```

A column which is defined explicitly names itself. `retitle` renames one after the fact,
which is useful where a table's definition is shared, and one caller wants a different
heading.

### Reusing a column definition

A `Column` has a type parameter for the type of row which it reads from, which would make a
column defined for one type useless for another. `contramap` adapts it. A column which is
defined once, with its alignment, sizing and title, then serves every type which can produce
the value it displays:

```scala
val nameColumn = Column[Text, Text, Text](t"Name")(identity)
val codebaseName = nameColumn.contramap[Codebase](_.name)
```

### Tabulating something that is not a case class

A sequence of anything with a `Tabulable` instance is tabulated, so a list of plain integers,
or of text, is drawn as a table of one column, without a wrapper type. That instance is also
where a type whose columns are not its fields takes part, such as a
[record](records.md) read from a schema, or a value whose columns for display are computed.

### Lists in columns

Not every arrangement of text in columns is a table. A list of short items, such as file
names or the entries of a legend, reads best when it is laid out as `ls` lays out a
directory: in as many columns as the width allows, with no titles and no rules. `columnate`
does this for a list of textual values, and gives one line for each row:

```scala fuel
run
##
List(t"apple", t"fig", t"kiwi", t"lime", t"pear", t"plum").columnate(24).each(Out.println(_))
```

The number of columns is the greatest at which the content of every column fits, with `gap`
cells between neighbors. Columns of equal width, each as wide as the widest item, are
preferred. A layout with one more column is admitted when the items which fall into some
column happen to be narrower, and that column can shrink. Whatever width is then spare is
shared back out among the shrunk columns, toward equality, without any column growing beyond
the widest item. The parameters of `columnate` are:

```tabular
column  Parameter
column  Default
column  Effect
row  `width`  none  the width to fill
row  `gap`  `2`  the cells between neighbouring columns
row  `uniform`  `false`  when `true`, considers only equal-width layouts
row  `downward`  `false`  when `true`, reads the items down each column rather than across each row
row  `align`  `TextAlignment.Left`  how each cell is padded within its column
```

A row which is aligned to the left has no trailing padding after its last item.

### When the table does not fit

Below some width, no arrangement of columns is satisfactory, and what should happen then is
a policy, not a fixed behavior. The `columnAttenuation` given decides:

```tabular
column  Import
column  A table which does not fit
row  `columnAttenuation.ignoreAttenuation`  is drawn anyway, as well as the width allows
row  `columnAttenuation.failAttenuation`  raises a `Table.Error`, instead of producing something misleading
```

The limit is not abstract. In the table above, the *Description* column can never be
narrower than the longest single word which it contains, `Statically-checked`. Below 37
characters, no slack will make it fit, however much the rest collapses.

`Table.Error` is a checked [error](errors.md#recovering) like any other. It carries the
minimum width which the table needed, and the width which it was given, so a handler can say
something useful, and need not merely fail:

```scala
def narrow(): Unit =
  import columnAttenuation.failAttenuation

  recover:
    case Table.Error(minimum, available) =>
      Out.println(t"The table needs a width of at least $minimum to be shown.")
  . protect:
      Out.println(table.tabulate(libraries).grid(30))
```

```scala fuel
run
##
narrow()
```

A report which is written to a file, where nobody will see that the columns were mangled,
wants the failure. An interactive display, where the user can widen the window, wants the
table drawn. And since `ignoreAttenuation` cannot fail, choosing it requires no error
handling at all.

### Related

- [Terminal](terminal.md) styles the text in a cell, and draws live interfaces.
- [CSV](csv.md) reads the rows which a table displays.
- [Hyphenation](hyphenation.md) breaks the words of a wrapped column.
- [Text](text.md#padding-and-fitting) pads and fits single values.
- [Charts](charts.md) draws the same data as a picture.
