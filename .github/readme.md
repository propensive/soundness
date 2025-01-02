[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/escritoire/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/escritoire/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Escritoire

__A library for writing tables__

_Escritoire_ is a simple library for printing tabular data in monospaced fonts,
typically for use in console applications. Tables can be displayed in a number
of styles.

Particular care has been given to ensuring a table will render to a maximum
width, and columns can be controlled to scale dynamically according to their
content and other parameters.

## Features

- prints data using Unicode box-drawing characters
- supports multiline cells
- cell contents may be left, right or centrally aligned
- compact display for tables which don't include multiline contents


## Availability






## Getting Started

Here is an example of a table rendered by Escritoire:

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

All Escritoire terms and types are defined in the `escritoire` package,
```amok
syntax scala
##
import escritoire.*
```
and are exported to the `soundness` package, so alternatively we can import:
```amok
syntax scala
##
import soundness.*
```

### A Simple Example

For many datatypes, the easiest way to create a table from a sequence of values
is to call
the extension method `table` on it. This is possible if the elements have
types which are _tabulable_. Tabulable types are simple fields (like `Text` or
`Int`) or a product of tabulable values, such as a case class where every
field is a tabulable type.

This includes nested case classes, which will be automatically collapsed into a
flattened structure. But it excludes coproduct types like structured
`enum`s—these would need a different set of columns for each row, so they don't
make sense in a tabular form.

This automatic tabulation uses generic derivation, which is provided by
[Wisteria](https://soundness.dev/wisteria/), and in many cases it produces
good output. Titles are converted from "camel case" into normal words,
and capitalized.

The table can be rendered to standard output with,
```amok
syntax scala
transform
  before  hide imports
  after   show imports
  replace
    match  // more imports
    replacement
        import tableStyles.default
        import textMetrics.uniform
        import columnAttenuation.ignore
        import stdioSources.virtualMachine.ansi
##
import soundness.*

// more imports

case class Digit(n: Int, digitName: Text)
val data = List(Digit(1, t"one"), Digit(2, t"two"), Digit(3, t"three"))
Out.println(data.table)
```
and will look like this:
```mono
┏━━━┯━━━━━━━━━━━━┓
┃ N │ Digit name ┃
┠───┼────────────┨
┃ 1 │ one        ┃
┃ 2 │ two        ┃
┃ 3 │ three      ┃
┗━━━┷━━━━━━━━━━━━┛
```

Although this hides some of the complexity of constructing and rendering a
table, which will be explained below, for many cases it is remarkably easy to
render a table for display in a terminal.

### `Table`s, `Tabulation`s and `Grid`s

Escapade provides three types which relate to the tabulation of a particular
type of data for rendering to the terminal, though not all of them are used
explicitly. Let's be clear about what each one does:

- `Table` defines how data of a particular type is put into columns, how those
  columns are arranged, and their parameters;
- `Tabulation` represents a set of data that has been arranged in the tabular
  form specified by a `Table`;
- `Grid` represents the textual content of a `Tabulation` that has been fitted
  to a particular size.

In the first example, the expression `data.table` created a `Tabulation` of
the data from `data`. It automatically constructed a `Table` instance for the
type `Digit`, using generic derivation, so we did not see this value.

And `Out.println` is able to print any `Printable` type, which includes
`Tabulation` instances. This `Printable` instance automatically rendered the
`Tabulation` as a `Grid` and printed it.

More advanced examples would, construct a `Table`, use it format a sequence of
data into a `Tabulation`, then render the `Tabulation` to a specific width to
get a `Grid`, which could then be printed line-by-line. Such an example offers
more control over the table layout, and the rendering width.

#### Defining a `Table`

The `Table` type is parameterized not just on the type of row data that it
handles, but also the type of textual content it will contain. In many cases,
that will simply be `Text`—the typesafe variant of `String` used in
Soundness—but other types may be used instead, most notably the `Teletype` type,
which can include colors and styles for use in ANSI-compatible terminals.

We can nevertheless construct a new table by specifying just its row-data type
parameter, and a set of columns whose content comes from instances of that type.
The type parameter of its content will be inferred.

Here is an example describing a table for instances of `Library`:

```amok
syntax scala
##
case class Library
    (id: Text, name: Text, linesOfCode: Int, year: Int, about: Text)

val table =
  Table[Library]
   (Column(t"Name")(_.name),
    Column(t"Identifier")(_.id),
    Column(t"LoC")(_.linesOfCode),
    Column(t"Year")(_.year),
    Column(t"Description")(_.about))
```

Each column is specified as a title (such as `t"Year"`) and a lambda from the
row type (`Library`) to the cell value.
Note that the lambdas return a mixture of `Text` and `Int` values, but we have
not had to specify any explicit types except `Library`.

There is some clever mechanics going on to make this work! Here is what is
happening:

1. The title for each `Column` infers the textual type for its cells
2. A `Textual` instance (defined in [Gossamer](https://soundness.dev/gossamer))
   is resolved corresponding to the title's type
3. The `Textual` instance specifies a corresponding typeclass type, often
   called `Show`, for converting values of other types to that textual type
4. The result type of each lambda is used to infer a `Show` instance so that
   its contents is _showable_
5. The cell type of the table is inferred as the least upper-bound of its
   columns' cell types; usually we would want these all the same

The definition above, `table`, therefore has the type `Table[Library, Text]`.
We could have created a `Table[Library, Teletype]` if we had specified the
column titles as `e"Name"`, and so on.

#### Tabulation

If we take a sequence of `Library` instances, such as,
```amok
syntax scala
##
val libraries: List[Library] = List
 (Library(t"wisteria", t"Wisteria", 581, 2017, t"Simple, fast and transparant generic derivation for typeclasses"),
  Library(t"quantitative", t"Quantitative", 1271, 2023, t"Statically-checked physical units with seamless syntax"),
  Library(t"turbulence", t"Turbulence", 1047, 2022, t"Simple tools for working with data streams"),
  Library(t"escritoire", t"Escritoire", 494, 2018, t"A library for writing tables"))
```
then we can tabulate them with `table.tabulate(libraries)`.

This will produce a `Tabulation[Text]`. The source data `List[Library]` has
been serialized into `Text`, and is contained in this object which no longer
refers to the `Library` type.

An instance of a `Tabulation` is an array of textual values, with one textual
value for each row and column. In order to view it, it much be rendered to a
particular size by calling its `grid` method, and specifying the width.

For example, `table.tabulate(libraries).grid(100)` will produce a new
`Grid[Text]`, representing a table rendered to a maximum width of 100
characters.

### Rendering to a Width

Fitting the content of a table into a certain width can be challenging if the
width isn't large enough to contain the content at its natural width.

Consequently, the `Column` definitions in a `Table` definition include
parameters for controlling their width and visibility, so that the sizing
algorithm can find a suitable width for each column.

#### The Algorithm

The `Table` delegates rendering of the cells to each `Column`, and passes in a
parameter (a `Double` between `0` and `1`) which expresses the slack on that
column to shrink. _Slack_ can be thought of as the opposite of _pressure_.
A column may decide not to render at all if the slack is too low.

Some columns can render to a narrower width if the slack is decreased, while
others will not be able to shrink past a minimum width. In any case, their
calculated render-width is returned back to the table-rendering algorithm.

Based on the total width of all the columns at a particular slack, the
algorithm will decide whether to decrease or increase the slack on all
columns simultaneously, to force them to fit within the available space.

Through several trials, the algorithm can find the highest slack value which
allows the table to still render within the width available.

Often, this maximum slack value is found at a trigger point _just after_
one or more columns are hidden, and besides allowing the remaining columns to
fit, this can leave additional unused space.

So the algorithm, in a second step, increases the slack on the remaining
columns (without reintroducing the removed columns) to allow them to fill up
more of the unused space.

Experimentally, this seems to produce good results.

#### Specifying Column Sizing

Each `Column` instance can be configured with its own sizing criteria.

```amok
syntax scala
transform
  replace  t"Identifier"  t"Identifier", sizing = Collapsible(0.9)
  replace  t"LoC"  t"LoC", sizing = Collapsible(0.3)
  replace  t"Year"  t"Year", sizing = Collapsible(0.5)
  replace  t"Description"  t"Description", textAlign = TextAlignment.Justify, sizing = Prose
##
case class Library
    (id: Text, name: Text, linesOfCode: Int, year: Int, about: Text)

val table =
  Table[Library]
   (Column(t"Name")(_.name),
    Column(t"Identifier")(_.id),
    Column(t"LoC")(_.linesOfCode),
    Column(t"Year")(_.year),
    Column(t"Description")(_.about))
```

Of these columns, we have specified that the _LoC_ column and the _Year_
column have the sizing, `Collapsible`, with thresholds of `0.3` and `0.5`
respectively.

Those threshold values determine how those columns respond to the slack
imposed upon them when calculating how to render them. The numbers are
arbitrary in an absolute sense, because the algorithm will hunt
(logarithmically) for a suitable value for the width. But the values relative
to each other are significant.

If the slack value tried is `0.6`, then both columns will be shown. If the
value is `0.4`, then only `LoC` will be shown. And if it is `0.2`, then neither
column will be visible.

The `Description` column is sized as `Prose`. That was chosen because it
contains words which can be split on spaces, so long lines can be split into
multiple lines. A _prose_ column usually responds well to changes in slack, and
its width grows or shrinks as the text is reflowed onto multiple lines.

The alignment is also specified as `Justify`, which means that additional
spaces are added to ensure each line (apart from the last in each paragraph)
fills all the space available on each line.

In addition to `Prose` and `Collapsible`, columns may be sized as `Fixed`,
specifying an unchanging width in characters, or `Shortened` which will crop
the content of a column down to a certain size, if there is not enough slack.

The concept is fully extensible, and other specifications may be designed to
control the widths of columns in response to different slack values.

There is a requirement that a column should respond monotonically to changes in
slack. That is to say, a decrease in slack should _never_ result in a column
that is wider, though it may be the same width.

#### Renderings at Different Widths

At a width of `120` characters, each row takes a single line, and all columns
are included:

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

Reducing the maximum width to `100`, the _Identifier_ column is hidden. This
column is hidden first because its sizing is `Collapsible`, and it has the
`threshold` value of `0.9`, which is closer to `1` than any other column.

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

With the maximum width reduced to `80`, some cells in the _Description_ column
are forced to use two lines:
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

The _Year_ column is the `Collapsible` column with the next-highest `threshold`
value, so it is hidden when the width is constrained to `60` or less.
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

At a maximum width of `40` characters, only two columns remain visible, and
the cells in the _Description_ column take as many as four lines.

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

### Attenuation

If we were to reduce the maximum width further to `36`, we would hit a hard
limit: the `Description` column may not be more narrow than the longest single
word it contains. For our data, that's the word `Statically-checked`.

This scenario must be handled one way or another.

One possibility is to abandon hope of fitting the content into such a narrow
width, and to print the table ignoring the limit. We know that it will not
fit within the space available, but a user may still be able to get useful
information from the table.

This route may be chosen by including the import `columnAttenuation.ignore`:
```amok
syntax  scala
##
import columnAttenuation.ignore
```

Alternatively, it might be considered unacceptable to render a table in a space
that's too small for it, and we can raise a `TableError` instead, by importing:
```amok
syntax  scala
##
import columnAttenuation.fail
```

Like all Soundness errors, `TableError` is a checked error, and using the
`fail` import implies that it must be handled. (And conversely, using `ignore`
requires no error handling.)

It is therefore the method for handling this `TableError` which determines how
a `TableError` should be handled. An error handler might choose to render the
data in another form, or to print a message explaining that the table cannot
be rendered in the space available.

```amok
syntax scala
##
import columnAttenuation.fail

mend:
  case TableError(minimum, available) =>
    Out.println(t"The table needs a width of at least $minimum to be shown.")
.within:
  Out.println(table.grid(width))
```

### Table styles

Tables can be rendered in a number of styles. The style is determined by a
contextual `TableStyle` instance, and several predefined styles are included
in the `escritoire.tableStyles` (or `soundness.tableStyles`) package.

Below are samples of each table style.

Aside from `tableStyles.default', `tableStyles.thinRounded` provides tables
with rounded corners:

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

Tables can be rendered with only horizontal lines using,
`tableStyles.horizontal`:

```mono
�
────────────────────────────────────────────────╴
  Name            LoC   Description
�
────────────────────────────────────────────────╴
  Wisteria        581   Simple,     fast     and
                        transparant      generic
                        derivation           for
                        typeclasses
  Quantitative   1271   Statically-checked
                        physical   units    with
                        seamless syntax
�
────────────────────────────────────────────────╴
```

Or with only vertical lines with `tableStyles.vertical`:

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

A "minimal" approach includes just a horizontal line under the title, with
`tableStyles.minimal`:

```mono
  Name            LoC   Description
�
────────────────────────────────────────────────╴
  Wisteria        581   Simple,     fast     and
                        transparant      generic
                        derivation           for
                        typeclasses
  Quantitative   1271   Statically-checked
                        physical   units    with
                        seamless syntax
```


## Status

Escritoire is classified as __maturescent__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Escritoire is designed to be _small_. Its entire source code currently consists
of 565 lines of code.

## Building

Escritoire will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Escritoire?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Escritoire's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Escritoire and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `escritoire`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Escritoire's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Escritoire are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/escritoire/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Escritoire
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Escritoire was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Escritoire is a pun: it is named after a type of writing table, since its purpose is for writing tables.

### Pronunciation

/ˌɛskɹiˈtwɑː/

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows some stylized papers laid out on a green writing table, or _escritoire_.

## License

Escritoire is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

