## CSV

### About

[CSV](https://en.wikipedia.org/wiki/Comma-separated_values), its tab-separated cousin TSV, and
other delimited formats are read and written in one way. Text parses into a `Sheet`, which is a
series of rows. Each row, a `Dsv`, decodes into a case class whose fields take the columns in
turn. A case class encodes back to a row in the same way. The encoder and decoder are
[derived][derivation] from the type, so there is nothing to write by hand.

The delimiter, the quoting and the presence of a header row are fixed by a format, chosen in
scope, so the same data reads as CSV or TSV by a change of import. Nested case classes spread
across several columns, optional fields are supported, and a column can be addressed by its
header instead of its position.

### On delimited data

CSV looks trivial, and is not:

- A field may contain the delimiter, if it is quoted.
- A quote inside a quoted field is doubled.
- A file may or may not begin with a header row.
- A "CSV" file is as likely to be separated by tabs or semicolons as by commas.

Code which splits on commas handles none of this. And the mapping from columns to a program's
own types is usually written out by hand, and kept in step by discipline.

Here that mapping is derived from the case class which the data should become. Parsing respects
quoting and escaping. The format is an explicit value, not a guess. Decoding a row to a type, or
a type to a row, is a single derived operation, through the same derivation as every other
format, so the shape of the data is checked once, as
[correctness](../philosophy/correctness.md) prefers.

Everything comes from the `soundness` package. A format and an error [strategy] are in scope,
and the last import lets the samples print their results:

```scala
import soundness.*
import dsvFormats.csvFormat
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Reading rows

`read[Sheet]` parses text into a `Sheet`. Its `rows` are `Dsv` values, one for each line, split
into cells according to the format:

```scala fuel
run
##
Out.println(t"hello,world".read[Sheet].rows(Prim).inspect)
```

The delimiter inside a quoted cell is part of the cell, so this row has three cells, not four:

```scala fuel
run
##
Out.println(t"1,\"Lovelace, Ada\",36".read[Sheet].rows(Prim).inspect)
```

A `Sheet` holds all its rows in memory, in an array, so they can be replayed, indexed and
compared cheaply. A file too large to hold needs no sheet at all. `rows` on a
[stream](streams.md) of characters gives the rows one at a time, parsed by the same reader, so
a quoted cell which spans the boundary between chunks, or between lines, is handled exactly as
it is in a sheet:

```scala
val names =
  t"name,age\nalpha,1\nbeta,2".source[Text].rows.map(_[Text](t"name").or(t"?")).to[List]
```

### Decoding to a case class

`as` decodes a row into a case class, taking the cells in order. A nested case class simply
consumes as many columns as it has fields:

```scala
case class FullName(first: Text, last: Text)
case class Person(id: Int, name: FullName, age: Int)

val ada = t"1,Ada,Lovelace,36".read[Sheet].rows(Prim).let(_.as[Person])
```

```scala fuel
run
##
Out.println(ada.inspect)
```

`rows(Prim)` gives an [optional](optional-values.md) row, since the sheet may be empty, so `let`
decodes the row if there is one.

Reading straight to a type combines the two steps, and decodes a single record from text:

```scala fuel
run
##
Out.println(t"hello,world".read[FullName in Dsv].inspect)
```

A cell which cannot be decoded as its field's type is an error:

```scala fuel
run
errors show
##
t"one,Ada,Lovelace,36".read[Person in Dsv]
```

### Encoding

`dsv` encodes a case class as a row, and a sequence of them as a `Sheet`. `show` writes the
result as delimited text:

```scala fuel
run
##
Out.println(FullName(t"Ada", t"Lovelace").dsv.inspect)
Out.println(List(FullName(t"Ada", t"Lovelace"), FullName(t"Grace", t"Hopper")).dsv.show)
```

`in[Text over Dsv]` encodes a value as a single record in one step, and `in[Data over Dsv]`
writes the same text as bytes, in the `Codepage` in scope:

```scala
val record: Text = FullName(t"hello", t"world").in[Text over Dsv]
```

```scala fuel
run
##
Out.println(record)
```

A cell is quoted only when it contains the delimiter, a quote or a line break, and a quote
within a cell is doubled:

```scala
val quoted: Text = FullName(t"Ada", t"Lovelace, Countess").in[Text over Dsv]
```

```scala fuel
run
##
Out.println(quoted)
```

### Headers

Data often begins with a header row. A format which expects one matches each column to the
field of the same name, so the order of the columns need not match the order of the fields:

```scala fuel
run
##
locally:
  import dsvFormats.csvWithHeaderFormat
  Out.println(t"last,first\nLovelace,Ada".read[FullName in Dsv].inspect)
```

### Formats

The format in scope decides the delimiter and the quoting. Each delimiter has a plain format
and a variant with a header, in the package `dsvFormats`, and all of them quote with `"`:

```tabular
column  Format
column  With a header
column  Delimiter
row  `csvFormat`  `csvWithHeaderFormat`  comma
row  `tsvFormat`  `tsvWithHeaderFormat`  tab
row  `ssvFormat`  `ssvWithHeaderFormat`  space
```

Importing another format in an inner scope changes the delimiter:

```scala fuel
run
##
locally:
  import dsvFormats.tsvFormat
  Out.println(List(FullName(t"Ada", t"Lovelace")).dsv.show.inspect)
```

### Column spans

A nested case class occupies as many columns as it has fields. That number is computed from the
type, not discovered while parsing. Each field's *span* says how many columns it consumes, so a
decoder knows where one field ends and the next begins, even when several are nested. `Person`
spans four columns in all:

```scala fuel
run
##
Out.println(Spannable.derived[Person].spans().to[List].inspect)
```

Decoding by position is brittle by nature. Adding a field to a nested case class shifts every
column after it, and nothing in the file says so. That is how positional data behaves, and no
decoder can change it. It is why a format with headers is worth using where there is a choice,
and why cells can be [addressed by name](#addressing-cells-by-name).

### Optional columns

An `Optional` field spans one column. A row which stops short of it decodes it as `Unset`,
which is what a trailing optional column in a real spreadsheet means:

```scala
case class Greeting(word: Text, name: Optional[Text])
```

```scala fuel
run
##
Out.println(t"hello,world".read[Greeting in Dsv].inspect)
Out.println(t"hello".read[Greeting in Dsv].inspect)
```

Reading an `Optional` field follows the policies which
[Decoding](decoding.md#policies-for-optional-fields) describes, each varied by a given in the
package `optionalityOptions`. Delimited text has no null:

```tabular
column  A field which is
column  Reads as
column  Unless
row  in a column the row lacks  `Unset`  `strictDsvAbsence` makes it an error
row  present but rejected by the inner decoder  an error  `lenientDsvFaults` reads it as `Unset`
```

A column the row lacks is one beyond the end of a short row, or a header column the row does
not have. The error raised for it under `strictDsvAbsence` has the reason
`Dsv.Error.Reason.Absent`. An empty cell is a present value, not a null.

```scala
case class Tally(name: Text, count: Optional[Int])

val tolerant =
  import optionalityOptions.lenientDsvFaults
  t"hello,many".read[Tally in Dsv]
```

```scala fuel
run
##
Out.println(tolerant.inspect)
```

### Addressing cells by name

With dynamic access enabled, a cell of a row which has a header is read as a named member, and
decoded to the type asked for. A *redesignation* in scope says how a member's name becomes a
header. `unchangedRedesignation` uses the name as it is:

```scala
import dynamicAccess.dynamicDsv

val number =
  import dsvFormats.csvWithHeaderFormat
  import dsvRedesignations.unchangedRedesignation

  t"greeting,number\nhello,23".read[Sheet].rows(Prim).let(_.number[Int])
```

The column names in a file rarely look like Scala's names. Another redesignation maps between
the two conventions, so the member `targetPerson` reads the column headed `Target Person`:

```scala
val target =
  import dsvFormats.csvWithHeaderFormat
  import dsvRedesignations.capitalizedWordsRedesignation

  t"Personal Greeting,Target Person\nHello,World".read[Sheet].rows(Prim).let(_.targetPerson[Text])
```

```scala fuel
run
##
Out.println(number.inspect)
Out.println(target.inspect)
```

Each redesignation in the package `dsvRedesignations` gives the member `targetPerson` this
header:

```tabular
column  Redesignation
column  Header
row  `unchangedRedesignation`  `targetPerson`
row  `capitalizedWordsRedesignation`  `Target Person`
row  `lowerWordsRedesignation`  `target person`
row  `lowerDottedRedesignation`  `target.person`
row  `lowerSlashedRedesignation`  `target/person`
```

Without a redesignation in scope, a cell cannot be read by name:

```scala fuel
compile
errors show
##
t"greeting,number\nhello,23".read[Sheet].rows(Prim).let(_.number[Int])
```

The import of `dynamicDsv` grants the permission `Dsv is Dynamical` to the rest of its scope.
`dynamically[Dsv]:` grants it to a single block instead, as
[JSON](json.md#fields-as-members) describes. A field of a case class whose name differs from
its header is renamed with [`@name`](json.md#renaming-fields).

### Reporting every bad cell

A spreadsheet exported from elsewhere is rarely wrong in only one place, and a decoder which
stops at the first bad cell makes fixing it a series of guesses. Under a strategy which
[accumulates failures](errors.md#accumulating-failures), every cell which fails is reported,
missing cells and unparseable ones together. Each is identified by its column, so one pass over
a file gives the whole list of what to correct.

The accumulation is a value of the caller's choosing, here an error which collects each fault
with the column it was found in. `Validate` says how each fault joins it, and is focused on the
`CellRef` which the decoder was reading:

```scala
import dysasymptotics.linearSize
import errorDiagnostics.stackTracesDiagnostics

case class Issues(items: List[(Text, Dsv.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} decoding issues"):
  def +(column: Text, error: Dsv.Error): Issues = Issues(items :+ (column, error))

case class Measurement(name: Text, age: Int, height: Int)

val damaged =
  import dsvFormats.csvWithHeaderFormat
  t"name,age,height\nAlice,thirty,tall".read[Sheet].rows(Prim)

val problems =
  Validate[Issues, [r] =>> r raises Dsv.Error, CellRef]
    ( Issues(),
      { case error: Dsv.Error => accrual + (prior.let(_.column).or(t"#"), error) } )
  . protect(damaged.let(_.as[Measurement]))
  . items
```

```scala fuel
run
##
problems.each: (column, error) =>
  Out.println(t"$column: ${error.message}")
```

### Parsing directly to a type

Building a `Dsv` for every row, only to decode it and throw it away, is wasted work. A type
with a `Dsv.Parsable` instance is parsed straight from the source. Its cells are read in place,
from the parser's buffer, and no row value is built at all:

```scala
case class Stat(name: Text, count: Int, note: Optional[Text])

given statParsable: (Stat is Dsv.Parsable) = Dsv.Parsable.derived
```

```scala fuel
run
##
Out.println(t"z,9,note".read[Stat in Dsv].inspect)
Out.println(t"a,1\nb,2".read[List[Stat] in Dsv].map(_.name).inspect)
```

A stream gives records in the same way, through `rowsOf`:

```scala fuel
run
##
Out.println(t"x,1\ny,2".source[Text].rowsOf[Stat].map(_.count).to[List].inspect)
```

Fields are matched by header name where the format has a header, and by position where it does
not. An `Optional` field which the row does not reach is `Unset`, not an error.

The `codecs` import which selects the route for every other format selects it for delimited
text too:

```tabular
column  Import
column  For a product
column  For any other type
row  `codecs.direct`  parsed directly, with no instance declared  the row decoder, unless it has a `Dsv.Parsable`
row  `codecs.derived`  the row decoder, even where a `Dsv.Parsable` is in scope  the row decoder
row  `codecs.strict`  parsed directly, with no instance declared  a compile error naming the type, unless it has a `Dsv.Parsable`
```

The modes are alternatives, so import one at a time. Reading a `List[Foo] in Dsv` is direct in
every mode, since the row decoder has no counterpart for a whole sheet.

Each field of a directly parsed product reads one cell through its `Decodable in Text`
instance, or is itself a product of such fields. A field which has only a `Decodable in Dsv`
instance keeps its type on the row decoder: declare no `Dsv.Parsable` for the type, and import
no mode.

### Related

- [Tables](tables.md) lays out rows of data for display in a terminal.
- [Decoding](decoding.md) describes the `Decodable in Text` instances which read each cell.
- [JSON](json.md) and [YAML](yaml.md) decode nested documents with the same derivation.
- [Streams](streams.md) covers the sources which `rows` and `rowsOf` read from.
- [Derivation](derivation.md) explains how a row is decoded to a case class without an instance written by hand.
