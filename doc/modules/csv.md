## CSV

### About

Soundness reads and writes [CSV](https://en.wikipedia.org/wiki/Comma-separated_values), its
tab-separated cousin TSV, and other delimited formats. Text parses into a `Sheet` — a stream of
rows — and each row, a `Dsv`, decodes into a case class whose fields take the columns in turn.
A case class encodes back to a row the same way, with the encoder and decoder derived from the
type, so there is nothing to write by hand.

The delimiter, quoting and header conventions are fixed by a format value chosen in scope, so
the same data reads as CSV or TSV by a change of import. Nested case classes flatten across
several columns, optional fields are supported, and columns can be addressed by header name
rather than position.

### On delimited data

CSV looks trivial and is not. A field may contain the delimiter if it is quoted, a quote inside
a quoted field is doubled, a file may or may not begin with a header row, and a "CSV" file is as
likely to be separated by tabs or semicolons. Code that splits on commas handles none of this,
and the mapping from columns to a program's own types is usually written out by hand and kept in
step by discipline.

Soundness derives that mapping from the case class the data should become. Parsing respects
quoting and escaping; the format — delimiter, quote character, whether a header is present — is an
explicit value rather than a guess; and decoding a row to a type, or a type to a row, is a single
derived operation. Everything comes from the `soundness` package, with a format and an error
strategy in scope:

```scala
import soundness.*
import dsvFormats.csvFormat
import strategies.throwUnsafely
```

### Reading rows

Text reads as a `Sheet`, whose `rows` are `Dsv` values — one per line, split into cells according
to the format:

```scala
t"hello,world".read[Sheet].rows.head   // Dsv(t"hello", t"world")
```

### Decoding to a case class

A row decodes into a case class with `as`, taking the cells in order; a nested case class simply
consumes as many columns as it has fields:

```scala
case class Name(first: Text, last: Text)
case class Person(id: Int, name: Name, age: Int)

t"1,Ada,Lovelace,36".read[Sheet].rows.head.as[Person]
// Person(1, Name(t"Ada", t"Lovelace"), 36)
```

Reading straight to a type combines the two steps, decoding a single record from text:

```scala
t"hello,world".read[Name in Dsv]   // Name(t"hello", t"world")
```

### Headers

When the data begins with a header row, the header-bearing format matches each column to the
field of the same name, so the order of the columns need not match the order of the fields:

```scala
import dsvFormats.csvWithHeaderFormat

t"last,first\nLovelace,Ada".read[Name in Dsv]   // Name(t"Ada", t"Lovelace")
```

### Encoding

A case class encodes to a row with `dsv`, and a sequence of them to a `Sheet`; rendering the
result with `show` produces the delimited text:

```scala
Name(t"Ada", t"Lovelace").dsv          // Dsv(t"Ada", t"Lovelace")
Seq(Name(t"Ada", t"Lovelace")).dsv.show   // t"Ada,Lovelace"
```

### Formats

The format in scope decides the delimiter and quoting. `csvFormat` and `tsvFormat` are the common
choices, each with a header-bearing variant, and `ssvFormat` separates on spaces:

```scala
import dsvFormats.tsvFormat

Seq(Name(t"Ada", t"Lovelace")).dsv.show   // t"Ada\tLovelace"
```

A cell is quoted only when it contains the delimiter, a quote, or a line break, and a quote
within a cell is doubled — the conventions the format prescribes, applied on the way out and
understood on the way in.

### Addressing cells by name

With dynamic access enabled, a cell of a header-bearing row reads as a named member, decoded to
the type asked for:

```scala
import dynamicDsvAccess.enabled

val row = t"greeting,number\nhello,23".read[Sheet].rows.head
row.number[Int]   // 23
```
