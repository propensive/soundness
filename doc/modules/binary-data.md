## Binary Data

### About

Structured binary data — the fixed-width records of file headers, network packets and device
protocols — reads through a typeclass that maps bytes onto Scala types. A case class of sized
[numeric types](numbers.md) describes a binary layout: each field states its exact width and
interpretation, so the record's shape is its definition, and reading a value is naming its type at
an offset.

### On binary layouts

Parsing a binary structure by hand is index arithmetic: byte 4 through 7 is the length,
little-endian; byte 8 is the flags. Every offset is computed relative to the last, every width is a
constant to keep in step with a specification, and a change to one field silently shifts every
field after it.

Soundness derives the layout from a case class. The sized types say how many bytes each field
occupies and how they are read — a `U32` is four bytes as an unsigned integer, a `B16` is two raw
bytes — and derivation composes them in declaration order, computing every offset. Everything comes
from the `soundness` package:

```scala
import soundness.*
```

### Describing a layout

A binary record is a case class of fixed-width fields:

```scala
case class Header(magic: B32, version: U16, flags: B16, length: U32)
```

Each field's width follows from its type — here four, two, two and four bytes — so the record is
twelve bytes, and `byteWidth[Header]` says so without a value in hand.

### Reading

`unpackFrom` reads a value of a chosen type from bytes at an offset, and within a `buffer` block,
successive `unpack` calls read one value after another, the cursor advancing by each value's
width:

```scala
val header = data.unpackFrom[Header](0)

data.buffer:
  val first = unpack[U32]
  val second = unpack[U16]
```

An array of records unpacks by count, for the repeated sections binary formats favour:

```scala
data.buffer:
  val entries = unpack[IArray[Entry]](12)
```

### The typeclasses

`Debufferable` is the reading side — the sixteen sized numeric types have instances, and a case
class derives its own from its fields — and `Bufferable` is the writing side, with the same
derivation. A format whose fields need more than fixed-width reads — a length-prefixed string, say —
defines its own instance, stating its width and its interpretation, and composes into derived
records like any primitive.
