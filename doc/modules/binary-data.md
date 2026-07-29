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
derivation. `Unpackable` sits above both, and is what `unpack` resolves: it covers a single
`Debufferable` value and, separately, an `IArray` of them, which is how a count-prefixed section
reads as one call rather than a loop.

An instance states two things: `width`, the number of bytes the value occupies, and how to read
those bytes from — or write them to — a `Buffer`. Both are visible to derivation, so a record's
total width is the sum of its fields' and every offset within it is computed at compiletime.

```scala
trait Debufferable:
  def width: Int
  def debuffer(buffer: Buffer): Self
```

A format whose fields need more than fixed-width reads — a length-prefixed string, say — defines
its own instance, and composes into derived records like any primitive. The sized types also fix
the *interpretation*, not merely the width: `B32` is four raw bytes, `U32` reads them as an
unsigned integer, `S32` as a signed one, and the plain Scala `Int`, `Short`, `Long` and `Byte`
have instances too, for layouts that are easier to state in them.

`Bufferable` is the symmetric writing-side typeclass, deriving over products in the same way, so a
layout is declared once and serves both directions.

### The buffer is a capability

A `Buffer` carries a mutable read position, which makes unpacking an effect rather than a pure
read, and the position is what makes successive `unpack` calls advance. It is therefore introduced
by the `buffer { … }` block that scopes it, and is an *exclusive* capability: two readers sharing
one position would each consume bytes the other expected, so the compiler prevents a buffer from
being aliased or from escaping its block.

`offset` reports the current position, for a format that must state where a section begins, and
`advance` skips past padding or a field this reader does not need.
