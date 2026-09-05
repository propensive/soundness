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

Describing a layout once, and deriving both its reader and its writer, keeps the two from drifting apart — [correctness](../philosophy/correctness.md) by having one definition.

### Describing a layout

A binary record is a case class of fixed-width fields, given a `Debufferable` instance by
derivation:

```scala
case class Header(magic: B32, version: U16, flags: B16, length: U32)

object Header:
  given Header is Debufferable = Debufferable.derived
```

Each field's width follows from its type — here four, two, two and four bytes — so the record is
twelve bytes, and `byteWidth` says so without a value in hand:

```scala
byteWidth[Header]   // 12
byteWidth[U16]      // 2
```

Records nest: a field whose type is itself a `Debufferable` record contributes its whole width,
so a layout is composed from smaller layouts rather than flattened by hand.

### Reading

`unpackFrom` reads a value of a chosen type from bytes at an offset:

```scala
val data = hex"cafebabe0001000000000010"

val header = data.unpackFrom[Header](0)
header.version   // 1
header.length    // 16
```

Within a `sextant` block — named for the instrument that reads a position — successive `unpack`
calls read one value after another, the cursor advancing by each value's width:

```scala
data.sextant:
  val magic = unpack[B32]
  val version = unpack[U16]
  version
```

An array of records unpacks by count, for the repeated sections binary formats favor: the
result of `unpack[Array[Pair]]` is a function from the count to the array, read when the count
is known:

```scala
case class Pair(left: U16, right: U16)

object Pair:
  given Pair is Debufferable = Debufferable.derived

hex"00010002000300040005000600070008".sextant:
  val pairs = unpack[Array[Pair]](4)
  pairs.length   // 4
```

### The typeclasses

`Debufferable` is the reading side — the sixteen sized numeric types have instances, and a case
class derives its own from its fields — and `Bufferable` is the writing side, with the same
derivation. `Unpackable` sits above both, and is what `unpack` resolves: it covers a single
`Debufferable` value and, separately, an array of them, which is how a count-prefixed section
reads as one call rather than a loop.

An instance states two things: `width`, the number of bytes the value occupies, and how to read
those bytes from — or write them to — a `Sextant`. Both are visible to derivation, so a record's
total width is the sum of its fields' and every offset within it is computed at compiletime.
`Debufferable.apply` builds an instance from a width and a function of the bytes and an offset,
which is how the primitives are defined:

```scala
case class Rgb(red: Byte, green: Byte, blue: Byte)

object Rgb:
  given Rgb is Debufferable =
    Debufferable(3) { (bytes, offset) =>
      Rgb(bytes.readUnchecked(offset), bytes.readUnchecked(offset + 1), bytes.readUnchecked(offset + 2)) }
```

A format whose fields need more than fixed-width reads — a length-prefixed string, say — defines
its own instance the same way, and composes into derived records like any primitive. The sized
types also fix the *interpretation*, not merely the width: `B32` is four raw bytes, `U32` reads
them as an unsigned integer, `S32` as a signed one, and the plain Scala `Int`, `Short`, `Long` and
`Byte` have instances too, for layouts that are easier to state in them.

`Bufferable` is the symmetric writing-side typeclass, deriving over products in the same way, so a
layout is declared once and serves both directions.

### The sextant is a capability

A `Sextant` carries a mutable read position, which makes unpacking an effect rather than a pure
read, and the position is what makes successive `unpack` calls advance. It is therefore introduced
by the `sextant { … }` block that scopes it, and is an *exclusive* capability: two readers sharing
one position would each consume bytes the other expected, so the compiler prevents a sextant from
being aliased or from escaping its block.

`offset` reports the current position, for a format that must state where a section begins, and
`advance` skips past padding or a field this reader does not need:

```scala
data.sextant:
  summon[Sextant].advance(4)      // skip the magic number
  val version = unpack[U16]
  summon[Sextant].offset          // 6
```
