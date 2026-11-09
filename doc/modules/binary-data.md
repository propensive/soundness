## Binary Data

### About

Structured binary data is made of fixed-width records: the headers of files, network packets,
the messages of device protocols. It is read through a [typeclass] which maps bytes onto Scala
types.

A case class of sized [numeric types](numbers.md#numeric-types) describes a binary layout.
Each field states its exact width and how its bytes are to be read, so the shape of the record
is its definition, and reading a value is a matter of naming its type at an offset.

### On binary layouts

Parsing a binary structure by hand is arithmetic on indexes. Bytes 4 to 7 are the length,
little-endian. Byte 8 is the flags. Every offset is computed relative to the one before, every
width is a constant to be kept in step with a specification, and a change to one field
silently shifts every field after it.

Here the layout is [derived][derivation] from a case class. The sized types say how many bytes
each field occupies and how they are read: a `U32` is four bytes read as an unsigned integer,
and a `B16` is two raw bytes. Derivation composes the fields in the order they are declared,
and computes every offset. Describing a layout once, and deriving both its reader and its
writer from it, keeps the two from drifting apart, which is
[correctness](../philosophy/correctness.md) by having one definition.

Everything comes from the `soundness` package. The second import lets the samples print their
results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Describing a layout

A binary record is a case class of fixed-width fields, which is given a `Debufferable`
instance by derivation:

```scala
case class Header(magic: B32, version: U16, flags: B16, length: U32)

object Header:
  given debufferable: (Header is Debufferable) = Debufferable.derived
```

Each field's width follows from its type. Here the widths are four, two, two and four bytes,
so the record is twelve bytes. `byteWidth` says so, without a value in hand:

```scala fuel
run
##
Out.println(byteWidth[Header].show)
Out.println(byteWidth[U16].show)
```

Records nest. A field whose type is itself a `Debufferable` record contributes its whole
width, so a layout is composed from smaller layouts, and need not be flattened by hand.

### Reading

`unpackFrom` reads a value of a chosen type from bytes, at an offset:

```scala
val data = hex"cafebabe0001000000000010"

val header = data.unpackFrom[Header](0)
```

```scala fuel
run
##
Out.println(header.magic.hex)
Out.println(header.version.inspect)
Out.println(header.length.inspect)
```

Within a `sextant` block, named for the instrument which reads a position, successive calls
to `unpack` read one value after another. The position advances by the width of each value:

```scala
val version = data.sextant:
  val magic = unpack[B32]
  unpack[U16]
```

Binary formats favor repeated sections, and an array of records is unpacked by its count. The
result of `unpack[Array[Pair]]` is a function from the count to the array, so the array is read
once the count is known:

```scala
case class Pair(left: U16, right: U16)

object Pair:
  given debufferable: (Pair is Debufferable) = Debufferable.derived

val count = hex"00010002000300040005000600070008".sextant:
  val pairs = unpack[Array[Pair]](4)
  pairs.length
```

```scala fuel
run
##
Out.println(version.inspect)
Out.println(count.show)
```

### The typeclasses

Three typeclasses do the work:

```tabular
column  Typeclass
column  Describes
row  `Debufferable`  reading a value from bytes; the sixteen sized numeric types have instances, and a case class derives one from its fields
row  `Bufferable`  writing a value to bytes, with the same derivation
row  `Unpackable`  what `unpack` asks for: a single `Debufferable` value, or an array of them
```

`Unpackable` is what lets a section which is prefixed by its count be read with one call
instead of a loop.

An instance states two things: `width`, the number of bytes which the value occupies, and how
to read those bytes from a `Sextant`, or write them to one. Both are visible to derivation. A
record's total width is the sum of its fields' widths, and every offset within it is computed
at [compiletime].

`Debufferable.apply` builds an instance from a width, and a function of the bytes and an
offset. The primitive instances are defined in this way:

```scala
case class Rgb(red: Byte, green: Byte, blue: Byte)

object Rgb:
  given debufferable: (Rgb is Debufferable) =
    Debufferable(3) { (bytes, offset) =>
      Rgb(bytes.readUnchecked(offset), bytes.readUnchecked(offset + 1), bytes.readUnchecked(offset + 2)) }
```

A format whose fields need more than fixed-width reads, such as a string prefixed by its
length, defines an instance of its own in the same way. It then composes into derived records
like any primitive.

The sized types fix how bytes are *interpreted*, not merely how many there are. A `B32` is four
raw bytes, a `U32` reads them as an unsigned integer, and an `S32` reads them as a signed one.
The plain Scala types have instances too, for layouts which are easier to state with them:

```tabular
column  Width in bytes
column  Raw bits
column  Unsigned
column  Signed
column  Scala type
row  1  `B8`  `U8`  `S8`  `Byte`
row  2  `B16`  `U16`  `S16`  `Short`
row  4  `B32`  `U32`  `S32`  `Int`
row  8  `B64`  `U64`  `S64`  `Long`
```

`Bufferable` is the writing side, and is derived over case classes in the same way, so a
layout is declared once and serves both directions.

### The sextant is a capability

A `Sextant` carries a mutable read position. That makes unpacking an effect, not a pure read,
and the position is what makes successive calls to `unpack` advance. A sextant is therefore
introduced by the `sextant` block which scopes it, and it is an *exclusive* [capability]. Two
readers which shared one position would each consume bytes the other expected, so the compiler
prevents a sextant from being aliased, or from escaping its block.

`offset` reports the current position, for a format which must state where a section begins.
`advance` skips past padding, or past a field which this reader does not need:

```scala
val position = data.sextant:
  summon[Sextant].advance(4)
  val skipped = unpack[U16]
  summon[Sextant].offset
```

```scala fuel
run
##
Out.println(position.show)
```

Outside a `sextant` block there is no position to read from, and `unpack` does not compile:

```scala fuel
compile
errors show
##
unpack[U16]
```

### Related

- [Numbers](numbers.md#numeric-types) describes the sized types which fields are declared
  with.
- [Bit flags](flags.md) names the bits of a word by the cases of an enumeration.
- [Streams](streams.md#parsing-with-a-cursor) reads formats whose records are not of fixed
  width.
- [CBOR](cbor.md) and [Protocol Buffers](protobuf.md) are binary formats which describe
  themselves.
