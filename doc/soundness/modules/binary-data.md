## Binary Data

### About

Structured binary data is made of fixed-width records: the headers of files, network packets,
the messages of device protocols. It is read through a [typeclass] which maps bytes onto Scala
types.

A case class of sized [numeric types](numbers.md#numeric-types) describes a binary layout.
Each field states its exact width and how its bytes are to be read, so the shape of the record
is its definition, and reading a value is a matter of naming its type at an offset. Writing one
is the same definition, used in the other direction.

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
[correctness](../../philosophy/correctness.md) by having one definition.

Everything comes from the `soundness` package. The second import lets the samples print their
results, and the third lets them read bytes without handling the error a short record raises:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import strategies.throwUnsafely
```

### Describing a layout

A binary record is a case class of fixed-width fields, which is given a `Structural` instance
by derivation. The order of the bytes within a field belongs to the format, so it is chosen
where the layout is declared, by importing it from `byteOrders`:

```scala
import byteOrders.bigEndianByteOrder

case class Header(magic: B32, version: U16, flags: B16, length: U32)

object Header:
  given structural: (Header is Structural) = Structural.derived
```

Each field's width follows from its type. Here the widths are four, two, two and four bytes,
so the record is twelve bytes. `Structural.width` says so, without a value in hand:

```scala fuel
run
##
Out.println(Structural.width[Header].show)
```

Records nest. A field whose type is itself `Structural` contributes its whole width, so a
layout is composed from smaller layouts, and need not be flattened by hand. A nested record
keeps the byte order it was declared with, which is how a format that mixes the two is
described.

A derivation with no byte order in scope does not compile, and neither does one whose field
has a type with no fixed layout.

### Reading

`unpack` reads a value of a chosen type from bytes, at an offset:

```scala
val data = hex"cafebabe0001000000000010"

val header = data.unpack[Header](0)
```

```scala fuel
run
##
Out.println(header.magic.hex)
Out.println(header.version.inspect)
Out.println(header.length.inspect)
```

The whole record is checked against the length of the data once, before any field is read. A
record which would run past the end raises a `Structural.Error`, which states the offset, the
record's width and the number of bytes there were:

```scala fuel
run
##
Out.println(safely(data.unpack[Header](4)).absent.inspect)
```

Binary formats favor repeated sections, and a section of records is unpacked by its offset
and its count, as one array:

```scala
case class Pair(left: U16, right: U16)

object Pair:
  given structural: (Pair is Structural) = Structural.derived

val pairs = hex"00010002000300040005000600070008".unpack[Pair](0, 4)
```

```scala fuel
run
##
Out.println(pairs.length.show)
```

The same call reads a record from an `Expanse`, such as a `Terrain` of memory-mapped bytes,
at a `Long` offset. Only the record's own bytes are read from it.

### Writing

`pack` is the reverse: it lays a value out in exactly as many bytes as its layout is wide,
with the same instance, so what is packed is what `unpack` reads back:

```scala
val bytes = Header(0xcafebabe.bits, 1.toShort.bits.u16, 0.toShort.bits, 16.bits.u32).pack
```

```scala fuel
run
##
Out.println(bytes.length.show)
Out.println((bytes.unpack[Header](0) == header).inspect)
```

### The typeclass

`Structural` describes a type with a fixed layout, in both directions. An instance states
three things: `width`, the number of bytes which the value occupies; `load`, which reads a
value from those bytes at an offset; and `store`, which writes one into a byte array. `unpack`
and `pack` check that the bytes are there, and then call them.

Derivation computes every offset at [compiletime], when the layout is declared, and expands to
one load for each field at its offset, in the byte order chosen. Reading a record is therefore
a single bounds check and as many loads as there are fields, with nothing allocated but the
record.

The sized types fix how bytes are *interpreted*, not merely how many there are. A `B32` is four
raw bytes, a `U32` reads them as an unsigned integer, and an `S32` reads them as a signed one.
The plain Scala types may be used too, for layouts which are easier to state with them:

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

A field of any other type needs a `Structural` instance of its own, which derivation then
composes like a nested record. This is how a field is given a representation the sized types
do not offer, such as three bytes of color:

```scala
case class Rgb(red: Byte, green: Byte, blue: Byte)

object Rgb:
  given structural: (Rgb is Structural) = new Structural:
    type Self = Rgb
    def width: Int = 3

    def load(data: Data, offset: Int): Rgb =
      Rgb(data.u8(offset).toByte, data.u8(offset + 1).toByte, data.u8(offset + 2).toByte)

    def store(value: Rgb, target: Array[Byte]^, offset: Int): Unit =
      target(offset) = value.red
      target(offset + 1) = value.green
      target(offset + 2) = value.blue
```

`load` and `store` are not checked: they are called only once the caller has established that
`width` bytes are there.

### Related

- [Numbers](numbers.md#numeric-types) describes the sized types which fields are declared
  with.
- [Bit flags](flags.md) names the bits of a word by the cases of an enumeration.
- [Streams](streams.md#parsing-with-a-cursor) reads formats whose records are not of fixed
  width.
- [CBOR](cbor.md) and [Protocol Buffers](protobuf.md) are binary formats which describe
  themselves.
- [Audio](audio.md), [images](images.md) and [base encoding](base-encoding.md) read and write their bytes as `Data`.
