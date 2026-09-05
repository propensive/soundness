## Protocol Buffers

### About

[Protocol Buffers](https://protobuf.dev/) is the wire format of gRPC and much of the service world:
compact, schema-driven, and defined by Google's proto3 specification. Soundness encodes and decodes
it directly from Scala types — a case class with numbered fields *is* the message definition, so
there is no `.proto` file to write and no code generator to run, and the bytes produced match what
`protoc` would produce for the equivalent schema.

### On Protocol Buffers

The conventional workflow puts the schema outside the language: messages are defined in `.proto`
files, a generator emits source code, and the build keeps the two in step. That indirection buys
cross-language schemas, but for a Scala program talking to a Scala program — or one that simply
must speak an existing proto3 protocol — it is machinery without benefit.

Field numbers and types living in the case class, and the encoder derived from them, keep the wire format and the code in agreement — [correctness](../philosophy/correctness.md) with one definition.

Soundness derives the wire format from the case class itself. Field numbers, the one thing proto3
needs that Scala does not, come from an annotation; everything else — varints, zig-zag encoding,
length-delimited messages, packed repeated fields — follows from the field types, and the encoding
is validated against `protoc`'s own output. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Messages

A message is a case class whose fields carry their proto3 field numbers:

```scala
case class Person(@field(1) name: Text, @field(2) age: Int)
```

Encoding produces the message value and then its bytes; decoding reads bytes back to the type:

```scala
val bytes = Person(t"Alice", 30).in[Protobuf].encode   // the wire bytes

Chain(bytes).read[Person in Protobuf]                  // Person(t"Alice", 30)
```

Decoding may also stop at the generic message, to be inspected or converted later: `read[Protobuf]`
gives a `Protobuf` value, and `as[Person]` converts it.

Fields left unannotated number themselves in declaration order, and the numbers may be sparse —
`@field(3)` and `@field(7)` with nothing between — as protocol evolution requires.

### Field types

The Scala type decides the proto3 encoding. The sized [numeric types](numbers.md) map onto proto3's
integer flavors precisely — an unsigned `U32` is a `uint32` varint, a signed `S32` uses zig-zag
`sint32`, a `B32` is a `fixed32` — text is length-delimited, an `Optional` field may be absent, a
`List` of numbers packs, and a `Map` becomes the standard repeated key–value entries:

```scala
case class Typed
   ( @field(1) unsigned: U32,
     @field(2) signed:   S32,
     @field(3) fixed:    B32 )
```

A nested case class is a nested message, and an enumeration or sealed hierarchy encodes as proto3's
`oneof`, with the variant chosen by field number.

### Presence, repetition and maps

Proto3's treatment of absence is a well-known source of confusion, and the type says which
behavior applies. An `Optional` field has explicit presence: unset, it writes nothing, and reads
back as `Unset` — distinct from a field that is present and zero.

A `List` field is repeated, and keeps its order and its default elements: a list of `0, 1, 2`
round-trips as three elements rather than losing the zero. Repeated numbers are packed into a
single length-delimited field, as proto3 requires of a writer, while a reader accepts both packed
and unpacked forms, as it requires of a reader.

A `Map` becomes the standard repeated key–value entry messages, so a `Map[Text, Int]` is on the
wire exactly what `map<string, int32>` would be:

```scala
case class Labels(@field(1) labels: Map[Text, Text])
case class Tags(@field(1) tags: List[Text])
case class MaybeName(@field(1) name: Optional[Text])
```

### Navigating a message

A message is number-keyed rather than name-keyed, so an optic selects a field by its number: `Prim`
is field 1, `Sec` field 2, and so on. A lens reaches through nested messages and replaces a field
without disturbing the rest:

```scala
import conversions.encodableToProtobuf

case class Point(@field(1) x: Int, @field(2) y: Int)
case class Wrapper(@field(1) point: Point, @field(2) label: Text)

val wrapper: Protobuf = Wrapper(Point(3, 4), t"origin").in[Protobuf]

wrapper.lens(_(Prim) = Point(7, 8)).as[Wrapper]   // Wrapper(Point(7, 8), t"origin")
```

This is what to use where a message must be relayed with one field altered and everything else —
including fields this program does not know about — passed through untouched.

### Compatibility

Compatibility with `protoc` is by construction and by test: the canonical example message from the
protobuf documentation encodes to its documented bytes:

```scala
case class Sample(@field(1) value: Int)

Sample(150).in[Protobuf].encode   // the bytes 08 96 01
```

Decoding accepts both packed and unpacked repeated fields, as the proto3 specification requires of
a conforming reader, and the encodings of the sized numeric types are checked against golden wire
vectors rather than merely round-tripped — a round trip agrees with itself even when both
directions are wrong.

Where a message needs no interoperability, the field numbers may be left off entirely: an
unannotated message numbers its fields from one in declaration order, which is what `protoc` would
have produced for the same declarations.

```scala
case class Unnumbered(value: Int, other: Int)   // fields 1 and 2
```

### Errors

A malformed message raises a `Protobuf.Error` naming the problem and the byte offset — truncated
input, a malformed varint, an unexpected wire type, a missing required field — so wire-level faults
are debugged from the error rather than a hex dump.
