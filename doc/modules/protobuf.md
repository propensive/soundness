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
val bytes = Person(t"Alice", 30).protobuf.encode   // the wire bytes

Stream(bytes).read[Person in Protobuf]             // Person(t"Alice", 30)
```

Fields left unannotated number themselves in declaration order, and the numbers may be sparse —
`@field(3)` and `@field(7)` with nothing between — as protocol evolution requires.

### Field types

The Scala type decides the proto3 encoding. The sized [numeric types](numbers.md) map onto proto3's
integer flavours precisely — an unsigned `U32` is a `uint32` varint, a signed `S32` uses zig-zag
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

### Compatibility

Compatibility with `protoc` is by construction and by test: the canonical example message from the
protobuf documentation encodes to its documented bytes:

```scala
case class Sample(@field(1) value: Int)

Sample(150).protobuf.encode   // the bytes 08 96 01
```

Decoding accepts both packed and unpacked repeated fields, as the proto3 specification requires of
a conforming reader.

### Errors

A malformed message raises a `ProtobufError` naming the problem and the byte offset — truncated
input, a malformed varint, an unexpected wire type, a missing required field — so wire-level faults
are debugged from the error rather than a hex dump.
