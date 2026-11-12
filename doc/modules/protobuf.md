## Protocol Buffers

### About

[Protocol Buffers](https://protobuf.dev/) is the wire format of gRPC, and of much of the world
of services. It is compact, driven by a schema, and defined by Google's proto3 specification.

Here it is encoded and decoded directly from Scala types. A case class with numbered fields
*is* the definition of a message. There is no `*.proto` file to write, and no code generator to
run, and the bytes produced match what `protoc` would produce for the equivalent schema.

### On Protocol Buffers

The conventional workflow puts the schema outside the language. Messages are defined in
`*.proto` files, a generator emits source code, and the build keeps the two in step. That
indirection buys schemas which are shared between languages. For a Scala program talking to a
Scala program, or one which simply must speak an existing proto3 protocol, it is machinery
without benefit.

Here the wire format is [derived][derivation] from the case class itself. Field numbers are the
one thing proto3 needs which Scala does not have, and they come from an annotation. Everything
else follows from the types of the fields: varints, zig-zag encoding, length-delimited messages
and packed repeated fields. The wire format and the code therefore cannot disagree, which is
[correctness](../philosophy/correctness.md) with one definition. The encoding is validated
against `protoc`'s own output.

Everything comes from the `soundness` package. The samples throw any error as an exception, and
the last two imports let them print their results, with bytes written in hexadecimal:

```scala
import soundness.*
import strategies.throwUnsafely
import alphabets.hexLowerCase
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Messages

A message is a case class whose fields carry their proto3 field numbers:

```scala
case class Person(@field(1) name: Text, @field(2) age: Int)
```

`in[Protobuf]` encodes a value as a message, and `encode` gives the message's bytes.
`read[Person in Protobuf]` reads bytes back to the type:

```scala
val bytes = Person(t"Hamlet", 30).in[Protobuf].encode
```

```scala fuel
run
##
Out.println(bytes.serialize[Hex])
Out.println(Chain(bytes).read[Person in Protobuf].inspect)
```

Decoding may also stop at the generic message, to be inspected or converted later.
`read[Protobuf]` gives a `Protobuf` value, and `as[Person]` converts it.

Fields which are left unannotated are numbered in the order they are declared. The numbers may
be sparse, with `@field(3)` and `@field(7)` and nothing between, as the evolution of a protocol
requires. [Annotations](annotations.md) describes how `@field` is read.

### Field types

The Scala type decides the proto3 encoding. The sized [numeric types](numbers.md#numeric-types)
map precisely onto proto3's kinds of integer, so the choice of type is the choice of wire
encoding:

```tabular
column  Scala type
column  Proto3 type
column  Wire encoding
row  `Int`  `int32`  varint
row  `Long`  `int64`  varint
row  `Boolean`  `bool`  varint
row  `U32`  `uint32`  varint
row  `U64`  `uint64`  varint
row  `S32`  `sint32`  zig-zag varint
row  `S64`  `sint64`  zig-zag varint
row  `B32`  `fixed32`  four bytes
row  `B64`  `fixed64`  eight bytes
row  `Float`  `float`  four bytes
row  `Double`  `double`  eight bytes
row  `Text`  `string`  length-delimited UTF-8
row  `Data`  `bytes`  length-delimited
row  a case class  a nested message  length-delimited
row  an enumeration or sealed hierarchy  `oneof`  the variant, chosen by field number
row  `Optional[T]`  `T`, with explicit presence  nothing when `Unset`
row  `List[T]`  `repeated T`  packed, for a number or `Boolean`
row  `Map[K, V]`  `map<K, V>`  repeated key–value entries
```

A message may mix them freely:

```scala
case class Typed
   ( @field(1) unsigned: U32,
     @field(2) signed:   S32,
     @field(3) fixed:    B32 )
```

A repeated field may also be a frozen array, `Array[Long]^{}`, `Array[Double]^{}` or
`Array[Message]^{}`, with the same bytes as the `List` form. It is the representation to choose
for data which is [read or written directly](#parsing-directly). The parser fills it without
allocating a cell for each element, and without boxing a number, and the printer walks it by
index.

### Presence, repetition and maps

Proto3's treatment of absence is a well-known source of confusion. Here the type says which
behavior applies:

```scala
case class MaybeName(@field(1) name: Optional[Text])
case class Tags(@field(1) tags: List[Text])
case class Labels(@field(1) labels: Map[Text, Text])
```

An `Optional` field has explicit presence. When it is unset it writes nothing, and it is read
back as `Unset`, which is distinct from a field which is present and empty:

```scala fuel
run
##
Out.println(MaybeName(Unset).in[Protobuf].encode.length.show)
Out.println(MaybeName(t"").in[Protobuf].encode.length.show)
```

A `List` field is repeated. It keeps its order, and keeps elements which have the default
value: a list of `0, 1, 2` is read back as three elements, without losing the zero. Repeated
numbers are packed into a single length-delimited field, as proto3 requires of a writer. A
reader accepts both the packed and the unpacked forms, as proto3 requires of a reader.

A `Map` becomes the standard repeated messages of a key and a value, so a `Map[Text, Int]` on
the wire is exactly what `map<string, int32>` would be.

### Navigating a message

The fields of a message are keyed by number, not by name, so an optic selects a field by its
number: `Prim` is field 1, `Sec` is field 2, and so on. A [lens](optics.md) reaches through
nested messages, and replaces a field without disturbing the rest:

```scala
import conversions.encodableToProtobuf

case class Point(@field(1) x: Int, @field(2) y: Int)
case class Wrapper(@field(1) point: Point, @field(2) label: Text)

val wrapper: Protobuf = Wrapper(Point(3, 4), t"origin").in[Protobuf]
```

```scala fuel
run
##
Out.println(wrapper.lens(_(Prim) = Point(7, 8)).as[Wrapper].inspect)
```

This is what to use where a message must be relayed with one field altered, and everything
else passed through untouched, including fields which this program does not know about.

### Errors

A malformed message raises a `Protobuf.Error`. It names the problem and, for a fault in the
bytes, its offset, so a fault on the wire is debugged from the error, not from a hex dump:

```tabular
column  Reason
column  Raised when
row  `Truncated`  the data ends in the middle of a field
row  `MalformedVarint`  a varint is malformed
row  `UnexpectedWireType`  a field has a wire type its Scala type cannot read
row  `Overflow`  a number is too large for its type
row  `MissingField`  a required field is absent; it names the field number, not an offset
```

The offset is a position within the message, or the nested message, which is being parsed.
Here the last byte of an encoded `Person` is lost:

```scala fuel
run
errors show
##
Chain(bytes.keep(bytes.length - 1)).read[Person in Protobuf]
```

### Compatibility

Compatibility with `protoc` is by construction, and by test. The canonical example message
from the Protocol Buffers documentation encodes to its documented bytes, `08 96 01`:

```scala
case class Sample(@field(1) value: Int)
```

```scala fuel
run
##
Out.println(Sample(150).in[Protobuf].encode.serialize[Hex])
```

The encodings of the sized numeric types are checked against recorded wire data, not merely
written and read back, since writing and reading agree with each other even when both are
wrong.

Where a message needs no interoperability, the field numbers may be left off entirely. A
message with no annotations numbers its fields from one, in the order they are declared, which
is what `protoc` would have produced for the same declarations:

```scala
case class Unnumbered(value: Int, other: Int)
```

### Parsing directly

A type with a `Protobuf.Parsable` instance is read straight from the wire bytes, field by
field, and no intermediate `Protobuf` value is built. The instance is derived at
[compiletime], and composes a parser for that exact shape of message:

```scala fuel
compile
##
import locomotion.Inlinable

given personParsable: (Person is Protobuf.Parsable) = Inlinable.parsable[Person]

val parsed = Chain(bytes).read[Person in Protobuf]
```

The derivation is imported by name. A message whose field types have hand-written `Inlinable`
instances also needs the `locomotion.staged` module, which evaluates those instances as the
code compiles. Without it, such a field is decoded through its `Decodable in Protobuf`.

Reading from a [stream](streams.md) works in the same way, and the input is never assembled
first. The parser reads each chunk as it arrives, so a field which is split across two chunks
reads exactly as one which arrives whole. The memory held is bounded by the one field being
read, except across a oneof, whose message is held while its variant is chosen.
`read[Protobuf]`, by contrast, reads the whole message into memory, since a `Protobuf` value
is its bytes.

### Printing directly

The same derivation generates the writing side. A `Protobuf.Printable` instance writes a
value's fields straight into the output array. It makes two passes, one to measure every nested
length and one to write. No field is encoded into an intermediate `Protobuf` value, and no
nested message is encoded and then copied into its parent. With an instance in scope,
`in[Protobuf]` uses it:

```scala fuel
compile
##
given personPrintable: (Person is Protobuf.Printable) = Inlinable.printable[Person]

val printed = Person(t"Hamlet", 30).in[Protobuf].encode
```

The bytes are exactly those which the derived encoding writes. A message may therefore be
printed directly on one side of a connection, and decoded through the `Protobuf` value on the
other.

`in[Data over Protobuf]` writes the bytes in one step, as the mirror of
`read[Person in Protobuf]`. It prints directly where a `Printable` exists, and encodes through
the `Protobuf` value otherwise:

```scala
val direct = Person(t"Hamlet", 30).in[Data over Protobuf]
```

```scala fuel
run
##
Out.println(direct.read[Person in Protobuf].inspect)
```

### Choosing the route for every type

Declaring an instance for each type is one way to opt in. A message can also generate both of
its instances at once, into its companion:

```scala
case class Visitor(name: Text, age: Int) derives Protobuf.Direct
```

A single import then selects the route for every message and oneof in scope, without any
declaration. A generic case class or a collection is neither a message nor a oneof:

```tabular
column  Import
column  For a message or oneof
column  For any other type
row  `codecs.direct`  a generated parser and printer  the derived codecs
row  `codecs.derived`  the derived codecs, even where a direct instance is in scope  the derived codecs
row  `codecs.strict`  a generated parser and printer  a compile error
```

The modes are alternatives, so import one at a time.

Under `codecs.direct`, a type which derives `Protobuf.Direct` uses the instances it generated
once, instead of generating them again at each use, which would cost compile time. The results
are the same by either route. Only the speed differs, and the positions reported in errors.

`codecs.strict` is `codecs.direct` for code which must not fall back. A
`read[Play in Protobuf]` or an `in[Data over Protobuf]` on a type with no direct codec is a
compile error which names the type. `in[Protobuf]` names the wire value itself, and is not
restricted.

### Protocol Buffers over HTTP

A `Protobuf` value is a request body or a response body, with the media type
`application/protobuf`, and a body is parsed back on arrival. A binary API is therefore
consumed and offered with the same code as a [JSON one](json.md#json-over-http). Only the
format's type differs. [RPC](rpc.md) builds typed remote calls on such messages.

### Related

- [Binary data](binary-data.md) describes the bytes beneath the wire format.
