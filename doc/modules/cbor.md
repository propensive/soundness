## CBOR

### About

[CBOR](https://en.wikipedia.org/wiki/CBOR) is a binary cousin of JSON — the same data model,
encoded compactly for machines rather than legibly for people, standardized as
[RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949). Soundness reads and writes it with the
same interface as its textual siblings: bytes parse into a `Cbor` value, case classes convert to
and from it with derived codecs, and navigation, updates and lenses work as they do for
[JSON](json.md) and [YAML](yaml.md).

### On binary interchange

JSON pays for its readability in bytes and parsing time, and where no human reads the wire — a
device protocol, a cache entry, a message queue — that price buys nothing. CBOR keeps JSON's shape
(maps, arrays, numbers, text, binary strings) in a compact binary form, but binary formats are
usually consumed through byte-fiddling APIs far clumsier than their textual equivalents.

Soundness gives CBOR the full textual-format treatment: derived codecs, typed errors with reasons,
dynamic access, and optics, so choosing the compact wire format costs no expressiveness in the
code. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Encoding and decoding

A value encodes with `in[Cbor]`, and CBOR data decodes to a type with `as` — or in one step, reading
bytes straight to the type:

```scala
case class Person(name: Text, age: Int)

val encoded = Person(t"Ada", 36).in[Cbor]       // a Cbor value
encoded.as[Person]                          // Person(t"Ada", 36)

stream.read[Person in Cbor]                 // bytes to Person directly
```

The `@name[Cbor]` annotation renames a field on the wire, and a sealed hierarchy encodes with a
discriminator declared once:

```scala
given (Status is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")
```

An `Optional` field is omitted from the map when it is unset and supplied as `Unset` when the map
lacks it, and a field with a default takes that default — so a message may gain fields without
breaking readers that predate them.

### Parsing directly, and from a stream

A type with a `Cbor.Parsable` instance is read straight from the bytes, with no intermediate tree
built. The instance is derived at compiletime, composing a parser for that exact shape:

```scala
import breviloquence.Inlinable

given (Person is Cbor.Parsable) = Inlinable.parsable[Person]

data.read[Person in Cbor]
```

The derivation composes the parser at expansion time, so it lives in a separate module from the
runtime codecs and is imported by name.

Reading also works over a [stream](streams.md) whose chunk boundaries fall anywhere: a value split
across two chunks reads exactly as one that arrives whole, so a message need not be assembled
before it is decoded.

### Tags

CBOR's tags annotate a value with its meaning — tag 1 is an epoch time, tag 2 a big integer, and
the registry runs to hundreds. A tagged value keeps both its tag and the value inside it, so a
document carrying tags round-trips without losing them:

```scala
val ast = Cbor.unseal(document)
ast.isTag && ast.tag.tag == 1L
```

### Diagnostic notation

RFC 8949 defines a human-readable rendering for CBOR — the format's counterpart of a hex dump that
can actually be read — and `show` produces it. Byte strings render as `h'…'`, arrays and maps as
their JSON-like equivalents:

```scala
Cbor.Ast.parse(bytes).show   // t"[1, 2, 3]", or t"h'01020304'"
```

This is what to reach for when a message is not what it should be: the notation says what the
bytes mean, in the format's own vocabulary.

### Navigating and updating

With dynamic access enabled, a map's fields read as members, an array indexes, and updates —
including removal, by assigning `Unset` — produce new values:

```scala
import dynamicAccess.dynamicCbor

val person = Person(t"Ada", 36).in[Cbor]
person.name.as[Text]                 // t"Ada"
(person.age = 40).as[Person]         // Person(t"Ada", 40)
```

Deeper updates use a lens, with `Each` and `Filter` optics touching many elements at once, exactly
as for the textual formats.

### Building directly

A CBOR map is assembled from named arguments where no case class fits:

```scala
Cbor.make(name = t"Anna".in[Cbor], age = 30.in[Cbor])
```

### Errors

A malformed document or a failed conversion raises a `CborError` whose reason is specific —
truncated input, an integer overflow, a wrong type, an absent field — so a protocol failure is
diagnosed from the error rather than from a hex dump.

Parse failures carry the byte offset at which they occurred, which for a binary format is the
whole of the diagnosis: input that stops mid-value names the offset of the byte that should have
followed, trailing bytes name where the surplus begins, and a reserved head byte names both its
offset and its value.

```scala
capture[CborError](Cbor.Ast.parse(data)).reason match
  case CborError.Reason.Truncated(offset) => offset
  case CborError.Reason.Trailing(offset)  => offset
  case _                                  => -1L
```

### CBOR over HTTP

A `Cbor` value serves as a request or response body with the `application/cbor` media type, and a
body parses back on arrival, so a binary API is consumed and offered with the same code as a JSON
one — only the format's type differs.
