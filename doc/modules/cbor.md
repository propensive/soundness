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

The codec derives from the type, so the type is the schema: the [correctness](../philosophy/correctness.md) of the encoding follows from the definition rather than from tests.

Soundness gives CBOR the full textual-format treatment: derived codecs, typed errors with reasons,
dynamic access, and optics, so choosing the compact wire format costs no expressiveness in the
code. Everything comes from the `soundness` package:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
```

### Encoding and decoding

A value encodes with `in[Cbor]`, and CBOR data decodes to a type with `as`:

```scala
case class Person(name: Text, age: Int)

val encoded = Person(t"Ada", 36).in[Cbor]   // a Cbor value
encoded.as[Person]                          // Person(t"Ada", 36)
```

A `Cbor` value is a tree; `Cbor.unseal` exposes it as a `Cbor.Ast`, which serializes to its bytes,
and bytes read straight back to a type in one step, whether they arrive whole or as a
[stream](streams.md) of chunks:

```scala
val bytes: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(encoded))
bytes.length                        // 15

Chain(bytes).read[Person in Cbor]   // Person(t"Ada", 36)
```

The two steps of writing combine in the same way. `in[Data over Cbor]` is the mirror of
`read[Person in Cbor]`, encoding a value straight to its bytes:

```scala
Person(t"Ada", 36).in[Data over Cbor].read[Person in Cbor]   // Person(t"Ada", 36)
```

The `@name[Cbor]` [annotation](annotations.md) renames a field on the wire, and a sealed
hierarchy encodes with a discriminator declared once:

```scala
enum Status:
  case Active(since: Int)
  case Retired

given (Status is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")

(Status.Active(2020): Status).in[Cbor].as[Status]   // Status.Active(2020)
```

An `Optional` field is omitted from the map when it is unset and supplied as `Unset` when the map
lacks it, and a field with a default takes that default — so a message may gain fields without
breaking readers that predate them. A CBOR `null` reads as `Unset` too (`undefined` is `Unset`
on the wire already), and a value the inner decoder rejects is an error. The
`optionalityOptions` package varies each answer separately — `strictCborAbsence`,
`strictCborNulls` and `lenientCborFaults` — as [decoding](decoding.md) describes:

```scala
case class Reading(sensor: Text, value: Optional[Int])

val tolerant =
  import dynamicAccess.dynamicCbor
  import optionalityOptions.lenientCborFaults
  (Reading(t"a", 1).in[Cbor].value = t"x").as[Reading]   // Reading(t"a", Unset)
```

### Parsing directly

A type with a `Cbor.Parsable` instance is read straight from the bytes, with no intermediate tree
built. The instance is derived at compiletime, composing a parser for that exact shape:

```scala
import breviloquence.Inlinable

given (Person is Cbor.Parsable) = Inlinable.parsable[Person]

Chain(bytes).read[Person in Cbor]   // parsed without building a tree
```

The derivation composes the parser at expansion time and is imported by name. A sealed hierarchy,
and a field whose type has a hand-written `Inlinable`, need the `breviloquence.staged` module
too: it evaluates the discriminator and those instances as the code compiles.

Reading over a stream works the same way, and the input is never assembled first: the parser
reads each chunk as it arrives, so a value split across two chunks reads exactly as one that
arrives whole, and the memory held is bounded by the one item being read, not the document. A
`read[Cbor]` likewise reads the chunks as they arrive, but builds the whole tree.

### Printing directly

The same derivation generates the writing side. A `Cbor.Printable` writes a value's bytes
straight out, with no tree built, and `in[Data over Cbor]` uses one wherever it exists:

```scala
given (Person is Cbor.Printable) = Inlinable.printable[Person]

Person(t"Ada", 36).in[Data over Cbor]   // the same bytes, written directly
```

The bytes are exactly those the derived encoding writes. `Cbor.Printable.emit` writes them a
block at a time, handing each to a function as it fills, so a value can be sent without its
whole encoding ever being held.

### Choosing the path for every type

A record can generate both instances once, into its companion, with `derives Cbor.Direct`, and a
single import selects the path for every record in scope:

```scala
import codecs.direct     // a generated parser and printer wherever a type is a record
import codecs.derived    // the derived codecs, even where a direct instance is in scope
```

Under `codecs.direct`, a type that is not a record — a generic case class, a sealed hierarchy —
keeps the derived path unless it has an instance of its own.

### Tags

CBOR's tags annotate a value with its meaning — tag 1 is an epoch time, tag 2 a big integer, and
the registry runs to hundreds. A tagged value keeps both its tag and the value inside it, so a
document carrying tags round-trips without losing them. These bytes are tag 1 around the integer
1363896240:

```scala
val document = Cbor.ast(Cbor.Ast.parse(hex"c11a514b67b0"))

val ast = Cbor.unseal(document)
ast.isTag && ast.tag.tag == 1L   // true
```

### Diagnostic notation

RFC 8949 defines a human-readable rendering for CBOR — the format's counterpart of a hex dump that
can actually be read — and `show` produces it. Byte strings render as `h'…'`, arrays and maps as
their JSON-like equivalents:

```scala
Cbor.Ast.parse(hex"83010203").show     // t"[1, 2, 3]"
Cbor.Ast.parse(hex"4401020304").show   // t"h'01020304'"
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

The import grants `Cbor is Dynamical` to the rest of its scope; `dynamically[Cbor]:` grants
it to a single block instead, as described in the JSON tutorial.

Deeper updates use a [lens](optics.md), with `Each` and `Filter` optics touching many elements at
once, exactly as for the textual formats.

### Building directly

A CBOR map is assembled from named arguments where no case class fits:

```scala
Cbor.make(name = t"Anna".in[Cbor], age = 30.in[Cbor]).as[Person]   // Person(t"Anna", 30)
```

### Errors

A malformed document or a failed conversion raises a `Cbor.Error` whose reason is specific —
truncated input, an integer overflow, a wrong type, an absent field — so a protocol failure is
diagnosed from the error rather than from a hex dump.

Parse failures carry the byte offset at which they occurred, which for a binary format is the
whole of the diagnosis: input that stops mid-value names the offset of the byte that should have
followed, trailing bytes name where the surplus begins, and a reserved head byte names both its
offset and its value. These bytes promise a three-element array and deliver two:

```scala
capture[Cbor.Error](Cbor.Ast.parse(hex"830102")).reason match
  case Cbor.Error.Reason.Truncated(offset) => offset   // 3
  case Cbor.Error.Reason.Trailing(offset)  => offset
  case _                                   => -1L
```

### CBOR over HTTP

A `Cbor` value serves as a request or response body with the `application/cbor` media type, and a
body parses back on arrival, so a binary API is consumed and offered with the same code as a JSON
one — only the format's type differs.
