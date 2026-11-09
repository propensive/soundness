## CBOR

### About

[CBOR](https://en.wikipedia.org/wiki/CBOR) is a binary cousin of JSON. It has the same data
model, encoded compactly for machines instead of legibly for people, and is standardized as
[RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949).

It is read and written with the same interface as the textual formats. Bytes parse into a
`Cbor` value. Case classes convert to and from it with [derived][derivation] codecs.
Navigation, updates and lenses work as they do for [JSON](json.md) and [YAML](yaml.md).

### On binary interchange

JSON pays for its readability in bytes and in parsing time. Where no person reads the data, as
in a device protocol, a cache entry or a message queue, that price buys nothing. CBOR keeps
JSON's shape of maps, arrays, numbers, text and byte strings, in a compact binary form.

Binary formats are usually consumed through byte-fiddling APIs, far clumsier than those of
their textual equivalents. Here CBOR gets the full treatment of a textual format: derived
codecs, typed errors with reasons, dynamic access, and optics. Choosing the compact format
costs the code no expressiveness.

The codec is derived from the type, so the type is the schema. The
[correctness](../philosophy/correctness.md) of the encoding follows from the definition, not
from tests.

Everything comes from the `soundness` package. The samples throw any error as an exception, and
the last import lets them print their results:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Encoding and decoding

`in[Cbor]` encodes a value as a `Cbor` value, and `as` decodes one to a type:

```scala
case class Person(name: Text, age: Int)

val encoded = Person(t"Ada", 36).in[Cbor]
```

```scala fuel
run
##
Out.println(encoded.as[Person].inspect)
```

A `Cbor` value is a tree. `Cbor.unseal` exposes it as a `Cbor.Ast`, which serializes to its
bytes:

```scala
val bytes: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(encoded))
```

Bytes are read straight back to a type in one step, with `read[Person in Cbor]`, whether they
arrive whole or as a [stream](streams.md) of chunks:

```scala fuel
run
##
Out.println(bytes.length.show)
Out.println(Chain(bytes).read[Person in Cbor].inspect)
```

The two steps of writing combine in the same way. `in[Data over Cbor]` encodes a value straight
to its bytes, and is the mirror of `read[Person in Cbor]`:

```scala fuel
run
##
Out.println(Person(t"Ada", 36).in[Data over Cbor].read[Person in Cbor].inspect)
```

The same `Person`, written as compact [JSON](json.md), is 23 bytes.

The `@name[Cbor]` [annotation](annotations.md) renames a field in the encoded map. A sealed
hierarchy encodes with a discriminator, declared once:

```scala
enum Status:
  case Active(since: Int)
  case Retired

given statusDiscriminable: (Status is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")

val active: Status = Status.Active(2020)
```

```scala fuel
run
##
Out.println(active.in[Cbor].as[Status].inspect)
```

### Optional fields

An `Optional` field is left out of the map when it is unset, and a field with a default takes
that default when the map lacks it. A message may therefore gain fields without breaking
readers which are older than they are.

Reading an `Optional` field follows the policies which
[Decoding](decoding.md#policies-for-optional-fields) describes. Each is varied separately, by a
given in the package `optionalityOptions`:

```tabular
column  A field which is
column  Reads as
column  Unless
row  omitted from the map, or `undefined`  `Unset`  `strictCborAbsence` makes it an error
row  a CBOR `null`  `Unset`  `strictCborNulls` hands it to the inner decoder
row  present but rejected by the inner decoder  an error  `lenientCborFaults` reads it as `Unset`
```

Here a text is put where a number belongs, and the lenient policy reads it as `Unset`:

```scala
case class Reading(sensor: Text, value: Optional[Int])

val tolerant =
  import dynamicAccess.dynamicCbor
  import optionalityOptions.lenientCborFaults
  (Reading(t"a", 1).in[Cbor].value = t"x").as[Reading]
```

```scala fuel
run
##
Out.println(tolerant.inspect)
```

### Navigating and updating

With dynamic access enabled, a map's fields are read as members and an array is indexed.
Assigning to a field gives a new value, and assigning `Unset` removes the field:

```scala
import dynamicAccess.dynamicCbor

val person = Person(t"Ada", 36).in[Cbor]
```

```scala fuel
run
##
Out.println(person.name.as[Text])
Out.println((person.age = 40).as[Person].inspect)
```

The import grants the permission `Cbor is Dynamical` to the rest of its scope.
`dynamically[Cbor]:` grants it to a single block instead, as
[JSON](json.md#fields-as-members) describes.

A deeper update uses a [lens](optics.md), with the optics `Each` and `Filter` to change many
elements at once, exactly as for the textual formats.

### Building directly

Where no case class fits, a CBOR map is assembled from named arguments:

```scala fuel
run
##
Out.println(Cbor.make(name = t"Anna".in[Cbor], age = 30.in[Cbor]).as[Person].inspect)
```

### Diagnostic notation

RFC 8949 defines a human-readable rendering of CBOR. It is the format's counterpart of a hex
dump, but one which can actually be read, and `show` produces it. A byte string is written as
`h'…'`, and arrays and maps as their JSON-like equivalents:

```scala fuel
run
##
Out.println(Cbor.Ast.parse(hex"83010203").show)
Out.println(Cbor.Ast.parse(hex"4401020304").show)
```

This is what to reach for when a message is not what it should be. The notation says what the
bytes mean, in the format's own vocabulary. [Base encoding](base-encoding.md) describes the
`hex"…"` literals.

### Tags

CBOR's tags annotate a value with its meaning. Tag 1 is an epoch time, tag 2 is a big integer,
and the registry runs to hundreds. A tagged value keeps both its tag and the value inside it,
so a document which carries tags is read and written without losing them. These bytes are
tag 1 around the integer 1363896240:

```scala
val document = Cbor.ast(Cbor.Ast.parse(hex"c11a514b67b0"))
val ast = Cbor.unseal(document)
```

```scala fuel
run
##
Out.println(ast.isTag.inspect)
Out.println(ast.tag.tag.show)
```

### Errors

A malformed document, or a conversion which cannot be made, raises a `Cbor.Error` with a
specific reason. A protocol failure is diagnosed from the error, not from a hex dump:

```tabular
column  Reason
column  Raised when
row  `Truncated(offset)`  the input stops mid-value
row  `Reserved(offset, byte)`  a head byte is one CBOR reserves
row  `BadSimpleValue(offset, value)`  a simple value is invalid
row  `InvalidUtf8(offset)`  a text string is not valid UTF-8
row  `Overflow(offset)`  an integer is too large for a `Long`
row  `UnexpectedBreak(offset)`  a break stop code is unexpected
row  `Trailing(offset)`  bytes follow the end of the value
row  `OutOfRange`  an array index is out of range
row  `NotType(found, expected)`  a value has the wrong type
row  `NonStringKey`  a map key is not a string
row  `Absent`  a value is not present
```

A parsing failure carries the offset of the byte at which it occurred, which for a binary
format is the whole of the diagnosis. Input which stops mid-value names the offset of the byte
which should have followed. Trailing bytes name where the surplus begins. A reserved head byte
names both its offset and its value.

These bytes promise an array of three elements, and deliver two:

```scala fuel
run
errors show
##
Cbor.Ast.parse(hex"830102")
```

```scala fuel
run
##
Out.println(capture[Cbor.Error](Cbor.Ast.parse(hex"830102")).reason.inspect)
```

A conversion to the wrong type is reported as clearly:

```scala fuel
run
errors show
##
encoded.as[List[Int]]
```

### Parsing directly

A type with a `Cbor.Parsable` instance is read straight from the bytes, with no intermediate
tree. The instance is derived at [compiletime], and composes a parser for that exact shape:

```scala fuel
compile
##
import breviloquence.Inlinable

given personParsable: (Person is Cbor.Parsable) = Inlinable.parsable[Person]

val direct = Chain(bytes).read[Person in Cbor]
```

The derivation is imported by name. A sealed hierarchy, and a field whose type has a
hand-written `Inlinable`, also need the `breviloquence.staged` module, which evaluates the
discriminator and those instances as the code compiles.

Reading from a stream works in the same way, and the input is never assembled first. The parser
reads each chunk as it arrives, so a value which is split across two chunks reads exactly as
one which arrives whole. The memory held is bounded by the one item being read, not by the
document. `read[Cbor]` likewise reads the chunks as they arrive, but builds the whole tree.

### Printing directly

The same derivation generates the writing side. A `Cbor.Printable` writes a value's bytes
straight out, with no tree, and `in[Data over Cbor]` uses one wherever it exists:

```scala fuel
compile
##
given personPrintable: (Person is Cbor.Printable) = Inlinable.printable[Person]

val written = Person(t"Ada", 36).in[Data over Cbor]
```

The bytes are exactly those the derived encoding writes. `Cbor.Printable.emit` writes them a
block at a time, handing each block to a function as it fills, so a value can be sent without
its whole encoding ever being held.

### Choosing the route for every type

A record can generate both instances at once, into its companion, with `derives Cbor.Direct`.
A single import then selects the route for every record in scope. A type which is not a record
is a generic case class or a sealed hierarchy:

```tabular
column  Import
column  For a record
column  For any other type
row  `codecs.direct`  a generated parser and printer  the derived codecs, unless it has a direct instance of its own
row  `codecs.derived`  the derived codecs, even over a direct instance  the derived codecs
row  `codecs.strict`  a generated parser and printer  a compile error naming the type, unless it has a direct instance
```

Under `codecs.strict`, `read[Foo in Cbor]` and `in[Data over Cbor]` never fall back to the
tree. The modes are alternatives, so import one at a time.

### CBOR over HTTP

A `Cbor` value is a request body or a response body, with the media type `application/cbor`,
and a body is parsed back on arrival. A binary API is therefore consumed and offered with the
same code as a [JSON one](json.md#json-over-http). Only the format's type differs.
