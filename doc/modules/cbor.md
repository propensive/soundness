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

A value encodes with `cbor`, and CBOR data decodes to a type with `as` — or in one step, reading
bytes straight to the type:

```scala
case class Person(name: Text, age: Int)

val encoded = Person(t"Ada", 36).cbor       // a Cbor value
encoded.as[Person]                          // Person(t"Ada", 36)

stream.read[Person in Cbor]                 // bytes to Person directly
```

The `@name[Cbor]` annotation renames a field on the wire, and a sealed hierarchy encodes with a
discriminator declared once:

```scala
given (Status is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")
```

### Navigating and updating

With dynamic access enabled, a map's fields read as members, an array indexes, and updates —
including removal, by assigning `Unset` — produce new values:

```scala
import dynamicCborAccess.enabled

val person = Person(t"Ada", 36).cbor
person.name.as[Text]                 // t"Ada"
(person.age = 40).as[Person]         // Person(t"Ada", 40)
```

Deeper updates use a lens, with `Each` and `Filter` optics touching many elements at once, exactly
as for the textual formats.

### Building directly

A CBOR map is assembled from named arguments where no case class fits:

```scala
Cbor.make(name = t"Anna".cbor, age = 30.cbor)
```

### Errors

A malformed document or a failed conversion raises a `CborError` whose reason is specific —
truncated input, an integer overflow, a wrong type, an absent field — so a protocol failure is
diagnosed from the error rather than from a hex dump.
