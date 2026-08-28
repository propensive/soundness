## Hashing

### About

A [hash](https://en.wikipedia.org/wiki/Hash_function) reduces data of any size to a short,
fixed-length fingerprint. Soundness computes the digest of any value — a case class, a collection,
a stream of bytes — with the algorithm named as a type: `Sha2[256]`, `Sha1`, `Md5` or `Blake3`.
The non-cryptographic checksums `Crc32`, `Crc64` and `Adler32` live in Corpuscular, and digest
through exactly the same API. The digest is a value whose bytes render to hexadecimal or Base64 through the same
[base encoding](base-encoding.md) used everywhere else.

Anything that describes how to feed its bytes into a hash can be digested, and a case class does so
automatically, derived from its fields — so hashing a structured value needs no manual
serialization. The algorithms known to be weak, MD5 and SHA-1, can be used only where their use is
explicitly permitted.

### On hashing

The usual hashing API takes a byte array and an algorithm named by a string, and hands back another
byte array. The algorithm is unchecked, so a typo is a runtime failure; the input must be serialized
to bytes by hand before hashing; the output must be rendered to text by hand after; and nothing
discourages reaching for a broken algorithm.

Soundness fixes each of these. The algorithm is a type, checked at the call site. Any value with a
`Digestible` instance is hashed directly, and case classes and collections derive theirs, so the
value's own structure is what gets hashed. The result renders through the base encodings. And a weak
algorithm is gated behind a capability that a program must opt into, so `digest[Md5]` does not
compile without a deliberate permission. Everything comes from the `soundness` package, with a
hashing provider and an alphabet in scope:

```scala
import soundness.*
import providers.javaStdlibProvider
import alphabets.hexLowerCase
```

### Hashing a value

`digest` computes the digest of a value in a named algorithm, and `serialize` renders it:

```scala
t"Hello world".digest[Sha2[256]].serialize[Hex]
// the 64-character hexadecimal SHA-256 digest
```

The provider in scope supplies the algorithms — `javaStdlibProvider` for the SHA and MD5 family and
CRC-32, and `soundnessProvider` for the pure-Scala BLAKE3.

The algorithm need not be named where the expected type already says it. A digest carries its
algorithm in its type — `Digest in Sha2[256]` — so a field declared that way fixes what `digest`
computes without the call restating it:

```scala
case class Block(digest: Digest in Sha2[256], payload: Json)

Block(payload.digest, payload)
```

That is the pattern throughout: digests, HMACs, keys and signatures are all bytes parameterized by
the algorithm that produced them, so a value from one algorithm cannot be passed where another is
expected, and the algorithm rarely has to be written twice.

`javaStdlibProvider` names the JDK's `MessageDigest` where there is one. Off the JVM there is
not, so the same import selects pure-Scala implementations of MD5, SHA-1, SHA-2 and CRC-32,
validated byte for byte against the JDK's. Code that hashes therefore reads the same, and
produces the same digests, on the JVM, in a browser and inside a WebAssembly component.

### Hashing your own types

Any value that can be reduced to bytes is digestible, and a case class derives that automatically
from its fields, so a structured value is hashed as one:

```scala
case class Person(name: Text, age: Int)

Person(t"Alice", 30).digest[Sha2[256]]
```

A digest is itself digestible, so a hash can be combined into a larger structure and hashed again.

### Hashing a stream

Data too large to hold is hashed as it passes. The digest accumulates over successive windows,
carrying partial blocks across the boundaries between them, so the result is identical whatever
sizes the chunks happen to arrive in — including chunks of one byte, and windows that begin part
way into a buffer:

```scala
file.stream.digest[Sha2[256]].serialize[Hex]
```

This is what makes a digest computable over a [stream](streams.md) with bounded memory, and it is
why hashing composes with [compression](compression.md) and [encryption](cryptography.md) in one
pipeline rather than requiring the data to be materialized between them.

### BLAKE3

`Blake3` is the pure-Scala implementation, and it offers the algorithm's three modes: an ordinary
hash, a *keyed* hash for message authentication, and key *derivation* from a context string. All
three are checked against the official test vectors, at every input length the vectors cover, so
the implementation is verified rather than merely tested for self-consistency.

Its output is extendable: a BLAKE3 digest may be taken at any length, not only at 256 bits, which
is what makes it usable as a key-derivation function as well as a hash.

### Rendering a digest

A digest's bytes render in any base encoding — hexadecimal for display, Base64 where it must be
compact — and a digest shows as Base64 by default:

```scala
import alphabets.base64Standard

t"Hello world".digest[Sha2[256]].serialize[Base64]
```

### Weak algorithms

MD5 and SHA-1 are broken for security purposes, and remain only for compatibility. Digesting with
one requires a permission in scope, so the choice is visible and deliberate:

```scala
import crypto.permitDisallowedCrypto

t"Hello world".digest[Md5].serialize[Hex]
```

Without such an import, `digest[Md5]` and `digest[Sha1]` do not compile; the strong algorithms need
no permission.

### Checksums of streams

A source of bytes — a file, a download — is checksummed without holding it all in memory, by
digesting its stream as it flows:

```scala
val fingerprint = source.checksum[Sha2[256]]
```
