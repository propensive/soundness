## Hashing

### About

A [hash](https://en.wikipedia.org/wiki/Hash_function) reduces data of any size to a short,
fixed-length fingerprint. Soundness computes the digest of any value — a case class, a collection,
a stream of bytes — with the algorithm named as a type: `Sha2[256]`, `Sha1`, `Md5`, `Crc32`, or
`Blake3`. The digest is a value whose bytes render to hexadecimal or Base64 through the same
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

### Hashing your own types

Any value that can be reduced to bytes is digestible, and a case class derives that automatically
from its fields, so a structured value is hashed as one:

```scala
case class Person(name: Text, age: Int)

Person(t"Alice", 30).digest[Sha2[256]]
```

A digest is itself digestible, so a hash can be combined into a larger structure and hashed again.

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
