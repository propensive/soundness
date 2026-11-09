## Hashing

### About

A [hash](https://en.wikipedia.org/wiki/Hash_function) reduces data of any size to a short
fingerprint of fixed length. The digest of any value can be computed, whether a case class, a
collection or a stream of bytes, with the algorithm named as a type: `Sha2[256]`, `Sha1`, `Md5`
or `Blake3`.

Anything which describes how to feed its bytes into a hash can be digested. A case class does
so automatically, [derived][derivation] from its fields, so hashing a structured value needs no
manual serialization. A digest is a value, and its bytes are written in hexadecimal or Base64
through the same [base encoding](base-encoding.md) used everywhere else.

The checksums `Crc32`, `Crc64` and `Adler32` are computed through exactly the same API. The
algorithms which are known to be weak, MD5 and SHA-1, can be used only where their use is
explicitly permitted. So can the checksums, which are not weak hashes but not hashes at all: a
checksum detects accidental corruption, and nothing more.

### On hashing

The usual hashing API takes a byte array, and an algorithm named by a string, and hands back
another byte array:

- The algorithm is unchecked, so a typo is a failure at [runtime].
- The input must be serialized to bytes by hand before it is hashed.
- The output must be written as text by hand afterwards.
- Nothing discourages reaching for a broken algorithm.

Here each of these is fixed. The algorithm is a type, checked where it is used, so a digest of
one algorithm cannot be confused with another's, which is
[safety by construction](../philosophy/safety-by-construction.md). Any value with a
`Digestible` instance is hashed directly. The result is written through the base encodings. And
a weak algorithm sits behind a permission which a program must opt into.

Everything comes from the `soundness` package. A provider of hash algorithms and an alphabet
are in scope, and the last import lets the samples print their results:

```scala
import soundness.*

import alphabets.hexLowerCase
import codepages.utf8Codepage
import providers.javaBaseProvider
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Hashing a value

`digest` computes the digest of a value, in a named algorithm, and `serialize` writes it out:

```scala fuel
run
##
Out.println(t"Hello world".digest[Sha2[256]].serialize[Hex])
```

The provider in scope supplies the algorithms. These are the algorithms, the permit each
needs, and the providers which offer each on the JVM:

```tabular
column  Algorithm
column  Kind
column  Permit
column  Providers
column  Bits
row  `Sha2[224]`, `Sha2[256]`, `Sha2[384]`, `Sha2[512]`  hash  none  `javaBaseProvider`, `soundnessProvider`  224–512
row  `Blake3`  hash  none  `soundnessProvider`  256
row  `Sha1`  weak hash  `permitDeprecatedCrypto`  `javaBaseProvider`, `soundnessProvider`  160
row  `Md5`  weak hash  `permitDisallowedCrypto`  `javaBaseProvider`, `soundnessProvider`  128
row  `Murmur3`  fast hash  `permitNonCryptographicHashes`  `soundnessProvider`  128
row  `Crc32`  checksum  `permitNonCryptographicHashes`  `javaBaseProvider`, `soundnessProvider`  32
row  `Crc64`  checksum  `permitNonCryptographicHashes`  `soundnessProvider`  64
row  `Adler32`  checksum  `permitNonCryptographicHashes`  `javaBaseProvider`, `soundnessProvider`  32
```

The permits are imported from the package `cryptoPermits`.

`javaBaseProvider` uses the JDK's `MessageDigest` where there is one. Off the JVM there is
not, and the same import then selects pure-Scala implementations of MD5, SHA-1, SHA-2 and
CRC-32, which are validated byte for byte against the JDK's. Code which hashes therefore reads
the same, and produces the same digests, on the JVM, in a browser and inside a WebAssembly
component.

### The algorithm in the type

A digest has its algorithm in its type, as in `Digest in Sha2[256]`. Where the expected type
already says which algorithm is wanted, `digest` need not name it. A field declared with the
type fixes what `digest` computes:

```scala
case class Block(digest: Digest in Sha2[256], payload: Text)

val payload = t"""{"amount": 100}"""
val block = Block(payload.digest, payload)
```

```scala fuel
run
##
Out.println(block.digest.serialize[Hex])
```

That is the pattern throughout. Digests, HMACs, keys and signatures are all bytes with a type
parameter for the algorithm which produced them. A value from one algorithm cannot be passed
where another is expected, and the algorithm rarely has to be written twice:

```scala fuel
compile
errors show
##
Block(payload.digest[Sha2[512]], payload)
```

[Cryptography](cryptography.md) covers HMACs, keys and signatures.

### Hashing your own types

Any value which can be reduced to bytes is digestible. A case class derives that from its
fields, so a structured value is hashed as one:

```scala
case class Person(name: Text, age: Int)
```

```scala fuel
run
##
Out.println(Person(t"Alice", 30).digest[Sha2[256]].serialize[Hex])
Out.println(Person(t"Alice", 31).digest[Sha2[256]].serialize[Hex])
```

A digest is itself digestible, so a hash can be combined into a larger structure, and hashed
again.

### Hashing a stream

Data which is too large to hold is hashed as it passes. The digest accumulates over successive
chunks, and carries partial blocks across the boundaries between them. The result is therefore
identical whatever sizes the chunks happen to arrive in, and is the digest of the whole text
above:

```scala
val chunks = Chain(t"Hello ".in[Data], t"wor".in[Data], t"ld".in[Data])
```

```scala fuel
run
##
Out.println(chunks.checksum[Sha2[256]].serialize[Hex])
```

This is what makes a digest computable over a [stream](streams.md) with bounded memory. It is
why hashing composes with [compression](compression.md) and
[encryption](cryptography.md) in one pipeline, without the data being held between them.

`checksum` digests what is *behind* a value: a stream's bytes and, for a
[filesystem](filesystem.md) path, the content of a file or of a directory. A directory's
checksum covers its regular files, in the order of their paths relative to it, and each
contributes its relative path and its content. So:

- a rename, a move or an edit changes the checksum;
- two directories which hold the same files have the same checksum, wherever they are;
- an empty directory has a checksum too.

That is the identity a build keys its caches by:

```scala fuel
typecheck
##
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.dereferenceSymlinks
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val directory = temporaryDirectory[Path on Linux] / "hashing"
directory.create[Directory](CreateFlag.Replace)

val identity: Digest in Sha2[256] = directory.checksum[Sha2[256]]
```

### Rendering a digest

A digest's bytes are written in any base encoding: hexadecimal for display, or Base64 where
they must be compact:

```scala fuel
run
##
locally:
  import alphabets.base64Standard
  Out.println(t"Hello world".digest[Sha2[256]].serialize[Base64])
```

### Self-describing digests

A digest's bytes say nothing about the algorithm which produced them. A digest which is stored
or sent alone can be checked only by something which already knows how it was made. A
[Multihash](https://multiformats.io/multihash/) prefixes the digest with a code for its
algorithm and its length, so it describes itself. Content-addressed storage, and the
distributed protocols built on it, exchange multihashes:

```scala fuel
run
##
Out.println(Multihash(t"Hello world".digest[Sha2[256]]).serialize.serialize[Hex])
```

The two bytes `12` and `20` say that a SHA-256 digest of thirty-two bytes follows. The
algorithm's code comes from the digest's type, so a multihash cannot claim an algorithm other
than the one which computed it.

### Weak algorithms

MD5 and SHA-1 are broken for the purposes of security, and remain only for compatibility.
Without a permission in scope, neither compiles:

```scala fuel
compile
errors show
##
t"Hello world".digest[Md5]
```

Importing the permission makes the choice visible and deliberate:

```scala fuel
run
##
locally:
  import cryptoPermits.permitDisallowedCrypto
  Out.println(t"Hello world".digest[Md5].serialize[Hex])
```

The strong algorithms need no permission.

### Checksums

`Crc32`, `Crc64` and `Adler32` are not hashes but checksums. They detect accidental
corruption, and defend against nothing. They sit behind a permit of their own, which concedes
only that no adversary is being defended against:

```scala
import cryptoPermits.permitNonCryptographicHashes
```

```scala fuel
run
##
Out.println(t"123456789".digest[Crc32].serialize[Hex])
```

A source of bytes, such as a file or a download, is checksummed without being held in memory,
by digesting its stream as it flows, exactly as `checksum` did above.

### Fast hashing

`Murmur3` is MurmurHash3 in its 128-bit form. It is not a checksum, but it is not
cryptographic either. It is a hash made for hash tables, [Bloom filters](bloom-filters.md) and
sharding, where the input is not adversarial, and the rounds of a cryptographic digest would
be several times the work for no benefit. It sits behind the same permit as the checksums, and
its output is byte for byte what Guava's `murmur3_128()` produces:

```scala fuel
run
##
locally:
  import providers.soundnessProvider
  Out.println(t"hello".digest[Murmur3].serialize[Hex])
```

### BLAKE3

`Blake3` is implemented in pure Scala, by `soundnessProvider`, and offers the algorithm's three
modes:

```tabular
column  Mode
column  Input
column  Use
row  hash  the data  an ordinary digest
row  keyed hash  a key and the data  message authentication
row  key derivation  a context string and key material  deriving keys
```

All three are checked against the official test data, at every input length which that data
covers, so the implementation is verified, and not merely tested for consistency with itself.

Its output is extendable. A BLAKE3 digest may be taken at any length, not only at 256 bits,
which is what makes it usable for deriving keys as well as for hashing.
