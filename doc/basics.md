All Gastronomy terms and types are defined in the `gastronomy` package:
```scala
import gastronomy.*
```

__Gastronomy__ provides representations of public, private and symmetric keys which offer a number
of cryptographic methods:

- `PublicKey` provides:
  - `verify` for verifying signatures
  - `encrypt` for encrypting data
  - `pem` to provide the public key as a PEM-encoded string
- `PrivateKey` provides:
  - `sign` for signing data
  - `decrypt` for decrypting encrypted data
  - `public` to derive a `PublicKey` from the `PrivateKey`
  - `pem` to provide the private key as a PEM-encoded string
- `SymmetricKey` provides `verify`, `encrypt`, `pem`, `sign` and `decrypt` in a single key.

Additionally, the extension methods, `digest` and `hmac` are provided for any value which can be
serialized to bytes.

The objects `PrivateKey` and `SymmetricKey` both have `generate` methods which will generate new
random keys.

### Signing

Given, for example, a `PrivateKey[Dsa[1024]]` instance, `key`, data may be signed with, for example,
```scala
val key: PrivateKey[Dsa[1024]] = ???
val signature: Signature[Dsa[1024]] = key.sign(data)
```
This works for any value, `data`, that has an appropriate `ByteCodec` instance. The type parameter
of the signature will depend on the type parameter of the private key.

### Verifying a signature

A public key, `pubKey`, which could, for example, be derived from the private key in the previous
example,
```scala
val pubKey = key.public
```
may be used to verify a signature of type `Signature[Dsa[1024]]` with:
```scala
val valid: Boolean = pubKey.verify(data, signature)
```

Here, `data` must be the same object that was used (with the private key) to produce the signature,
and may be any type that has a contextual `ByteCodec` instance.

### Encryption

A public key instance, for example, `pubKey` of type `PublicKey[Rsa[2048]]`, can encrypt some data
by calling,
```scala
val encrypted: Message[Rsa[2048]] = pubKey.encrypt(data)
```

### Decryption

An encrypted message may conversely be decrypted using the corresponding `PrivateKey[Rsa[2048]]`
instance, `key`:
```scala
val data: String = key.decrypt[String](encrypted)
```

The return type (`String` in the above example) must be specified as a parameter to the `decrypt`
method, and may be any type for which a corresponding `ByteCodec` exists in context. However, the
type should be the same as the type of the object that was originally encrypted, otherwise it may
fail to decode.

### Digests

A cryptographic digest (or hash) of any value may be calculated by calling `digest[A]` on that
value, for an appropriate choice of `A`, provided a `Hashable` instance is in context for that type
of object. `Hashable` instances exist for `String`s, primitive types, sequences of these types, and
product and coproduct types consisting of just other hashable types.

Cryptographic digests have the type `Digest[A]` where `A` is the algorithm type.

For example,
```scala
val digest: Digest[Sha2[384]] = (10, "alpha", 'z').digest[Sha2[384]]
```

### HMACs

Any value whose type has a corresponding `ByteCodec` instance in context may have an HMAC value
calculated, of type `Hmac[A]` (where `A` is the cryptographic algorithm). As a parameter, this
needs an `IArray[Byte]` representing (in some form) the key to be used.

Here is an example using SHA-512:
```scala
val hmac: Hmac[Sha2[512]] = "Hello world".hmac("secret".bytes)
```

### Type inference

Whenever an expression is used in a position with an expected type, the type parameters of the
methods `decrypt`, `digest` and `hmac` may be omitted, for example given the case class,
```scala
case class Block(digest: Digest[Sha2[256]], json: Json, hmac: Hmac[Sha2[512]])
```
we can instantiate it with just,
```scala
val block = Block(data.digest, data.decrypt, value.hmac)
```

Alternatively, a particular given may be imported directly into the current scope to prioritize it,
such that it may be used in preference to the alternatives.

### Byte data

Representations of binary data are common with low-level cryptographic operations. All operations in
Gastronomy use the immutable `IArray[Byte]` type as the underlying representation of binary data,
but typically wrap the data in a type which more precisely indicates the content of that data.

These types include the key types, `PublicKey`, `PrivateKey` and `SymmetricKey`, and result types,
`Signature`, `Hmac`, `Digest` and `Message`.

These types are all further refined with a type parameter representing the cryptographic algorithm
associated with that data. For example, an MD5 digest is typed as, `Digest[Md5]` and a 384-bit SHA-2
HMAC has the type, `Hmac[Sha2[384]]`.

In order to make it easier to share these values, they can be encoded to and from `String`s using
a number of different encodings:
- binary (`Binary`)
- hexadecimal (`Hex`)
- BASE-64 (`Base64`)
- URL-safe BASE-64 (`Base64Url`)

The `encode` method, which exists as an extension on `IArray[Byte]`, as well as (directly) on all
types representing byte data. It takes one of these as a type parameter to produce a `String` of
that data, encoded with the specified encoding.

#### Algorithms

Gastronomy's cryptographic functions are implemented through different algorithms, which are
represented by types. Their names follow the conventions of other Scala types, hence:
- [`Rsa[B]`](https://en.wikipedia.org/wiki/RSA_(cryptosystem)) for `B` of `1024` or `2048`,
- [`Dsa[B]`](https://en.wikipedia.org/wiki/RSA_(cryptosystem)) for `B` of `512`, `1024`, `2048` or
  `3072`,
- [`Aes[B]`](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) for `B` of `128`, `192` or
  `256`,
- [`Sha1`](https://en.wikipedia.org/wiki/SHA-1),
- [`Sha2[B]`](https://en.wikipedia.org/wiki/SHA-2) for `B` of `224`, `256`, `384` or `512`, and
- [`Md5`](https://en.wikipedia.org/wiki/MD5)

Additionally, the types `Base64`, `Base64Url`, `Hex` and `Binary` represent non-cryptographic
byte-to-string encodings.

### Generating keys

The `PrivateKey` object provides the `generate[A]()` method, where `A` is `Rsa[B]`, `Dsa[B]` or
`Aes[B]` for an appropriate choice of `B`.

The algorithm `Aes[B]` can also be used with the `SymmetricKey` object to get a symmetric key which
has the functionality of both a public and private key.

### Byte codecs

Any object which can be serialized to bytes may be digested, signed, verified, HMACked or encrypted,
and can be returned from a decryption operation, provided a corresponding `ByteCodec` instance is
available for that type.

`ByteCodec`s are provided for `IArray[Byte]` (trivially) and for `String`s (assuming a UTF-8
encoding).

### PEM encoding

The _Privacy-Enhanced Mail_ format is commonly used for exchanging cryptographic values safely in
ASCII-only environments.

A `Pem` type is provided for reading, writing and representing this data. The case class `Pem` has
two fields: `kind`, which is the label that appears after the words `BEGIN` and `END` in the
serialized format, and `data`, which is an `IArray[Byte]` of the byte data.

The `serialize` method will produce a `String` of the data, properly encoded as BASE-64, and
delimited.

The method `Pem.parse` will attempt to parse a `String` containing PEM-encoded data.

All Gastronomy's key types offer a `pem` method which will return an appropriately-labelled `Pem`
value containing that key, however to avoid the risk of accidentally exposing a private key, the
`pem` method of `PrivateKey` must be called with a special singleton value, like so:

```scala
privateKey.pem(RevealSecretKey)
```

### Other Cryptographic Algorithms

Gastronomy may be easily extended to support other cryptographic algorithms. The existing
implementations of `Rsa`, `Dsa`, `Aes`, `Sha1`, `Sha2` and `Md5` should be studied to investigate
this possibility.


