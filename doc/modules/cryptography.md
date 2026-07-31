## Cryptography

### About

Encryption, signing and message authentication are typed operations whose *choices* — algorithm,
key size, block-cipher mode, padding — are types, checked where the code is written. Encrypting
with AES-256 in CBC mode with PKCS7 padding names all four decisions; a combination the algorithms
do not permit, such as a stream mode with a padding, does not compile; and the weak algorithms —
DES, small RSA keys, unauthenticated modes — require an explicit permission in scope before they
can be used at all.

Keys are typed by their cipher, and a private key's bytes are reachable only inside an `uncloak`
block, so where secret material is used is visible in the code's structure. Where that material
is *stored* while it is not in use is an explicit choice too: a `Cloak` in scope decides between
the heap, off-heap memory, and encrypted-at-rest variants of both.

### On cryptography

Cryptographic APIs traditionally take their parameters as strings —
`Cipher.getInstance("AES/CBC/PKCS5Padding")` — deferring to runtime the discovery that an
algorithm name is misspelled or a combination unsupported. Worse, they are neutral about
strength: DES and AES are equally easy to reach, and nothing marks the code that quietly uses a
broken primitive.

Soundness types the choices and gates the dangerous ones. Modes and paddings are types with their
legal combinations encoded; weak algorithms demand a *permit*, so a codebase's acceptance of
legacy cryptography is one searchable import. Everything comes from the `soundness` package, with
a provider in scope:

```scala
import soundness.*
import strategies.throwUnsafely
import providers.javaStdlibProvider
import cloaks.cloakHeap
```

There is deliberately no default `Cloak`: constructing any secret value — a password, a
symmetric key, a private key — needs one in scope, so the storage decision is always written
down. `cloakHeap` keeps the material in a private byte array; `cloakOffHeap` moves it out of
the Java heap, so it does not appear in a heap dump; `cloakVeiledHeap` and `cloakVeiledOffHeap`
additionally keep it encrypted under an ephemeral key between uses. No cloak defends against a
debugger in the same process; what the stronger ones shrink is the window during which
cleartext is reachable from a heap dump or core file.

### Symmetric encryption

A symmetric key is generated for its cipher — algorithm, key size, and optionally the mode and
padding, all in the type — and encryption happens inside the key's `uncloak` block, with the
[initialization vector](https://en.wikipedia.org/wiki/Initialization_vector) supplied explicitly:

```scala
import blockCipherMode.cbc
import blockCipherPadding.pkcs7

val key = SymmetricKey.generate[Aes[256]]()

key.uncloak:
  t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
// t"Hello world"
```

`uncloak` is a [delimited scope](../philosophy/delimited-scopes.md): it lends the key to its
block as an `Encryptor` and `Decryptor` capability, and capture
checking confines both: neither the capability nor a closure that would use it later can escape
the block, so the compiler — not a convention — keeps the key's use inside its scope.

Streams encrypt chunk by chunk through cipher *ducts*, so large data never assembles in memory,
and a stream encrypted piecewise decrypts as a whole value, or the reverse:

```scala
key.uncloak:
  t"Hello world".in[Data].stream.encrypt(InitializationVector.random).memoize.decrypt.as[Text]
```

Decrypting with the wrong key raises a `CryptoError` naming the failure. AES's mode and padding
may also be fixed in the key's type — `Aes[256] over Cbc against Pkcs7` — and a pairing the
specification forbids does not compile.

### Passwords

A `Password` is a cloaked secret like a key, but with no cipher attached. Its cleartext is
reachable only inside `uncloak`, as a `Cleartext` capability, and its `show` form never reveals
it:

```scala
val password = Password(t"hunter2")
password.uncloak(String(cleartext.chars).tt)   // t"hunter2"
password.show                                  // t"Password(•••)"
```

### Public-key encryption and signing

An RSA key pair encrypts toward the public key and decrypts with the private; a DSA pair signs
with the private key and verifies with the public:

```scala
val privateKey = PrivateKey.generate[Rsa[2048]]()

val message = privateKey.public.uncloak:
  t"secret".encrypt(InitializationVector.random)

privateKey.uncloak(message.decrypt.as[Text])

val signer = PrivateKey.generate[Dsa[2048]]()
val signature = signer.sign(document)
signer.public.verify(document, signature)   // true
```

### HMAC

A message authenticates with `hmac`, over any of the [hash](hashing.md) algorithms, rendered
through the usual [base encodings](base-encoding.md):

```scala
message.hmac[Sha2[256]](secretKey).serialize[Hex]
```

### PEM

Keys travel in [PEM](https://en.wikipedia.org/wiki/Privacy-Enhanced_Mail) form. A public key
exports freely; exporting a *private* key demands the `Divulgence` marker — one more place where
handling secret material is deliberate:

```scala
privateKey.public.pem.serialize
privateKey.pem(Divulgence)
pemText.read[Pem]
```

PEM parses incrementally, so a source holding many blocks — a certificate chain, a bundle of
keys — yields them one at a time, lazily, rather than as one parsed document:

```scala
chainText.read[LazyList[Pem]].map(_.label).to(List)
```

Text that is not part of a block — the `subject=…` lines OpenSSL writes between certificates —
is skipped, and a source with no blocks at all reads as an empty sequence rather than an error.

### ASN.1 and DER

The structures underneath PKIX — certificates, keys, signatures — are
[ASN.1](https://en.wikipedia.org/wiki/ASN.1) values carried in
[DER](https://en.wikipedia.org/wiki/X.690#DER_encoding). An `Asn1` value models that structure
directly, as an enumeration of the universal types PKIX uses, and `Der` is the encoded form:

```scala
val value = Asn1.Sequence(List(Asn1.Integer(BigInt(1)), Asn1.Utf8String(t"hello")))

val bytes = value.in[Der]        // the DER octets
bytes.as[Asn1]                   // the same value again
```

DER is canonical, so a document has exactly one valid encoding, and the codec enforces that in
both directions: an overlong length, a non-minimal integer, an unsorted `SET`, an indefinite
length, or trailing bytes all raise an `Asn1Error` whose reason names the fault and the byte
offset at which it was found.

Decoding is total. A tag this layer does not model — `T61String`, `BMPString`, `ENUMERATED`, an
implicitly tagged value — decodes to `Asn1.Unknown`, carrying its content octets verbatim, so
decoding and re-encoding reproduce the original bytes exactly. That property is what verifying
a signature over a certificate's `TBSCertificate` depends on: a structure can be taken apart,
inspected, and put back together without disturbing the bytes the signature covers.

### Keystores

A [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) keystore is opened for the duration of a
scope, with the password given as a flag — an opaque `Password`, so its cleartext is reached only
through the scoped `Cleartext` capability, and the character array the platform needs is zeroed
once the store is loaded:

```scala
path.open[Keystore](Password(t"sesame")):
  keystore.aliases
  keystore.certificate(t"server")
```

A missing alias is `Unset`. A wrong password and a corrupt store are deliberately
indistinguishable — both are `Unreadable` — since telling them apart would say which of the two
went wrong to someone guessing.

### Weak algorithms

Encrypting with DES, RC2, 1024-bit RSA, ECB mode or any unauthenticated block cipher requires a
permission from the `crypto` package in scope — the same [permit machinery](hashing.md) that gates
MD5 and SHA-1 — so accepting legacy cryptography is an explicit, auditable decision rather than a
default.
