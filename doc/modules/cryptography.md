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
import soundness.textDecodable

import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.skipSanitizer
import cloaks.heapCloak
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import strategies.throwUnsafely
```

There is deliberately no default `Cloak`: constructing any secret value — a password, a
symmetric key, a private key — needs one in scope, so the storage decision is always written
down. `heapCloak` keeps the material in a private byte array; `offHeapCloak` moves it out of
the Java heap, so it does not appear in a heap dump; `veiledHeapCloak` and `veiledOffHeapCloak`
additionally keep it encrypted under an ephemeral key between uses. No cloak defends against a
debugger in the same process; what the stronger ones shrink is the window during which
cleartext is reachable from a heap dump or core file.

### Symmetric encryption

A symmetric key is generated for its cipher — algorithm, key size, and optionally the mode and
padding, all in the type — and encryption happens inside the key's `uncloak` block, with the
[initialization vector](https://en.wikipedia.org/wiki/Initialization_vector) supplied explicitly:

```scala
import blockCipherModes.cbc
import blockCipherPaddings.pkcs7
import cryptoPermits.permitUnauthenticatedCrypto

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

Decrypting with the wrong key raises a `Crypto.Error` naming the failure. AES's mode and padding
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

An RSA key pair encrypts toward the public key and decrypts with the private:

```scala
val privateKey = PrivateKey.generate[Rsa[2048]]()

val message = privateKey.public.uncloak:
  t"secret".encrypt(InitializationVector.random)

privateKey.uncloak(message.decrypt.as[Text])   // t"secret"
```

Signing runs the other way: the private key signs, and anyone holding the public key verifies.
RSA, DSA, ECDSA and ML-DSA — the lattice-based scheme standardized as FIPS 204, for a
post-quantum signature — all sign the same way; the digest an RSA or ECDSA signature is computed
over is chosen from the `signatureDigests` package:

```scala
import signatureDigests.sha256Signature

val document = t"an important document"

val signer = PrivateKey.generate[Ecdsa[256]]()
val signature = signer.sign(document)

signer.public.verify(document, signature)             // true
signer.public.verify(t"a forged document", signature) // false
```

### HMAC

A message authenticates with `hmac` under a shared secret, over any of the [hash](hashing.md)
algorithms, rendered through the usual [base encodings](base-encoding.md):

```scala
import alphabets.hexLowerCase

val secret = t"shared secret".in[Data]
document.hmac[Sha2[256]](secret).serialize[Hex]
```

### PEM

Keys travel in [PEM](https://en.wikipedia.org/wiki/Privacy-Enhanced_Mail) form. A public key
exports freely; exporting a *private* key demands the `Divulgence` marker — one more place where
handling secret material is deliberate. A `Pem` value carries its label and its bytes, and
serializes to the familiar armored text:

```scala
val pemText = privateKey.public.pem.serialize   // t"-----BEGIN PUBLIC KEY-----…"
privateKey.pem(Divulgence)

pemText.read[Pem].label   // Pem.Label.PublicKey
```

PEM parses incrementally, so a source holding many blocks — a certificate chain, a bundle of
keys — yields them one at a time, lazily, rather than as one parsed document:

```scala
val chainText = t"$pemText\n$pemText"
chainText.read[Chain[Pem]].map(_.label).to[List]   // two public-key labels
```

Text that is not part of a block — the `subject=…` lines OpenSSL writes between certificates —
is skipped, and a source with no blocks at all reads as an empty sequence rather than an error.

### Reading keys

A key that already exists — in `~/.ssh`, or written by `openssl` — reads into a `PrivateKey` of
the cipher you name, from any of the forms a key file takes: PKCS#8 (`PRIVATE KEY`), PKCS#1 (`RSA
PRIVATE KEY`), SEC1 (`EC PRIVATE KEY`) or OpenSSH's own container (`OPENSSH PRIVATE KEY`). A
`Cloak` must be in scope, as for any secret:

```scala
val keyText = t"""-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUgAAAJB+g2KdfoNi
nQAAAAtzc2gtZWQyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUg
AAAEDTzzy05nIC9r9aKtXYNJMgY1F/g5ZiPNyksGUvj8yR6xGH6R8uL9Aww7+jnLISnQu3
kiglJKYyOcKlvCJ7v+xSAAAADHRlc3RAZXhhbXBsZQE=
-----END OPENSSH PRIVATE KEY-----"""

val sshKey = keyText.read[PrivateKey[Ed25519] in Pem]
sshKey.public.verify(t"message", sshKey.sign(t"message"))   // true
```

The type is checked against the document, not assumed from it: the algorithm identifier, the
curve and, for RSA, the modulus size must be those of the cipher named, so a 4096-bit key can
never be read as a `PrivateKey[Rsa[2048]]`. A key that does not match is a `PrivateKey.Error`
whose `Mismatch` reason names what was found:

<!-- doccheck: skip -->
```scala
keyText.read[PrivateKey[Rsa[2048]] in Pem]   // raises PrivateKey.Error: a RSA-2048 key was expected but Ed25519 was found
```

A passphrase-protected key is reported as `Encrypted`; reading one is not yet supported. Public
keys read the same way from `PUBLIC KEY` blocks, and from the single-line form OpenSSH keeps in
`id_*.pub`, `authorized_keys` and `known_hosts`, which `OpenSsh.line` writes back:

```scala
val line = t"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS test@example"
val sshPublicKey = OpenSsh.publicKey[Ed25519](line)
OpenSsh.line(sshPublicKey, t"test@example") == line   // true
```

### Key agreement and authenticated encryption

Two parties who hold each other's public keys can agree on a shared secret with X25519 or ECDH;
the secret is lent to a block and zeroed afterwards, since a protocol only ever hashes it into
session keys:

```scala
val alice = PrivateKey.generate[X25519]()
val bob = PrivateKey.generate[X25519]()
alice.agree(bob.public) { secret => secret.length }   // 32
```

What the key agreement produces is then used with an authenticated cipher. AES-GCM seals a
message under a nonce, which must never repeat under one key, together with associated data that
is authenticated but not encrypted, and refuses to open anything that has been tampered with:

```scala
val sessionKey = SymmetricKey.generate[AesGcm[256]]()
val nonce: Data = Array.fill[Byte](12)(0)
val box = sessionKey.seal(nonce, t"header".bytestream, t"message".bytestream)
sessionKey.unseal(nonce, t"header".bytestream, box).read[Text]   // t"message"
```

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
length, or trailing bytes all raise an `Asn1.Error` whose reason names the fault and the byte
offset at which it was found.

Decoding is total. A tag this layer does not model — `T61String`, `BMPString`, `ENUMERATED`, an
implicitly tagged value — decodes to `Asn1.Unknown`, carrying its content octets verbatim, so
decoding and re-encoding reproduce the original bytes exactly. That property is what verifying
a signature over a certificate's `TBSCertificate` depends on: a structure can be taken apart,
inspected, and put back together without disturbing the bytes the signature covers.

### Certificates

An [X.509](https://en.wikipedia.org/wiki/X.509) certificate binds a public key to a
*distinguished name* for a period of validity, signed by its issuer. A self-signed certificate —
the kind a development server or a private certificate authority needs — is generated from a
subject, a private key, a validity [period](time.md) and a serial number, with the
`dNSName` alternatives a TLS client will check:

```scala
import chronometries.unixChronometry

val subject = Distinguished(commonName = t"example.com", organization = t"Example", country = t"GB")
val validity = Instant(1_700_000_000_000L) ~ Instant(1_900_000_000_000L)

val certificate =
  Certificate.selfSigned(subject, privateKey, validity, BigInt(1), alternatives = List(t"example.com"))

certificate.pem.serialize   // t"-----BEGIN CERTIFICATE-----…"
```

A certificate is an `Asn1` value underneath, so its fields are readable through the same
structure, and `authority = true` marks it as one that may sign others. `verify` checks the
signature against the issuer's public key — the certificate's own, when it is self-signed.

An ML-DSA key signs a certificate the same way, with the FIPS 204 algorithm identifiers. But a
TLS stack may not be able to handshake with a post-quantum key — the JDK's cannot — so a service
whose identity is an ML-DSA key instead has that key *issue* a certificate over a classical key
it can present, and presents it together with the ML-DSA key's own self-signed certificate. A
peer that knows the ML-DSA key verifies the leaf against it:

```scala
val swarmKey = PrivateKey.generate[MlDsa[65]]()
val tlsKey = PrivateKey.generate[Ecdsa[256]]()
val identity = Distinguished(commonName = t"swarm.example.com")

val root = Certificate.selfSigned(identity, swarmKey, validity, BigInt(1), authority = true)

val leaf =
  Certificate.issued(subject, tlsKey.public, identity, swarmKey, validity, BigInt(2), alternatives = List(t"example.com"))

leaf.verify(swarmKey.public)   // true
root.verify(swarmKey.public)   // true
```

An issued certificate carries an authority key identifier naming the key that signed it, which
is how a verifier finds the root in a chain.

### Keystores

A [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) keystore is opened for the duration of a
scope, with the password given as a flag — an opaque `Password`, so its cleartext is reached only
through the scoped `Cleartext` capability, and the character array the platform needs is zeroed
once the store is loaded. Inside the scope, `keystore` is the open store:

```scala
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val storePath = temporaryDirectory[Path on Linux] / "cryptography" / "store.p12"

def readAliases(): List[Text] =
  storePath.open[Keystore](Password(t"sesame")):
    keystore.certificate(t"server")   // Unset, unless the store holds that alias
    keystore.aliases
```

A missing alias is `Unset`. A wrong password and a corrupt store are deliberately
indistinguishable — both are `Unreadable` in the `Keystore.Error` — since telling them apart
would say which of the two went wrong to someone guessing.

### Weak algorithms

Encrypting with DES, RC2, 1024-bit RSA, ECB mode or any unauthenticated block cipher requires a
permission from the `cryptoPermits` package in scope — the same [permit machinery](hashing.md)
that gates MD5 and SHA-1 — so accepting legacy cryptography is an explicit, auditable decision
rather than a default.
