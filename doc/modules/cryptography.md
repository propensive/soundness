## Cryptography

### About

Encryption, signing and message authentication are typed operations. Their *choices* are
types too: the algorithm, the size of the key, the mode of a block cipher and its padding are
all checked where the code is written. Encrypting with AES-256, in CBC mode, with PKCS7
padding, names all four decisions. A combination which the algorithms do not permit, such as a
stream mode with a padding, does not compile. The weak algorithms, which are DES, small RSA
keys and unauthenticated modes, need an explicit permission in scope before they can be used
at all.

A key is typed by its cipher. The bytes of a private key can be reached only inside an
`uncloak` block, so the places where secret material is used are visible in the structure of
the code. Where that material is *stored*, while it is not in use, is an explicit choice as
well.

### On cryptography

Cryptographic APIs traditionally take their parameters as strings, as in
`Cipher.getInstance("AES/CBC/PKCS5Padding")`. A misspelled algorithm, or an unsupported
combination, is then discovered only at [runtime]. Worse, such an API is neutral about
strength. DES and AES are equally easy to reach, and nothing marks the code which quietly uses
a broken primitive.

Here the choices are typed, and the dangerous ones are gated:

- Modes and paddings are types, and only their legal combinations compile.
- A weak algorithm demands a *permit*, so a codebase's acceptance of legacy cryptography is
  one import, which can be searched for.
- Secret material is a [capability], lent to a block.

Everything comes from the `soundness` package. A provider of algorithms is in scope, and the
last import lets the samples print their results:

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
import alphabets.hexLowerCase
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Where secrets are kept

There is deliberately no default `Cloak`. Constructing any secret value, whether a password, a
symmetric key or a private key, needs one in scope, so the decision about storage is always
written down. The package `cloaks` offers four. They differ in where the material rests
between uses, and in whether it rests encrypted:

```tabular
column  Cloak
column  At rest, the secret is
column  Ephemeral key
row  `heapCloak`  cleartext, in a private byte array on the heap
row  `offHeapCloak`  cleartext, outside the Java heap
row  `veiledHeapCloak`  ciphertext, on the heap  off the heap
row  `veiledOffHeapCloak`  ciphertext, outside the Java heap  on the heap
```

Only `heapCloak` is available on Scala.js and Scala Native. No cloak defends against a
debugger in the same process. What the stronger ones shrink is the window during which
cleartext can be reached from a heap dump or a core file.

### Symmetric encryption

A symmetric key is generated for its cipher. The algorithm and the size of the key are in the
type, and so, optionally, are the mode and the padding. Encryption happens inside the key's
`uncloak` block, and the
[initialization vector](https://en.wikipedia.org/wiki/Initialization_vector) is supplied
explicitly:

```scala
import blockCipherModes.cbc
import blockCipherPaddings.pkcs7
import cryptoPermits.permitUnauthenticatedCrypto

val key = SymmetricKey.generate[Aes[256]]()

val decrypted = key.uncloak:
  t"Hello world".encrypt(InitializationVector.random).decrypt.as[Text]
```

```scala fuel
run
##
Out.println(decrypted)
```

`uncloak` is a [delimited scope](../philosophy/delimited-scopes.md). It lends the key to its
block, as an `Encryptor` and a `Decryptor` capability, and capture checking confines both.
Neither the capability, nor a closure which would use it later, can escape the block. The
compiler, not a convention, keeps the use of the key inside its scope. Outside the block there
is nothing to encrypt with:

```scala fuel
compile
errors show
##
t"Hello world".encrypt(InitializationVector.random)
```

A [stream](streams.md) is encrypted a chunk at a time, so large data is never assembled in
memory. A stream which was encrypted in pieces decrypts as a whole value, and the reverse:

```scala
val streamed = key.uncloak:
  t"Hello world".in[Data].stream.encrypt(InitializationVector.random).memoize.decrypt.as[Text]
```

Decrypting with the wrong key raises a `Crypto.Error` which names the failure. The mode and
padding of AES may also be fixed in the key's type, as in `Aes[256] over Cbc against Pkcs7`,
and a pairing which the specification forbids does not compile.

### Passwords

A `Password` is a cloaked secret, like a key, but with no cipher attached. Its cleartext can
be reached only inside `uncloak`, as a `Cleartext` capability, and its `show` form never
reveals it:

```scala
val password = Password(t"hunter2")
```

```scala fuel
run
##
Out.println(password.show)
Out.println(password.uncloak(String(cleartext.chars).tt))
```

### Public-key encryption

An RSA key pair encrypts toward the public key, and decrypts with the private key:

```scala
val privateKey = PrivateKey.generate[Rsa[2048]]()

val message = privateKey.public.uncloak:
  t"secret".encrypt(InitializationVector.random)
```

```scala fuel
run
##
Out.println(privateKey.uncloak(message.decrypt.as[Text]))
```

### Signing

Signing runs the other way. The private key signs, and anyone who holds the public key
verifies. RSA, DSA, ECDSA and ML-DSA all sign in the same way. ML-DSA is the lattice-based
scheme standardized as FIPS 204, for a signature which resists quantum computers. The digest
which an RSA or ECDSA signature is computed over is chosen from the package
`signatureDigests`:

```scala
import signatureDigests.sha256Signature

val document = t"an important document"

val signer = PrivateKey.generate[Ecdsa[256]]()
val signature = signer.sign(document)
```

```scala fuel
run
##
Out.println(signer.public.verify(document, signature).inspect)
Out.println(signer.public.verify(t"a forged document", signature).inspect)
```

### HMAC

`hmac` authenticates a message under a shared secret, over any of the
[hash algorithms](hashing.md#hashing-a-value). The result is written through the usual
[base encodings](base-encoding.md):

```scala
val secret = t"shared secret".in[Data]
```

```scala fuel
run
##
Out.println(document.hmac[Sha2[256]](secret).serialize[Hex])
```

### Key agreement and authenticated encryption

Two parties who hold each other's public keys can agree on a shared secret, with X25519 or
ECDH. The secret is lent to a block, and zeroed afterwards, since a protocol only ever hashes
it into session keys:

```scala
val alice = PrivateKey.generate[X25519]()
val bob = PrivateKey.generate[X25519]()
```

```scala fuel
run
##
Out.println(alice.agree(bob.public) { secret => secret.length }.show)
```

What the key agreement produces is then used with an authenticated cipher. AES-GCM seals a
message under a *nonce*, which must never repeat under one key, together with associated data
which is authenticated but not encrypted. It refuses to open anything which has been tampered
with:

```scala
val sessionKey = SymmetricKey.generate[AesGcm[256]]()
val nonce: Data = Array.fill[Byte](12)(0)
val box = sessionKey.seal(nonce, t"header".bytestream, t"message".bytestream)
```

```scala fuel
run
##
Out.println(sessionKey.unseal(nonce, t"header".bytestream, box).read[Text])
```

```scala fuel
run
errors show
##
sessionKey.unseal(nonce, t"another header".bytestream, box).read[Text]
```

### PEM

Keys travel in [PEM](https://en.wikipedia.org/wiki/Privacy-Enhanced_Mail) form. A public key
is exported freely. Exporting a *private* key demands the marker `Divulgence`, which is one
more place where handling secret material is deliberate. A `Pem` value has a label and its
bytes, and serializes to the familiar armored text:

```scala
val pemText = privateKey.public.pem.serialize
val privatePem = privateKey.pem(Divulgence)
```

```scala fuel
run
##
Out.println(pemText.cut(t"\n").head)
Out.println(pemText.read[Pem].label.inspect)
```

PEM is parsed incrementally. A source which holds many blocks, such as a certificate chain or
a bundle of keys, gives them one at a time, lazily, and not as one parsed document:

```scala
val chainText = t"$pemText\n$pemText"
```

```scala fuel
run
##
Out.println(chainText.read[Chain[Pem]].map(_.label).to[List].inspect)
```

Text which is not part of a block, such as the `subject=…` lines which OpenSSL writes between
certificates, is skipped. A source with no blocks at all reads as an empty sequence, not as an
error.

### Reading keys

A key which already exists, in `~/.ssh` or written by `openssl`, is read into a `PrivateKey`
of the cipher named, from any of the forms a key file takes:

```tabular
column  Form
column  PEM label
column  Label value
row  PKCS#8  `PRIVATE KEY`  `Pem.Label.PrivateKey`
row  PKCS#1  `RSA PRIVATE KEY`  `Pem.Label.RsaPrivateKey`
row  SEC1  `EC PRIVATE KEY`  `Pem.Label.EcPrivateKey`
row  OpenSSH's own container  `OPENSSH PRIVATE KEY`  `Pem.Label.OpensshPrivateKey`
```

A `Cloak` must be in scope, as for any secret:

```scala fuel
substitute b3Blbn..bXBsZQE=  ...
##
val keyText = t"""-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUgAAAJB+g2KdfoNi
nQAAAAtzc2gtZWQyNTUxOQAAACARh+kfLi/QMMO/o5yyEp0Lt5IoJSSmMjnCpbwie7/sUg
AAAEDTzzy05nIC9r9aKtXYNJMgY1F/g5ZiPNyksGUvj8yR6xGH6R8uL9Aww7+jnLISnQu3
kiglJKYyOcKlvCJ7v+xSAAAADHRlc3RAZXhhbXBsZQE=
-----END OPENSSH PRIVATE KEY-----"""

val sshKey = keyText.read[PrivateKey[Ed25519] in Pem]
```

```scala fuel
run
##
Out.println(sshKey.public.verify(t"message", sshKey.sign(t"message")).inspect)
```

The type is checked against the document, and is not assumed from it. The algorithm's
identifier, the curve and, for RSA, the size of the modulus must be those of the cipher named,
so a 4096-bit key can never be read as a `PrivateKey[Rsa[2048]]`. A key which does not match
is a `PrivateKey.Error`, whose reason, `Mismatch`, names what was found:

```scala fuel
run
errors show
##
keyText.read[PrivateKey[Rsa[2048]] in Pem]
```

A key which is protected by a passphrase is reported as `Encrypted`. Reading one is not yet
supported.

Public keys are read in the same way from `PUBLIC KEY` blocks. They are also read from the
single-line form which OpenSSH keeps in `id_*.pub`, `authorized_keys` and `known_hosts`, and
`OpenSsh.line` writes that form back:

```scala fuel
substitute AAAAC3..v+xS  ...
##
val line = t"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS test@example"
val sshPublicKey = OpenSsh.publicKey[Ed25519](line)
```

```scala fuel
run
##
Out.println((OpenSsh.line(sshPublicKey, t"test@example") == line).inspect)
```

[SSH](ssh.md) uses these keys to authenticate a connection.

### ASN.1 and DER

The structures beneath certificates, keys and signatures are
[ASN.1](https://en.wikipedia.org/wiki/ASN.1) values, carried in
[DER](https://en.wikipedia.org/wiki/X.690#DER_encoding). An `Asn1` value models that structure
directly, as an enumeration of the universal types which certificates use, and `Der` is the
encoded form:

```scala
val value = Asn1.Sequence(List(Asn1.Integer(BigInt(1)), Asn1.Utf8String(t"hello")))

val bytes = value.in[Der]
```

```scala fuel
run
##
Out.println((bytes.as[Asn1] == value).inspect)
```

DER is canonical, so a document has exactly one valid encoding, and the codec enforces that in
both directions. Each departure from it raises an `Asn1.Error`, whose reason names the fault,
and the offset of the byte at which it was found. Among them:

```tabular
column  Fault
column  Reason
row  an overlong length  `NonMinimalLength`
row  a non-minimal integer  `NonMinimalInteger`
row  an unsorted `SET`  `UnsortedSet`
row  an indefinite length  `IndefiniteLength`
row  trailing bytes  `Trailing`
```

Decoding never fails for want of a type. A tag which this layer does not model, such as
`T61String`, `BMPString`, `ENUMERATED` or an implicitly tagged value, decodes to
`Asn1.Unknown`, which holds its content bytes verbatim. Decoding and encoding again therefore
reproduce the original bytes exactly. Verifying a signature over a certificate's
`TBSCertificate` depends on that property: a structure can be taken apart, inspected and put
back together without disturbing the bytes which the signature covers.

### Certificates

An [X.509](https://en.wikipedia.org/wiki/X.509) certificate binds a public key to a
*distinguished name*, for a period of validity, and is signed by its issuer. A self-signed
certificate is the kind which a development server, or a private certificate authority,
needs. It is generated from a subject, a private key, a [period](time.md) of validity and a
serial number, with the `dNSName` alternatives which a TLS client will check:

```scala
import chronometries.unixChronometry

val subject = Distinguished(commonName = t"example.com", organization = t"Example", country = t"GB")
val validity = Instant(1_700_000_000_000L) ~ Instant(1_900_000_000_000L)

val certificate =
  Certificate.selfSigned(subject, privateKey, validity, BigInt(1), alternatives = List(t"example.com"))
```

```scala fuel
run
##
Out.println(certificate.pem.serialize.cut(t"\n").head)
Out.println(certificate.verify(privateKey.public).inspect)
```

A certificate is an `Asn1` value underneath, so its fields can be read through the same
structure. `authority = true` marks a certificate as one which may sign others. `verify`
checks the signature against the issuer's public key, which for a self-signed certificate is
its own.

An ML-DSA key signs a certificate in the same way, with the algorithm identifiers of
FIPS 204. But a TLS implementation may not be able to complete a handshake with a
post-quantum key, and the JDK's cannot. A service whose identity is an ML-DSA key therefore
has that key *issue* a certificate over a classical key which it can present. It presents
that certificate together with the ML-DSA key's own self-signed certificate, and a peer which
knows the ML-DSA key verifies the leaf against it:

```scala
val swarmKey = PrivateKey.generate[MlDsa[65]]()
val tlsKey = PrivateKey.generate[Ecdsa[256]]()
val identity = Distinguished(commonName = t"swarm.example.com")

val root = Certificate.selfSigned(identity, swarmKey, validity, BigInt(1), authority = true)

val leaf =
  Certificate.issued(subject, tlsKey.public, identity, swarmKey, validity, BigInt(2), alternatives = List(t"example.com"))
```

```scala fuel
run
##
Out.println(leaf.verify(swarmKey.public).inspect)
Out.println(root.verify(swarmKey.public).inspect)
Out.println(leaf.verify(privateKey.public).inspect)
```

An issued certificate carries an authority key identifier, which names the key that signed
it. That is how a verifier finds the root in a chain. An
[HTTP server](http-server.md) presents such a certificate over TLS.

### Keystores

A [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) keystore is opened for the duration of a
scope. The password is given as an opaque `Password`, so its cleartext is reached only
through the scoped `Cleartext` capability, and the character array which the platform needs
is zeroed once the store is loaded. Inside the scope, `keystore` is the open store:

```scala
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val storePath = temporaryDirectory[Path on Linux] / "cryptography" / "store.p12"

def readAliases(): List[Text] =
  storePath.open[Keystore](Password(t"sesame")):
    val server = keystore.certificate(t"server")
    keystore.aliases
```

An alias which the store does not hold gives `Unset`. A wrong password and a corrupt store
are deliberately indistinguishable: both are `Unreadable` in the `Keystore.Error`. Telling
them apart would tell someone who was guessing which of the two had gone wrong.

### Weak algorithms

Encrypting with DES, RC2, 1024-bit RSA, ECB mode, or any unauthenticated block cipher, needs a
permission from the package `cryptoPermits` in scope. These are the same
[permits](hashing.md#weak-algorithms) which gate MD5 and SHA-1. Accepting legacy cryptography
is thus an explicit decision which can be audited, not a default. The permits nest, and
`permitDisallowedCrypto` includes every other:

```tabular
column  Algorithm
column  Permitted by
row  any block cipher, which is unauthenticated  `permitUnauthenticatedCrypto`
row  Triple-DES  `permitDeprecatedCrypto`; to decrypt or verify only, `permitLegacyCrypto`
row  DSA  `permitDisallowedCrypto`; to verify only, `permitLegacyCrypto`
row  DES, RC2, Blowfish  `permitDisallowedCrypto`
row  1024-bit RSA  `permitDisallowedCrypto`
row  ECB mode  `permitDisallowedCrypto`
```

Across the algorithms, these are the operations which each supports, and the permit it needs:

```tabular
column  Algorithm
column  Operations
column  Permit
row  `Aes[128 | 192 | 256]`  encrypt, decrypt  `permitUnauthenticatedCrypto`
row  `TripleDes[112 | 168]`  encrypt, decrypt  `permitDeprecatedCrypto`
row  `Des`, `Rc2[40 | 64 | 128]`, `Blowfish[128 | 256 | 448]`  encrypt, decrypt  `permitDisallowedCrypto`
row  `AesGcm[128 | 256]`, `ChaCha20Poly1305`  seal, unseal
row  `Rsa[2048 | 3072 | 4096]`  encrypt, decrypt, sign, verify
row  `Rsa[1024]`  encrypt, decrypt, sign, verify  `permitDisallowedCrypto`
row  `Dsa[512 | 1024 | 2048 | 3072]`  sign, verify  `permitDisallowedCrypto`
row  `Ecdsa[256 | 384 | 521]`, `Ed25519`, `MlDsa[44 | 65 | 87]`  sign, verify
row  `X25519`, `Ecdh[256 | 384 | 521]`  agree
```
