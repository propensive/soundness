## Cryptography

### About

Encryption, signing and message authentication are typed operations whose *choices* — algorithm,
key size, block-cipher mode, padding — are types, checked where the code is written. Encrypting
with AES-256 in CBC mode with PKCS7 padding names all four decisions; a combination the algorithms
do not permit, such as a stream mode with a padding, does not compile; and the weak algorithms —
DES, small RSA keys, unauthenticated modes — require an explicit permission in scope before they
can be used at all.

Keys are typed by their cipher, and a private key's bytes are reachable only inside an `expose`
block, so where secret material is used is visible in the code's structure.

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
```

### Symmetric encryption

A symmetric key is generated for its cipher — algorithm, key size, and optionally the mode and
padding, all in the type — and encryption happens inside the key's `expose` block, with the
[initialization vector](https://en.wikipedia.org/wiki/Initialization_vector) supplied explicitly:

```scala
import blockCipherMode.cbc
import blockCipherPadding.pkcs7

val key = SymmetricKey.generate[Aes[256]]()

val ciphertext = key.expose:
  t"Hello world".encrypt(InitializationVector.random)

key.expose:
  ciphertext.decrypt.text   // t"Hello world"
```

Streams encrypt chunk by chunk, so large data never assembles in memory. Decrypting with the wrong
key raises a `CryptoError` naming the failure. AES's mode and padding may also be fixed in the
key's type — `Aes[256] over Cbc against Pkcs7` — and a pairing the specification forbids does not
compile.

### Public-key encryption and signing

An RSA key pair encrypts toward the public key and decrypts with the private; a DSA pair signs
with the private key and verifies with the public:

```scala
val privateKey = PrivateKey.generate[Rsa[2048]]()

val message = privateKey.public.expose:
  t"secret".encrypt(InitializationVector.random)

privateKey.expose(message.decrypt.text)

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
Pem.parse(pemText)
```

### Weak algorithms

Encrypting with DES, RC2, 1024-bit RSA, ECB mode or any unauthenticated block cipher requires a
permission from the `crypto` package in scope — the same [permit machinery](hashing.md) that gates
MD5 and SHA-1 — so accepting legacy cryptography is an explicit, auditable decision rather than a
default.
