## JOSE: signed and encrypted tokens

### About

[JOSE](https://www.rfc-editor.org/rfc/rfc7515) is the family of standards behind the tokens
that web APIs authenticate with. A *JWS* is content with a signature over it. A *JWE* is
content which has been encrypted. A *JWK* is a key, written as JSON. A *JWT* is a set of
claims, such as who issued a token and when it expires, carried inside a JWS or a JWE.

All four are provided here, over the keys and ciphers of [cryptography](cryptography.md) and
the values of [JSON](json.md). A server which only needs to accept bearer tokens can do so
with one call, as middleware for an [HTTP server](http-server.md).

### On choosing the algorithm

A JOSE object begins with a header which says how it was made, and the header is written by
whoever made the object. Reading the algorithm from the header, and then doing whatever it
says with the key at hand, is the origin of the best-known attacks on these standards: a
token whose header is changed to `none` and whose signature is removed, or an RSA public key
replayed as though it were an HMAC secret.

Here the key's type chooses the algorithm. A `PublicKey[Ecdsa[256]]` verifies `ES256` and
nothing else. The header's `alg` is compared with the algorithm the key's type stands for,
and a difference is an error. It is never used to decide what to do with the key, so neither
attack can be expressed.

Everything comes from the `soundness` package. A provider of algorithms and a place to keep
keys are in scope, as they are for any cryptography, and the last import lets the samples
print their results:

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
import formatting.compactJsonFormatting
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Signing

`Jws` signs a payload with a private key. The compact serialization, which is the familiar
three parts joined by periods, is the object's `Text` form:

```scala
val key = PrivateKey.generate[Ecdsa[256]]()
val signed: Jws = Jws(t"Shall I compare thee to a summer's day?", key)
val compact: Text = signed.in[Text]
```

The header names the algorithm of the key:

```scala fuel
run
##
Out.println(signed.header.algorithm.or(t"none"))
```

A `Jws` which has been read from text is a claim, and nothing more. Its payload is reached
only through `verify`, which checks the signature against a public key:

```scala
val line: Text = compact.as[Jws].verify(key.public).read[Text]
```

```scala fuel
run
##
Out.println(line)
```

A signature which does not verify raises a `Jose.Error`, as does a key of the wrong type.
This is the second of the two attacks, attempted with an HMAC key:

```scala
val secret = SymmetricKey.generate[HmacCipher[Sha2[256]]]()
val refusal = capture[Jose.Error](compact.as[Jws].verify(secret))
```

```scala fuel
run
##
Out.println(refusal.message.text)
```

Each signing cipher stands for one algorithm:

```tabular
column  Key
column  Algorithm
row  `SymmetricKey[HmacCipher[Sha2[256]]]`  `HS256`, and likewise `HS384` and `HS512`
row  `PrivateKey[Rsa[2048]]`  `RS256`
row  `PrivateKey[RsaPss[2048]]`  `PS256`
row  `PrivateKey[Ecdsa[256]]`  `ES256`, and `ES384` and `ES512` for P-384 and P-521
row  `PrivateKey[Ed25519]`  `EdDSA`
```

An RSA key signs with SHA-256 unless `signatureDigests.sha384Signature` or
`signatureDigests.sha512Signature` is imported, which makes it `RS384` or `RS512`. An ECDSA
key's curve fixes its digest, as the standard requires.

Other header parameters are passed as a `Jose.Header`, whose `alg` is always the key's:

```scala
val identified = Jws(t"Good night, sweet prince", key, Jose.Header(keyId = t"globe-1599"))
```

An unsecured object, whose algorithm is `none`, never verifies. Its payload can be read only
by saying so, with the `Jose.Unsecured` token:

```scala
val unsecured: Jws = Jws.unsecured(t"The rest is silence")
val unvouched: Text = unsecured.payload(Jose.Unsecured).read[Text]
```

### Keys as JSON

A `Jwk` is a key as a JSON object. A public key is written directly. A private or symmetric
key needs the `Divulgence` token, the same one which lets a private key out as PEM:

```scala
val published: Jwk = Jwk(key.public)
val divulged: Jwk = Jwk(key, Divulgence)
```

```scala fuel
run
##
Out.println(published.in[Json].show)
```

A `Jwk` is read back as a key of a named cipher. The key's type and its size or curve must
agree with the cipher, or a `Jose.Error` is raised:

```scala
val restored: PublicKey[Ecdsa[256]] = published.publicKey[Ecdsa[256]]
```

The same JSON object can stand for more than one cipher. An `RSA` key is an `Rsa[2048]` key
for `RS256`, an `RsaPss[2048]` key for `PS256` and an `RsaOaep[2048, Sha1]` key for
encryption, and the cipher asked for decides which.

A key's *thumbprint* is a digest of its essential members, so two descriptions of one key
have one thumbprint. It makes a natural key identifier:

```scala
val thumbprint: Text = published.thumbprint[Sha2[256]]
```

A `Jwks` is a set of keys, such as an identity provider publishes, in which a key is found by
its identifier:

```scala
val keys = Jwks(List(published.copy(keyId = t"globe-1599")))
val found: Optional[Jwk] = keys(t"globe-1599")
```

`Jwks.retrieve` fetches a set from the URL its owner publishes it at. That is an
[HTTP request](http-client.md#what-a-request-needs-in-scope), so it needs what a request
needs in scope.

### Encrypting

`Jwe` encrypts a payload to a recipient. Two choices are made: how the content is encrypted,
which is the type argument, and how the content's key reaches the recipient, which the
recipient's key decides:

```scala
val recipient = PrivateKey.generate[Ecdh[256]]()
val sealedLine: Jwe = Jwe[AesGcm[256]](t"Speak the speech, I pray you", recipient.public)
val opened: Text = sealedLine.in[Text].as[Jwe].decrypt[AesGcm[256]](recipient).read[Text]
```

```scala fuel
run
##
Out.println(sealedLine.header.algorithm.or(t"none"))
Out.println(sealedLine.header.encryption.or(t"none"))
Out.println(opened)
```

The content ciphers are AES-GCM and AES-CBC with HMAC:

```tabular
column  Content cipher
column  `enc`
row  `AesGcm[128]`, `AesGcm[192]`, `AesGcm[256]`  `A128GCM`, `A192GCM`, `A256GCM`
row  `AesCbcHmac[256]`, `AesCbcHmac[384]`, `AesCbcHmac[512]`  `A128CBC-HS256`, `A192CBC-HS384`, `A256CBC-HS512`
```

The size of an `AesCbcHmac` is that of its whole key, half of which is for AES and half for
HMAC.

A recipient's key stands for a family of key-management algorithms. Where the family has
more than one member, the first is used unless the header passed to `Jwe` names another, and
decryption accepts any of the family but nothing outside it:

```tabular
column  Recipient's key
column  `alg`
row  `PublicKey[RsaOaep[2048, Sha1]]`  `RSA-OAEP`
row  `PublicKey[RsaOaep[2048, Sha2[256]]]`  `RSA-OAEP-256`
row  `PublicKey[Rsa[2048]]`  `RSA1_5`
row  `PublicKey[Ecdh[256]]`, `PublicKey[X25519]`  `ECDH-ES`, or `ECDH-ES+A128KW`, `ECDH-ES+A192KW`, `ECDH-ES+A256KW`
row  `SymmetricKey[AesKeyWrap[128]]`  `A128KW`, and likewise `A192KW` and `A256KW`
row  `SymmetricKey[AesGcm[128]]`  `dir`, or `A128GCMKW`
row  `SymmetricKey[AesCbcHmac[256]]`  `dir`
row  `Password`  `PBES2-HS256+A128KW`, or `PBES2-HS384+A192KW`, `PBES2-HS512+A256KW`
```

A member other than the first is chosen in the header:

```scala
val wrapped: Jwe =
  Jwe[AesGcm[256]]
    ( t"Speak the speech, I pray you",
      recipient.public,
      Jose.Header(algorithm = t"ECDH-ES+A256KW") )
```

`RSA1_5` is provided because it is still met, but it should not be chosen for anything new.

A password derives its key slowly, on purpose. The number of iterations is
`Jwe.DefaultIterations` unless the header's `iterations` says otherwise, and an object which
asks its recipient for more than `Jwe.MaximumIterations` is refused before any work is done.

Every failure to decrypt is the same `Jose.Error`, whose reason is `Undecryptable`. It does
not say whether the key or the content was at fault, since that is exactly what an attacker
would like to learn.

### Tokens

A `Jwt` is a set of claims. The seven which the standard registers are a `Jwt.Claims`, and an
application's own are a value of its own type, written to the same JSON object:

```scala
case class Role(part: Text, play: Text)

val claims =
  Jwt.Claims
    ( issuer   = t"globe",
      subject  = t"burbage",
      audience = List(t"groundlings"),
      expiry   = now() + 1*Hour )

val token: Text = Jwt(claims, Role(t"Hamlet", t"Hamlet")).sign(key).in[Text]
```

`Jwt.verify` is the whole of what a server does with a token it has been sent. It checks the
signature against the key, reads the claims, and checks that the token has not expired and is
not presented before its time, by the `Clock` in scope:

```scala
val jwt: Jwt = Jwt.verify(token, key.public)
val role: Role = jwt.json.as[Role]
```

```scala fuel
run
##
Out.println(jwt.claims.subject.or(t"nobody"))
Out.println(role.part)
```

`validate` checks who issued the token and whom it is for, and allows some seconds of
`leeway` for the difference between two machines' clocks:

```scala
val accepted: Jwt = jwt.validate(issuer = t"globe", audience = t"groundlings", leeway = 30)
```

There is no way to read a token's claims without verifying it.

### Bearer authentication

For an [HTTP server](http-server.md), `bearerAuth` is the counterpart of `basicAuth`. Its
handler runs only for a request whose `Authorization: Bearer` header carries a token which
verifies, and which names the issuer and audience expected of it. The token is in scope as
`jwt`:

```scala fuel
typecheck
##
def handler(using Http.Connection): Http.Response =
  bearerAuth(key.public, t"globe", audience = t"groundlings"):
    Http.Response(Http.Ok)(t"Welcome, ${jwt.claims.subject.or(t"stranger")}")
```

A request with no token is answered `401`, with a challenge. One whose token does not check
out is answered the same way, with `error="invalid_token"`, and is never told why.

Where the keys are a set, the token's `kid` chooses among them, and the type argument is the
cipher every key in the set is read as: `bearerAuth[Ecdsa[256]](keys, …)`.

### Other serializations

The compact serialization carries one signature, or one recipient. The JSON serializations
carry several, and unprotected headers besides. `Jws.read` and `Jwe.read` take either JSON
form, and give one object for each signature or recipient. `flattened` writes one, and
`Jws.general` and `Jwe.general` write several:

```scala
val flattened: Json = signed.flattened
val several: List[Jws] = Jws.read(flattened)
```

### Related

- [Cryptography](cryptography.md) provides the keys, the ciphers, and the files keys are
  read from.
- [OAuth 2](oauth.md) is how a client obtains a token.
- [JSON](json.md) is what a token's claims are read from and written to.
