### ASN.1 and DER

Most of public-key cryptography's file formats — X.509 certificates, PKCS#10 certificate
requests, PKCS#8 keys, CMS messages — are ASN.1 structures serialized with the
_Distinguished Encoding Rules_. Enigmatic models an ASN.1 value with the `Asn1` enumeration,
and DER with the `Der` type:
```scala
val name = Asn1.Sequence
             (List
               (Asn1.ObjectId(List(2, 5, 4, 3)),
                Asn1.Utf8String(t"example.com")))

val bytes: Data = name.in[Der].data
```
Reading goes the other way, and needs a `Tactic[Asn1Error]` since the input may not be
valid DER:
```scala
val value: Asn1 = bytes.read[Der].as[Asn1]
```
DER content usually arrives armored as PEM. Reading a `Pem` and decoding its payload can be
done in one step, from any source a `Text` can be read from:
```scala
val certificate: Asn1 = text.read[Asn1 in Pem]
val payload: Der = text.read[Der in Pem]

val armored: Pem = Pem(PemLabel.Certificate, certificate.in[Der])
```
or in stages, if the label matters:
```scala
val pem: Pem = text.read[Pem]
val certificate: Asn1 = pem.as[Der].as[Asn1]
```

#### Canonicity

DER gives every value exactly one valid encoding, and Enigmatic holds to that in both
directions. The encoder writes integers in the fewest possible bytes, `true` as `0xFF`,
definite lengths in their shortest form, and the members of a `SET` in ascending order of
their encodings. The decoder rejects everything BER permits but DER does not: indefinite
lengths, overlong lengths and tag numbers, non-minimal integers, constructed strings and
unordered sets each raise an `Asn1Error` naming the byte at which the problem was found.

The point of that strictness is that decoding and re-encoding reproduces the original bytes
exactly — the property that verifying a signature over a certificate's `TBSCertificate`
depends on:
```scala
val pem = text.read[Pem]
pem.as[Der].as[Asn1].in[Der] == pem.as[Der]   // true
```

#### Tagged and unknown values

An _explicit_ context tag, `[0] EXPLICIT`, wraps a complete inner value, so it reads and
writes as `Asn1.Tagged(0, true, inner)`. An _implicit_ context tag, `[0] IMPLICIT`, merely
replaces the inner value's tag, which makes `[0] IMPLICIT INTEGER` and `[0] IMPLICIT OCTET
STRING` byte-identical; recovering the inner type needs the ASN.1 module's schema, which
this layer does not have. `Asn1.Tagged(0, false, inner)` is therefore write-only: the
decoder yields `Asn1.Unknown` for an implicit tag instead.

`Asn1.Unknown` carries content octets verbatim, and is also what the decoder produces for
universal types outside the PKIX subset modelled here, such as `T61String` and
`BMPString`. It is what makes decoding total, and what makes the round-trip above hold for
certificates in the wild.

### Certificates

A self-signed certificate needs a name, a key, a validity period and a serial number:
```scala
import chronometries.unix

val subject = Distinguished(commonName = t"example.com", organization = t"Example Ltd")
val key = PrivateKey.generate[Rsa[2048]]()
val validity = now() ~ (now() + 365*Day)

val certificate =
  Certificate.selfSigned
   (subject, key, validity, BigInt(1), alternatives = List(t"example.com", t"www.example.com"))

val armored: Text = certificate.pem.serialize
```
The certificate is version 3, and carries basic constraints, key usage and a subject key
identifier; `alternatives` become `dNSName` subject alternative names, which is what TLS
clients check. Passing `authority = true` marks it as a certificate authority and gives it
the key usage to sign other certificates.

The signature algorithm follows the key and the ambient `SignatureDigest`: an `Rsa` key under
the default digest signs with `sha256WithRSAEncryption`, an `Ecdsa[256]` key with
`ecdsa-with-SHA256`. `Ecdsa` is available where the provider offers it; `Rsa` always signs.

Reading a certificate back is the ordinary codec:
```scala
val parsed: Certificate = armored.read[Certificate in Pem]
```
