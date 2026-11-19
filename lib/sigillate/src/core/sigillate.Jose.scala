                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package sigillate

import scala.reflect.Selectable.reflectiveSelectable

import adversaria.*
import anticipation.*
import contingency.*
import corpuscular.*
import distillate.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import jacinta.*
import monotonous.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

import codepages.utf8Codepage
import formatting.compactJsonFormatting

// JSON Object Signing and Encryption: the family of standards (RFC 7515–7519) behind signed and
// encrypted tokens. A `Jws` is a signed object, a `Jwe` an encrypted one, a `Jwk` a key and a
// `Jwt` a token of claims; `Jose` holds what they share — the header, the error, and the
// mapping from enigmatic's cipher types to the algorithm names the standards register.
//
// One rule runs through all of it: **the key's type chooses the algorithm**. A
// `PublicKey[Ecdsa[256]]` verifies `ES256` and nothing else; a
// `SymmetricKey[HmacCipher[Sha2[256]]]` verifies `HS256` and nothing else. The `alg` a token's
// header names is compared with the algorithm the key's type stands for, and a difference is
// an error; it is never used to decide what to do with the key. That closes the classic JOSE
// attacks — an RSA public key replayed as an HMAC secret, a token relabelled `none` — by
// construction rather than by a check a caller could forget.
object Jose:
  // The token that licenses reading an unsecured (`"alg":"none"`) object:
  // `jws.payload(Jose.Unsecured)`.
  // It is passed explicitly, as `Divulgence` is, so that accepting an unsigned token is visible
  // at the place it happens.
  object Unsecured

  object Error:
    given communicable: Reason is Communicable =
      case Reason.Malformed(detail)        => m"it was malformed: $detail"
      case Reason.Unsupported(name)        => m"the algorithm $name is not supported"
      case Reason.Mismatch(expected, found) =>
        m"the key is for the algorithm $expected but the header names $found"

      case Reason.BadSignature             => m"the signature did not verify"
      case Reason.Undecryptable            => m"it could not be decrypted with the key"
      case Reason.Critical(name) =>
        m"the critical header parameter $name is not understood"

      case Reason.Secured                  => m"it is not an unsecured object"
      case Reason.BadKey(detail)           => m"the key was not usable: $detail"
      case Reason.Expired                  => m"the token has expired"
      case Reason.Premature                => m"the token is not yet valid"
      case Reason.Issuer(found)            => m"the token was issued by $found"
      case Reason.Audience                 => m"the token is intended for a different audience"
      case Reason.Claim(name)              => m"the claim $name was absent or of the wrong type"
      case Reason.Unavailable(detail)      => m"the keys could not be fetched: $detail"

    enum Reason(val number: Int) extends Clarification:
      case Malformed(detail: Text)                extends Reason(1)
      case Unsupported(name: Text)                extends Reason(2)
      case Mismatch(expected: Text, found: Text)  extends Reason(3)
      case BadSignature                           extends Reason(4)
      case Undecryptable                          extends Reason(5)
      case Critical(name: Text)                   extends Reason(6)
      case Secured                                extends Reason(7)
      case BadKey(detail: Text)                   extends Reason(8)
      case Expired                                extends Reason(9)
      case Premature                              extends Reason(10)
      case Issuer(found: Text)                    extends Reason(11)
      case Audience                               extends Reason(12)
      case Claim(name: Text)                      extends Reason(13)
      case Unavailable(detail: Text)              extends Reason(14)

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(147, 1, reason.number)
    ( m"the JOSE object could not be processed because $reason" )

  // Base64url without padding (RFC 7515 §2), which is how every binary value in JOSE is written.
  private[sigillate] def encode(data: Data): Text =
    import alphabets.base64Url
    data.serialize[Base64]

  private[sigillate] def decode(text: Text)(using Tactic[Jose.Error], Diagnostics): Data =
    import alphabets.base64Url

    mitigate:
      case Serialization.Error(_, _) =>
        Jose.Error(Error.Reason.Malformed(t"a value was not base64url"))

    . protect(text.deserialize[Base64])

  private[sigillate] def ascii(text: Text): Data = text.in[Data]

  // JSON read from its UTF-8 bytes.
  private[sigillate] def parse(data: Data)(using Tactic[Jose.Error], Diagnostics): Json =
    mitigate:
      case Parse.Error(_, _, _) => Jose.Error(Error.Reason.Malformed(t"a value was not JSON"))

    . protect(Chain(data).read[Json])

  object Header:
    // A header from the base64url text of its JSON, as a serialization carries it.
    private[sigillate] def read(encoded: Text)(using Tactic[Jose.Error], Diagnostics): Header =
      Header.of(parse(decode(encoded)))

    private[sigillate] def of(json: Json)(using Tactic[Jose.Error], Diagnostics): Header =
      mitigate:
        case Json.Error(_) =>
          Jose.Error(Error.Reason.Malformed(t"a header parameter was of the wrong type"))

      . protect(json.as[Header])

  // The header parameters RFC 7515 §4.1, RFC 7516 §4.1 and RFC 7518 §4 register, by the names
  // the standards give them. A parameter that is absent is `Unset`, and is left out when the
  // header is written; a parameter this type does not name is ignored when a header is read —
  // unless the header lists it in `crit`, which makes it an error.
  case class Header
    ( @name[Json](t"alg")      algorithm:        Optional[Text]       = Unset,
      @name[Json](t"enc")      encryption:       Optional[Text]       = Unset,
      @name[Json](t"zip")      compression:      Optional[Text]       = Unset,
      @name[Json](t"kid")      keyId:            Optional[Text]       = Unset,
      @name[Json](t"typ")      mediaType:        Optional[Text]       = Unset,
      @name[Json](t"cty")      contentType:      Optional[Text]       = Unset,
      @name[Json](t"crit")     critical:         Optional[List[Text]] = Unset,
      @name[Json](t"jku")      keySetUrl:        Optional[Text]       = Unset,
      @name[Json](t"jwk")      key:              Optional[Jwk]        = Unset,
      @name[Json](t"x5u")      certificateUrl:   Optional[Text]       = Unset,
      @name[Json](t"x5c")      certificates:     Optional[List[Text]] = Unset,
      @name[Json](t"x5t")      certificateThumbprint: Optional[Text]  = Unset,
      @name[Json](t"x5t#S256") certificateSha256:     Optional[Text] = Unset,
      @name[Json](t"epk")      ephemeralKey:     Optional[Jwk]        = Unset,
      @name[Json](t"apu")      producer:         Optional[Text]       = Unset,
      @name[Json](t"apv")      recipient:        Optional[Text]       = Unset,
      @name[Json](t"iv")       vector:           Optional[Text]       = Unset,
      @name[Json](t"tag")      tag:              Optional[Text]       = Unset,
      @name[Json](t"p2s")      salt:             Optional[Text]       = Unset,
      @name[Json](t"p2c")      iterations:       Optional[Int]        = Unset ):

    // This header, with any parameter it lacks taken from another.
    private[sigillate] def merge(other: Header): Header =
      Header
        ( algorithm.or(other.algorithm),
          encryption.or(other.encryption),
          compression.or(other.compression),
          keyId.or(other.keyId),
          mediaType.or(other.mediaType),
          contentType.or(other.contentType),
          critical.or(other.critical),
          keySetUrl.or(other.keySetUrl),
          key.or(other.key),
          certificateUrl.or(other.certificateUrl),
          certificates.or(other.certificates),
          certificateThumbprint.or(other.certificateThumbprint),
          certificateSha256.or(other.certificateSha256),
          ephemeralKey.or(other.ephemeralKey),
          producer.or(other.producer),
          recipient.or(other.recipient),
          vector.or(other.vector),
          tag.or(other.tag),
          salt.or(other.salt),
          iterations.or(other.iterations) )

    // The header as the base64url text of its compact JSON.
    private[sigillate] def encoded: Text = encode(ascii(this.in[Json].show))

    // Every parameter a header marks critical must be one this implementation acts on; none of
    // the registered parameters may be listed (RFC 7515 §4.1.11), so any entry at all is one
    // it does not understand.
    private[sigillate] def vet()(using Tactic[Jose.Error], Diagnostics): Unit =
      critical.let: names =>
        names.each: name =>
          abort(Jose.Error(Error.Reason.Critical(name)))

  object Encryption:
    def apply[cipher <: Cipher](name0: Text, cipher0: cipher & Authenticated)
    :   cipher is Encryption =

      new Encryption:
        type Self = cipher
        val name: Text = name0
        val cipher: Authenticated = cipher0
        def size: Int = cipher0.keySize/8
        def fresh(): Data = cipher0.genKey()

    // AES-GCM: `A128GCM`, `A192GCM` and `A256GCM`.
    given aesGcm: [bits <: 128 | 192 | 256: ValueOf] => (cipher: AesGcm[bits])
    =>  AesGcm[bits] is Encryption =
      Encryption(t"A${valueOf[bits]}GCM", cipher)

    // AES-CBC with HMAC: `A128CBC-HS256`, `A192CBC-HS384` and `A256CBC-HS512`. The size in
    // the cipher's type is that of the whole key, twice the AES key's.
    given aesCbcHmac: [bits <: 256 | 384 | 512: ValueOf] => (cipher: AesCbcHmac[bits])
    =>  AesCbcHmac[bits] is Encryption =
      Encryption(t"A${valueOf[bits]/2}CBC-HS${valueOf[bits]}", cipher)

  // The JWE `enc` a content cipher stands for (RFC 7518 §5.1), with the cipher, the length of
  // its key in bytes, and a fresh random key for it.
  trait Encryption extends Typeclass:
    type Self <: Cipher
    def name: Text
    def cipher: Authenticated
    def size: Int
    def fresh(): Data

  object Algorithm:
    def apply[cipher <: Cipher](name0: Text, cipher0: cipher & gastronomy.Signing)
      ( encode0: enigmatic.Signature[cipher] -> Optional[Data],
        decode0: Data -> Optional[enigmatic.Signature[cipher]] )
    :   cipher is Algorithm =

      new Algorithm:
        type Self = cipher
        val name: Text = name0
        val cipher: cipher & gastronomy.Signing = cipher0
        def wire(signature: enigmatic.Signature[cipher]): Optional[Data] = encode0(signature)
        def signature(wire: Data): Optional[enigmatic.Signature[cipher]] = decode0(wire)

    private def plain[cipher <: Cipher](name: Text, cipher: cipher & gastronomy.Signing)
    :   cipher is Algorithm =
      Algorithm(name, cipher)(_.bytes, enigmatic.Signature[cipher](_))

    // HMAC over SHA-2: `HS256`, `HS384` and `HS512`.
    given hmac: [bits <: 256 | 384 | 512: ValueOf]
    =>  ( cipher: HmacCipher[Sha2[bits]] )
    =>  HmacCipher[Sha2[bits]] is Algorithm =
      plain(t"HS${valueOf[bits]}", cipher)

    // RSASSA-PKCS1-v1_5: `RS256` under the default `Signature.Digest`, and `RS384` or `RS512`
    // where `signatureDigests.sha384Signature` or `sha512Signature` is imported.
    given rsa: [bits <: 2048 | 3072 | 4096: ValueOf]
    =>  ( digest: Signature.Digest, cipher: Rsa[bits] )
    =>  Rsa[bits] is Algorithm =
      plain(t"RS${digest.token.skip(3)}", cipher)

    // RSASSA-PSS: `PS256`, `PS384` or `PS512`, by the `Signature.Digest` in scope.
    given rsaPss: [bits <: 2048 | 3072 | 4096: ValueOf]
    =>  ( digest: Signature.Digest, cipher: RsaPss[bits] )
    =>  RsaPss[bits] is Algorithm =
      plain(t"PS${digest.token.skip(3)}", cipher)

    // ECDSA, where the curve fixes the digest (RFC 7518 §3.4): `ES256` is P-256 with SHA-256,
    // `ES384` P-384 with SHA-384 and `ES512` P-521 with SHA-512. The cipher is therefore built
    // here, over the right digest, and the `Signature.Digest` in scope is not consulted. The
    // signature travels as the fixed-width concatenation of `r` and `s`, not the provider's DER.
    private type EcdsaCrypto = Crypto { def ecdsa(digest: Text): Crypto.SignatureScheme }

    given es256: (crypto: EcdsaCrypto) => Ecdsa[256] is Algorithm =
      val cipher = Ecdsa.value[256](using summon, enigmatic.Signature.Digest(t"SHA256"), crypto)
      Algorithm(t"ES256", cipher)(Ecdsa.concatenation[256](_), Ecdsa.signature[256](_))

    given es384: (crypto: EcdsaCrypto) => Ecdsa[384] is Algorithm =
      val cipher = Ecdsa.value[384](using summon, enigmatic.Signature.Digest(t"SHA384"), crypto)
      Algorithm(t"ES384", cipher)(Ecdsa.concatenation[384](_), Ecdsa.signature[384](_))

    given es512: (crypto: EcdsaCrypto) => Ecdsa[521] is Algorithm =
      val cipher = Ecdsa.value[521](using summon, enigmatic.Signature.Digest(t"SHA512"), crypto)
      Algorithm(t"ES512", cipher)(Ecdsa.concatenation[521](_), Ecdsa.signature[521](_))

    // Ed25519 (RFC 8037): `EdDSA`.
    given edDsa: (cipher: Ed25519) => Ed25519 is Algorithm = plain(t"EdDSA", cipher)

  // The JWS `alg` a signing cipher stands for (RFC 7518 §3.1), with the cipher itself and the
  // conversions between a provider's signature and the form JOSE writes it in — the same
  // bytes for every algorithm but ECDSA.
  trait Algorithm extends Typeclass:
    type Self <: Cipher
    def name: Text
    def cipher: Self & gastronomy.Signing
    def wire(signature: enigmatic.Signature[Self]): Optional[Data]
    def signature(wire: Data): Optional[enigmatic.Signature[Self]]
