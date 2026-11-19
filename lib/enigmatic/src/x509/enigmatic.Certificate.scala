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
package enigmatic

import scala.caps
import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import aviation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gastronomy.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

import Certificate.Error.Reason

// An X.509 certificate. It is held as the ASN.1 value it is, so that it encodes to DER — and hence
// to PEM — through the ordinary codec, and so that a certificate read back from the wire re-encodes
// byte-for-byte, which is what checking its signature depends on.
object Certificate:
  given encodable: Certificate is Encodable in Der = _.asn1.in[Der]

  given decodable: (tactic: Tactic[Asn1.Error]^)
  =>  ( (Certificate is Decodable in Der)^{tactic, caps.any} ) =
    der => Certificate(der.as[Asn1])

  // The instant at which RFC 5280 §4.1.2.5 switches from `UTCTime` to `GeneralizedTime`: midnight
  // on 1 January 2050, after which a two-digit year is ambiguous.
  private val Generalized: Long = 2524608000L

  // Extension object identifiers, all under the certificate-extension arc 2.5.29.
  private val SubjectKeyIdentifier: List[Int] = List(2, 5, 29, 14)
  private val KeyUsage: List[Int] = List(2, 5, 29, 15)
  private val SubjectAltName: List[Int] = List(2, 5, 29, 17)
  private val BasicConstraints: List[Int] = List(2, 5, 29, 19)
  private val AuthorityKeyIdentifier: List[Int] = List(2, 5, 29, 35)

  // A self-signed certificate: the subject is its own issuer, and the signature is made with the
  // private key whose public half the certificate carries. That is the whole of a root certificate
  // authority, and of the throwaway certificate a test server or a development tool mints for
  // itself. A certificate over one key signed by another is `issued`.
  //
  // `authority` decides both the basic constraints and the key usage: a certificate authority
  // signs certificates and revocation lists, while an end-entity certificate signs and encrypts.
  // `alternatives` are `dNSName` subject alternative names, which is what every TLS client checks
  // in place of the common name.
  def selfSigned[cipher <: Cipher]
    ( subject:      Distinguished,
      key:          PrivateKey[cipher],
      validity:     Period[Instant over Unix],
      serial:       BigInt,
      authority:    Boolean = false,
      alternatives: List[Text] = Nil )
    ( using algorithm:     cipher & Signing,
            signature:     cipher is SignatureAlgorithm,
            digest:        Signature.Digest,
            hash:          Hash in Sha2[256],
            erased permit: Permit[Weakness[cipher]] )
    ( using Tactic[Certificate.Error], Tactic[Asn1.Error], Diagnostics )
  :   Certificate =

    build(subject, key.public.bytes, subject, key, validity, serial, authority, alternatives)(Unset)

  // A certificate over `key`, signed by a different key: the issuer's. This is what a certificate
  // authority does, and what a service whose identity is a key its TLS stack cannot handshake
  // with — an ML-DSA key, on today's JDK — does to bind a classical key it can present to the
  // identity it wants to be known by: the leaf certificate over the classical key is issued by the
  // ML-DSA key, and presented alongside that key's own self-signed certificate.
  //
  // RFC 5280 §4.2.1.1 requires an issued certificate to carry an authority key identifier, which
  // is the issuer's subject key identifier, so a verifier can find the certificate that signed it.
  def issued[holder <: Cipher, signer <: Cipher]
    ( subject:      Distinguished,
      key:          PublicKey[holder],
      issuer:       Distinguished,
      issuerKey:    PrivateKey[signer],
      validity:     Period[Instant over Unix],
      serial:       BigInt,
      authority:    Boolean = false,
      alternatives: List[Text] = Nil )
    ( using algorithm:     signer & Signing,
            signature:     signer is SignatureAlgorithm,
            digest:        Signature.Digest,
            hash:          Hash in Sha2[256],
            erased permit: Permit[Weakness[signer]] )
    ( using Tactic[Certificate.Error], Tactic[Asn1.Error], Diagnostics )
  :   Certificate =

    val authorityBits = Der(issuerKey.public.bytes).as[Asn1] match
      case Asn1.Sequence(List(_, bits: Asn1.BitString)) => bits
      case _                                            => abort(Error(Reason.BadPublicKey))

    // `AuthorityKeyIdentifier ::= SEQUENCE { keyIdentifier [0] IMPLICIT KeyIdentifier OPTIONAL … }`
    val authority0 = Asn1.OctetString(keyIdentifier(authorityBits.bytes))
    val authorityKey: Asn1 = Asn1.Sequence(List(Asn1.Tagged(0, false, authority0)))

    build(subject, key.bytes, issuer, issuerKey, validity, serial, authority, alternatives)
      ( authorityKey )

  private def build[signer <: Cipher]
    ( subject:      Distinguished,
      publicKey0:   Data,
      issuer:       Distinguished,
      issuerKey:    PrivateKey[signer],
      validity:     Period[Instant over Unix],
      serial:       BigInt,
      authority:    Boolean,
      alternatives: List[Text] )
    ( authorityKey: Optional[Asn1] )
    ( using algorithm:     signer & Signing,
            signature:     signer is SignatureAlgorithm,
            digest:        Signature.Digest,
            hash:          Hash in Sha2[256],
            erased permit: Permit[Weakness[signer]] )
    ( using Tactic[Certificate.Error], Tactic[Asn1.Error], Diagnostics )
  :   Certificate =

    if serial <= 0 then abort(Certificate.Error(Reason.BadSerialNumber))
    if validity.finish.long <= validity.start.long then abort(Certificate.Error(Reason.BadValidity))

    val unknown = Certificate.Error(Reason.UnknownAlgorithm(digest.token))
    val identifier = signature.identifier(digest).lay(abort(unknown))(identity)

    // `PublicKey#bytes` is already a DER `SubjectPublicKeyInfo`, so it is decoded rather than
    // rebuilt, and embedded in the certificate exactly as the provider wrote it.
    val publicKey = Der(publicKey0).as[Asn1]

    val publicBits = publicKey match
      case Asn1.Sequence(List(_, bits: Asn1.BitString)) => bits

      case _ =>
        abort(Certificate.Error(Reason.BadPublicKey))

    val period: Asn1 =
      Asn1.Sequence(List(instant(validity.start), instant(validity.finish)))

    val constraints: Asn1 =
      if authority then Asn1.Sequence(List(Asn1.Boolean(true))) else Asn1.Sequence(Nil)

    // Key usage is a named-bit `BIT STRING`, numbered from the most significant bit of the first
    // octet, with the trailing bits after the last one set declared unused. A certificate authority
    // needs `keyCertSign` (5) and `cRLSign` (6); an end entity `digitalSignature` (0) and
    // `keyEncipherment` (2).
    val usage: Asn1 =
      if authority then Asn1.BitString(Array[Byte](0x06.toByte), 1)
      else Asn1.BitString(Array[Byte](0xa0.toByte), 5)

    // `dNSName` is `[2] IMPLICIT IA5String`, which is exactly what an implicit tag is for: the
    // choice is identified by its tag, and the string type never appears on the wire.
    val alternativeNames: Optional[Asn1] =
      if alternatives.nil then Unset else
        val names = alternatives.map: name => Asn1.Tagged(2, false, Asn1.Ia5String(name))

        entry(SubjectAltName, false, Asn1.Sequence(names))

    val identity0 = Asn1.OctetString(keyIdentifier(publicBits.bytes))

    val extensions =
      List
        ( entry(BasicConstraints, true, constraints),
          entry(KeyUsage, true, usage),
          entry(SubjectKeyIdentifier, false, identity0),
          authorityKey.let(entry(AuthorityKeyIdentifier, false, _)),
          alternativeNames )

    val fields =
      List
        ( Asn1.Tagged(0, true, Asn1.Integer(BigInt(2))),
          Asn1.Integer(serial),
          identifier,
          Distinguished.sequence(issuer),
          period,
          Distinguished.sequence(subject),
          publicKey,
          Asn1.Tagged
            ( 3, true, Asn1.Sequence(extensions.sweep { case extension: Asn1 => extension }) ) )

    val tbs: Asn1 = Asn1.Sequence(fields)

    val signed = issuerKey.sign(tbs.in[Der].data)

    Certificate(Asn1.Sequence(List(tbs, identifier, Asn1.BitString(signed.bytes, 0))))

  // RFC 5280's `critical` field defaults to `FALSE`, and DER forbids encoding a field that holds
  // its default, so a non-critical extension omits it entirely.
  private def entry(identifier: List[Int], critical: Boolean, value: Asn1): Asn1 =
    val octets = Asn1.OctetString(value.in[Der].data)

    val fields =
      if critical then List(Asn1.ObjectId(identifier), Asn1.Boolean(true), octets)
      else List(Asn1.ObjectId(identifier), octets)

    Asn1.Sequence(fields)

  // RFC 7093 method 1: the leftmost 160 bits of the SHA-256 digest of the public key's bit string.
  // RFC 5280 describes a SHA-1 digest instead, but the two are interchangeable — a key identifier
  // only has to be unique, not unforgeable — and this avoids making every caller permit SHA-1.
  private def keyIdentifier(publicKey: Data)(using Hash in Sha2[256]): Data =
    publicKey.digest[Sha2[256]].data.keep(20)

  private def instant(instant: Instant over Unix): Asn1 =
    val seconds = Math.floorDiv(instant.long, 1000L)
    if seconds < Generalized then Asn1.UtcTime(seconds) else Asn1.GeneralizedTime(seconds)

  // CertificateError → Certificate.Error
  object Error:
    given communicable: Reason is Communicable =
      case Reason.UnknownAlgorithm(digest) => m"no signature algorithm is assigned to $digest"
      case Reason.BadPublicKey             => m"the public key was not a SubjectPublicKeyInfo"
      case Reason.BadSerialNumber          => m"the serial number was negative or zero"
      case Reason.BadValidity              => m"the validity period ended before it began"

    enum Reason(val number: Int) extends Clarification:
      case UnknownAlgorithm(digest: Text) extends Reason(1)
      case BadPublicKey extends Reason(2)
      case BadSerialNumber extends Reason(3)
      case BadValidity extends Reason(4)

  case class Error(reason: Certificate.Error.Reason)(using Diagnostics)
  extends fulminate.Error(105, 5, reason.number)(m"could not build the certificate because $reason")

  private val RsaSha256: List[Int] = List(1, 2, 840, 113549, 1, 1, 11)
  private val RsaSha384: List[Int] = List(1, 2, 840, 113549, 1, 1, 12)
  private val RsaSha512: List[Int] = List(1, 2, 840, 113549, 1, 1, 13)
  private val EcdsaSha256: List[Int] = List(1, 2, 840, 10045, 4, 3, 2)
  private val EcdsaSha384: List[Int] = List(1, 2, 840, 10045, 4, 3, 3)
  private val EcdsaSha512: List[Int] = List(1, 2, 840, 10045, 4, 3, 4)
  private val Ed25519Signature: List[Int] = List(1, 3, 101, 112)

  // What a provider must offer for a certificate's signature to be checked by the algorithm
  // the certificate itself names, rather than by a cipher known when the code was written.
  type Verification =
    Crypto
      { def ecdsa(digest: Text): Crypto.SignatureScheme
        def ed25519: Crypto.SignatureScheme }

  private def same(left: Asn1, right: Asn1): Boolean =
    Crypto.constantTimeEquals(left.in[Der].data, right.in[Der].data)

  // Why a chain of certificates does not lead to a trusted one.
  object Path:
    object Error:
      given communicable: Reason is Communicable =
        case Reason.Empty        => m"it holds no certificate"
        case Reason.Malformed    => m"a certificate in it is not well-formed"
        case Reason.Expired      => m"a certificate in it is outside its validity period"
        case Reason.BadSignature => m"a certificate in it was not signed by the next"
        case Reason.Unlinked     => m"a certificate in it was not issued by the next"
        case Reason.NotAuthority => m"a certificate in it was issued by one that is not an authority"
        case Reason.Untrusted    => m"it does not lead to a trusted certificate"

      enum Reason(val number: Int) extends Clarification:
        case Empty        extends Reason(1)
        case Malformed    extends Reason(2)
        case Expired      extends Reason(3)
        case BadSignature extends Reason(4)
        case Unlinked     extends Reason(5)
        case NotAuthority extends Reason(6)
        case Untrusted    extends Reason(7)

    case class Error(reason: Path.Error.Reason)(using Diagnostics)
    extends fulminate.Error(105, 9, reason.number)
      ( m"the certificate chain was not accepted because $reason" )

  // Checks that a chain of certificates — the one being relied on first, each followed by the
  // certificate of the authority that issued it — leads to one of the trusted `anchors`. Every
  // certificate must be within its validity period by the clock in scope; each must name the
  // next as its issuer and carry a signature the next's key made; every issuer must be a
  // certificate authority; and the last must be an anchor, or be issued by one.
  //
  // This is the core of RFC 5280 §6 and no more: revocation, name constraints, policies and
  // path-length limits are not examined, so it suits a closed set of anchors chosen for one
  // purpose — a vendor's attestation roots — rather than the public web's.
  def validate(chain: List[Certificate], anchors: List[Certificate])
    ( using clock: Clock, crypto: Verification )
    ( using Tactic[Path.Error], Diagnostics )
  :   Unit =

    def fail(reason: Path.Error.Reason): Nothing = abort(Path.Error(reason))

    def issued(certificate: Certificate, authority: Certificate): Boolean =
      certificate.issuerName.lay(false): issuer =>
        authority.subjectName.lay(false)(same(issuer, _))

    def recur(certificate: Certificate, rest: List[Certificate]): Unit =
      if certificate.fields.nil then fail(Path.Error.Reason.Malformed)
      if !certificate.current then fail(Path.Error.Reason.Expired)

      if !anchors.exists(anchor => same(anchor.asn1, certificate.asn1)) then rest match
        case authority :: more =>
          if !issued(certificate, authority) then fail(Path.Error.Reason.Unlinked)
          if !authority.authority then fail(Path.Error.Reason.NotAuthority)
          if !certificate.signedBy(authority) then fail(Path.Error.Reason.BadSignature)
          recur(authority, more)

        case _ =>
          val trusted = anchors.exists: anchor =>
            issued(certificate, anchor) && anchor.current && certificate.signedBy(anchor)

          if !trusted then fail(Path.Error.Reason.Untrusted)

    chain match
      case certificate :: rest => recur(certificate, rest)
      case _                   => fail(Path.Error.Reason.Empty)

case class Certificate(asn1: Asn1):
  // The armored form, which is how certificates are almost always exchanged.
  def pem: Pem = Pem(Pem.Label.Certificate, asn1.in[Der])

  // Whether the signature was made over this certificate's `TBSCertificate` by the private half of
  // `issuer` — the certificate's own public key, if it is self-signed. For a digest-taking scheme
  // (RSA, ECDSA) the digest is the `Signature.Digest` in scope, not the one the certificate's
  // algorithm identifier names, so the two must agree; ML-DSA has no digest to agree on. A
  // certificate that is not the three-element structure a signed certificate is cannot verify.
  def verify[cipher <: Cipher](issuer: PublicKey[cipher])
    ( using algorithm: cipher & Signing, erased weakness: ProcessingPermit[Weakness[cipher]] )
  :   Boolean =

    asn1 match
      case Asn1.Sequence(List(tbs, _, Asn1.BitString(bytes, 0))) =>
        issuer.verify(tbs.in[Der].data, Signature[cipher](bytes))

      case _ => false

  // The fields of the `TBSCertificate`, without the optional leading version: the serial
  // number, signature algorithm, issuer, validity, subject, public key and, in a version 3
  // certificate, the extensions. Empty for a value that is not a certificate.
  private[enigmatic] def fields: List[Asn1] = asn1 match
    case Asn1.Sequence(List(Asn1.Sequence(fields), _, _)) => fields match
      case Asn1.Tagged(0, true, _) :: rest => rest
      case other                           => other

    case _ =>
      Nil

  private[enigmatic] def issuerName: Optional[Asn1] = fields match
    case _ :: _ :: name :: _ => name
    case _                   => Unset

  private[enigmatic] def subjectName: Optional[Asn1] = fields match
    case _ :: _ :: _ :: _ :: name :: _ => name
    case _                             => Unset

  private def moment(time: Asn1): Optional[Instant over Unix] = time match
    case Asn1.UtcTime(seconds)         => Instant.of[Unix](seconds*1000L)
    case Asn1.GeneralizedTime(seconds) => Instant.of[Unix](seconds*1000L)
    case _                             => Unset

  def serial: Optional[BigInt] = fields match
    case Asn1.Integer(serial) :: _ => serial
    case _                         => Unset

  // The certificate authority that issued the certificate, and whom it certifies.
  def issuer: Optional[Distinguished] = issuerName.let(Distinguished.read(_))
  def subject: Optional[Distinguished] = subjectName.let(Distinguished.read(_))

  // The first and last instants at which the certificate is valid.
  def notBefore: Optional[Instant over Unix] = fields match
    case _ :: _ :: _ :: Asn1.Sequence(List(start, _)) :: _ => moment(start)
    case _                                                 => Unset

  def notAfter: Optional[Instant over Unix] = fields match
    case _ :: _ :: _ :: Asn1.Sequence(List(_, finish)) :: _ => moment(finish)
    case _                                                  => Unset

  // Whether the clock in scope is within the validity period.
  def current(using clock: Clock): Boolean =
    val moment = clock().long

    notBefore.lay(false)(_.long <= moment) && notAfter.lay(false)(_.long >= moment)

  // The certified key, as the `SubjectPublicKeyInfo` document every provider takes.
  def publicKey: Optional[Data] = fields match
    case _ :: _ :: _ :: _ :: _ :: key :: _ => key.in[Der].data
    case _                                 => Unset

  // The certified key, as a key of a named cipher, which its algorithm and size must agree with.
  def key[cipher <: Cipher: KeyAlgorithm](using Tactic[PublicKey.Error], Diagnostics)
  :   PublicKey[cipher] =

    val document = publicKey.or:
      abort(PublicKey.Error(PublicKey.Error.Reason.Malformed(t"the certificate carries no key")))

    PublicKey.adopt[cipher](document)

  // The value of the extension with the given object identifier: the content of its
  // `extnValue`, which is itself DER whose structure the extension defines.
  def extension(identifier: List[Int]): Optional[Data] =
    val extensions: List[Asn1] = fields match
      case _ :: _ :: _ :: _ :: _ :: _ :: rest =>
        val found = rest.sweep:
          case Asn1.Tagged(3, true, Asn1.Sequence(extensions)) => extensions

        found.head.or(Nil)

      case _ =>
        Nil

    // A match rather than `sweep`: a lambda that returns the frozen bytes gives them a
    // capture set the result cannot carry.
    def find(entries: List[Asn1]): Optional[Data] = entries match
      case Asn1.Sequence(List(Asn1.ObjectId(arcs), Asn1.OctetString(value))) :: _
          if arcs == identifier =>
        value

      case Asn1.Sequence(List(Asn1.ObjectId(arcs), Asn1.Boolean(_), Asn1.OctetString(value))) :: _
          if arcs == identifier =>
        value

      case _ :: rest =>
        find(rest)

      case _ =>
        Unset

    find(extensions)

  // Whether the certificate is a certificate authority's: its basic constraints say so
  // (RFC 5280 §4.2.1.9). A certificate without the extension is not one.
  def authority: Boolean =
    this.extension(Certificate.BasicConstraints).lay(false): value =>
      safely(Der(value).as[Asn1]) match
        case Asn1.Sequence(Asn1.Boolean(authority) :: _) => authority
        case _                                           => false

  // Whether the signature was made by the key `issuer` certifies, under the algorithm this
  // certificate names — where `verify` takes a key whose cipher the caller already knows. An
  // algorithm that is not RSA PKCS#1 v1.5 or ECDSA over SHA-2, or Ed25519, is not verified.
  def signedBy(issuer: Certificate)(using crypto: Certificate.Verification): Boolean =
    asn1 match
      case Asn1.Sequence
            ( List
                ( tbs,
                  Asn1.Sequence(Asn1.ObjectId(algorithm) :: _),
                  Asn1.BitString(signature, 0) ) ) =>

        val scheme: Optional[Crypto.SignatureScheme] =
          if algorithm == Certificate.RsaSha256 then crypto.rsaSignature(t"SHA256")
          else if algorithm == Certificate.RsaSha384 then crypto.rsaSignature(t"SHA384")
          else if algorithm == Certificate.RsaSha512 then crypto.rsaSignature(t"SHA512")
          else if algorithm == Certificate.EcdsaSha256 then crypto.ecdsa(t"SHA256")
          else if algorithm == Certificate.EcdsaSha384 then crypto.ecdsa(t"SHA384")
          else if algorithm == Certificate.EcdsaSha512 then crypto.ecdsa(t"SHA512")
          else if algorithm == Certificate.Ed25519Signature then crypto.ed25519
          else Unset

        issuer.publicKey.lay(false): key =>
          scheme.lay(false): scheme =>
            // A key of another algorithm than the signature's is refused by the provider with
            // an exception, which is one more way of not having signed it.
            try scheme.verify(tbs.in[Der].data, signature, key)
            catch case error: Exception => false

      case _ =>
        false
