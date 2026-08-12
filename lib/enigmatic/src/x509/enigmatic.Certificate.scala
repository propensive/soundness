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

import proscenium.compat.*

import anticipation.*
import aviation.*
import contingency.*
import distillate.*
import fulminate.*
import gastronomy.*
import prepositional.*
import rudiments.*
import vacuous.*

import CertificateError.Reason

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

  // A self-signed certificate: the subject is its own issuer, and the signature is made with the
  // private key whose public half the certificate carries. That is the whole of a root certificate
  // authority, and of the throwaway certificate a test server or a development tool mints for
  // itself.
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
            digest:        SignatureDigest,
            hash:          Hash in Sha2[256],
            erased permit: Permit[Weakness[cipher]] )
    ( using Tactic[CertificateError], Tactic[Asn1.Error], Diagnostics )
  :   Certificate =

    if serial <= 0 then abort(CertificateError(Reason.BadSerialNumber))
    if validity.finish.long <= validity.start.long then abort(CertificateError(Reason.BadValidity))

    val unknown = CertificateError(Reason.UnknownAlgorithm(digest.token))
    val identifier = signature.identifier(digest).lay(abort(unknown))(identity)

    // `PublicKey#bytes` is already a DER `SubjectPublicKeyInfo`, so it is decoded rather than
    // rebuilt, and embedded in the certificate exactly as the provider wrote it.
    val publicKey = Der(key.public.bytes).as[Asn1]

    val publicBits = publicKey match
      case Asn1.Sequence(List(_, bits: Asn1.BitString)) => bits

      case _ =>
        abort(CertificateError(Reason.BadPublicKey))

    val name = Distinguished.sequence(subject)

    val period: Asn1 =
      Asn1.Sequence(List(instant(validity.start), instant(validity.finish)))

    val constraints: Asn1 =
      if authority then Asn1.Sequence(List(Asn1.Boolean(true))) else Asn1.Sequence(Nil)

    // Key usage is a named-bit `BIT STRING`, numbered from the most significant bit of the first
    // octet, with the trailing bits after the last one set declared unused. A certificate authority
    // needs `keyCertSign` (5) and `cRLSign` (6); an end entity `digitalSignature` (0) and
    // `keyEncipherment` (2).
    val usage: Asn1 =
      if authority then Asn1.BitString(Array.of[Byte](0x06.toByte), 1)
      else Asn1.BitString(Array.of[Byte](0xa0.toByte), 5)

    // `dNSName` is `[2] IMPLICIT IA5String`, which is exactly what an implicit tag is for: the
    // choice is identified by its tag, and the string type never appears on the wire.
    val alternativeNames: Optional[Asn1] =
      if alternatives.isEmpty then Unset else
        val names = alternatives.map: name =>
          Asn1.Tagged(2, false, Asn1.Ia5String(name))

        entry(SubjectAltName, false, Asn1.Sequence(names))

    val identity0 = Asn1.OctetString(keyIdentifier(publicBits.bytes))

    val extensions =
      List
        ( entry(BasicConstraints, true, constraints),
          entry(KeyUsage, true, usage),
          entry(SubjectKeyIdentifier, false, identity0),
          alternativeNames )

    val fields =
      List
        ( Asn1.Tagged(0, true, Asn1.Integer(BigInt(2))),
          Asn1.Integer(serial),
          identifier,
          name,
          period,
          name,
          publicKey,
          Asn1.Tagged(3, true, Asn1.Sequence(List.from(extensions.stdlib.compact))) )

    val tbs: Asn1 = Asn1.Sequence(fields)

    val signed = key.sign(tbs.in[Der].data)

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
    publicKey.digest[Sha2[256]].data.take(20)

  private def instant(instant: Instant over Unix): Asn1 =
    val seconds = Math.floorDiv(instant.long, 1000L)
    if seconds < Generalized then Asn1.UtcTime(seconds) else Asn1.GeneralizedTime(seconds)

case class Certificate(asn1: Asn1):
  // The armored form, which is how certificates are almost always exchanged.
  def pem: Pem = Pem(Pem.Label.Certificate, asn1.in[Der])
