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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

import PrivateKey.Error.Reason

// The shape of a PKCS#8 `PrivateKeyInfo` (RFC 5958's `OneAsymmetricKey`): a version, an
// `AlgorithmIdentifier`, the key material as an OCTET STRING, and whatever follows — attributes,
// and in version 2 the optional public key. This is the form every `Crypto` provider takes a
// private key in, so each other format a key file may use is converted to it here, and the
// conversions are plain ASN.1 surgery: PKCS#1 and SEC1 documents are *exactly* what a PKCS#8
// wrapper's OCTET STRING holds for an RSA or EC key.
object Pkcs8:
  private val Version: Asn1 = Asn1.Integer(0)

  def unwrap(data: Data)(using Tactic[PrivateKey.Error], Diagnostics): Pkcs8 =
    asn1(data) match
      case Asn1.Sequence(Asn1.Integer(version) :: identifier :: Asn1.OctetString(octets) :: rest) =>
        Pkcs8(version, identifier, octets, rest)

      case _ => abort(PrivateKey.Error(Reason.Malformed(t"the document is not a PrivateKeyInfo")))

  def wrap(identifier: Asn1, octets: Data): Data =
    val info: Asn1 = Asn1.Sequence(List(Version, identifier, Asn1.OctetString(octets)))
    info.in[Der].data

  // A PKCS#1 `RSAPrivateKey` is the key material of a PKCS#8 RSA key, whose identifier is
  // `rsaEncryption` with NULL parameters (RFC 8017 §A.1).
  def fromPkcs1(data: Data)(using Tactic[PrivateKey.Error], Diagnostics): Data =
    asn1(data) match
      case Asn1.Sequence(Asn1.Integer(_) :: Asn1.Integer(_) :: _) =>
        wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Rsa), Asn1.Null)), data)

      case _ => abort(PrivateKey.Error(Reason.Malformed(t"the document is not an RSAPrivateKey")))

  // A SEC1 `ECPrivateKey` (RFC 5915) is the key material of a PKCS#8 EC key, whose identifier
  // names the curve in its parameters — taken from the SEC1 document's own `[0]` field, which
  // RFC 5915 lets a document omit only when the curve is known from elsewhere, as it is not here.
  def fromSec1(data: Data)(using Tactic[PrivateKey.Error], Diagnostics): Data =
    asn1(data) match
      case Asn1.Sequence(Asn1.Integer(1) :: Asn1.OctetString(_) :: rest) =>
        val curve = rest.reap:
          case Asn1.Tagged(0, true, curve: Asn1.ObjectId) => curve

        curve.lay(abort(PrivateKey.Error(Reason.Malformed(t"the EC key names no curve")))): curve =>
          wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ec), curve)), data)

      case _ => abort(PrivateKey.Error(Reason.Malformed(t"the document is not an ECPrivateKey")))

  // Whether `public` can be answered for the key: an RSA or DSA public key is computed from the
  // private one, but an EC, Ed25519 or ML-DSA public key is `d·G`, which no provider recomputes,
  // so it must be carried in the document — inside the `ECPrivateKey` for EC (RFC 5915's `[1]`),
  // and at the top level of the `OneAsymmetricKey` for the others (RFC 5958's `[1]`, in either
  // its explicit or its implicit form; see `JavaBaseCrypto.extractTopLevel`).
  def carriesPublicKey(identifier: Asn1, octets: Data, rest: List[Asn1]): Boolean =
    KeyAlgorithm.identifier(identifier).lay(false): (arcs, _) =>
      if arcs == KeyAlgorithm.Rsa || arcs == KeyAlgorithm.Dsa then true
      else if arcs == KeyAlgorithm.Ec then safely(Der(octets).as[Asn1]) match
        case Asn1.Sequence(elements) => elements.exists:
          case Asn1.Tagged(1, true, _: Asn1.BitString) => true
          case _                                       => false

        case _ => false
      else rest.exists:
        case Asn1.Tagged(1, true, _: Asn1.BitString)   => true
        case Asn1.Unknown(Asn1.Context, 1, false, _)   => true
        case _                                         => false

  // The numbers an RSA private key is made of, as RFC 8017 §A.1.2 names them (and as a JWK
  // spells them `n`, `e`, `d`, `p`, `q`, `dp`, `dq` and `qi`). All eight are needed: a provider
  // signs with the Chinese-remainder form, and derives the public key from the first two.
  case class Rsa
    ( modulus:         BigInt,
      publicExponent:  BigInt,
      privateExponent: BigInt,
      prime1:          BigInt,
      prime2:          BigInt,
      exponent1:       BigInt,
      exponent2:       BigInt,
      coefficient:     BigInt )

  private val RsaIdentifier: Asn1 = Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Rsa), Asn1.Null))

  // A PKCS#8 RSA key from its numbers, and — `Unset` for any other document — the numbers
  // from the key.
  def rsa(key: Rsa): Data =
    val document: Asn1 =
      Asn1.Sequence
        ( List
            ( Asn1.Integer(BigInt(0)),
              Asn1.Integer(key.modulus),
              Asn1.Integer(key.publicExponent),
              Asn1.Integer(key.privateExponent),
              Asn1.Integer(key.prime1),
              Asn1.Integer(key.prime2),
              Asn1.Integer(key.exponent1),
              Asn1.Integer(key.exponent2),
              Asn1.Integer(key.coefficient) ) )

    wrap(RsaIdentifier, document.in[Der].data)

  def rsa(data: Data): Optional[Rsa] = safely(Der(data).as[Asn1]) match
    case Asn1.Sequence(_ :: _ :: Asn1.OctetString(octets) :: _) =>
      safely(Der(octets).as[Asn1]) match
        case Asn1.Sequence
              ( List
                  ( Asn1.Integer(_),
                    Asn1.Integer(modulus),
                    Asn1.Integer(publicExponent),
                    Asn1.Integer(privateExponent),
                    Asn1.Integer(prime1),
                    Asn1.Integer(prime2),
                    Asn1.Integer(exponent1),
                    Asn1.Integer(exponent2),
                    Asn1.Integer(coefficient) ) ) =>
          Rsa
            ( modulus,
              publicExponent,
              privateExponent,
              prime1,
              prime2,
              exponent1,
              exponent2,
              coefficient )

        case _ =>
          Unset

    case _ =>
      Unset

  // A PKCS#8 EC key over the NIST prime curve of `bits` bits, from its private scalar and the
  // uncompressed public point (`04 ‖ x ‖ y`): RFC 5915's `ECPrivateKey`, naming its curve and
  // carrying its public key, which is the form `carriesPublicKey` requires.
  def ec(bits: Int, scalar: Data, point: Data): Data =
    val curve: Asn1 = Asn1.ObjectId(KeyAlgorithm.curve(bits))

    val document: Asn1 =
      Asn1.Sequence
        ( List
            ( Asn1.Integer(BigInt(1)),
              Asn1.OctetString(scalar),
              Asn1.Tagged(0, true, curve),
              Asn1.Tagged(1, true, Asn1.BitString(point, 0)) ) )

    wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ec), curve)), document.in[Der].data)

  // The private scalar of a PKCS#8 EC key, or `Unset` for any other document.
  def scalar(data: Data): Optional[Data] = safely(Der(data).as[Asn1]) match
    case Asn1.Sequence(_ :: _ :: Asn1.OctetString(octets) :: _) =>
      safely(Der(octets).as[Asn1]) match
        case Asn1.Sequence(Asn1.Integer(_) :: Asn1.OctetString(scalar) :: _) => scalar
        case _                                                              => Unset

    case _ =>
      Unset

  // A PKCS#8 key for one of RFC 8410's curves (Ed25519, X25519), from its 32-byte private seed
  // and public key: `identifier` is the curve's object identifier, the key material is the seed
  // as a nested OCTET STRING, and the public key is carried at the top level.
  def seeded(identifier: List[Int], seed: Data, publicKey: Data): Data =
    val inner: Asn1 = Asn1.OctetString(seed)

    val document: Asn1 =
      Asn1.Sequence
        ( List
            ( Asn1.Integer(BigInt(0)),
              Asn1.Sequence(List(Asn1.ObjectId(identifier))),
              Asn1.OctetString(inner.in[Der].data),
              Asn1.Tagged(1, true, Asn1.BitString(publicKey, 0)) ) )

    document.in[Der].data

  // The private seed of such a key, or `Unset` for any other document.
  def seed(data: Data): Optional[Data] = safely(Der(data).as[Asn1]) match
    case Asn1.Sequence(_ :: _ :: Asn1.OctetString(octets) :: _) =>
      safely(Der(octets).as[Asn1]) match
        case Asn1.OctetString(seed) => seed
        case _                      => Unset

    case _ =>
      Unset

  private def asn1(data: Data)(using Tactic[PrivateKey.Error], Diagnostics): Asn1 =
    safely(Der(data).as[Asn1]).or:
      abort(PrivateKey.Error(Reason.Malformed(t"the document is not DER")))

case class Pkcs8(version: BigInt, identifier: Asn1, octets: Data, rest: List[Asn1])

// The shape of an X.509 `SubjectPublicKeyInfo`: an `AlgorithmIdentifier` and the key as a BIT
// STRING, the form every provider takes a public key in.
object Spki:
  def unwrap(data: Data)(using Tactic[PublicKey.Error], Diagnostics): Spki =
    safely(Der(data).as[Asn1]) match
      case Asn1.Sequence(List(identifier, Asn1.BitString(bits, 0))) => Spki(identifier, bits)

      case _ =>
        val reason = PublicKey.Error.Reason.Malformed(t"the document is not a SubjectPublicKeyInfo")
        abort(PublicKey.Error(reason))

  def wrap(identifier: Asn1, bits: Data): Data =
    val info: Asn1 = Asn1.Sequence(List(identifier, Asn1.BitString(bits, 0)))
    info.in[Der].data

  // An RSA public key from its modulus and exponent, and — `Unset` for any other document —
  // the two from the key.
  def rsa(modulus: BigInt, exponent: BigInt): Data =
    val key: Asn1 = Asn1.Sequence(List(Asn1.Integer(modulus), Asn1.Integer(exponent)))
    wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Rsa), Asn1.Null)), key.in[Der].data)

  def rsa(data: Data): Optional[(BigInt, BigInt)] = safely(Der(data).as[Asn1]) match
    case Asn1.Sequence(List(_, Asn1.BitString(bits, 0))) => safely(Der(bits).as[Asn1]) match
      case Asn1.Sequence(List(Asn1.Integer(modulus), Asn1.Integer(exponent))) => (modulus, exponent)
      case _                                                                 => Unset

    case _ =>
      Unset

  // An EC public key over the NIST prime curve of `bits` bits, from its uncompressed point
  // (`04 ‖ x ‖ y`); `PublicKey.raw` is the way back.
  def ec(bits: Int, point: Data): Data =
    val curve: Asn1 = Asn1.ObjectId(KeyAlgorithm.curve(bits))
    wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ec), curve)), point)

  // A PKCS#1 `RSAPublicKey` is the bit string of a SubjectPublicKeyInfo RSA key.
  def fromPkcs1(data: Data)(using Tactic[PublicKey.Error], Diagnostics): Data =
    safely(Der(data).as[Asn1]) match
      case Asn1.Sequence(List(Asn1.Integer(_), Asn1.Integer(_))) =>
        wrap(Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Rsa), Asn1.Null)), data)

      case _ =>
        abort:
          PublicKey.Error(PublicKey.Error.Reason.Malformed(t"the document is not an RSAPublicKey"))

case class Spki(identifier: Asn1, bits: Data)
