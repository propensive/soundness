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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gastronomy.ProcessingPermit
import gastronomy.Signing
import gossamer.*
import monotonous.*
import prepositional.*
import spectacular.*
import vacuous.*

import KeyAlgorithm.Material

object PublicKey:
  // Why a key document could not be read as a `PublicKey` of the requested cipher.
  object Error:
    given communicable: Reason is Communicable =
      case Reason.Malformed(detail)         => m"the key document was malformed: $detail"
      case Reason.Mismatch(expected, found) => m"a $expected key was expected but $found was found"
      case Reason.Unsupported(detail)       => m"the key uses an unsupported $detail"

    enum Reason(val number: Int) extends Clarification:
      case Malformed(detail: Text) extends Reason(1)
      case Mismatch(expected: Text, found: Text) extends Reason(2)
      case Unsupported(detail: Text) extends Reason(3)

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(598, reason.number)(m"could not read the public key because $reason")

  // A key in X.509 `SubjectPublicKeyInfo` form, checked against the cipher the caller named, as
  // `PrivateKey.derDecodable` checks a `PrivateKeyInfo`.
  given derDecodable: [cipher <: Cipher: KeyAlgorithm]
  =>  ( tactic: Tactic[PublicKey.Error]^, diagnostics: Diagnostics )
  =>  ( (PublicKey[cipher] is Decodable in Der)^{tactic, caps.any} ) =
    der => adopt[cipher](der.data)

  // A key armored as `PUBLIC KEY` (SubjectPublicKeyInfo) or `RSA PUBLIC KEY` (PKCS#1's
  // `RSAPublicKey`, which is wrapped). OpenSSH's single-line form is not PEM; it is read with
  // `OpenSsh.publicKey`.
  given pemDecodable: [cipher <: Cipher: KeyAlgorithm]
  =>  ( tactic: Tactic[PublicKey.Error]^, diagnostics: Diagnostics )
  =>  ( (PublicKey[cipher] is Decodable in Pem)^{tactic, caps.any} ) =
    pem => pem.label match
      case Pem.Label.PublicKey    => adopt[cipher](pem.data)
      case Pem.Label.RsaPublicKey => adopt[cipher](Spki.fromPkcs1(pem.data))

      case other =>
        val found = t"a ${Pem.Label.showable.text(other).lower} block"
        abort(PublicKey.Error(Error.Reason.Mismatch(t"public", found)))

  // A public key from its raw form — the content of a SubjectPublicKeyInfo's BIT STRING, which
  // is what a wire protocol such as SSH or TLS carries: 32 bytes for X25519 or Ed25519, an
  // uncompressed point for an EC key. The algorithm must have a fixed identifier.
  def raw[cipher <: Cipher](bits: Data)(using algorithm: cipher is KeyAlgorithm)
  :   Optional[PublicKey[cipher]] =

    algorithm.algorithm.let(identifier => PublicKey(Spki.wrap(identifier, bits)))

  private[enigmatic] def adopt[cipher <: Cipher](data: Data)
    ( using algorithm: cipher is KeyAlgorithm )
    ( using Tactic[PublicKey.Error], Diagnostics )
  :   PublicKey[cipher] =

    Spki.unwrap(data) match
      case Spki(identifier, bits) =>
        val material = Material.Public(bits)

        if !algorithm.conforms(identifier, material)
        then abort:
          PublicKey.Error:
            Error.Reason.Mismatch(algorithm.name, KeyAlgorithm.describe(identifier, material))

        PublicKey(data)

  given showable: [key <: Cipher] => PublicKey[key] is Showable = key =>
    import alphabets.hexLowerCase
    t"PublicKey(${key.bytes.serialize[Hex]})"

  given encodable: [cipher <: Cipher] => PublicKey[cipher] is Encodable in Data = _.bytes

  // The same hexadecimal `showable` produces, but rendered without needing an `Alphabet[Hex]`
  // in scope: a debug rendering must always be available. A public key is not a secret, so it
  // is shown in full.
  given inspectable: [key <: PublicKey[?]] => key is Inspectable = key =>
    t"PublicKey(${Inspection.hex(key.bytes)})"

case class PublicKey[cipher <: Cipher](bytes: Data):
  def verify[encodable: Encodable in Data](value: encodable, signature: Signature[cipher])
    ( using algorithm: cipher & Signing, erased weakness: ProcessingPermit[Weakness[cipher]] )
  :   Boolean =

    algorithm.verify(encodable.encode(value), signature.bytes, bytes)


  def pem: Pem = Pem(Pem.Label.PublicKey, bytes)

  // The raw key — the SubjectPublicKeyInfo's BIT STRING content — or `Unset` if the document is
  // not one; the inverse of `PublicKey.raw`.
  def raw: Optional[Data] = safely(Der(bytes).as[Asn1]) match
    case Asn1.Sequence(List(_, Asn1.BitString(bits, 0))) => bits
    case _                                               => Unset
