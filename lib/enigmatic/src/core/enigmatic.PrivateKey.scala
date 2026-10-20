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
import gastronomy.*, providers.javaBaseProvider
import gossamer.*
import monotonous.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import KeyAlgorithm.Material

object PrivateKey:
  // Why a key document could not be read as a `PrivateKey` of the requested cipher.
  object Error:
    given communicable: Reason is Communicable =
      case Reason.Malformed(detail)         => m"the key document was malformed: $detail"
      case Reason.Mismatch(expected, found) => m"a $expected key was expected but $found was found"
      case Reason.Encrypted                 => m"the key is protected by a passphrase"
      case Reason.Unsupported(detail)       => m"the key uses an unsupported $detail"
      case Reason.PublicKeyAbsent           => m"the key does not carry its public key"

    enum Reason(val number: Int) extends Clarification:
      case Malformed(detail: Text) extends Reason(1)
      case Mismatch(expected: Text, found: Text) extends Reason(2)
      case Encrypted extends Reason(3)
      case Unsupported(detail: Text) extends Reason(4)
      case PublicKeyAbsent extends Reason(5)

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(597, reason.number)(m"could not read the private key because $reason")

  // A key in PKCS#8 form (RFC 5958's `PrivateKeyInfo`), checked against the cipher the caller
  // named: the document's algorithm identifier and the key material's size must be those of
  // `cipher`, or the result would be a `PrivateKey[Rsa[2048]]` over a 4096-bit key. A key whose
  // public half is neither carried in the document nor derivable from the private half (an EC,
  // Ed25519 or ML-DSA key written without its optional public-key field) is rejected here, since
  // `public` would otherwise fail later.
  //
  // The decoded bytes are copied into the cloak, which zeroes the copy it is handed; the `Data`
  // itself, and the ASN.1 values and base64 text the key was read through, are immutable and
  // cannot be zeroed, so a key read from text is, like a `Password` made from `Text`, only ever
  // as private as the heap it was read into.
  given derDecodable: [cipher <: Cipher: KeyAlgorithm]
  =>  ( cloak: Cloak^, tactic: Tactic[PrivateKey.Error]^, diagnostics: Diagnostics )
  =>  ( ((PrivateKey[cipher]^{cloak}) is Decodable in Der)^{cloak, tactic, caps.any} ) =
    der => adopt[cipher](der.data)

  // A key in any of the armored forms: PKCS#8 (`PRIVATE KEY`), PKCS#1 (`RSA PRIVATE KEY`), SEC1
  // (`EC PRIVATE KEY`) or OpenSSH's own container (`OPENSSH PRIVATE KEY`), each converted to PKCS#8
  // for the provider. A passphrase-protected key is reported as `Encrypted`.
  given pemDecodable: [cipher <: Cipher: KeyAlgorithm]
  =>  ( cloak: Cloak^, tactic: Tactic[PrivateKey.Error]^, diagnostics: Diagnostics )
  =>  ( ((PrivateKey[cipher]^{cloak}) is Decodable in Pem)^{cloak, tactic, caps.any} ) =
    pem => pem.label match
      case Pem.Label.PrivateKey           => adopt[cipher](pem.data)
      case Pem.Label.RsaPrivateKey        => adopt[cipher](Pkcs8.fromPkcs1(pem.data))
      case Pem.Label.EcPrivateKey         => adopt[cipher](Pkcs8.fromSec1(pem.data))
      case Pem.Label.OpensshPrivateKey    => adopt[cipher](OpenSsh.privateKey(pem.data))
      case Pem.Label.EncryptedPrivateKey  => abort(PrivateKey.Error(Error.Reason.Encrypted))

      case other =>
        val found = t"a ${Pem.Label.showable.text(other).lower} block"
        abort(PrivateKey.Error(Error.Reason.Mismatch(t"private", found)))

  private def adopt[cipher <: Cipher](data: Data)
    ( using algorithm: cipher is KeyAlgorithm, cloak: Cloak^ )
    ( using Tactic[PrivateKey.Error], Diagnostics )
  :   PrivateKey[cipher]^{cloak} =

    Pkcs8.unwrap(data) match
      case Pkcs8(_, identifier, octets, rest) =>
        val material = Material.Private(octets)

        if !algorithm.conforms(identifier, material)
        then abort:
          PrivateKey.Error:
            Error.Reason.Mismatch(algorithm.name, KeyAlgorithm.describe(identifier, material))

        if !Pkcs8.carriesPublicKey(identifier, octets, rest)
        then abort(PrivateKey.Error(Error.Reason.PublicKeyAbsent))

        PrivateKey(data)

  def generate[cipher <: Cipher]()(using cipher: cipher, cloak: Cloak^)
  :   PrivateKey[cipher]^{cloak} =

    PrivateKey(cipher.genKey())

  // Adopts freshly-generated key material. `Cloak.cloak` zeroes what it is given, so it
  // gets an exclusive copy: a frozen `Data` cannot be written through (see `SymmetricKey`).
  private[enigmatic] def apply[cipher <: Cipher](data: Data)(using cloak: Cloak^)
  :   PrivateKey[cipher]^{cloak} =

    val material: Array[Byte]^ = Array.allocate[Byte](data.length)
    material.place(data)
    new PrivateKey(cloak.cloak(material.raw))

  // Redacted. `showable` uncloaks the key to fingerprint it; an inspection is produced in far
  // more places (test output, a debugger's variable pane, a nested rendering of an enclosing
  // value), so it reveals nothing of the key material at all, not even a digest of it.
  given inspectable: [key <: PrivateKey[?]] => key is Inspectable =
    _ => t"PrivateKey(•••)"

  given showable: [key <: Cipher] => PrivateKey[key] is Showable = key =>
    import alphabets.base64Standard

    key.secret.uncloak: bytes =>
      t"PrivateKey(${Array.unsafeFrozen(bytes).digest[Sha2[256]].serialize[Base64]})"

// A private key held opaquely by whichever `Cloak` was in scope at construction, capturing
// that cloak. Operations that need the key material — `public`, `sign`, `pem` — materialize
// it transiently through the cloak, and the transient copy is zeroed as soon as the
// operation completes; only `pem`, gated on `Divulgence`, lets the material escape.
class PrivateKey[cipher <: Cipher](private[enigmatic] val secret: Secret^):
  def public(using cipher: cipher): PublicKey[cipher] =
    secret.uncloak: bytes => PublicKey(cipher.privateToPublic(Array.unsafeFrozen(bytes)))


  def sign[encodable: Encodable in Data](value: encodable)
    ( using cipher: cipher & Signing, erased weakness: Permit[Weakness[cipher]] )
  :   Signature[cipher] =

    secret.uncloak: bytes =>
      Signature(cipher.sign(encodable.encode(value), Array.unsafeFrozen(bytes)))


  // The immutable `Data` in the result outlives the cloak's zeroing, which is exactly why
  // revealing it demands the explicit `Divulgence` token.
  def pem(reveal: Divulgence.type): Pem = secret.uncloak: bytes =>
    Pem(Pem.Label.PrivateKey, Array.unsafeFrozen(bytes.clone))
