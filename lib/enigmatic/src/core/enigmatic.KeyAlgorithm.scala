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
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

// How a cipher's keys are identified in a key document. A `PrivateKeyInfo` (PKCS#8) and a
// `SubjectPublicKeyInfo` (X.509) both carry an `AlgorithmIdentifier` — an object identifier with
// optional parameters, such as the curve of an EC key — and key material whose size, for RSA and
// DSA, is the only record of the key's strength. Reading a key file into a `PrivateKey[Rsa[2048]]`
// has to check both, since the JDK will happily sign with a 4096-bit key handed to it under that
// type, and the type would then be a lie.
//
// `sshName` is the algorithm's name in OpenSSH's key formats (`ssh-ed25519`, `ssh-rsa`,
// `ecdsa-sha2-nistp256`), or `Unset` for an algorithm OpenSSH has no name for.
object KeyAlgorithm:
  // The two halves of a key document which an algorithm checks: the content of a
  // `PrivateKeyInfo`'s `privateKey` OCTET STRING, or of a `SubjectPublicKeyInfo`'s
  // `subjectPublicKey` BIT STRING.
  enum Material:
    case Private(octets: Data)
    case Public(bits: Data)

  // The algorithm arcs this module knows, for recognition and for naming what was found.
  val Rsa: List[Int] = List(1, 2, 840, 113549, 1, 1, 1)
  val Ec: List[Int] = List(1, 2, 840, 10045, 2, 1)
  val Ed25519: List[Int] = List(1, 3, 101, 112)
  val X25519: List[Int] = List(1, 3, 101, 110)
  val Dsa: List[Int] = List(1, 2, 840, 10040, 4, 1)

  // One identifier per ML-DSA parameter set (RFC 9881).
  def MlDsa(level: Int): List[Int] = level match
    case 44 => List(2, 16, 840, 1, 101, 3, 4, 3, 17)
    case 65 => List(2, 16, 840, 1, 101, 3, 4, 3, 18)
    case _  => List(2, 16, 840, 1, 101, 3, 4, 3, 19)

  val P256: List[Int] = List(1, 2, 840, 10045, 3, 1, 7)
  val P384: List[Int] = List(1, 3, 132, 0, 34)
  val P521: List[Int] = List(1, 3, 132, 0, 35)

  def curve(bits: Int): List[Int] = bits match
    case 256 => P256
    case 384 => P384
    case _   => P521

  // The object identifier and parameters of an `AlgorithmIdentifier`, if it is one.
  def identifier(algorithm: Asn1): Optional[(List[Int], Optional[Asn1])] = algorithm match
    case Asn1.Sequence(List(Asn1.ObjectId(arcs)))             => (arcs, Unset)
    case Asn1.Sequence(List(Asn1.ObjectId(arcs), parameters)) => (arcs, parameters)
    case _                                                    => Unset

  // The modulus of RSA key material, from which the key's size is read: the second element of
  // an `RSAPrivateKey` (after its version) or the first of an `RSAPublicKey`.
  def rsaModulus(material: Material): Optional[BigInt] = material match
    case Material.Private(octets) => safely(Der(octets).as[Asn1]) match
      case Asn1.Sequence(_ :: Asn1.Integer(modulus) :: _)  => modulus
      case _                                               => Unset

    case Material.Public(bits) => safely(Der(bits).as[Asn1]) match
      case Asn1.Sequence(Asn1.Integer(modulus) :: _)  => modulus
      case _                                          => Unset

  // A human-readable name for what a key document carries, for the `Mismatch` error: the known
  // algorithms by name and size, and anything else by its dotted object identifier.
  def describe(algorithm: Asn1, material: Material): Text =
    identifier(algorithm).lay(t"an unrecognized algorithm"): (arcs, parameters) =>
      if arcs == Rsa then rsaModulus(material).lay(t"RSA"): modulus => t"RSA-${modulus.bitLength}"
      else if arcs == Ed25519 then t"Ed25519"
      else if arcs == X25519 then t"X25519"
      else if arcs == Ec then parameters match
        case Asn1.ObjectId(P256) => t"ECDSA P-256"
        case Asn1.ObjectId(P384) => t"ECDSA P-384"
        case Asn1.ObjectId(P521) => t"ECDSA P-521"
        case _                   => t"ECDSA over an unrecognized curve"
      else if arcs == Dsa then parameters match
        case Asn1.Sequence(Asn1.Integer(p) :: _) => t"DSA-${p.bitLength}"
        case _                                   => t"DSA"
      else if arcs == MlDsa(44) then t"ML-DSA-44"
      else if arcs == MlDsa(65) then t"ML-DSA-65"
      else if arcs == MlDsa(87) then t"ML-DSA-87"
      else t"the object identifier ${arcs.map(_.show).join(t".")}"

trait KeyAlgorithm extends Typeclass:
  def name: Text
  def sshName: Optional[Text]

  // The `AlgorithmIdentifier` of a key of this type, for building a key document around raw key
  // material, or `Unset` where the identifier carries parameters only the key knows (DSA).
  def algorithm: Optional[Asn1]

  // Whether `algorithm` (an `AlgorithmIdentifier`) names this algorithm, with its parameters, and
  // `material` is key material of this algorithm's size.
  def conforms(algorithm: Asn1, material: KeyAlgorithm.Material): Boolean
