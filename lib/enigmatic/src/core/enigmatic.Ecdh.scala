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

import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import gossamer.*
import vacuous.*

// ECDH over a NIST prime curve, chosen by key size as for `Ecdsa` (256 is P-256, 384 P-384 and
// 521 P-521): the key agreement of SSH's `ecdh-sha2-nistp*` and TLS's `secp*r1` groups. The
// keys are ordinary EC keys — a `PrivateKey[Ecdh[256]]` and a `PrivateKey[Ecdsa[256]]` are the
// same document — but using one key for both agreement and signing is a mistake the distinct
// types prevent. Reached through a structural refinement of the provider, like `Ecdsa`.
object Ecdh:
  given value: [bits <: 256 | 384 | 521: ValueOf]
  =>  ( crypto: Crypto { def ecdh: Crypto.KeyAgreement } )
  =>  Ecdh[bits] =
    Ecdh(crypto.ecdh)

  given keyAlgorithm: [bits <: 256 | 384 | 521: ValueOf] => Ecdh[bits] is KeyAlgorithm =
    new KeyAlgorithm:
      type Self = Ecdh[bits]
      def name: Text = t"ECDH P-${valueOf[bits]}"
      def sshName: Optional[Text] = Unset

      def algorithm: Optional[Asn1] =
        Asn1.Sequence
          ( List(Asn1.ObjectId(KeyAlgorithm.Ec), Asn1.ObjectId(KeyAlgorithm.curve(valueOf[bits]))) )

      def conforms(algorithm: Asn1, material: KeyAlgorithm.Material): Boolean =
        KeyAlgorithm.identifier(algorithm).lay(false): (arcs, parameters) =>
          arcs == KeyAlgorithm.Ec && parameters == Asn1.ObjectId(KeyAlgorithm.curve(valueOf[bits]))

class Ecdh[bits <: 256 | 384 | 521: ValueOf](scheme: Crypto.KeyAgreement)
extends Cipher, Agreeing:
  type Size = bits

  def keySize: bits = valueOf[bits]
  def genKey(): Data = scheme.generateKeyPair(keySize)
  def privateToPublic(keyData: Data): Data = scheme.privateToPublic(keyData)
  def agree(privateKey: Data, publicKey: Data): Data = scheme.agree(privateKey, publicKey)
