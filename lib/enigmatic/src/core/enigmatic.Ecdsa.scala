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
import gastronomy.Signing

// ECDSA over a NIST prime curve, with the curve chosen by key size: 256 is P-256 (`secp256r1`),
// 384 is P-384 and 521 is P-521. Unlike `Rsa`, ECDSA is not part of the mandatory provider
// baseline, so — like `Dsa` — it is reached through a structural refinement, and a provider that
// does not offer it is a compile error at the use site rather than a failure at run time.
object Ecdsa:
  given value: [bits <: 256 | 384 | 521: ValueOf]
  =>  ( digest: Signature.Digest,
        crypto: Crypto { def ecdsa(digest: Text): Crypto.SignatureScheme } )
  =>  Ecdsa[bits] =
    Ecdsa(crypto.ecdsa(digest.token))

  given keyAlgorithm: [bits <: 256 | 384 | 521: ValueOf] => Ecdsa[bits] is KeyAlgorithm =
    new KeyAlgorithm:
      type Self = Ecdsa[bits]
      def name: Text = t"ECDSA P-${valueOf[bits]}"
      def sshName: Optional[Text] = t"ecdsa-sha2-nistp${valueOf[bits]}"

      def algorithm: Optional[Asn1] =
        Asn1.Sequence
          ( List(Asn1.ObjectId(KeyAlgorithm.Ec), Asn1.ObjectId(KeyAlgorithm.curve(valueOf[bits]))) )

      // The curve is in the parameters; the key material carries nothing the curve does not fix.
      def conforms(algorithm: Asn1, material: KeyAlgorithm.Material): Boolean =
        KeyAlgorithm.identifier(algorithm).lay(false): (arcs, parameters) =>
          arcs == KeyAlgorithm.Ec && parameters == Asn1.ObjectId(KeyAlgorithm.curve(valueOf[bits]))

class Ecdsa[bits <: 256 | 384 | 521: ValueOf](scheme: Crypto.SignatureScheme)
extends Cipher, Signing:
  type Size = bits

  def keySize: bits = valueOf[bits]
  def genKey(): Data = scheme.generateKeyPair(keySize)
  def privateToPublic(keyData: Data): Data = scheme.privateToPublic(keyData)
  def sign(data: Data, keyData: Data): Data = scheme.sign(data, keyData)

  def verify(data: Data, signature: Data, keyData: Data): Boolean =
    scheme.verify(data, signature, keyData)
