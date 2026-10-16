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

object Dsa:
  given value: [bits <: 512 | 1024 | 2048 | 3072: ValueOf]
  =>  ( crypto: Crypto { def dsa: Crypto.SignatureScheme } )
  =>  Dsa[bits] = Dsa(crypto.dsa)

  given keyAlgorithm: [bits <: 512 | 1024 | 2048 | 3072: ValueOf] => Dsa[bits] is KeyAlgorithm =
    new KeyAlgorithm:
      type Self = Dsa[bits]
      def name: Text = t"DSA-${valueOf[bits]}"
      def sshName: Optional[Text] = Unset
      def algorithm: Optional[Asn1] = Unset

      // The key's size is the size of the prime `p`, the first of the domain parameters.
      def conforms(algorithm: Asn1, material: KeyAlgorithm.Material): Boolean =
        KeyAlgorithm.identifier(algorithm).lay(false): (arcs, parameters) =>
          arcs == KeyAlgorithm.Dsa && (parameters match
            case Asn1.Sequence(Asn1.Integer(p) :: _) => p.bitLength == valueOf[bits]
            case _                                   => false)

class Dsa[bits <: 512 | 1024 | 2048 | 3072: ValueOf](scheme: Crypto.SignatureScheme)
extends Cipher, Signing:
  type Size = bits

  def keySize: bits = valueOf[bits]
  def genKey(): Data = scheme.generateKeyPair(keySize)
  def sign(data: Data, keyData: Data): Data = scheme.sign(data, keyData)
  def privateToPublic(keyData: Data): Data = scheme.privateToPublic(keyData)

  def verify(data: Data, signature: Data, keyData: Data): Boolean =
    scheme.verify(data, signature, keyData)
