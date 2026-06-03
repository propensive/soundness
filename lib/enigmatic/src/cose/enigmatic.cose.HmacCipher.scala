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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package cose

import javax.crypto.spec.SecretKeySpec

import anticipation.*
import enigmatic.*
import gastronomy.*
import prepositional.*
import rudiments.*
import vacuous.*

// Adapter that exposes HMAC as a `Cipher & Signing & Symmetric`, so that
// `SymmetricKey[HmacCipher[Sha2[256]]]` plugs into the existing
// `PrivateKey`/`SymmetricKey` machinery in enigmatic.core. Avoids a refactor
// of `enigmatic.Hmac` (which is an output type, not a cipher).
object HmacCipher:
  given value: [algorithm <: Algorithm] => (hash: Hash in algorithm) => HmacCipher[algorithm] =
    HmacCipher[algorithm]()

class HmacCipher[algorithm <: Algorithm]()(using hash: Hash in algorithm)
extends Cipher, Signing, Symmetric:
  type Size = 256
  def keySize: 256 = 256

  def genKey(): Data =
    val random = java.security.SecureRandom()
    val bytes = new Array[Byte](32)
    random.nextBytes(bytes)
    bytes.immutable(using Unsafe)

  def privateToPublic(key: Data): Data = key

  def sign(data: Data, keyData: Data): Data =
    val mac = hash.hmac0
    mac.init(SecretKeySpec(keyData.to[Array], hash.name.s))
    mac.doFinal(data.to[Array]).nn.immutable(using Unsafe)

  def verify(data: Data, signature: Data, keyData: Data): Boolean =
    val expected = sign(data, keyData)
    constantTimeEquals(expected, signature)

  private def constantTimeEquals(a: Data, b: Data): Boolean =
    if a.length != b.length then false else
      var diff = 0
      var index = 0
      while index < a.length do
        diff |= (a(index) ^ b(index)) & 0xFF
        index += 1
      diff == 0
