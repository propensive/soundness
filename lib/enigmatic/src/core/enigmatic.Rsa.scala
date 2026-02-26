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

import java.security as js, js.spec as jss, js.interfaces as jsi
import javax.crypto as jc

import anticipation.*
import contingency.*
import fulminate.*
import rudiments.*
import vacuous.*

object Rsa:
  given value: [bits <: 1024 | 2048: ValueOf] => Rsa[bits] = Rsa()

class Rsa[bits <: 1024 | 2048: ValueOf]() extends Cipher, Encryption:
  type Size = bits

  def keySize: bits = valueOf[bits]

  def privateToPublic(bytes: Data): Data =
    val javaKey = keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(unsafely(bytes.mutable))).nn

    val key = javaKey match
      case key: jsi.RSAPrivateCrtKey => key
      case key: js.PrivateKey        => panic(m"unexpected private key type")

    val spec = jss.RSAPublicKeySpec(key.getModulus, key.getPublicExponent)
    keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  def decrypt(bytes: Data, key: Data): Data =
    val cipher = init()

    val privateKey =
      keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(key.mutable(using Unsafe)))

    cipher.init(jc.Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def encrypt(bytes: Data, key: Data): Data =
    val cipher = init()
    val publicKey = keyFactory().generatePublic(jss.X509EncodedKeySpec(key.mutable(using Unsafe)))
    cipher.init(jc.Cipher.ENCRYPT_MODE, publicKey)
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def genKey(): Data =
    val generator = js.KeyPairGenerator.getInstance("RSA").nn
    generator.initialize(keySize)
    val keyPair = generator.generateKeyPair().nn
    keyPair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

  private def init(): jc.Cipher = jc.Cipher.getInstance("RSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("RSA").nn
