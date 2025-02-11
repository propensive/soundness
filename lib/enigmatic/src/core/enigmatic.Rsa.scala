/*
    Enigmatic, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package enigmatic

import javax.crypto as jc
import java.security as js, js.spec as jss, js.interfaces as jsi

import anticipation.*
import contingency.*
import fulminate.*
import rudiments.*
import vacuous.*

object Rsa:
  given [I <: 1024 | 2048: ValueOf] => Rsa[I] = Rsa()

class Rsa[BitsType <: 1024 | 2048: ValueOf]() extends Cipher, Encryption:
  type Size = BitsType
  def keySize: BitsType = valueOf[BitsType]

  def privateToPublic(bytes: Bytes): Bytes =
    val javaKey = keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(unsafely(bytes.mutable))).nn
    val key = javaKey match
      case key: jsi.RSAPrivateCrtKey => key
      case key: js.PrivateKey        => panic(m"unexpected private key type")

    val spec = jss.RSAPublicKeySpec(key.getModulus, key.getPublicExponent)
    keyFactory().generatePublic(spec).nn.getEncoded.nn.immutable(using Unsafe)

  def decrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn

    val privateKey =
      keyFactory().generatePrivate(jss.PKCS8EncodedKeySpec(key.mutable(using Unsafe)))

    cipher.init(jc.Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def encrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    val publicKey = keyFactory().generatePublic(jss.X509EncodedKeySpec(key.mutable(using Unsafe)))
    cipher.init(jc.Cipher.ENCRYPT_MODE, publicKey)
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def genKey(): Bytes =
    val generator = js.KeyPairGenerator.getInstance("RSA").nn
    generator.initialize(keySize)
    val keyPair = generator.generateKeyPair().nn
    keyPair.getPrivate.nn.getEncoded.nn.immutable(using Unsafe)

  private def init(): jc.Cipher = jc.Cipher.getInstance("RSA").nn
  private def keyFactory(): js.KeyFactory = js.KeyFactory.getInstance("RSA").nn
