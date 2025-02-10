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

import javax.crypto as jc, javax.crypto.spec.*

import anticipation.*
import rudiments.*
import vacuous.*

object Aes:
  given [BitsType <: 128 | 192 | 256: ValueOf] => Aes[BitsType] = Aes()

class Aes[BitsType <: 128 | 192 | 256: ValueOf]() extends Cipher, Encryption, Symmetric:
  type Size = BitsType
  def keySize: BitsType = valueOf[BitsType]

  private def init() = jc.Cipher.getInstance("AES/ECB/PKCS5Padding")
  private def makeKey(key: Bytes): SecretKeySpec = SecretKeySpec(key.mutable(using Unsafe), "AES")

  def encrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(jc.Cipher.ENCRYPT_MODE, makeKey(key))
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def decrypt(bytes: Bytes, key: Bytes): Bytes =
    val cipher = init().nn
    cipher.init(jc.Cipher.DECRYPT_MODE, makeKey(key))
    cipher.doFinal(bytes.mutable(using Unsafe)).nn.immutable(using Unsafe)

  def genKey(): Bytes =
    val keyGen = jc.KeyGenerator.getInstance("AES").nn
    keyGen.init(keySize)

    keyGen.generateKey().nn.getEncoded.nn.immutable(using Unsafe)

  def privateToPublic(key: Bytes): Bytes = key
