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

import anticipation.*
import contingency.*
import gastronomy.*
import gossamer.*
import monotonous.*
import prepositional.*
import spectacular.*

object PrivateKey:
  def generate[CipherType <: Cipher]()(using cipher: CipherType): PrivateKey[CipherType] =
    PrivateKey(cipher.genKey())

  given [KeyType <: Cipher] => PrivateKey[KeyType] is Showable = key =>
    import alphabets.base64.standard
    t"PrivateKey(${key.privateBytes.digest[Sha2[256]].serialize[Base64]})"

open case class PrivateKey[CipherType <: Cipher](private[enigmatic] val privateBytes: Bytes):
  def public(using cipher: CipherType): PublicKey[CipherType] =
    PublicKey(cipher.privateToPublic(privateBytes))

  def decrypt[ValueType: Decodable in Bytes](bytes: Bytes)(using cipher: CipherType & Encryption)
  :     ValueType raises CryptoError =

    ValueType.decode(cipher.decrypt(bytes, privateBytes))

  def sign[ValueType: Encodable in Bytes](value: ValueType)(using cipher: CipherType & Signing)
  :     Signature[CipherType] =

    Signature(cipher.sign(ValueType.encode(value), privateBytes))

  def pem(reveal: ExposeSecretKey.type): Pem = Pem(PemLabel.PrivateKey, privateBytes)
