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
import gossamer.*
import monotonous.*
import prepositional.*
import spectacular.*

object PublicKey:
  given [KeyType <: Cipher] => PublicKey[KeyType] is Showable = key =>
    import alphabets.hex.lowerCase
    t"PublicKey(${key.bytes.serialize[Hex]})"

  given [CipherType <: Cipher] => PublicKey[CipherType] is Encodable in Bytes = _.bytes

case class PublicKey[CipherType <: Cipher](bytes: Bytes):
  def encrypt[ValueType: Encodable in Bytes](value: ValueType)
     (using algorithm: CipherType & Encryption)
  :     Bytes =

    algorithm.encrypt(value.bytestream, bytes)

  def verify[ValueType: Encodable in Bytes](value: ValueType, signature: Signature[CipherType])
     (using algorithm: CipherType & Signing)
  :     Boolean =

    algorithm.verify(ValueType.encode(value), signature.bytes, bytes)

  def pem: Pem = Pem(PemLabel.PublicKey, bytes)
