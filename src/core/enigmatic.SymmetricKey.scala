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
import prepositional.*

object SymmetricKey:
  given [CipherType <: Cipher] => SymmetricKey[CipherType] is Encodable in Bytes = _.bytes
  def generate[CipherType <: Cipher & Symmetric]()(using cipher: CipherType)
  :     SymmetricKey[CipherType] =

    SymmetricKey(cipher.genKey())

class SymmetricKey[CipherType <: Cipher](private[enigmatic] val bytes: Bytes)
extends PrivateKey[CipherType](bytes):
  def encrypt[ValueType: Encodable in Bytes](value: ValueType)(using CipherType & Encryption)
  :     Bytes =
    public.encrypt(value)

  def verify[ValueType: Encodable in Bytes](value: ValueType, signature: Signature[CipherType])
     (using CipherType & Signing)
  :     Boolean =

    public.verify(value, signature)
