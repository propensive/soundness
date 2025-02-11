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

import javax.crypto.spec.SecretKeySpec

import scala.compiletime.*, ops.int.*

import anticipation.*
import contingency.*
import gastronomy.*
import prepositional.*
import rudiments.*

extension [ValueType: Encodable in Bytes](value: ValueType)
  def hmac[HashType <: Algorithm](key: Bytes)(using function: HashFunction in HashType)
  :     Hmac in HashType =

    val mac = function.hmac0
    mac.init(SecretKeySpec(key.to(Array), function.name.s))

    Hmac(unsafely(mac.doFinal(ValueType.encode(value).mutable).nn.immutable))
