/*
    Gastronomy, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gastronomy

import java.util as ju
import ju.Base64.getDecoder as Base64Decoder

import scala.collection.*
import scala.compiletime.*, ops.int.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

trait Deserializable:
  type In <: Serialization
  def decode(value: Text): Bytes

object Deserializable:
  given (using Errant[CryptoError]) => Deserializable in Base64:
    def decode(value: Text): Bytes =
      try Base64Decoder.nn.decode(value.s).nn.immutable(using Unsafe)
      catch case _: IllegalArgumentException => abort(CryptoError(t"an invalid BASE-64 character found"))

  given Deserializable in Hex:
    def decode(value: Text): Bytes =
      import java.lang.Character.digit
      val data = Array.fill[Byte](value.length/2)(0)

      (0 until value.length by 2).each: i =>
        data(i/2) = unsafely(((digit(value.at(i).vouch, 16) << 4) + digit(value.at(i + 1).vouch, 16)).toByte)

      data.immutable(using Unsafe)
