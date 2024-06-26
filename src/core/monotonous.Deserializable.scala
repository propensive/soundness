/*
    Monotonous, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package monotonous

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
  def deserialize(value: Text): Bytes

object Deserializable:
  given (using Errant[SerializationError]) => Deserializable in Base64:
    def deserialize(value: Text): Bytes =
      try Base64Decoder.nn.decode(value.s).nn.immutable(using Unsafe)
      catch case _: IllegalArgumentException =>
        abort(SerializationError(t"an invalid BASE-64 character found"))

  given (using alphabet: Alphabet[Base32])(using Errant[SerializationError])
      => Deserializable in Base32:
    def deserialize(text: Text): Bytes =
      val padding: Char = alphabet(32)
      val length = text.where(_ != padding, bidi = Rtl).let(_ + 1).or(text.length)*5/8

      IArray.create[Byte](length): array =>
        def recur(buffer: Int = 0, bits: Int = 0, count: Int = 0, index: Int = 0): Unit =
          if count < length then
            val next: Int = ((buffer << 5) | alphabet.invert(text.s.charAt(index)))
            if bits >= 3 then
              array(count) = ((next >> (bits - 3)) & 0xff).toByte
              recur(next, bits - 3, count + 1, index + 1)
            else recur(next, bits + 5, count, index + 1)

        recur()

  given Deserializable in Hex:
    def deserialize(value: Text): Bytes =
      import java.lang.Character.digit
      val data = Array.fill[Byte](value.length/2)(0)

      (0 until value.length by 2).each: i =>
        data(i/2) = unsafely(((digit(value.at(i).vouch, 16) << 4) + digit(value.at(i + 1).vouch,
            16)).toByte)

      data.immutable(using Unsafe)
