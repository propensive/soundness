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
  def base[BaseType <: Serialization](base: Int)(using alphabet: Alphabet[BaseType])
      (using Errant[SerializationError])
          : Deserializable in BaseType =
    new:
      def deserialize(text: Text): Bytes =
        val padding: Char = if alphabet.padding then alphabet(1 << base) else '\u0000'
        val length = text.where(_ != padding, bidi = Rtl).let(_ + 1).or(text.length)*base/8

        IArray.create[Byte](length): array =>
          def recur(buffer: Int = 0, bits: Int = 0, count: Int = 0, index: Int = 0): Unit =
            if count < length then
              val value: Int = alphabet.invert(index, text.s.charAt(index))
              val next: Int = (buffer << base) | value

              if bits + base >= 8 then
                array(count) = ((next >>> (bits + base - 8)) & 0xff).toByte
                recur(next, bits + base - 8, count + 1, index + 1)
              else recur(next, bits + base, count, index + 1)

          recur()

  given (using Alphabet[Base64],  Errant[SerializationError]) => Deserializable in Base64 = base(6)
  given (using Alphabet[Base32], Errant[SerializationError]) => Deserializable in Base32 = base(5)
  given (using Alphabet[Hex], Errant[SerializationError]) => Deserializable in Hex = base(4)
  given (using Alphabet[Octal], Errant[SerializationError]) => Deserializable in Octal = base(3)
  given (using Alphabet[Binary], Errant[SerializationError]) => Deserializable in Binary = base(1)
