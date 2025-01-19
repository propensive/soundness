/*
    Monotonous, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import prepositional.*
import rudiments.*

object Serializable:
  def base[BaseType <: Serialization](bits: Int)(using alphabet: Alphabet[BaseType])
          : Serializable in BaseType = new:

    def encode(bytes: Bytes): Text =
      val mask = (1 << bits) - 1

      val length = if alphabet.padding then (bytes.length + bits - 1)/bits*8 else
        (8*bytes.length + bits - 1)/bits

      val chars = IArray.create[Char](length): array =>
        def recur(current: Int = 0, next: Int = 0, index: Int = 0, loaded: Int = 0): Unit =
          if index < length then
            if loaded < bits then
              if next < bytes.length then
                recur((current << 8) | (bytes(next) & 0xff), next + 1, index, loaded + 8)
              else
                array(index) = alphabet((current << (bits - loaded)) & mask)
                ((index + 1) until length).each { i => array(i) = alphabet(1 << bits) }
            else
              array(index) = alphabet((current >>> (loaded - bits)) & mask)
              recur(current, next, index + 1, loaded - bits)

        recur()

      Text(chars)


  given Alphabet[Binary] => Serializable in Binary = base(1)
  given Alphabet[Quaternary] => Serializable in Quaternary = base(2)
  given Alphabet[Octal] => Serializable in Octal = base(3)
  given Alphabet[Hex] => Serializable in Hex = base(4)
  given Alphabet[Base32] => Serializable in Base32 = base(5)
  given Alphabet[Base64] => Serializable in Base64 = base(6)
  given Alphabet[Base256] => Serializable in Base256 = base(8)

trait Serializable:
  type Format <: Serialization
  def encode(bytes: Bytes): Text
