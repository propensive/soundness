                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package monotonous

import anticipation.*
import beneficence.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import vacuous.*

object Serializable:
  def base[base <: Serialization](bits: Int)(using alphabet: Alphabet[base]): Serializable in base =
    new:
      def encode(bytes: Data): Text =
        val mask = (1 << bits) - 1
        val multiple = 8/bits.gcd(8)
        val divisor = bits/bits.gcd(8)

        val length =
          if alphabet.padding then multiple*((bytes.length + divisor - 1)/divisor)
          else (bytes.length*8 + bits - 1)/bits

        val array = new Array[Char](length)

        // A while-loop rather than a recursive def: a closure over the exclusive
        // array would hide it from the statements that follow.
        var current = 0
        var next = 0
        var index = 0
        var loaded = 0

        while index < length do
          if loaded < bits then
            if next < bytes.length then
              current = (current << 8) | (bytes(next) & 0xff)
              next += 1
              loaded += 8
            else
              array(index) = alphabet((current << (bits - loaded)) & mask)
              var filler = index + 1
              while filler < length do
                array(filler) = alphabet(1 << bits)
                filler += 1
              index = length
          else
            array(index) = alphabet((current >>> (loaded - bits)) & mask)
            index += 1
            loaded -= bits

        Text(array.immutable(using Unsafe))

  given binary: Alphabet[Binary] => Serializable in Binary = base(1)
  given quaternary: Alphabet[Quaternary] => Serializable in Quaternary = base(2)
  given octal: Alphabet[Octal] => Serializable in Octal = base(3)
  given hex: Alphabet[Hex] => Serializable in Hex = base(4)
  given base32: Alphabet[Base32] => Serializable in Base32 = base(5)
  given base64: Alphabet[Base64] => Serializable in Base64 = base(6)

trait Serializable extends Findable:
  type Form <: Serialization

  def encode(bytes: Data): Text
