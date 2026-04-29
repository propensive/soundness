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
┃    Soundness, version 0.54.0.                                                                    ┃
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

        Text(array.immutable(using Unsafe))

  given binary: Alphabet[Binary] => Serializable in Binary = base(1)
  given quaternary: Alphabet[Quaternary] => Serializable in Quaternary = base(2)
  given octal: Alphabet[Octal] => Serializable in Octal = base(3)
  given hex: Alphabet[Hex] => Serializable in Hex = base(4)
  given base32: Alphabet[Base32] => Serializable in Base32 = base(5)
  given base64: Alphabet[Base64] => Serializable in Base64 = base(6)
  given base256: Alphabet[Base256] => Serializable in Base256 = base(8)

trait Serializable:
  type Form <: Serialization

  def encode(bytes: Data): Text
