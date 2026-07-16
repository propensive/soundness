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

import scala.collection.*

import anticipation.*
import beneficence.*
import contingency.*
import denominative.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import vacuous.*

object Deserializable:
  def base[base <: Serialization](base: Int)(using alphabet: Alphabet[base])
  :   Deserializable in base =

    new:
      override protected val atomicity = 8.lcm(base)/base

      def deserialize(previous: Text, text: Text, index0: Int, last: Boolean)
      :   Data raises SerializationError =

        val padding: Char = if alphabet.padding then alphabet(1 << base) else '\u0000'

        val length =
          if last then text.pinpoint(_ != padding, bidi = Rtl).let(_.n0 + 1).or(text.length)*base/8
          else ((text.length - index0)/atomicity)*atomicity*base/8

        IArray.build[Byte](length): array =>
          var source = if index0 < 0 then previous else text

          def recur(buffer: Int = 0, bits: Int = 0, count: Int = 0, index0: Int = 0): Unit =
            val index = if index0 >= 0 then index0 else index0 + source.length
            if index == 0 then source = text

            if count < length then
              val value: Int = alphabet.invert(index, source.s.charAt(index))
              val next: Int = (buffer << base) | value

              if bits + base >= 8 then
                array(count) = ((next >>> (bits + base - 8)) & 0xff).toByte
                recur(next, bits + base - 8, count + 1, index0 + 1)
              else
                recur(next, bits + base, count, index0 + 1)

          recur(index0 = index0)

  given base64: Alphabet[Base64] => Deserializable in Base64 = base(6)
  given base32: Alphabet[Base32] => Deserializable in Base32 = base(5)
  given hex: Alphabet[Hex] => Deserializable in Hex = base(4)
  given octal: Alphabet[Octal] => Deserializable in Octal = base(3)
  given quaternary: Alphabet[Quaternary] => Deserializable in Quaternary = base(2)
  given binary: Alphabet[Binary] => Deserializable in Binary = base(1)

// `caps.Pure` directly (not `Typeclass.Pure`) because `Deserializable` has no `Self`: it is
// selected by its `Form` member alone. Error handling is per-call (`raises`/`Tactic` method
// parameters), so instances themselves capture nothing.
trait Deserializable extends Findable, caps.Pure:
  type Form <: Serialization

  protected val atomicity: Int = 1

  def deserialize(previous: Text, current: Text, index0: Int, last: Boolean)
  :   Data raises SerializationError

  def deserialize(value: Text)(using Tactic[SerializationError]): Data =
    deserialize(t"", value, 0, true)

  def deserialize(stream: LazyList[Text])(using Tactic[SerializationError]): LazyList[Data] =
    def recur(stream: LazyList[Text], previous: Text, carry: Int): LazyList[Data] = stream match
      case head #:: tail =>
        val carry2 = (carry + head.length)%atomicity
        deserialize(previous, head, -carry, tail.nil) #:: recur(tail, head, carry2)

      case _ =>
        if carry > 0 then LazyList(deserialize(previous, t"", -carry, true)) else LazyList()

    recur(stream, t"", 0)
