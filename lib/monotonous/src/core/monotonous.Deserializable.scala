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

import scala.collection.*
import scala.compiletime.*, ops.int.*

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

trait Deserializable:
  type Form <: Serialization
  protected val atomicity: Int = 1

  def deserialize(previous: Text, current: Text, index0: Int, last: Boolean): Data
  def deserialize(value: Text): Data = deserialize(t"", value, 0, true)

  def deserialize(stream: Stream[Text]): Stream[Data] =
    def recur(stream: Stream[Text], previous: Text, carry: Int): Stream[Data] = stream match
      case head #:: tail =>
        val carry2 = (carry + head.length)%atomicity
        deserialize(previous, head, -carry, tail.nil) #:: recur(tail, head, carry2)

      case _ =>
        if carry > 0 then Stream(deserialize(previous, t"", -carry, true)) else Stream()

    recur(stream, t"", 0)

object Deserializable:
  def base[base <: Serialization](base: Int)(using alphabet: Alphabet[base])
  :   Deserializable in base raises SerializationError =

      new:
        override protected val atomicity = 8.lcm(base)/base

        def deserialize(previous: Text, text: Text, index0: Int, last: Boolean): Data =
          val padding: Char = if alphabet.padding then alphabet(1 << base) else '\u0000'

          val length =
            if last then text.where(_ != padding, bidi = Rtl).let(_.n0 + 1).or(text.length)*base/8
            else ((text.length - index0)/atomicity)*atomicity*base/8

          IArray.create[Byte](length): array =>
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
                else recur(next, bits + base, count, index0 + 1)

            recur(index0 = index0)


  given base256: (Alphabet[Base256], Tactic[SerializationError]) => Deserializable in Base256 =
    base(8)

  given base64: (Alphabet[Base64], Tactic[SerializationError]) => Deserializable in Base64 = base(6)
  given base32: (Alphabet[Base32], Tactic[SerializationError]) => Deserializable in Base32 = base(5)
  given hex: (Alphabet[Hex], Tactic[SerializationError]) => Deserializable in Hex = base(4)
  given octal: (Alphabet[Octal], Tactic[SerializationError]) => Deserializable in Octal = base(3)


  given quaternary: (Alphabet[Quaternary], Tactic[SerializationError])
  =>  Deserializable in Quaternary =

      base(2)

  given binary: (Alphabet[Binary], Tactic[SerializationError]) => Deserializable in Binary = base(1)
