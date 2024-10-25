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
import gossamer.*
import rudiments.*
import prepositional.*
import hypotenuse.*
import vacuous.*

trait Deserializable:
  type Format <: Serialization
  protected val atomicity: Int = 1

  def deserialize(previous: Text, current: Text, index0: Int, last: Boolean): Bytes
  def deserialize(value: Text): Bytes = deserialize(t"", value, 0, true)

  def deserialize(stream: LazyList[Text]): LazyList[Bytes] =
    def recur(stream: LazyList[Text], previous: Text, carry: Int): LazyList[Bytes] = stream match
      case head #:: tail =>
        println(s"   carry = $carry")
        val carry2 = (carry + head.length)%atomicity
        deserialize(previous, head, -carry, tail.isEmpty) #:: recur(tail, head, carry2)

      case _ =>
        println(s"   carry = $carry")
        if carry > 0 then LazyList(deserialize(previous, t"", -carry, true)) else LazyList()

    recur(stream, t"", 0)

object Deserializable:
  def base[BaseType <: Serialization](base: Int)(using alphabet: Alphabet[BaseType])
          : Deserializable in BaseType raises SerializationError =
    new:
      override protected val atomicity = 8.lcm(base)/base

      def deserialize(previous: Text, text: Text, index0: Int, last: Boolean): Bytes =
        //println(s"deserialize(previous.length=${previous.length}, text.length=${text.length}, index0=$index0, last=$last)")
        val padding: Char = if alphabet.padding then alphabet(1 << base) else '\u0000'

        val length =
          if last then text.where(_ != padding, bidi = Rtl).let(_.n0 + 1).or(text.length)*base/8
          else ((text.length - index0)/atomicity)*atomicity*base/8

        println(s"   length = $length")

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
                println(t"  array($count) = ${array(count)}")
                recur(next, bits + base - 8, count + 1, index0 + 1)
              else recur(next, bits + base, count, index0 + 1)

          recur(index0 = index0)

  given (using Alphabet[Base64], Tactic[SerializationError]) => Deserializable in Base64 = base(6)
  given (using Alphabet[Base32], Tactic[SerializationError]) => Deserializable in Base32 = base(5)
  given (using Alphabet[Hex], Tactic[SerializationError]) => Deserializable in Hex = base(4)
  given (using Alphabet[Octal], Tactic[SerializationError]) => Deserializable in Octal = base(3)
  given (using Alphabet[Quaternary], Tactic[SerializationError]) => Deserializable in Quaternary = base(2)
  given (using Alphabet[Binary], Tactic[SerializationError]) => Deserializable in Binary = base(1)
