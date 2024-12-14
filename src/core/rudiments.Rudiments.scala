/*
    Rudiments, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import language.experimental.captureChecking

import scala.quoted.*

import anticipation.*
import fulminate.*
import symbolism.*
import prepositional.*
import vacuous.*

object Rudiments:
  given Realm = realm"rudiments"
  opaque type Memory = Long
  opaque type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

  @annotation.targetName("And")
  object `&`:
    def unapply[ValueType](value: ValueType): Some[(ValueType, ValueType)] = Some((value, value))

  object Digit:
    def apply(char: Char): Optional[Digit] = apply(char - '0')

    def apply(int: Int): Optional[Digit] = int match
      case digit: Digit => digit
      case _            => Unset

  extension (digit: Digit)
    def int: Int = digit
    def char: Char = ('0' + int).toChar

  object Memory:
    def apply(long: Long): Memory = long
    given ("content-length" is GenericHttpRequestParam[Memory]) = _.long.toString.tt
    given ordering: Ordering[Memory] = Ordering.Long.on(_.long)
    given communicable[MemoryType <: Memory]: (Communicable { type Self = MemoryType }) = memory => Message(memory.text)

    given Memory is Addable by Memory into Memory as addable = _ + _
    given Memory is Subtractable by Memory into Memory as subtractable = _ - _
    given Memory is Multiplicable by Int into Memory as multiplicable = _*_
    given Memory is Divisible by Int into Memory as divisible = _/_

    extension (left: Memory)
      def long: Long = left
      def text: Text = (left.toString+" bytes").tt

  def bin(expr: Expr[StringContext])(using Quotes): Expr[AnyVal] =
    import quotes.reflect.*
    val bits = expr.valueOrAbort.parts.head

    bits.indexWhere { ch => ch != '0' && ch != '1' && ch != ' ' }.match
      case -1  => ()

      case idx =>
        val startPos = expr.asTerm.pos
        val pos = Position(startPos.sourceFile, startPos.start + idx, startPos.start + idx + 1)
        abandon(m"a binary value can only contain characters '0' or '1'", pos)

    val bits2 = bits.filter(_ != ' ')

    val long: Long = bits2.foldLeft(0L): (acc, next) =>
      (acc << 1) + (if next == '1' then 1 else 0)

    bits2.length match
      case 8  => Expr[Byte](long.toByte)
      case 16 => Expr[Short](long.toShort)
      case 32 => Expr[Int](long.toInt)
      case 64 => Expr[Long](long)
      case _  => abandon(m"a binary literal must be 8, 16, 32 or 64 bits long")

  def hex(expr: Expr[StringContext])(using Quotes): Expr[IArray[Byte]] =
    import quotes.reflect.*

    val startPos = expr.asTerm.pos
    val nibbles = expr.valueOrAbort.parts.head
    val nibbles2 = nibbles.map(_.toLower)

    nibbles2.indexWhere: char =>
      !(char >= '0' && char <= '9') && !(char >= 'a' && char <= 'f') && char != ' ' && char != '\n'

    . match
        case -1  => ()

        case idx =>
          val pos = Position(startPos.sourceFile, startPos.start + idx, startPos.start + idx + 1)
          abandon(m"${nibbles(idx)} is not a valid hexadecimal character", pos)

    val nibbles3 = nibbles2.filterNot { ch => ch == ' ' || ch == '\n' }

    if nibbles3.length%2 != 0
    then abandon(m"a hexadecimal value must have an even number of digits", Position.ofMacroExpansion)

    val bytes = nibbles3.grouped(2).map(Integer.parseInt(_, 16).toByte).to(List)

    '{IArray.from(${Expr(bytes)})}

export Rudiments.{Memory, Digit}
