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
package hypotenuse

import language.experimental.genericNumberLiterals

import java.lang.{Integer as JInt, Long as JLong}

import scala.quoted.*

import anticipation.*
import fulminate.*
import proscenium.*
import rudiments.*

object Hypotenuse2:
  private given realm: Realm = realm"hypotenuse"

  def bin(expr: Expr[StringContext]): Macro[AnyVal] =
    import quotes.reflect.*
    val bits = expr.valueOrAbort.parts.head

    bits.indexWhere { ch => ch != '0' && ch != '1' && ch != ' ' }.match
      case -1 => ()

      case index =>
        val startPosition = expr.asTerm.pos

        val position =
          Position
            ( startPosition.sourceFile,
              startPosition.start + index,
              startPosition.start + index + 1 )

        halt(m"a binary value can only contain characters '0' or '1'", position)

    val bits2 = bits.filter(_ != ' ')

    val long: Long = bits2.fuse(0L)((state << 1) + (if next == '1' then 1 else 0))

    bits2.length match
      case 8  => Expr[Byte](long.toByte)
      case 16 => Expr[Short](long.toShort)
      case 32 => Expr[Int](long.toInt)
      case 64 => Expr[Long](long)
      case _  => halt(m"a binary literal must be 8, 16, 32 or 64 bits long")

  def hex(expr: Expr[StringContext]): Macro[IArray[Byte]] =
    import quotes.reflect.*

    val startPosition = expr.asTerm.pos
    val nibbles = expr.valueOrAbort.parts.head
    val nibbles2 = nibbles.map(_.toLower)

    nibbles2.indexWhere: char =>
      !(char >= '0' && char <= '9') && !(char >= 'a' && char <= 'f') && char != ' ' && char != '\n'

    . match
      case -1 => ()

      case index =>
        val position =
          Position
            ( startPosition.sourceFile,
              startPosition.start + index,
              startPosition.start + index + 1 )

        halt(m"${nibbles(index)} is not a valid hexadecimal character", position)

    val nibbles3 = nibbles2.filterNot { ch => ch == ' ' || ch == '\n' }

    if nibbles3.length%2 != 0
    then halt(m"a hexadecimal value must have an even number of digits", Position.ofMacroExpansion)

    val bytes = nibbles3.grouped(2).map(Integer.parseInt(_, 16).toByte).to(List)

    '{IArray.from(${Expr(bytes)})}

  def parseU64(digits: Expr[String]): Macro[Long] = digits.value match
    case None         => '{JLong.parseUnsignedLong($digits)}
    case Some(digits) => Expr(JLong.parseUnsignedLong(digits))

  def parseS64(digits: Expr[String]): Macro[Long] = digits.value match
    case None         => '{JLong.parseLong($digits)}
    case Some(digits) => Expr(JLong.parseLong(digits))

  def parseU32(digits: Expr[String]): Macro[Int] = digits.value match
    case None         => '{JInt.parseUnsignedInt($digits)}
    case Some(digits) => Expr(JInt.parseUnsignedInt(digits))

  def parseS32(digits: Expr[String]): Macro[Int] = digits.value match
    case None         => '{JInt.parseInt($digits)}
    case Some(digits) => Expr(JInt.parseInt(digits))

  def parseU16(digits: Expr[String]): Macro[Short] = digits.value match
    case None => '{JInt.parseInt($digits).toShort}

    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < 0 then halt(m"a U16 may not be less than ${0}")
      if int > 0xffff then halt(m"a U16 may not be greater than ${0xffff}")

      Expr(int.toShort)

  def parseS16(digits: Expr[String]): Macro[Short] = digits.value match
    case None => '{JInt.parseInt($digits).toShort}

    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < Short.MinValue then halt(m"an S16 may not be less than ${Short.MinValue.toInt}")
      if int > Short.MaxValue then halt(m"an S16 may not be greater than ${Short.MaxValue.toInt}")

      Expr(int.toShort)

  def parseU8(digits: Expr[String]): Macro[Byte] = digits.value match
    case None => '{JInt.parseInt($digits).toByte}

    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < 0 then halt(m"a U8 may not be less than ${0}")
      if int > 0xffff then halt(m"a U8 may not be greater than ${0xffff}")

      Expr(int.toByte)

  def parseS8(digits: Expr[String]): Macro[Byte] = digits.value match
    case None => '{JInt.parseInt($digits).toByte}

    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < Byte.MinValue then halt(m"an S8 may not be less than ${Byte.MinValue.toInt}")
      if int > Byte.MaxValue then halt(m"an S8 may not be greater than ${Byte.MaxValue.toInt}")

      Expr(int.toByte)


  def commensurable
    ( expr:        Expr[Boolean],
      bound:       Expr[Int | Double | Char | Byte | Short | Long | Float],
      strict:      Expr[Boolean],
      greaterThan: Expr[Boolean] )
  :   Macro[Boolean] =

    val errorMessage = m"this cannot be written as a range expression"

    val value =
      if greaterThan.valueOrAbort then expr match
        case '{($bound: Int) > ($value: Int)}        => value
        case '{($bound: Int) >= ($value: Int)}       => value
        case '{($bound: Double) > ($value: Double)}  => value
        case '{($bound: Double) >= ($value: Double)} => value
        case '{($bound: Char) > ($value: Char)}      => value
        case '{($bound: Char) >= ($value: Char)}     => value
        case '{($bound: Byte) > ($value: Byte)}      => value
        case '{($bound: Byte) >= ($value: Byte)}     => value
        case '{($bound: Short) > ($value: Short)}    => value
        case '{($bound: Short) >= ($value: Short)}   => value
        case '{($bound: Long) > ($value: Long)}      => value
        case '{($bound: Long) >= ($value: Long)}     => value
        case '{($bound: Float) > ($value: Float)}    => value
        case '{($bound: Float) >= ($value: Float)}   => value
        case _                                       => halt(errorMessage)

      else expr match
        case '{($bound: Int) < ($value: Int)}        => value
        case '{($bound: Int) <= ($value: Int)}       => value
        case '{($bound: Double) < ($value: Double)}  => value
        case '{($bound: Double) <= ($value: Double)} => value
        case '{($bound: Char) < ($value: Char)}      => value
        case '{($bound: Char) <= ($value: Char)}     => value
        case '{($bound: Byte) < ($value: Byte)}      => value
        case '{($bound: Byte) <= ($value: Byte)}     => value
        case '{($bound: Short) < ($value: Short)}    => value
        case '{($bound: Short) <= ($value: Short)}   => value
        case '{($bound: Long) < ($value: Long)}      => value
        case '{($bound: Long) <= ($value: Long)}     => value
        case '{($bound: Float) < ($value: Float)}    => value
        case '{($bound: Float) <= ($value: Float)}   => value
        case _                                       => halt(errorMessage)

    val (lessStrict, less) =
      (if greaterThan.valueOrAbort then (bound, value) else (value, bound)) match
        case ('{$left: Int}, '{$right: Int})       => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Float}, '{$right: Float})   => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Double}, '{$right: Double}) => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Long}, '{$right: Long})     => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Char}, '{$right: Char})     => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Byte}, '{$right: Byte})     => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Short}, '{$right: Short})   => ('{$left < $right}, '{$left <= $right})
        case _                                     => halt(errorMessage)

    '{$expr && ${if strict.valueOrAbort then lessStrict else less}}
