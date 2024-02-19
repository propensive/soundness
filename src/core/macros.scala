/*
    Hypotenuse, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hypotenuse

import fulminate.*

import scala.quoted.*

import java.lang.{Integer as JInt, Long as JLong}

import language.experimental.genericNumberLiterals

object Hypotenuse2:
  given Realm = realm"hypotenuse"

  def parseU64(digits: Expr[String])(using Quotes): Expr[Long] = digits.value match
    case None         => '{JLong.parseUnsignedLong($digits)}
    case Some(digits) => Expr(JLong.parseUnsignedLong(digits))

  def parseI64(digits: Expr[String])(using Quotes): Expr[Long] = digits.value match
    case None         => '{JLong.parseLong($digits)}
    case Some(digits) => Expr(JLong.parseLong(digits))
  
  def parseU32(digits: Expr[String])(using Quotes): Expr[Int] = digits.value match
    case None         => '{JInt.parseUnsignedInt($digits)}
    case Some(digits) => Expr(JInt.parseUnsignedInt(digits))

  def parseI32(digits: Expr[String])(using Quotes): Expr[Int] = digits.value match
    case None         => '{JInt.parseInt($digits)}
    case Some(digits) => Expr(JInt.parseInt(digits))
  
  def parseU16(digits: Expr[String])(using Quotes): Expr[Short] = digits.value match
    case None => '{JInt.parseInt($digits).toShort}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < 0 then fail(msg"a U16 may not be less than ${0}")
      if int > 0xffff then fail(msg"a U16 may not be greater than ${0xffff}")
      
      Expr(int.toShort)

  def parseI16(digits: Expr[String])(using Quotes): Expr[Short] = digits.value match
    case None => '{JInt.parseInt($digits).toShort}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < Short.MinValue then fail(msg"an I16 may not be less than ${Short.MinValue.toInt}")
      if int > Short.MaxValue then fail(msg"an I16 may not be greater than ${Short.MaxValue.toInt}")
      
      Expr(int.toShort)
  
  def parseU8(digits: Expr[String])(using Quotes): Expr[Byte] = digits.value match
    case None => '{JInt.parseInt($digits).toByte}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < 0 then fail(msg"a U8 may not be less than ${0}")
      if int > 0xffff then fail(msg"a U8 may not be greater than ${0xffff}")
      
      Expr(int.toByte)

  def parseI8(digits: Expr[String])(using Quotes): Expr[Byte] = digits.value match
    case None => '{JInt.parseInt($digits).toByte}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < Byte.MinValue then fail(msg"an I8 may not be less than ${Byte.MinValue.toInt}")
      if int > Byte.MaxValue then fail(msg"an I8 may not be greater than ${Byte.MaxValue.toInt}")
      
      Expr(int.toByte)
  
  def inequality
      (expr: Expr[Boolean], bound: Expr[Int | Double | Char | Byte | Short | Long | Float],
          strict: Expr[Boolean], greaterThan: Expr[Boolean])
      (using Quotes)
      : Expr[Boolean] =
    
    val errorMessage = msg"this cannot be written as a range expression"
    
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
        case _                                       => fail(errorMessage)
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
        case _                                       => fail(errorMessage)
    
    val (lessStrict, less) =
      (if greaterThan.valueOrAbort then (bound, value) else (value, bound)) match
        case ('{$left: Int}, '{$right: Int})       => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Float}, '{$right: Float})   => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Double}, '{$right: Double}) => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Long}, '{$right: Long})     => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Char}, '{$right: Char})     => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Byte}, '{$right: Byte})     => ('{$left < $right}, '{$left <= $right})
        case ('{$left: Short}, '{$right: Short})   => ('{$left < $right}, '{$left <= $right})
        case _                                      => fail(errorMessage)

    '{$expr && ${if strict.valueOrAbort then lessStrict else less}}