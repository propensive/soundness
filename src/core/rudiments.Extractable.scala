/*
    Rudiments, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.deriving.*
import scala.reflect.*

import anticipation.*
import prepositional.*
import vacuous.*

infix type isAbove[SelfType, TypeclassType] = TypeclassType { type Self <: SelfType }

object Extractable:
  given optional: [MatchType: Extractable]
  =>    Optional[MatchType] is Extractable into MatchType.Result =
    value => value.let(MatchType.extract(_))

  given irrefutable: [ResultType] => (irrefutable: Text is Irrefutable into ResultType)
  =>    String is Irrefutable into ResultType =
    value => irrefutable.unapply(value.tt)

  given textChar: [TextType <: Text] => TextType is Extractable into Char =
    text => if text.s.length == 1 then text.s.head else Unset

  given textByte: [TextType <: Text] => TextType is Extractable into Byte =
    text => try text.s.toByte catch case _: NumberFormatException => Unset

  given textShort: [TextType <: Text] => TextType is Extractable into Short =
    text => try text.s.toShort catch case _: NumberFormatException => Unset

  given textInt: [TextType <: Text] => TextType is Extractable into Int =
    text => try text.s.toInt catch case e: NumberFormatException => Unset

  given textLong: [TextType <: Text] => TextType is Extractable into Long =
    text => try text.s.toLong catch case e: NumberFormatException => Unset

  given textFloat: [TextType <: Text] => TextType is Extractable into Float = text =>
    try text.s.toFloat catch case e: NumberFormatException => Unset

  given textDouble: [TextType <: Text] => TextType is Extractable into Double = text =>
    try text.s.toDouble catch case _: NumberFormatException => Unset

  given textBoolean: [TextType <: Text] => TextType is Extractable into Boolean = v =>
    if v.s == "true" then true else if v.s == "false" then false else Unset

  given shortByte: [ShortType <: Short] => ShortType is Extractable into Byte =
    short => short.toByte.unless(_.toInt != short)

  given intByte: [IntType <: Int] => IntType is Extractable into Byte =
    int => int.toByte.unless(_.toInt != int)

  given intShort: [IntType <: Int] => IntType is Extractable into Short =
    int => int.toShort.unless(_.toInt != int)

  given intFloat: [IntType <: Int] => IntType is Extractable into Float =
    int => int.toFloat.unless(_.toInt != int)

  given longByte: [LongType <: Long] => LongType is Extractable into Byte =
    long => long.toByte.unless(_.toLong != long)

  given longShort: [LongType <: Long] => LongType is Extractable into Short =
    long => long.toShort.unless(_.toLong != long)

  given longInt: [LongType <: Long] => LongType is Extractable into Int =
    long => long.toInt.unless(_.toLong != long)

  given longFloat: [LongType <: Long] => LongType is Extractable into Float =
    long => long.toFloat.unless(_.toLong != long)

  given longDouble: [LongType <: Long] => LongType is Extractable into Double =
    long => long.toDouble.unless(_.toLong != long)

  given floatByte: [FloatType <: Float] => FloatType is Extractable into Byte =
    float => float.toByte.unless(_.toFloat != float)

  given floatShort: [FloatType <: Float] => FloatType is Extractable into Short =
    float => float.toShort.unless(_.toFloat != float)

  given floatInt: [FloatType <: Float] => FloatType is Extractable into Int =
    float => float.toInt.unless(_.toFloat != float)

  given floatLong: [FloatType <: Float] => FloatType is Extractable into Long =
    float => float.toLong.unless(_.toFloat != float)

  given doubleByte: [DoubleType <: Double] => DoubleType is Extractable into Byte =
    double => double.toByte.unless(_.toDouble != double)

  given doubleShort: [DoubleType <: Double] => DoubleType is Extractable into Short =
    double => double.toShort.unless(_.toDouble != double)

  given doubleInt: [DoubleType <: Double] => DoubleType is Extractable into Int =
    double => double.toInt.unless(_.toDouble != double)

  given doubleLong: [DoubleType <: Double] => DoubleType is Extractable into Long =
    double => double.toLong.unless(_.toDouble != double)

  given doubleFloat: [DoubleType <: Double] => DoubleType is Extractable into Float =
    double => double.toFloat.unless(_.toDouble != double)

  given valueOf: [EnumType <: Enum: Mirror.SumOf as mirror, TextType <: Text]
  =>    TextType is Extractable into EnumType =
    text =>
      import Selectable.reflectiveSelectable

      mirror match
        case mirror: { def valueOf(name: String): EnumType } @unchecked =>
          try mirror.valueOf(text.s) catch case error: Exception => Unset

  given fromOrdinal: [EnumType <: Enum: Mirror.SumOf as mirror, IntType <: Int]
  =>    IntType is Extractable into EnumType =
    ordinal =>
      import Selectable.reflectiveSelectable
      mirror match
        case mirror: { def fromOrdinal(ordinal: Int): EnumType } @unchecked =>
          try mirror.fromOrdinal(ordinal) catch case error: Exception => Unset

trait Extractable:
  type Self
  type Result
  def extract(value: Self): Optional[Result]
  def unapply(value: Self): Option[Result] = extract(value).option
