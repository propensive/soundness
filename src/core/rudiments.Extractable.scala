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

object Extractable:
  given optional: [MatchType: Extractable]
  =>  Optional[MatchType] is Extractable into MatchType.Result =
    value => value.let(MatchType.extract(_))

  given [ResultType]
  => (irrefutable: Irrefutable[Text, ResultType])
  =>  Irrefutable[String, ResultType] =
    value => irrefutable.unapply(value.tt)

  given textChar: Text is Extractable into Char =
    text => if text.s.length == 1 then text.s.head else Unset

  given textByte: Text is Extractable into Byte =
    text => try text.s.toByte catch case _: NumberFormatException => Unset

  given textShort: Text is Extractable into Short =
    text => try text.s.toShort catch case _: NumberFormatException => Unset

  given textInt: [TextType <: Text] => TextType is Extractable into Int =
    v => try v.s.toInt catch case e: NumberFormatException => Unset

  given textLong: Text is Extractable into Long =
    v => try v.s.toLong catch case e: NumberFormatException => Unset

  given textFloat: Text is Extractable into Float = v =>
    try v.s.toFloat catch case e: NumberFormatException => Unset

  given textDouble: Text is Extractable into Double = v =>
    try v.s.toDouble catch case _: NumberFormatException => Unset

  given textBoolean: Text is Extractable into Boolean = v =>
    if v.s == "true" then true else if v.s == "false" then false else Unset

  given shortByte: Short is Extractable into Byte = v => if v.toByte.toShort == v then v.toByte else Unset

  given intByte: Int is Extractable into Byte =
    v => if v.toByte.toInt == v then (v.toByte) else Unset

  given intShort: Int is Extractable into Short =
    v => if v.toShort.toInt == v then v.toShort else Unset

  given intFloat: Int is Extractable into Float =
    v => if v.toFloat.toInt == v then v.toFloat else Unset

  given longByte: Long is Extractable into Byte =
    v => if v.toByte.toLong == v then v.toByte else Unset

  given longShort: Long is Extractable into Short = v => if v.toShort.toLong == v then v.toShort else Unset
  given longInt: Long is Extractable into Int = v => if v.toInt.toLong == v then v.toInt else Unset
  given longFloat: Long is Extractable into Float = v => if v.toFloat.toLong == v then v.toFloat else Unset
  given longDouble: Long is Extractable into Double = v => if v.toDouble.toLong == v then v.toDouble else Unset

  given floatByte: Float is Extractable into Byte = v => if v.toByte.toFloat == v then v.toByte else Unset
  given floatShort: Float is Extractable into Short = v => if v.toShort.toFloat == v then v.toShort else Unset
  given floatInt: Float is Extractable into Int = v => if v.toInt.toFloat == v then v.toInt else Unset
  given floatLong: Float is Extractable into Long = v => if v.toLong.toFloat == v then v.toLong else Unset

  given doubleByte: Double is Extractable into Byte = v => if v.toByte.toDouble == v then v.toByte else Unset
  given doubleShort: Double is Extractable into Short = v => if v.toShort.toDouble == v then v.toShort else Unset
  given doubleInt: Double is Extractable into Int = v => if v.toInt.toDouble == v then v.toInt else Unset
  given doubleLong: Double is Extractable into Long = v => if v.toLong.toDouble == v then v.toLong else Unset
  given doubleFloat: Double is Extractable into Float = v => if v.toFloat.toDouble == v then v.toFloat else Unset

  given valueOf: [EnumType <: Enum: Mirror.SumOf as mirror] => Text is Extractable into EnumType =
    text =>
      import Selectable.reflectiveSelectable

      mirror match
        case mirror: { def valueOf(name: String): EnumType } @unchecked =>
          try mirror.valueOf(text.s) catch case error: Exception => Unset

  given fromOrdinal: [EnumType <: Enum: Mirror.SumOf as mirror]
  => Int is Extractable into EnumType =
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
