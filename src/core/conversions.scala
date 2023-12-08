/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import anticipation.*

import scala.deriving.*

import language.experimental.captureChecking

trait DecimalConverter:
  def decimalize(value: Double): Text

extension (value: Any)
  def as[ResultType](using Irrefutable[value.type, ResultType]): ResultType =
    summon[Irrefutable[value.type, ResultType]].unapply(value)

object Irrefutable:
  given stringText: Irrefutable[String, Text] = _.tt
  
  given [ResultType](using ext: Irrefutable[Text, ResultType]): Irrefutable[String, ResultType] = v =>
    ext.unapply(v.tt)

  given textString: Irrefutable[Text, String] = _.s
  given ident[ResultType]: Irrefutable[ResultType, ResultType] = identity(_)
  
  given byteShort: Irrefutable[Byte, Short] = _.toShort
  given byteInt: Irrefutable[Byte, Int] = _.toInt
  given byteLong: Irrefutable[Byte, Long] = _.toLong
  given byteFloat: Irrefutable[Byte, Float] = _.toFloat
  given byteDouble: Irrefutable[Byte, Double] = _.toDouble

  given shortInt: Irrefutable[Short, Int] = _.toInt
  given shortLong: Irrefutable[Short, Long] = _.toLong
  given shortFloat: Irrefutable[Short, Float] = _.toShort
  given shortDouble: Irrefutable[Short, Double] = _.toDouble
  given intLong: Irrefutable[Int, Long] = _.toLong
  given intDoule: Irrefutable[Int, Double] = _.toDouble
  given floatDouble: Irrefutable[Float, Double] = _.toDouble

object Unapply:

  given maybe[MatchType, ResultType](using ext: Unapply[MatchType, ResultType])
      : Unapply[Optional[MatchType], ResultType] =
    value => if value.absent then None else ext.unapply(value.asInstanceOf[MatchType])

  given [ResultType](using ext: Irrefutable[Text, ResultType]): Irrefutable[String, ResultType] = v =>
    ext.unapply(v.tt)
  
  given textChar: Unapply[Text, Char] = v => if v.s.length == 1 then Some(v.s.head) else None
  given textByte: Unapply[Text, Byte] = v => try Some(v.s.toByte) catch case e: NumberFormatException => None
  
  given textShort: Unapply[Text, Short] = v =>
    try Some(v.s.toShort) catch case e: NumberFormatException => None
  
  given textInt: Unapply[Text, Int] = v => try Some(v.s.toInt) catch case e: NumberFormatException => None
  given textLong: Unapply[Text, Long] = v => try Some(v.s.toLong) catch case e: NumberFormatException => None
  
  given textFloat: Unapply[Text, Float] = v =>
    try Some(v.s.toFloat) catch case e: NumberFormatException => None
  
  given textDouble: Unapply[Text, Double] = v =>
    try Some(v.s.toDouble) catch case e: NumberFormatException => None
  
  given textBoolean: Unapply[Text, Boolean] = v =>
    if v.s == "true" then Some(true) else if v.s == "false" then Some(false) else None

  given shortByte: Unapply[Short, Byte] = v => if v.toByte.toShort == v then Some(v.toByte) else None
  
  given intByte: Unapply[Int, Byte] = v => if v.toByte.toInt == v then Some(v.toByte) else None
  given intShort: Unapply[Int, Short] = v => if v.toShort.toInt == v then Some(v.toShort) else None
  given intFloat: Unapply[Int, Float] = v => if v.toFloat.toInt == v then Some(v.toFloat) else None
  
  given longByte: Unapply[Long, Byte] = v => if v.toByte.toLong == v then Some(v.toByte) else None
  given longShort: Unapply[Long, Short] = v => if v.toShort.toLong == v then Some(v.toShort) else None
  given longInt: Unapply[Long, Int] = v => if v.toInt.toLong == v then Some(v.toInt) else None
  given longFloat: Unapply[Long, Float] = v => if v.toFloat.toLong == v then Some(v.toFloat) else None
  given longDouble: Unapply[Long, Double] = v => if v.toDouble.toLong == v then Some(v.toDouble) else None
  
  given floatByte: Unapply[Float, Byte] = v => if v.toByte.toFloat == v then Some(v.toByte) else None
  given floatShort: Unapply[Float, Short] = v => if v.toShort.toFloat == v then Some(v.toShort) else None
  given floatInt: Unapply[Float, Int] = v => if v.toInt.toFloat == v then Some(v.toInt) else None
  given floatLong: Unapply[Float, Long] = v => if v.toLong.toFloat == v then Some(v.toLong) else None
  
  given doubleByte: Unapply[Double, Byte] = v => if v.toByte.toDouble == v then Some(v.toByte) else None
  given doubleShort: Unapply[Double, Short] = v => if v.toShort.toDouble == v then Some(v.toShort) else None
  given doubleInt: Unapply[Double, Int] = v => if v.toInt.toDouble == v then Some(v.toInt) else None
  given doubleLong: Unapply[Double, Long] = v => if v.toLong.toDouble == v then Some(v.toLong) else None
  given doubleFloat: Unapply[Double, Float] = v => if v.toFloat.toDouble == v then Some(v.toFloat) else None

  given valueOf[EnumType <: reflect.Enum](using mirror: Mirror.SumOf[EnumType]): Unapply[Text, EnumType] =
    text =>
      import scala.reflect.Selectable.reflectiveSelectable
      mirror match
        case mirror: { def valueOf(name: String): EnumType } @unchecked =>
          try Some(mirror.valueOf(text.s)) catch case error: Exception => None
  
  given fromOrdinal[EnumType <: reflect.Enum](using mirror: Mirror.SumOf[EnumType]): Unapply[Int, EnumType] =
    ordinal =>
      import scala.reflect.Selectable.reflectiveSelectable
      mirror match
        case mirror: { def fromOrdinal(ordinal: Int): EnumType } @unchecked =>
          try Some(mirror.fromOrdinal(ordinal)) catch case error: Exception => None

trait Unapply[-ValueType, +ResultType]:
  def unapply(value: ValueType): Option[ResultType]

trait Irrefutable[-ValueType, +ResultType]:
  def unapply(value: ValueType): ResultType

object As:
  def unapply[MatchType](scrutinee: Any)(using Unapply[scrutinee.type, MatchType]): Option[MatchType] =
    summon[Unapply[scrutinee.type, MatchType]].unapply(scrutinee)
