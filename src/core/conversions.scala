/*
    Rudiments, version 0.4.0. Copyright 2020-23 Jon Pretty, Propensive OÜ.

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

import scala.compiletime.*

case class IncompatibleTypeError() extends Error(err"the value is not compatible")

extension (value: Any)
  transparent inline def as[To](using Unapply[value.type, To]): To throws IncompatibleTypeError =
    summon[Unapply[value.type, To]].unapply(value).getOrElse(throw IncompatibleTypeError())
  
  transparent inline def as[To](using Irrefutable[value.type, To]): To =
    summon[Irrefutable[value.type, To]].unapply(value)

object Irrefutable:
  given Irrefutable[String, Text] = Text(_)
  given [T](using ext: Irrefutable[Text, T]): Irrefutable[String, T] = v => ext.unapply(Text(v))

  given Irrefutable[Text, String] = _.s

  given [T]: Irrefutable[T, T] = identity(_)
  
  given Irrefutable[Byte, Short] = _.toShort
  given Irrefutable[Byte, Int] = _.toInt
  given Irrefutable[Byte, Long] = _.toLong
  given Irrefutable[Byte, Float] = _.toFloat
  given Irrefutable[Byte, Double] = _.toDouble

  given Irrefutable[Short, Int] = _.toInt
  given Irrefutable[Short, Long] = _.toLong
  given Irrefutable[Short, Float] = _.toShort
  given Irrefutable[Short, Double] = _.toDouble
  given Irrefutable[Int, Long] = _.toLong
  given Irrefutable[Int, Double] = _.toDouble
  given Irrefutable[Float, Double] = _.toDouble

object Unapply:
  given [T](using ext: Irrefutable[Text, T]): Irrefutable[String, T] = v => ext.unapply(Text(v))
  
  given Unapply[Text, Char] = v => if v.s.length == 1 then Some(v.s.head) else None
  given Unapply[Text, Byte] = v => try Some(v.s.toByte) catch case e: NumberFormatException => None
  
  given Unapply[Text, Short] = v =>
    try Some(v.s.toShort) catch case e: NumberFormatException => None
  
  given Unapply[Text, Int] = v => try Some(v.s.toInt) catch case e: NumberFormatException => None
  given Unapply[Text, Long] = v => try Some(v.s.toLong) catch case e: NumberFormatException => None
  
  given Unapply[Text, Float] = v =>
    try Some(v.s.toFloat) catch case e: NumberFormatException => None
  
  given Unapply[Text, Double] = v =>
    try Some(v.s.toDouble) catch case e: NumberFormatException => None
  
  given Unapply[Text, Boolean] = v =>
    if v.s == "true" then Some(true) else if v.s == "false" then Some(false) else None

  given Unapply[Short, Byte] = v => if v.toByte.toShort == v then Some(v.toByte) else None
  
  given Unapply[Int, Byte] = v => if v.toByte.toInt == v then Some(v.toByte) else None
  given Unapply[Int, Short] = v => if v.toShort.toInt == v then Some(v.toShort) else None
  given Unapply[Int, Float] = v => if v.toFloat.toInt == v then Some(v.toFloat) else None
  
  given Unapply[Long, Byte] = v => if v.toByte.toLong == v then Some(v.toByte) else None
  given Unapply[Long, Short] = v => if v.toShort.toLong == v then Some(v.toShort) else None
  given Unapply[Long, Int] = v => if v.toInt.toLong == v then Some(v.toInt) else None
  given Unapply[Long, Float] = v => if v.toFloat.toLong == v then Some(v.toFloat) else None
  given Unapply[Long, Double] = v => if v.toDouble.toLong == v then Some(v.toDouble) else None
  
  given Unapply[Float, Byte] = v => if v.toByte.toFloat == v then Some(v.toByte) else None
  given Unapply[Float, Short] = v => if v.toShort.toFloat == v then Some(v.toShort) else None
  given Unapply[Float, Int] = v => if v.toInt.toFloat == v then Some(v.toInt) else None
  given Unapply[Float, Long] = v => if v.toLong.toFloat == v then Some(v.toLong) else None
  
  given Unapply[Double, Byte] = v => if v.toByte.toDouble == v then Some(v.toByte) else None
  given Unapply[Double, Short] = v => if v.toShort.toDouble == v then Some(v.toShort) else None
  given Unapply[Double, Int] = v => if v.toInt.toDouble == v then Some(v.toInt) else None
  given Unapply[Double, Long] = v => if v.toLong.toDouble == v then Some(v.toLong) else None
  given Unapply[Double, Float] = v => if v.toFloat.toDouble == v then Some(v.toFloat) else None

trait Unapply[-From, +To]:
  def unapply(value: From): Option[To]

trait Irrefutable[-From, +To]:
  def unapply(value: From): To

object As:
  transparent inline def unapply[T](v: Any)(using ext: Unapply[v.type, T]): Option[T] = ext.unapply(v)