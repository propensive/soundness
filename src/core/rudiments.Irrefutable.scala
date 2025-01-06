/*
    Rudiments, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*

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

trait Irrefutable[-ValueType, +ResultType]:
  def unapply(value: ValueType): ResultType
