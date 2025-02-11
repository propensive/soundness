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

import anticipation.*
import prepositional.*

object Irrefutable:
  given stringText: String is Irrefutable into Text = _.tt

  given [ResultType] => (irrefutable: Text is Irrefutable into ResultType)
  =>    String is Irrefutable into ResultType =
    string => irrefutable.unapply(string.tt)

  given textString: [TextType <: Text] => TextType is Irrefutable into String = _.s
  given ident: [ResultType] => ResultType is Irrefutable into ResultType = identity(_)

  given byteShort: Byte is Irrefutable into Short = _.toShort
  given byteInt: Byte is Irrefutable into Int = _.toInt
  given byteLong: Byte is Irrefutable into Long = _.toLong
  given byteFloat: Byte is Irrefutable into Float = _.toFloat
  given byteDouble: Byte is Irrefutable into Double = _.toDouble

  given shortInt: Short is Irrefutable into Int = _.toInt
  given shortLong: Short is Irrefutable into Long = _.toLong
  given shortFloat: Short is Irrefutable into Float = _.toShort
  given shortDouble: Short is Irrefutable into Double = _.toDouble
  given intLong: Int is Irrefutable into Long = _.toLong
  given intDoule: Int is Irrefutable into Double = _.toDouble
  given floatDouble: Float is Irrefutable into Double = _.toDouble

trait Irrefutable:
  type Self
  type Result
  def unapply(value: Self): Result
