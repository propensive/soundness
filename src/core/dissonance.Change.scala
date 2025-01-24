/*
    Dissonance, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dissonance

import language.experimental.pureFunctions

import vacuous.*

sealed trait Change[+ElemType] extends Product:
  def map[ElemType2](lambda: ElemType => ElemType2): Change[ElemType2] = this match
    case Sub(left, right, leftValue, rightValue) =>
      Sub(left, right, leftValue.let(lambda), rightValue.let(lambda))

    case edit: Edit[ElemType] =>
      edit.map(lambda)

sealed trait Edit[+ElemType] extends Change[ElemType]:
  def value: Optional[ElemType]

  override def map[ElemType2](lambda: ElemType => ElemType2): Edit[ElemType2] = this match
    case Par(left, right, value) => Par(left, right, value.let(lambda))
    case Del(left, value)        => Del(left, value.let(lambda))
    case Ins(right, value)       => Ins(right, lambda(value))

case class Del[+ElemType](left: Int, value: Optional[ElemType] = Unset) extends Edit[ElemType]
case class Ins[+ElemType](right: Int, value: ElemType) extends Edit[ElemType]

case class Par[+ElemType](left: Int, right: Int, value: Optional[ElemType] = Unset)
extends Edit[ElemType]

case class Sub[+ElemType]
   (left: Int, right: Int, leftValue: Optional[ElemType], rightValue: Optional[ElemType])
extends Change[ElemType]
