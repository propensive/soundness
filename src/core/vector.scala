/*
    Mosquito, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package mosquito

import rudiments.*
import anticipation.*
import spectacular.*
import symbolism.*
import gossamer.*
import hieroglyph.*

object Mosquito:
  opaque type Euclidean[+ValueType, SizeType <: Int] = Tuple

  object Euclidean:
    def apply(elems: Tuple): Euclidean[Tuple.Union[elems.type], Tuple.Size[elems.type]] = elems

    given show
        [SizeType <: Int: ValueOf, ElemType: Show]
        (using TextMetrics)
        : Show[Euclidean[ElemType, SizeType]] =
      euclidean =>
        val items = euclidean.list.map(_.show)
        val width = items.maxBy(_.length).length
        val size = valueOf[SizeType]
        if size == 1 then t"( ${items(0)} )"
        else
          val top = t"⎛ ${items.head.pad(width, Rtl)} ⎞"
          val bottom = t"⎝ ${items.last.pad(width, Rtl)} ⎠"
          
          val middle = items.tail.init.map: item =>
            t"⎜ ${item.pad(width, Rtl)} ⎟"
          
          (top :: middle ::: bottom :: Nil).join(t"\n")

  extension [LeftType](left: Euclidean[LeftType, 3])
    def cross
        [RightType]
        (right: Euclidean[RightType, 3])
        (using multiply: MulOperator[LeftType, RightType])
        (using add: AddOperator[multiply.Result, multiply.Result],
            subtract: SubOperator[multiply.Result, multiply.Result])
        : Euclidean[add.Result, 3] =
      (left(1)*right(2) - left(2)*right(1)) *:
          (left(2)*right(0) - left(0)*right(2)) *:
          (left(0)*right(1) - left(1)*right(0)) *:
          EmptyTuple
      

  extension [SizeType <: Int, LeftType](left: Euclidean[LeftType, SizeType])
    def apply(index: Int): LeftType = left.toArray(index).asInstanceOf[LeftType]
    def list: List[LeftType] = left.toList.asInstanceOf[List[LeftType]]
    
    def dot
        [RightType]
        (right: Euclidean[RightType, SizeType])
        (using multiply: MulOperator[LeftType, RightType])
        (using size: ValueOf[SizeType])
        (using add: AddOperator[multiply.Result, multiply.Result])
        (using add.Result =:= multiply.Result)
        : multiply.Result =
      
      def recur(index: Int, sum: multiply.Result): multiply.Result =
        if index < 0 then sum else recur(index - 1, sum + left(index)*right(index))

      val start = size.value - 1
      recur(start - 1, left(start)*right(start))

export Mosquito.Euclidean
