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
import prepositional.*
import hieroglyph.*
import vacuous.*

object Mosquito:
  opaque type Euclidean[ValueType, SizeType <: Int] = Tuple

  object Euclidean:
    def apply(elems: Tuple): Euclidean[Tuple.Union[elems.type], Tuple.Size[elems.type]] = elems

    def take[ElementType](list: List[ElementType], size: Int): Optional[Euclidean[ElementType, size.type]] =
      if size == 0 then EmptyTuple else list match
        case Nil          => Unset
        case head :: tail => take(tail, size - 1).let(head *: _)

    given [SizeType <: Int: ValueOf, ElemType: Showable](using TextMetrics)
        => Euclidean[ElemType, SizeType] is Showable as showable =

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
    def cross[RightType](right: Euclidean[RightType, 3])
       (using multiplication: LeftType is Multiplicable by RightType,
              addition:       multiplication.Result is Addable by multiplication.Result,
              subtraction:    multiplication.Result is Subtractable by multiplication.Result)
            : Euclidean[addition.Result, 3] =

      (left(1)*right(2) - left(2)*right(1)) *:
          (left(2)*right(0) - left(0)*right(2)) *:
          (left(0)*right(1) - left(1)*right(0)) *:
          EmptyTuple


  extension [SizeType <: Int, LeftType](left: Euclidean[LeftType, SizeType])
    def apply(index: Int): LeftType = left.toArray(index).asInstanceOf[LeftType]
    def list: List[LeftType] = left.toList.asInstanceOf[List[LeftType]]
    def iarray: IArray[LeftType] = left.toIArray.asInstanceOf[IArray[LeftType]]

    def map[LeftType2](fn: LeftType => LeftType2): Euclidean[LeftType2, SizeType] =
      def recur(tuple: Tuple): Tuple = tuple match
        case head *: tail => fn(head.asInstanceOf[LeftType]) *: recur(tail)
        case _            => EmptyTuple

      recur(left)

    @targetName("add")
    def + [RightType](right: Euclidean[RightType, SizeType])
       (using addition: LeftType is Addable by RightType)
            : Euclidean[addition.Result, SizeType] =

      def recur(left: Tuple, right: Tuple): Tuple = left match
        case leftHead *: leftTail => right match
          case rightHead *: rightTail =>
            (leftHead.asInstanceOf[LeftType] + rightHead.asInstanceOf[RightType]) *: recur(leftTail, rightTail)

          case _ =>
            EmptyTuple

        case _ =>
          EmptyTuple

      recur(left, right)

    @targetName("sub")
    def - [RightType](right: Euclidean[RightType, SizeType])(using sub: LeftType is Subtractable by RightType)
            : Euclidean[sub.Result, SizeType] =

      def recur(left: Tuple, right: Tuple): Tuple = left match
        case leftHead *: leftTail => right match
          case rightHead *: rightTail =>
            (leftHead.asInstanceOf[LeftType] - rightHead.asInstanceOf[RightType]) *: recur(leftTail, rightTail)
          case _ =>
            EmptyTuple
        case _ =>
          EmptyTuple

      recur(left, right)

    @targetName("scalarMul")
    def * [RightType](right: RightType)(using multiplication: LeftType is Multiplicable by RightType)
            : Euclidean[multiplication.Result, SizeType] =

      map(_*right)

    @targetName("scalarDiv")
    def / [RightType](right: RightType)(using div: LeftType is Divisible by RightType)
            : Euclidean[div.Result, SizeType] =

      map(_/right)

    def dot[RightType](right: Euclidean[RightType, SizeType])
       (using multiply: LeftType is Multiplicable by RightType,
              size:     ValueOf[SizeType],
              addition:      multiply.Result is Addable by multiply.Result,
              equality: addition.Result =:= multiply.Result)
            : multiply.Result =

      def recur(index: Int, sum: multiply.Result): multiply.Result =
        if index < 0 then sum else recur(index - 1, sum + left(index)*right(index))

      val start = size.value - 1
      recur(start - 1, left(start)*right(start))

extension [ElementType](list: List[ElementType])
  def slide(size: Int): LazyList[Euclidean[ElementType, size.type]] = list match
    case Nil          => LazyList()
    case head :: tail => Euclidean.take(list, size).lay(LazyList())(_ #:: tail.slide(size))
