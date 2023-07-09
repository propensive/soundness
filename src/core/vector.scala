package mosquito

import rudiments.*
import anticipation.*
import spectacular.*
import gossamer.*
import hieroglyph.*

import scala.quoted.*
import scala.reflect.*

object Mosquito:
  opaque type Euclidean[+ValueType, SizeType <: Int] = Tuple

  object Euclidean:
    def apply(elems: Tuple): Euclidean[Tuple.Union[elems.type], Tuple.Size[elems.type]] = elems

    given show
        [SizeType <: Int: ValueOf, ElemType: Show]
        (using TextWidthCalculator)
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
        (using multiply: Multiply[LeftType, RightType])
        (using add: Add[multiply.Result, multiply.Result])
        : Euclidean[add.Result, 3] =
      add(multiply(left(1), right(2)), multiply(left(2), right(1)), true) *:
          add(multiply(left(2), right(0)), multiply(left(0), right(2)), true) *:
          add(multiply(left(0), right(1)), multiply(left(1), right(0)), true) *: EmptyTuple
      

  extension [SizeType <: Int, LeftType](left: Euclidean[LeftType, SizeType])
    def apply(index: Int): LeftType = left.toArray(index).asInstanceOf[LeftType]
    def list: List[LeftType] = left.toList.asInstanceOf[List[LeftType]]
    
    def dot
        [RightType]
        (right: Euclidean[RightType, SizeType])
        (using multiply: Multiply[LeftType, RightType])
        (using size: ValueOf[SizeType])
        (using add: Add[multiply.Result, multiply.Result])
        (using add.Result =:= multiply.Result)
        : multiply.Result =
      
      def recur(index: Int, sum: multiply.Result): multiply.Result =
        if index < 0 then sum else recur(index - 1, add(sum, multiply(left(index), right(index)), false))

      val start = size.value - 1
      recur(start - 1, multiply(left(start), right(start)))

export Mosquito.Euclidean
