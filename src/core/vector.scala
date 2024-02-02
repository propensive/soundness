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
import vacuous.*

import scala.compiletime.*, ops.int.*

class Matrix[+ElementType, RowsType <: Int, ColumnsType <: Int](val rows: Int, val columns: Int, val elements: IArray[ElementType]):
  def apply(row: Int, column: Int): ElementType = elements(columns*row + column)

  override def equals(right: Any): Boolean = right.asMatchable match
    case matrix: Matrix[?, ?, ?] => elements.sameElements(matrix.elements)
    case _                       => false

  override def hashCode: Int = scala.util.hashing.MurmurHash3.arrayHash(elements.mutable(using Unsafe))
  
  override def toString(): String = t"[${elements.debug}]".s

  @targetName("mul2")
  def * 
      [RightType]
      (right: RightType)
      (using mul: MulOperator[ElementType, RightType])
      (using ClassTag[mul.Result])
      : Matrix[mul.Result, RowsType, ColumnsType] =
    val elements2 = IArray.create[mul.Result](elements.length): array =>
      elements.indices.foreach: index =>
        array(index) = elements(index)*right

    new Matrix(rows, columns, elements2)

  @targetName("mul")
  def *
      [RightType, RightColumnsType <: Int: ValueOf]
      (right: Matrix[RightType, ColumnsType, RightColumnsType])
      (using mul: MulOperator[ElementType, RightType])
      (using ValueOf[RowsType], ValueOf[ColumnsType])
      (using ClassTag[mul.Result])
      (using add: AddOperator[mul.Result, mul.Result])
      (using add.Result =:= mul.Result)
      : Matrix[mul.Result, RowsType, RightColumnsType] =
    val columns2 = valueOf[RightColumnsType]
    val inner = valueOf[ColumnsType]
    
    val elements = IArray.create[mul.Result](rows*columns2): array =>
      for row <- 0 until rows; column <- 0 until columns2
      do array(columns2*column + row) =
        (0 until inner).map { index => apply(row, index)*right(index, column) }.reduce(_ + _)
    
    new Matrix(rows, columns2, elements)

object Matrix:
  transparent inline def apply
      [Rows <: Int: ValueOf, Columns <: Int: ValueOf]
      (using DummyImplicit)
      [ElementType]
      (rows: Tuple)
      (using Tuple.Union[Tuple.Fold[rows.type, EmptyTuple, [left, right] =>> Tuple.Concat[left & Tuple, right & Tuple]] & Tuple] <:< ElementType)
      (using Columns =:= Tuple.Union[Tuple.Map[rows.type, [tuple] =>> Tuple.Size[tuple & Tuple]]])
      (using Rows =:= Tuple.Size[rows.type])
      (using ClassTag[ElementType])
      : Any =
    val rowCount: Int = valueOf[Rows]
    val columnCount = valueOf[Columns]
    
    new Matrix[ElementType, Rows, Columns](rowCount, columnCount,
      IArray.create[ElementType](columnCount*rowCount): array =>
        for row <- 0 until rowCount; column <- 0 until columnCount
        do (rows.productElement(row).asMatchable: @unchecked) match
          case tuple: Tuple =>
            array(columnCount*row + column) = tuple.productElement(column).asInstanceOf[ElementType]
    )


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
    def iarray: IArray[LeftType] = left.toIArray.asInstanceOf[IArray[LeftType]]
    
    def map[LeftType2](fn: LeftType => LeftType2): Euclidean[LeftType2, SizeType] =
      def recur(tuple: Tuple): Tuple = tuple match
        case head *: tail => fn(head.asInstanceOf[LeftType]) *: recur(tail)
        case _            => EmptyTuple

      recur(left)

    @targetName("add")
    def +
        [RightType]
        (right: Euclidean[RightType, SizeType])
        (using add: AddOperator[LeftType, RightType])
        : Euclidean[add.Result, SizeType] =

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
    def -
        [RightType]
        (right: Euclidean[RightType, SizeType])
        (using sub: SubOperator[LeftType, RightType])
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
