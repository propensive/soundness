/*
    Mosquito, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.compiletime.*, ops.int.*

class Matrix[ElementType, RowsType <: Int, ColumnsType <: Int]
   (val rows: Int, val columns: Int, val elements: IArray[ElementType]):

  def apply(row: Int, column: Int): ElementType = elements(columns*row + column)

  override def equals(right: Any): Boolean = right.asMatchable match
    case matrix: Matrix[?, ?, ?] => elements.sameElements(matrix.elements)
    case _                       => false

  override def hashCode: Int = scala.util.hashing.MurmurHash3.arrayHash(elements.mutable(using Unsafe))

  override def toString(): String = t"[${elements.inspect}]".s

  @targetName("scalarMul")
  def * [RightType](right: RightType)
     (using multiplication: ElementType is Multiplicable by RightType)
     (using ClassTag[multiplication.Result])
          : Matrix[multiplication.Result, RowsType, ColumnsType] =

    val elements2 = IArray.create[multiplication.Result](elements.length): array =>
      elements.indices.foreach: index =>
        array(index) = elements(index)*right

    new Matrix(rows, columns, elements2)

  @targetName("scalarDiv")
  def / [RightType](right: RightType)(using div: ElementType is Divisible by RightType)
     (using ClassTag[div.Result])
          : Matrix[div.Result, RowsType, ColumnsType] =

    val elements2 = IArray.create[div.Result](elements.length): array =>
      elements.indices.foreach: index =>
        array(index) = elements(index)/right

    new Matrix(rows, columns, elements2)

  @targetName("mul")
  def * [RightType, RightColumnsType <: Int: ValueOf]
     (right: Matrix[RightType, ColumnsType, RightColumnsType])
     (using multiplication: ElementType is Multiplicable by RightType,
            addition:       multiplication.Result is Addable by multiplication.Result,
            equality:       addition.Result =:= multiplication.Result,
            rowValue:       ValueOf[RowsType],
            columnValue:    ValueOf[ColumnsType],
            classTag:       ClassTag[multiplication.Result])
          : Matrix[multiplication.Result, RowsType, RightColumnsType] =

    val columns2 = valueOf[RightColumnsType]
    val inner = valueOf[ColumnsType]

    val elements = IArray.create[multiplication.Result](rows*columns2): array =>
      for row <- 0 until rows; column <- 0 until columns2
      do array(columns2*column + row) =
        (0 until inner).map { index => apply(row, index)*right(index, column) }.reduce(_ + _)

    new Matrix(rows, columns2, elements)

object Matrix:
  given [ElementType: Showable] => TextMetrics => Matrix[ElementType, ?, ?] is Showable = matrix =>
    val textElements = matrix.elements.map(_.show)
    val sizes = textElements.map(_.length)

    val columnWidths: IArray[Int] = IArray.from:
      (0 until matrix.columns).map: column =>
        sizes(column + matrix.columns*(0 until matrix.rows).maxBy { row => sizes(matrix.columns*row + column) })

    (0 until matrix.rows).map: row =>
      val before = if row == 0 then t"⎡ " else if row == matrix.rows - 1 then t"⎣ " else t"⎪ "
      val after = if row == 0 then t" ⎤" else if row == matrix.rows - 1 then t" ⎦" else t" ⎪"

      (0 until matrix.columns).map: column =>
        textElements(matrix.columns*row + column).pad(columnWidths(column), Rtl)

      . join(before, t" ", after)
    . join(t"\n")

  transparent inline def apply[Rows <: Int: ValueOf, Columns <: Int: ValueOf]
     (using erased DummyImplicit)
     [ElementType]
     (rows: Tuple)
     (using Tuple.Union
             [Tuple.Fold
               [rows.type,
                EmptyTuple,
                [left, right] =>>
                  Tuple.Concat[left & Tuple, right & Tuple]] & Tuple] <:< ElementType,
            Columns =:= Tuple.Union[Tuple.Map[rows.type, [tuple] =>> Tuple.Size[tuple & Tuple]]],
            Rows =:= Tuple.Size[rows.type],
            ClassTag[ElementType])
      : Any =

    val rowCount: Int = valueOf[Rows]
    val columnCount = valueOf[Columns]

    new Matrix[ElementType, Rows, Columns]
     (rowCount,
      columnCount,
      IArray.create[ElementType](columnCount*rowCount): array =>
        for row <- 0 until rowCount; column <- 0 until columnCount
        do (rows.productElement(row).asMatchable: @unchecked) match
          case tuple: Tuple =>
            array(columnCount*row + column) = tuple.productElement(column).asInstanceOf[ElementType])
