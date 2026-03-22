                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package mosquito

import scala.compiletime.*

import anticipation.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Matrix:
  given showable: [element: Showable] => Text is Measurable => Matrix[element, ?, ?] is Showable =
    matrix =>
      val textElements = matrix.elements.map(_.show)
      val sizes = textElements.map(_.length)

      val columnWidths: IArray[Int] = IArray.from:
        (0 until matrix.columns).map: column =>
          sizes:
            column + matrix.columns*(0 until matrix.rows).maxBy: row =>
              sizes(matrix.columns*row + column)

      (0 until matrix.rows).map: row =>
        val before = if row == 0 then t"⎡ " else if row == matrix.rows - 1 then t"⎣ " else t"⎪ "
        val after = if row == 0 then t" ⎤" else if row == matrix.rows - 1 then t" ⎦" else t" ⎪"

        (0 until matrix.columns).map: column =>
          textElements(matrix.columns*row + column).pad(columnWidths(column), Rtl)

        . join(before, t" ", after)

      . join(t"\n")

  private type Constraint[rows <: Tuple, element] =
    Tuple.Union
      [ Tuple.Fold
          [ rows, Zero, [left, right] =>> Tuple.Concat[left & Tuple, right & Tuple] ]
          & Tuple ]
    <:< element

  private type ColumnConstraint[rows <: Tuple] =
    Tuple.Union[Tuple.Map[rows, [tuple] =>> Tuple.Size[tuple & Tuple]]]


  transparent inline def apply[Rows <: Int: ValueOf, Columns <: Int: ValueOf](using erased Void)
    [ element ]
    ( rows: Tuple )
    ( using Constraint[rows.type, element],
            Columns =:= ColumnConstraint[rows.type],
            Rows =:= Tuple.Size[rows.type],
            ClassTag[element] )
  :   Any =

    val rowCount: Int = valueOf[Rows]
    val columnCount = valueOf[Columns]

    new Matrix[element, Rows, Columns]
      ( rowCount,
        columnCount,
        IArray.build[element](columnCount*rowCount): array =>
          for row <- 0 until rowCount; column <- 0 until columnCount
          do rows.productElement(row).asMatchable.absolve match
            case tuple: Tuple =>
              array(columnCount*row + column) =
                tuple.productElement(column).asInstanceOf[element] )


class Matrix[element, rows <: Int, columns <: Int]
  ( val rows: Int, val columns: Int, val elements: IArray[element] ):

  def apply(row: Int, column: Int): element = elements(columns*row + column)

  override def equals(right: Any): Boolean = right.asMatchable match
    case matrix: Matrix[?, ?, ?] => elements.sameElements(matrix.elements)
    case _                       => false

  override def hashCode: Int =
    scala.util.hashing.MurmurHash3.arrayHash(elements.mutable(using Unsafe))

  override def toString(): String = t"[${elements.inspect}]".s


  @targetName("scalarMul")
  def * [right](right: right)
    ( using multiplication: element is Multiplicable by right )
    ( using ClassTag[multiplication.Result] )
  :   Matrix[multiplication.Result, rows, columns] =


    val elements2 = IArray.build[multiplication.Result](elements.length): array =>
      elements.indices.foreach: index =>
        array(index) = elements(index)*right

    new Matrix(rows, columns, elements2)


  @targetName("scalarDiv")
  def / [right](right: right)(using div: element is Divisible by right)(using ClassTag[div.Result])
  :   Matrix[div.Result, rows, columns] =

    val elements2 = IArray.build[div.Result](elements.length): array =>
      elements.indices.foreach: index =>
        array(index) = elements(index)/right

    new Matrix(rows, columns, elements2)


  @targetName("mul")
  def * [right, rightColumns <: Int: ValueOf]
    ( right: Matrix[right, columns, rightColumns] )
    ( using multiplication: element is Multiplicable by right,
            addition:       multiplication.Result is Addable by multiplication.Result,
            equality:       addition.Result =:= multiplication.Result,
            rowValue:       ValueOf[rows],
            columnValue:    ValueOf[columns],
            classTag:       ClassTag[multiplication.Result] )
  :   Matrix[multiplication.Result, rows, rightColumns] =

    val columns2 = valueOf[rightColumns]
    val inner = valueOf[columns]

    val elements = IArray.build[multiplication.Result](rows*columns2): array =>
      for row <- 0 until rows; column <- 0 until columns2
      do array(columns2*column + row) =
        (0 until inner).map { index => apply(row, index)*right(index, column) }.reduce(_ + _)

    new Matrix(rows, columns2, elements)
