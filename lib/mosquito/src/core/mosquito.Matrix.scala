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


  private def submatrix[element: ClassTag]
    (data: IArray[element], n: Int, skipRow: Int, skipCol: Int)
  :   IArray[element] =

    IArray.build[element]((n - 1)*(n - 1)): array =>
      var idx = 0
      var row = 0
      while row < n do
        if row != skipRow then
          var col = 0
          while col < n do
            if col != skipCol then
              array(idx) = data(n*row + col)
              idx += 1
            col += 1
        row += 1


  private def computeDeterminant[element]
    (data: IArray[element], n: Int)
    (using multiplication: element is Multiplicable by element to element,
           addition:       element is Addable by element to element,
           subtraction:    element is Subtractable by element to element,
           classTag:       ClassTag[element])
  :   element =

    if n == 1 then data(0)
    else if n == 2 then data(0)*data(3) - data(1)*data(2)
    else
      var result: element = data(0)*computeDeterminant(submatrix(data, n, 0, 0), n - 1)
      var j = 1
      while j < n do
        val term: element = data(j)*computeDeterminant(submatrix(data, n, 0, j), n - 1)
        result = if j % 2 == 1 then result - term else result + term
        j += 1

      result


  extension [element, n <: Int](matrix: Matrix[element, n, n])
    def determinant
      (using multiplication: element is Multiplicable by element to element,
             addition:       element is Addable by element to element,
             subtraction:    element is Subtractable by element to element,
             classTag:       ClassTag[element])
    :   element =

      computeDeterminant(matrix.elements, matrix.rows)


    def inverse
      (using multiplication: element is Multiplicable by element to element,
             addition:       element is Addable by element to element,
             subtraction:    element is Subtractable by element to element,
             divisible:      element is Divisible by element to element,
             zeroic:         element is Zeroic,
             classTag:       ClassTag[element])
    :   Optional[Matrix[element, n, n]] =

      val size = matrix.rows
      val data = matrix.elements
      val det = computeDeterminant(data, size)

      if det == zeroic.zero then Unset
      else if size == 1 then
        val one: element = data(0)/data(0)
        new Matrix[element, n, n](1, 1, IArray(one/data(0)))
      else
        val resultData = IArray.build[element](size*size): array =>
          var i = 0
          while i < size do
            var j = 0
            while j < size do
              val minorDet = computeDeterminant(submatrix(data, size, j, i), size - 1)
              val signed = if (i + j) % 2 == 0 then minorDet else zeroic.zero - minorDet
              array(size*i + j) = signed/det
              j += 1
            i += 1

        new Matrix[element, n, n](size, size, resultData)


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
      var row = 0
      while row < rows do
        var column = 0
        while column < columns2 do
          var sum: multiplication.Result = apply(row, 0)*right(0, column)
          var k = 1
          while k < inner do
            sum = sum + apply(row, k)*right(k, column)
            k += 1

          array(columns2*row + column) = sum
          column += 1

        row += 1

    new Matrix(rows, columns2, elements)
