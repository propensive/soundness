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

import mosquito.internal.Tensor

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

  given addable
  :   [ a,
        rows <: Int,
        columns <: Int,
        left <: Matrix[a, rows, columns],
        b,
        right <: Matrix[b, rows, columns],
        result ]
  =>  ( addable: a is Addable by b to result, classTag: ClassTag[result] )
  =>  left is Addable:

    type Operand = right
    type Result = Matrix[result, rows, columns]

    def add(left: left, right: right): Matrix[result, rows, columns] =
      val length = left.elements.length

      val arr = IArray.build[result](length): array =>
        var i = 0
        while i < length do
          array(i) = addable.add(left.elements(i), right.elements(i))
          i += 1

      new Matrix[result, rows, columns](left.rows, left.columns, arr)


  given subtractable
  :   [ a,
        rows <: Int,
        columns <: Int,
        left <: Matrix[a, rows, columns],
        b,
        right <: Matrix[b, rows, columns],
        result ]
  =>  ( subtractable: a is Subtractable by b to result, classTag: ClassTag[result] )
  =>  left is Subtractable:

    type Operand = right
    type Result = Matrix[result, rows, columns]

    def subtract(left: left, right: right): Matrix[result, rows, columns] =
      val length = left.elements.length

      val arr = IArray.build[result](length): array =>
        var i = 0
        while i < length do
          array(i) = subtractable.subtract(left.elements(i), right.elements(i))
          i += 1

      new Matrix[result, rows, columns](left.rows, left.columns, arr)


  def identity[element, dimension <: Int]
    ( using unital:        element is Unital,
            zeroic:        element is Zeroic,
            classTag:      ClassTag[element],
            dimensionSize: ValueOf[dimension] )
  :   Matrix[element, dimension, dimension] =

    val size = dimensionSize.value
    val one = unital.one
    val zero = zeroic.zero

    val arr = IArray.build[element](size*size): array =>
      var i = 0
      while i < size do
        var j = 0
        while j < size do
          array(size*i + j) = if i == j then one else zero
          j += 1

        i += 1

    new Matrix[element, dimension, dimension](size, size, arr)


  def zero[element, rowCount <: Int, columnCount <: Int]
    ( using zeroic:   element is Zeroic,
            classTag: ClassTag[element],
            rowValue: ValueOf[rowCount],
            colValue: ValueOf[columnCount] )
  :   Matrix[element, rowCount, columnCount] =

    val rowsValue = valueOf[rowCount]
    val colsValue = valueOf[columnCount]
    val zero = zeroic.zero

    val arr = IArray.build[element](rowsValue*colsValue): array =>
      var i = 0
      while i < rowsValue*colsValue do
        array(i) = zero
        i += 1

    new Matrix[element, rowCount, columnCount](rowsValue, colsValue, arr)


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


  private def laplaceExpansion[element]
    ( elements:      IArray[element],
      dimension:     Int,
      rowMask:       Long,
      columnMask:    Long,
      submatrixSize: Int )
    ( using multiplication: element is Multiplicable by element to element,
            addition:       element is Addable by element to element,
            subtraction:    element is Subtractable by element to element )
  :   element =

    if submatrixSize == 1 then
      val row = java.lang.Long.numberOfTrailingZeros(rowMask)
      val column = java.lang.Long.numberOfTrailingZeros(columnMask)
      elements(dimension*row + column)
    else if submatrixSize == 2 then
      val firstRow = java.lang.Long.numberOfTrailingZeros(rowMask)
      val secondRow = java.lang.Long.numberOfTrailingZeros(rowMask & (rowMask - 1L))
      val firstColumn = java.lang.Long.numberOfTrailingZeros(columnMask)
      val secondColumn = java.lang.Long.numberOfTrailingZeros(columnMask & (columnMask - 1L))

      elements(dimension*firstRow + firstColumn)*elements(dimension*secondRow + secondColumn)
      - elements(dimension*firstRow + secondColumn)*elements(dimension*secondRow + firstColumn)
    else
      val expansionRow = java.lang.Long.numberOfTrailingZeros(rowMask)
      val remainingRows = rowMask & ~(1L << expansionRow)
      val firstColumn = java.lang.Long.numberOfTrailingZeros(columnMask)
      val firstColumnsRemoved = columnMask & ~(1L << firstColumn)

      val firstMinor =
        laplaceExpansion(elements, dimension, remainingRows, firstColumnsRemoved, submatrixSize - 1)

      var result: element = elements(dimension*expansionRow + firstColumn)*firstMinor
      var remainingColumns = firstColumnsRemoved
      var position = 1

      while remainingColumns != 0L do
        val column = java.lang.Long.numberOfTrailingZeros(remainingColumns)
        val columnsRemoved = columnMask & ~(1L << column)

        val minor =
          laplaceExpansion(elements, dimension, remainingRows, columnsRemoved, submatrixSize - 1)

        val term: element = elements(dimension*expansionRow + column)*minor
        result = if position % 2 == 1 then result - term else result + term
        remainingColumns = remainingColumns & ~(1L << column)
        position += 1

      result


  extension [element, n <: Int](matrix: Matrix[element, n, n])
    def determinant
      ( using multiplication: element is Multiplicable by element to element,
              addition:       element is Addable by element to element,
              subtraction:    element is Subtractable by element to element )
    :   element =

      val dimension = matrix.rows
      val fullMask: Long = (1L << dimension) - 1L
      laplaceExpansion(matrix.elements, dimension, fullMask, fullMask, dimension)


    def trace(using addition: element is Addable by element to element): element =
      val dimension = matrix.rows
      var sum: element = matrix(0, 0)
      var i = 1
      while i < dimension do
        sum = sum + matrix(i, i)
        i += 1

      sum


    def minor(row: Int, column: Int)
      ( using multiplication: element is Multiplicable by element to element,
              addition:       element is Addable by element to element,
              subtraction:    element is Subtractable by element to element )
    :   element =

      val dimension = matrix.rows
      val fullMask: Long = (1L << dimension) - 1L
      val rowMask = fullMask & ~(1L << row)
      val columnMask = fullMask & ~(1L << column)
      laplaceExpansion(matrix.elements, dimension, rowMask, columnMask, dimension - 1)


    def cofactor(row: Int, column: Int)
      ( using multiplication: element is Multiplicable by element to element,
              addition:       element is Addable by element to element,
              subtraction:    element is Subtractable by element to element,
              zeroic:         element is Zeroic )
    :   element =

      val minorValue = matrix.minor(row, column)
      if (row + column) % 2 == 0 then minorValue else zeroic.zero - minorValue


    def adjugate
      ( using multiplication: element is Multiplicable by element to element,
              addition:       element is Addable by element to element,
              subtraction:    element is Subtractable by element to element,
              zeroic:         element is Zeroic,
              unital:         element is Unital,
              classTag:       ClassTag[element] )
    :   Matrix[element, n, n] =

      val dimension = matrix.rows
      val elements = matrix.elements

      if dimension == 1 then new Matrix[element, n, n](1, 1, IArray(unital.one))
      else
        val fullMask: Long = (1L << dimension) - 1L

        val resultElements = IArray.build[element](dimension*dimension): array =>
          var outputRow = 0

          while outputRow < dimension do
            var outputColumn = 0

            while outputColumn < dimension do
              val rowMask = fullMask & ~(1L << outputColumn)
              val columnMask = fullMask & ~(1L << outputRow)

              val minorDeterminant =
                laplaceExpansion(elements, dimension, rowMask, columnMask, dimension - 1)

              val signedMinor =
                if (outputRow + outputColumn) % 2 == 0 then minorDeterminant
                else zeroic.zero - minorDeterminant

              array(dimension*outputRow + outputColumn) = signedMinor
              outputColumn += 1

            outputRow += 1

        new Matrix[element, n, n](dimension, dimension, resultElements)


    def inverse
      ( using multiplication: element is Multiplicable by element to element,
              addition:       element is Addable by element to element,
              subtraction:    element is Subtractable by element to element,
              divisible:      element is Divisible by element to element,
              zeroic:         element is Zeroic,
              classTag:       ClassTag[element] )
    :   Optional[Matrix[element, n, n]] =

      val dimension = matrix.rows
      val elements = matrix.elements
      val fullMask: Long = (1L << dimension) - 1L
      val determinantValue = laplaceExpansion(elements, dimension, fullMask, fullMask, dimension)

      if determinantValue == zeroic.zero then Unset
      else if dimension == 1 then
        val one: element = elements(0)/elements(0)
        new Matrix[element, n, n](1, 1, IArray(one/elements(0)))
      else
        val resultElements = IArray.build[element](dimension*dimension): array =>
          var outputRow = 0

          while outputRow < dimension do
            var outputColumn = 0

            while outputColumn < dimension do
              val rowMask = fullMask & ~(1L << outputColumn)
              val columnMask = fullMask & ~(1L << outputRow)

              val minorDeterminant =
                laplaceExpansion(elements, dimension, rowMask, columnMask, dimension - 1)

              val signedMinor =
                if (outputRow + outputColumn) % 2 == 0 then minorDeterminant
                else zeroic.zero - minorDeterminant

              array(dimension*outputRow + outputColumn) = signedMinor/determinantValue
              outputColumn += 1

            outputRow += 1

        new Matrix[element, n, n](dimension, dimension, resultElements)


class Matrix[element, rows <: Int, columns <: Int]
  ( val rows: Int, val columns: Int, val elements: IArray[element] ):

  def apply(row: Int, column: Int): element = elements(columns*row + column)

  def row(index: Int): Tensor[element, columns] =
    val cols = columns
    val arr = IArray.build[Any](cols): array =>
      var i = 0
      while i < cols do
        array(i) = elements(cols*index + i)
        i += 1

    new Tensor[element, columns](arr)


  def column(index: Int): Tensor[element, rows] =
    val arr = IArray.build[Any](rows): array =>
      var i = 0
      while i < rows do
        array(i) = elements(columns*i + index)
        i += 1

    new Tensor[element, rows](arr)


  def transpose(using ClassTag[element]): Matrix[element, columns, rows] =
    val arr = IArray.build[element](rows*columns): array =>
      var row = 0
      while row < rows do
        var col = 0
        while col < columns do
          array(rows*col + row) = elements(columns*row + col)
          col += 1

        row += 1

    new Matrix[element, columns, rows](columns, rows, arr)

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


  @targetName("mulVec")
  def * [right]
    ( right: Tensor[right, columns] )
    ( using multiplication: element is Multiplicable by right,
            addition:       multiplication.Result is Addable by multiplication.Result,
            equality:       addition.Result =:= multiplication.Result,
            columnValue:    ValueOf[columns] )
  :   Tensor[multiplication.Result, rows] =

    val inner = valueOf[columns]

    val arr = IArray.build[Any](rows): array =>
      var row = 0
      while row < rows do
        var sum: multiplication.Result =
          apply(row, 0)*right.data(0).asInstanceOf[right]

        var k = 1
        while k < inner do
          sum = sum + apply(row, k)*right.data(k).asInstanceOf[right]
          k += 1

        array(row) = sum
        row += 1

    new Tensor[multiplication.Result, rows](arr)
