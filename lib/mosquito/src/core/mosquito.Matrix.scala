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
import scala.compiletime.ops.int.-

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


    def solve(rhs: Tensor[element, n])
      ( using zeroic:         element is Zeroic,
              subtraction:    element is Subtractable by element to element,
              multiplication: element is Multiplicable by element to element,
              divisible:      element is Divisible by element to element,
              classTag:       ClassTag[element] )
    :   Optional[Tensor[element, n]] =

      val size = matrix.rows
      val zero = zeroic.zero
      val a: Array[element] = matrix.elements.mutable(using Unsafe).clone()
      val b: Array[element] = new Array[element](size)
      var copyIndex = 0
      while copyIndex < size do
        b(copyIndex) = rhs.data(copyIndex).asInstanceOf[element]
        copyIndex += 1

      var col = 0
      var singular = false

      while col < size && !singular do
        var pivotRow = -1
        var search = col
        while search < size && pivotRow < 0 do
          if a(size*search + col) != zero then pivotRow = search
          search += 1

        if pivotRow < 0 then singular = true
        else
          if pivotRow != col then
            var swap = col
            while swap < size do
              val tmp = a(size*col + swap)
              a(size*col + swap) = a(size*pivotRow + swap)
              a(size*pivotRow + swap) = tmp
              swap += 1

            val rhsTmp = b(col)
            b(col) = b(pivotRow)
            b(pivotRow) = rhsTmp

          val pivotValue = a(size*col + col)
          var rowIdx = col + 1
          while rowIdx < size do
            val factor = a(size*rowIdx + col)/pivotValue
            var elim = col
            while elim < size do
              a(size*rowIdx + elim) = a(size*rowIdx + elim) - factor*a(size*col + elim)
              elim += 1

            b(rowIdx) = b(rowIdx) - factor*b(col)
            rowIdx += 1

        col += 1

      if singular then Unset
      else
        val x: Array[element] = new Array[element](size)
        var i = size - 1
        while i >= 0 do
          var sum: element = b(i)
          var j = i + 1
          while j < size do
            sum = sum - a(size*i + j)*x(j)
            j += 1

          x(i) = sum/a(size*i + i)
          i -= 1

        val tensorData = IArray.build[Any](size): array =>
          var k = 0
          while k < size do
            array(k) = x(k)
            k += 1

        new Tensor[element, n](tensorData)


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


  extension [n <: Int](matrix: Matrix[Double, n, n])
    def eigensystem(using ValueOf[n])
    :   Optional[(Tensor[Double, n], Matrix[Double, n, n])] =

      val dimension = matrix.rows
      val tolerance = 1.0e-12
      val maxIterations = 100*dimension*dimension

      var symmetric = true
      var checkRow = 0
      while checkRow < dimension && symmetric do
        var checkCol = checkRow + 1
        while checkCol < dimension && symmetric do
          if math.abs(matrix(checkRow, checkCol) - matrix(checkCol, checkRow)) > tolerance
          then symmetric = false
          checkCol += 1

        checkRow += 1

      if !symmetric then Unset
      else
        val mat: Array[Double] = matrix.elements.mutable(using Unsafe).clone()
        val vec: Array[Double] = new Array[Double](dimension*dimension)
        var diagIndex = 0
        while diagIndex < dimension do
          vec(dimension*diagIndex + diagIndex) = 1.0
          diagIndex += 1

        var iteration = 0
        var converged = false

        while !converged && iteration < maxIterations do
          var maxAbs = 0.0
          var p = 0
          var q = 0
          var i = 0
          while i < dimension do
            var j = i + 1
            while j < dimension do
              val absValue = math.abs(mat(dimension*i + j))
              if absValue > maxAbs then
                maxAbs = absValue
                p = i
                q = j

              j += 1

            i += 1

          if maxAbs < tolerance then converged = true
          else
            val mpp = mat(dimension*p + p)
            val mqq = mat(dimension*q + q)
            val mpq = mat(dimension*p + q)

            val theta = (mqq - mpp)/(2.0*mpq)

            val tan =
              if theta >= 0.0 then 1.0/(theta + math.sqrt(theta*theta + 1.0))
              else 1.0/(theta - math.sqrt(theta*theta + 1.0))

            val cos = 1.0/math.sqrt(1.0 + tan*tan)
            val sin = tan*cos

            val newMpp = cos*cos*mpp - 2.0*cos*sin*mpq + sin*sin*mqq
            val newMqq = sin*sin*mpp + 2.0*cos*sin*mpq + cos*cos*mqq

            mat(dimension*p + p) = newMpp
            mat(dimension*q + q) = newMqq
            mat(dimension*p + q) = 0.0
            mat(dimension*q + p) = 0.0

            var rotateIndex = 0
            while rotateIndex < dimension do
              if rotateIndex != p && rotateIndex != q then
                val mip = mat(dimension*rotateIndex + p)
                val miq = mat(dimension*rotateIndex + q)
                val updatedMip = cos*mip - sin*miq
                val updatedMiq = sin*mip + cos*miq
                mat(dimension*rotateIndex + p) = updatedMip
                mat(dimension*rotateIndex + q) = updatedMiq
                mat(dimension*p + rotateIndex) = updatedMip
                mat(dimension*q + rotateIndex) = updatedMiq

              rotateIndex += 1

            var vecIndex = 0
            while vecIndex < dimension do
              val vkp = vec(dimension*vecIndex + p)
              val vkq = vec(dimension*vecIndex + q)
              vec(dimension*vecIndex + p) = cos*vkp - sin*vkq
              vec(dimension*vecIndex + q) = sin*vkp + cos*vkq
              vecIndex += 1

          iteration += 1

        if !converged then Unset
        else
          val eigvalArr = IArray.build[Any](dimension): array =>
            var i = 0
            while i < dimension do
              array(i) = mat(dimension*i + i)
              i += 1

          val eigvecArr = IArray.build[Double](dimension*dimension): array =>
            var i = 0
            while i < dimension*dimension do
              array(i) = vec(i)
              i += 1

          val eigvals = new Tensor[Double, n](eigvalArr)
          val eigvecs = new Matrix[Double, n, n](dimension, dimension, eigvecArr)
          (eigvals, eigvecs)


    def eigenvalues(using ValueOf[n]): Optional[Tensor[Double, n]] =
      matrix.eigensystem.let(_(0))


    def eigenvectors(using ValueOf[n]): Optional[Matrix[Double, n, n]] =
      matrix.eigensystem.let(_(1))


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


  def submatrix(droppedRow: Int, droppedColumn: Int)(using ClassTag[element])
  :   Matrix[element, rows - 1, columns - 1] =

    val newRows = rows - 1
    val newCols = columns - 1

    val arr = IArray.build[element](newRows*newCols): array =>
      var r = 0
      while r < newRows do
        var c = 0
        while c < newCols do
          val origRow = if r < droppedRow then r else r + 1
          val origCol = if c < droppedColumn then c else c + 1
          array(newCols*r + c) = elements(columns*origRow + origCol)
          c += 1

        r += 1

    new Matrix[element, rows - 1, columns - 1](newRows, newCols, arr)


  def frobeniusNorm
    ( using multiplicable: element is Multiplicable by element,
            addable:       multiplicable.Result is Addable by multiplicable.Result
                           to multiplicable.Result,
            rootable:      multiplicable.Result is Rootable[2] to element )
  :   element =

    val length = elements.length
    var sum: multiplicable.Result = elements(0)*elements(0)
    var i = 1
    while i < length do
      sum = addable.add(sum, elements(i)*elements(i))
      i += 1

    sum.sqrt


  def rank
    ( using zeroic:         element is Zeroic,
            subtraction:    element is Subtractable by element to element,
            multiplication: element is Multiplicable by element to element,
            divisible:      element is Divisible by element to element )
  :   Int =

    val r = rows
    val c = columns
    val zero = zeroic.zero
    val a: Array[element] = elements.mutable(using Unsafe).clone()

    var rankCount = 0
    var col = 0

    while col < c && rankCount < r do
      var pivotRow = -1
      var i = rankCount
      while i < r && pivotRow < 0 do
        if a(c*i + col) != zero then pivotRow = i
        i += 1

      if pivotRow >= 0 then
        if pivotRow != rankCount then
          var k = 0
          while k < c do
            val tmp = a(c*rankCount + k)
            a(c*rankCount + k) = a(c*pivotRow + k)
            a(c*pivotRow + k) = tmp
            k += 1

        val pivotValue = a(c*rankCount + col)
        var rowIdx = rankCount + 1

        while rowIdx < r do
          val factor = a(c*rowIdx + col)/pivotValue
          var k = col
          while k < c do
            a(c*rowIdx + k) = a(c*rowIdx + k) - factor*a(c*rankCount + k)
            k += 1

          rowIdx += 1

        rankCount += 1

      col += 1

    rankCount

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
