/*
    Escritoire, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire

import rudiments.*
import vacuous.*
import gossamer.*
import hieroglyph.*
import anticipation.*
import spectacular.*
import perforate.*

import language.experimental.pureFunctions

import Table.ShortPair

enum Breaks:
  case Never, Space, Zwsp, Character

enum Alignment:
  case Left, Right, Center

enum DelimitRows:
  case None, Rule, Space, SpaceIfMultiline, RuleIfMultiline

object ColumnAlignment:
  val left: ColumnAlignment[Any] = () => Alignment.Left
  given byte: ColumnAlignment[Byte] = () => Alignment.Right
  given short: ColumnAlignment[Short] = () => Alignment.Right
  given int: ColumnAlignment[Int] = () => Alignment.Right
  given long: ColumnAlignment[Long] = () => Alignment.Right
  given text: ColumnAlignment[Text] = () => Alignment.Left

trait ColumnAlignment[-ColumnType]:
  def alignment(): Alignment

object Column:
  def apply
      [RowType, CellType, TextType]
      (title: TextType, width: Optional[Int] = Unset, align: Optional[Alignment] = Unset,
          breaks: Breaks = Breaks.Space, hide: Boolean = false)(get: RowType -> CellType)
      (using textual: Textual[TextType], columnAlignment: ColumnAlignment[CellType] = ColumnAlignment.left)
      (using textual.ShowType[CellType])
      : Column[RowType, TextType] =

    def contents(row: RowType): TextType = textual.show(get(row))
    
    Column(title, contents, breaks, align.or(columnAlignment.alignment()), width, hide)

  def constrain
      [TextType: Textual]
      (text: TextType, breaks: Breaks, maxWidth: Int, init: Int = 0)
      (using calc: TextWidthCalculator)
      : Table.ShortPair =

    @tailrec
    def recur
        (pos: Int, space: Int = 0, count: Int = 0, max: Int = 0, lines: Int = 1)
        : Table.ShortPair =

      if pos == text.length then Table.ShortPair(lines, max.max(count))
      else unsafely(text(pos)) match
        case ' ' if breaks == Breaks.Space || breaks == Breaks.Zwsp =>
          if count >= maxWidth then recur(pos + 1, 0, 0, max.max(count), lines + 1)
          else recur(pos + 1, count, count + 1, max, lines)
        case '\u200b' if breaks == Breaks.Zwsp =>
          if count >= maxWidth then recur(pos + 1, 0, 0, max.max(count), lines + 1)
          else recur(pos + 1, count, count, max, lines)
        case '\n' =>
          recur(pos + 1, 0, 0, max.max(count), lines + 1)
        case ch if breaks == Breaks.Character =>
          if count >= maxWidth then recur(pos + 1, 0, 0, max.max(count), lines + 1)
          else recur(pos + 1, count, count + calc.width(ch), max, lines)
        case ch =>
          if count >= maxWidth then recur(pos + 1, 0, count - space, max.max(space), lines + 1)
          else recur(pos + 1, space, count + calc.width(ch), max, lines)
 
    recur(init)

case class Column
    [RowType, TextType: Textual]
    (title: TextType, get: RowType -> TextType, breaks: Breaks, align: Alignment, width: Optional[Int],
        hide: Boolean)

object Table:
  opaque type ShortPair = Int
  
  object ShortPair:
    def apply(left: Int, right: Int): ShortPair = ((right&65535) << 16) + (left&65535)
    
    given Ordering[ShortPair] = Ordering.Int
    
    extension (value: ShortPair)
      def right: Int = value >> 16
      def left: Int = value&65535


  @targetName("make")
  def apply
      [RowType]
      (using ClassTag[RowType])
      [TextType]
      (initCols: Column[RowType, TextType]*)
      (using textTypeClassTag: ClassTag[TextType], textual: Textual[TextType]) =
    new Table[RowType, TextType](initCols*)

case class Table
    [RowType: ClassTag, TextType]
    (initCols: Column[RowType, TextType]*)
    (using classTag: ClassTag[TextType], textual: Textual[TextType]):

  def tabulate
      (data: Seq[RowType], maxWidth: Int, delimitRows: DelimitRows = DelimitRows.RuleIfMultiline)
      (using style: TableStyle, calc: TextWidthCalculator)
      : LazyList[TextType] =
    val cols: IArray[Column[RowType, TextType]] = IArray.from(initCols.filterNot(_.hide))
    val titles: IArray[TextType] = IArray.from(cols.map(_.title))
    
    val cells: IArray[IArray[TextType]] = IArray.from:
      titles +: data.map { row => IArray.from(cols.map(_.get(row))) }
    
    val freeWidth: Int =
      maxWidth - cols.filter(!_.width.absent).map(_.width.or(0)).sum - style.cost(cols.length)
    
    val cellRefs: Array[Array[ShortPair]] =
      Array.tabulate(data.length + 1, cols.length): (row, col) =>
        Column.constrain(cells(row)(col), cols(col).breaks, freeWidth)
    
    val widths: Array[Int] = Array.from:
      cols.indices.map: col =>
        cellRefs.maxBy(_(col).right).apply(col).right

    @tailrec
    def recur(): Unit = if widths.sum > freeWidth then
      val maxLinesByRow: IArray[Int] = cellRefs.map(_.maxBy(_.left).left).immutable(using Unsafe)
      
      val totalUnfilledCellsByColumn: IArray[Int] = IArray.tabulate[Int](cols.length): col =>
        if !cols(col).width.absent then 0 else cellRefs.indices.count: row =>
          cellRefs(row)(col).left < maxLinesByRow(row)
      
      val mostSpaceInAnyColumn = totalUnfilledCellsByColumn.max
      
      val focus: Int = cols.indices.maxBy: col =>
        if totalUnfilledCellsByColumn(col) == mostSpaceInAnyColumn && cols(col).width.absent
        then widths(col)
        else -1

      val target = widths(focus) - 1
      
      cellRefs.indices.foreach: row =>
        if cellRefs(row)(focus).right > target
        then cellRefs(row)(focus) = Column.constrain(cells(row)(focus), cols(focus).breaks, target)
      
      widths(focus) = target
      recur()
      
    recur()

    val Multiline: Boolean = cellRefs.exists(_.exists(_.left > 1))

    val columns = cols.zip(widths).map: (col, width) =>
      col.copy(width = width)
    
    def extent(text: TextType, width: Int, start: Int): ShortPair =
      
      @tailrec
      def search(pos: Int, space: Int = 0, count: Int = 0): ShortPair =
        if pos >= text.length then ShortPair(pos, pos)
        else unsafely(text(pos)) match
          case '\n' => ShortPair(pos, pos + 1)
          case ' '  => if count >= width then ShortPair(pos, pos + 1)
                       else search(pos + 1, pos, count + 1)
          
          case ch =>
            if count >= width then
              if space > 0 then ShortPair(space, space + 1) else ShortPair(pos, pos)
            else search(pos + 1, space, count + 1)
      
      search(start)
    
    def rows(row: Int = 0, offsets: Array[Int]): LazyList[TextType] =
      if row >= cells.length then LazyList()
      else
        if columns.indices.exists { col => offsets(col) != cells(row)(col).length }
        then
          val text: TextType = columns.indices.map: col =>
            val content: TextType = cells(row)(col)
            val ext: ShortPair = extent(content, columns(col).width.or(0), offsets(col))
            val slice: TextType = content.slice(offsets(col), ext.left)
            offsets(col) = ext.right
            val width: Int = columns(col).width.or(0)
            
            columns(col).align match
              case Alignment.Left   => slice.pad(width)(using calc)
              case Alignment.Center => slice.center(width)(using calc)
              case Alignment.Right  => slice.pad(width, Rtl)(using calc)
          
          .join(textual.make(t"${style.left} ".s), textual.make(t" ${style.sep} ".s),
              textual.make(t" ${style.right}".s))
        
          text #:: rows(row, offsets)
        else
          offsets.indices.foreach(offsets(_) = 0)
          
          if row + 1 < cells.length
          then delimitRows match
            case _ if row == 0 =>
              val next = rule(style.midLeft, style.midSep, style.midRight, style.midBar)
              next #:: rows(row + 1, offsets)
            case DelimitRows.Rule =>
              val next = rule(style.midLeft, style.midSep, style.midRight, style.midBar)
              next #:: rows(row + 1, offsets)
            case DelimitRows.Space  =>
              rule(style.left, style.sep, style.right, ' ') #:: rows(row + 1, offsets)
            case DelimitRows.RuleIfMultiline if Multiline =>
              val next = rule(style.midLeft, style.midSep, style.midRight, style.midBar)
              next #:: rows(row + 1, offsets)
            case DelimitRows.SpaceIfMultiline if Multiline =>
              rule(style.left, style.sep, style.right, ' ') #:: rows(row + 1, offsets)
            case _ =>
              rows(row + 1, offsets)
          else LazyList(rule(style.bottomLeft, style.bottomSep, style.bottomRight, style.bottomBar))

    def rule(left: Char, separator: Char, right: Char, bar: Char): TextType =
      columns.map: col =>
        textual.make(bar.show.s)*(col.width.or(0) + 2)
      .join(textual.make(left.show.s), textual.make(separator.show.s), textual.make(right.show.s))

    val next = rule(style.topLeft, style.topSep, style.topRight, style.topBar)
    next #:: rows(0, Array.fill[Int](cols.length)(0))

package tableStyles:
 given default: TableStyle =
   TableStyle(1, '│', '│', '│', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '─', '─', '─')
 
 given horizontal: TableStyle =
   TableStyle(1, ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ', ' ', '─', ' ', '─', '─', '─')
 
 given horizontalGaps: TableStyle =
   TableStyle(1, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '─', '─', '─')
 
 given horizontalDots: TableStyle =
   TableStyle(1, ' ', ' ', ' ', ' ', '╌', ' ', ' ', '╌', ' ', ' ', '╌', ' ', '╌', '╌', '╌')
 
 given doubled: TableStyle =
   TableStyle(1, '║', '│', '║', '╔', '╤', '╗', '╚', '╧', '╝', '╟', '┼', '╢', '═', '─', '═')
 
 given rounded: TableStyle =
   TableStyle(1, '│', '│', '│', '╭', '┬', '╮', '╰', '┴', '╯', '├', '┼', '┤', '─', '─', '─')
 
 given dotted: TableStyle =
   TableStyle(1, '┊', '┊', '┊', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '╌', '╌', '╌')
 
 given outline: TableStyle =
   TableStyle(1, '┊', '┊', '┊', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '╌', '╌', '╌')
 
 given ascii: TableStyle =
   TableStyle(1, '|', '|', '|', '+', '+', '+', '+', '+', '+', '+', '+', '+', '-', '-', '-')
 
 given borderless: TableStyle =
   TableStyle(0, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

case class TableStyle
    (pad: Int, left: Char, sep: Char, right: Char, topLeft: Char, topSep: Char, topRight: Char,
        bottomLeft: Char, bottomSep: Char, bottomRight: Char, midLeft: Char, midSep: Char,
        midRight: Char, topBar: Char, midBar: Char, bottomBar: Char):

  def cost(cols: Int): Int = cols*pad*2 + cols + 1
