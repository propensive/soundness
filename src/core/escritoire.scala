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
import gossamer.*
import escapade.*
import deviation.*

import language.experimental.namedTypeArguments

import Table.BiShort

enum Breaks:
  case Never, Space, Zwsp, Character

enum Alignment:
  case Left, Right, Center

enum DelimitRows:
  case None, Rule, Space, SpaceIfMultiline, RuleIfMultiline

object Column:
  def apply[T, H, S](title: H, width: Maybe[Int] = Unset, align: Alignment = Alignment.Left,
                         breaks: Breaks = Breaks.Space, hide: Boolean = false)(get: T => S)
                    (using ashow: AnsiShow[H], ashow2: AnsiShow[S])
                    : Column[T] =
    Column[T](ashow.ansiShow(title), get.andThen(ashow2.ansiShow(_)), breaks, align, width, hide)

  def constrain(text: Text, breaks: Breaks, maxWidth: Int, init: Int = 0)
               (using calc: TextWidthCalculator): Table.BiShort =

    @tailrec
    def recur(pos: Int, space: Int = 0, count: Int = 0, max: Int = 0, lines: Int = 1): Table.BiShort =
      if pos == text.length then Table.BiShort(lines, max.max(count))
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

case class Column[T](title: AnsiText, get: T => AnsiText, breaks: Breaks, align: Alignment, width: Maybe[Int],
                         hide: Boolean)

object Table:
  opaque type BiShort = Int
  
  object BiShort:
    def apply(left: Int, right: Int): BiShort = ((right&65535) << 16) + (left&65535)
    
    given Ordering[BiShort] = Ordering.Int
    
    extension (value: BiShort)
      def right: Int = value >> 16
      def left: Int = value&65535


case class Table[T: ClassTag](initCols: Column[T]*):

  def tabulate(data: Seq[T], maxWidth: Int, delimitRows: DelimitRows = DelimitRows.RuleIfMultiline)
              (using style: TableStyle, calc: TextWidthCalculator)
              : LazyList[AnsiText] =
    val cols: IArray[Column[T]] = IArray.from(initCols.filterNot(_.hide))
    val titles: IArray[AnsiText] = IArray.from(cols.map(_.title))
    
    val cells: IArray[IArray[AnsiText]] = IArray.from:
      titles +: data.map { row => IArray.from(cols.map(_.get(row))) }
    
    val freeWidth: Int = maxWidth - cols.filter(!_.width.unset).map(_.width.or(0)).sum - style.cost(cols.length)
    
    val cellRefs: Array[Array[BiShort]] = Array.tabulate(data.length + 1, cols.length): (row, col) =>
      Column.constrain(cells(row)(col).plain, cols(col).breaks, freeWidth)
    
    val widths: Array[Int] = Array.from:
      cols.indices.map: col =>
        cellRefs.maxBy(_(col).right).apply(col).right

    @tailrec
    def recur(): Unit = if widths.sum > freeWidth then
      val maxLinesByRow: IArray[Int] = cellRefs.map(_.maxBy(_.left).left).immutable(using Unsafe)
      
      val totalUnfilledCellsByColumn: IArray[Int] = IArray.tabulate[Int](cols.length): col =>
        if !cols(col).width.unset then 0 else cellRefs.indices.count: row =>
          cellRefs(row)(col).left < maxLinesByRow(row)
      
      val mostSpaceInAnyColumn = totalUnfilledCellsByColumn.max
      
      val focus: Int = cols.indices.maxBy: col =>
        if totalUnfilledCellsByColumn(col) == mostSpaceInAnyColumn && cols(col).width.unset then widths(col)
        else -1

      val target = widths(focus) - 1
      
      cellRefs.indices.foreach: row =>
        if cellRefs(row)(focus).right > target
        then cellRefs(row)(focus) = Column.constrain(cells(row)(focus).plain, cols(focus).breaks, target)
      
      widths(focus) = target
      recur()
      
    recur()

    val Multiline: Boolean = cellRefs.exists(_.exists(_.left > 1))

    val columns = cols.zip(widths).map: (col, width) =>
      col.copy(width = width)
    
    def extent(text: Text, width: Int, start: Int): BiShort =
      
      @tailrec
      def search(pos: Int, space: Int = 0, count: Int = 0): BiShort =
        if pos >= text.length then BiShort(pos, pos)
        else unsafely(text(pos)) match
          case '\n' => BiShort(pos, pos + 1)
          case ' '  => if count >= width then BiShort(pos, pos + 1) else search(pos + 1, pos, count + 1)
          
          case ch =>
            if count >= width then
              if space > 0 then BiShort(space, space + 1)
              else BiShort(pos, pos + 1)
            else search(pos + 1, space, count + 1)
      
      search(start)
    
    def rows(row: Int = 0, offsets: Array[Int]): LazyList[AnsiText] =
      if row >= cells.length then LazyList()
      else
        if columns.indices.exists { col => offsets(col) != cells(row)(col).length }
        then
          val text = columns.indices.map: col =>
            val content = cells(row)(col)
            val ext = extent(content.plain, columns(col).width.or(0), offsets(col))
            val slice = content.slice(offsets(col), ext.left)
            offsets(col) = ext.right
            val width = columns(col).width.or(0)
            
            columns(col).align match
              case Alignment.Left   => slice.pad(width)(using calc)
              case Alignment.Center => slice.center(width)(using calc)
              case Alignment.Right  => slice.pad(width, Rtl)(using calc)
          
          .join(ansi"${style.left} ", ansi" ${style.sep} ", ansi" ${style.right}")
        
          text #:: rows(row, offsets)
        else
          offsets.indices.foreach(offsets(_) = 0)
          if row + 1 < cells.length
          then
            delimitRows match
              case _ if row == 0 =>
                rule(style.midLeft, style.midSep, style.midRight, style.midBar) #:: rows(row + 1, offsets)
              case DelimitRows.Rule =>
                rule(style.midLeft, style.midSep, style.midRight, style.midBar) #:: rows(row + 1, offsets)
              case DelimitRows.Space  =>
                rule(style.left, style.sep, style.right, ' ') #:: rows(row + 1, offsets)
              case DelimitRows.RuleIfMultiline if Multiline =>
                rule(style.midLeft, style.midSep, style.midRight, style.midBar) #:: rows(row + 1, offsets)
              case DelimitRows.SpaceIfMultiline if Multiline =>
                rule(style.left, style.sep, style.right, ' ') #:: rows(row + 1, offsets)
              case _ =>
                rows(row + 1, offsets)
          else LazyList(rule(style.bottomLeft, style.bottomSep, style.bottomRight, style.bottomBar))

    def rule(left: Char, separator: Char, right: Char, bar: Char): AnsiText =
      columns.map { col => ansi"$bar"*(col.width.or(0) + 2) }.join(ansi"$left", ansi"$separator", ansi"$right")
    rule(style.topLeft, style.topSep, style.topRight, style.topBar) #:: rows(0, Array.fill[Int](cols.length)(0))

package tableStyles:
  given default:        TableStyle(1, '│', '│', '│', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '─', '─', '─')
  given horizontal:     TableStyle(1, ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ', ' ', '─', ' ', '─', '─', '─')
  given horizontalGaps: TableStyle(1, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '─', '─', '─')
  given horizontalDots: TableStyle(1, ' ', ' ', ' ', ' ', '╌', ' ', ' ', '╌', ' ', ' ', '╌', ' ', '╌', '╌', '╌')
  given doubled:        TableStyle(1, '║', '│', '║', '╔', '╤', '╗', '╚', '╧', '╝', '╟', '┼', '╢', '═', '─', '═')
  given rounded:        TableStyle(1, '│', '│', '│', '╭', '┬', '╮', '╰', '┴', '╯', '├', '┼', '┤', '─', '─', '─')
  given dotted:         TableStyle(1, '┊', '┊', '┊', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '╌', '╌', '╌')
  given outline:        TableStyle(1, '┊', '┊', '┊', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '╌', '╌', '╌')
  given ascii:          TableStyle(1, '|', '|', '|', '+', '+', '+', '+', '+', '+', '+', '+', '+', '-', '-', '-')
  given borderless:     TableStyle(0, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

case class TableStyle(pad: Int, left: Char, sep: Char, right: Char, topLeft: Char, topSep: Char, topRight: Char,
                          bottomLeft: Char, bottomSep: Char, bottomRight: Char, midLeft: Char, midSep: Char,
                          midRight: Char, topBar: Char, midBar: Char, bottomBar: Char):
  def cost(cols: Int): Int = cols*pad*2 + cols + 1
