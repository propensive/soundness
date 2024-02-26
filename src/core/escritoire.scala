/*
    Escritoire, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import language.experimental.pureFunctions


enum BoxLine:
  case None, Thin, Thick, Double

case class BoxDrawingCharacter(vertical: BoxLine, horizontal: BoxLine)

object BoxDrawing:
  val simpleChars: IArray[Char] = IArray(
      ' ', '─', '━', '═',
      '│', '┼', '┿', '╪',
      '┃', '╂', '╋', '═',
      '║', '╫', '║', '╬')
  
  def simple(vertical: BoxLine, horizontal: BoxLine): Char =
    simpleChars(vertical.ordinal*4 + horizontal.ordinal)
  
  private val box: IArray[Char] =
    t" ╴╸ ╷┐┑╕╻┒┓  ╖ ╗╶─╾ ┌┬┭ ┎┰┱ ╓╥╓ ╺╼━ ┍┮┯╕┏┲┳  ╖ ╗   ═╒ ╒╤   ═╔ ╔╦╵┘┙╛│┤┥╡╽┧┪╛    └┴┵ ├┼┽ ┟╁╅     ┕┶┷╛┝┾┿╡┢╆╈╛    ╘ ╘╧╞ ╞╪╘ ╘╧    ╹┚┛ ╿┦┩╕┃┨┫  ╖ ╗┖┸┹ ┞╀╃ ┠╂╊ ╓╥╓ ┗┺┻ ┡╄╇╕┣ ╋  ╖ ╗   ═╒ ╒╤   ═╔ ╔╦ ╜ ╝     ╜ ╝║╢║╣╙╨╙     ╙╨╙ ╟╫╟  ╜ ╝     ╜ ╝║╢║╣╚ ╚╩    ╚ ╚╩╠ ╠╬".chars

  def apply(top: BoxLine, right: BoxLine, bottom: BoxLine, left: BoxLine): Char =
    box(top.ordinal + right.ordinal*4 + bottom.ordinal*16 + left.ordinal*64)

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
          breaks: Breaks = Breaks.Space, hide: Boolean = false, sizing: ColumnSizing)
      (get: RowType -> CellType)
      (using textual: Textual[TextType], columnAlignment: ColumnAlignment[CellType] = ColumnAlignment.left)
      (using textual.ShowType[CellType])
      : Column[RowType, TextType] =

    def contents(row: RowType): TextType = textual.show(get(row))
    
    Column(title, contents, breaks, align.or(columnAlignment.alignment()), width, hide, sizing)

case class Column[RowType, TextType: Textual]
    (title: TextType, get: RowType -> TextType, breaks: Breaks, align: Alignment, width: Optional[Int],
        hide: Boolean, sizing: ColumnSizing)

trait ColumnSizing:
  def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int]
  def confine[TextType: Textual](lines: IArray[TextType], width: Int): IArray[TextType]

package columnSizing:
  object Prose extends ColumnSizing:
    def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int] =
      def longestWord(text: TextType, pos: Int, lastStart: Int, max: Int): Int =
        if pos < text.length then
          if summon[Textual[TextType]].unsafeChar(text, pos) == ' '
          then longestWord(text, pos + 1, pos + 1, max.max(pos - lastStart))
          else longestWord(text, pos + 1, lastStart, max)
        else max.max(pos - lastStart)
      
      lines.map(longestWord(_, 0, 0, 0)).max.max((slack*maxWidth).toInt)
    
    def confine[TextType: Textual](lines: IArray[TextType], width: Int): IArray[TextType] =
      val textual = summon[Textual[TextType]]
      given ClassTag[TextType] = summon[Textual[TextType]].classTag
      
      def format(text: TextType, pos: Int, lineStart: Int, lastSpace: Int, lines: List[TextType])
          : List[TextType] =

        if pos < text.length then
          if textual.unsafeChar(text, pos) == ' '
          then format(text, pos + 1, lineStart, pos, lines)
          else if pos - lineStart >= width
          then format(text, pos + 1, lastSpace + 1, lastSpace, text.slice(lineStart, lastSpace) :: lines)
          else format(text, pos + 1, lineStart, lastSpace, lines)
        else if lineStart == pos then lines else text.slice(lineStart, pos) :: lines
      
      lines.flatMap(format(_, 0, 0, 0, Nil).reverse)


  case class Fixed(fixedWidth: Int, ellipsis: Text = t"…") extends ColumnSizing:
    def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int] =
      fixedWidth
    
    def confine[TextType](lines: IArray[TextType], width: Int)
        (using textual: Textual[TextType])
        : IArray[TextType] =
      given ClassTag[TextType] = summon[Textual[TextType]].classTag
      lines.map: line =>
        if line.length > width then line.take(width - ellipsis.length)+textual.make(ellipsis.s) else line

  case class Shorted(fixedWidth: Int, ellipsis: Text = t"…") extends ColumnSizing:
    def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int] =
      val naturalWidth = lines.map(_.length).max
      (maxWidth*slack).toInt.min(naturalWidth)
    
    def confine[TextType](lines: IArray[TextType], width: Int)
        (using textual: Textual[TextType])
        : IArray[TextType] =
      given ClassTag[TextType] = summon[Textual[TextType]].classTag
      lines.map: line =>
        if line.length > width then line.take(width - ellipsis.length)+textual.make(ellipsis.s) else line

  case class Collapsible(threshold: Double) extends ColumnSizing:
    def width[TextType: Textual](lines: IArray[TextType], maxWidth: Int, slack: Double): Optional[Int] =
      if slack > threshold then lines.map(_.length).max else Unset

    def confine[TextType: Textual](lines: IArray[TextType], width: Int): IArray[TextType] = lines
      

object Table:
  @targetName("make")
  def apply
      [RowType]
      (using classTag: ClassTag[RowType])
      [TextType: ClassTag: Textual]
      (initColumns: Column[RowType, TextType]*): Table[RowType, TextType] =
    new Table(initColumns*)

abstract class Tabulation[TextType: ClassTag]():
  type Row
  def columns: IArray[Column[Row, TextType]]
  def titles: IArray[(IArray[TextType], Int)] // rows of columns of lines
  def rows: Seq[IArray[IArray[TextType]]] // rows of columns of lines
  def dataLength: Int

  def layout(width: Int)
      (using style: TableStyle, metrics: TextMetrics, textual: Textual[TextType])
      : LazyList[IArray[IArray[TextType]]] =
    
    case class Layout(slack: Double, indices: IArray[Int], widths: IArray[Int], totalWidth: Int):
      lazy val columnWidths: IArray[(Int, Column[Row, TextType], Int)] = IArray.from:
        indices.indices.map: index =>
          val columnIndex = indices(index)
          (columnIndex, columns(columnIndex), widths(index))

    def layout(slack: Double): Layout =
      val widths: IndexedSeq[Optional[Int]] = columns.indices.map: index =>
        rows.map { cells => columns(index).sizing.width[TextType](cells(index), width, slack).or(0) }.max
      
      val indices: IndexedSeq[Int] = widths.indices.map { index => widths(index).let(index.waive) }.vouched
      val totalWidth = widths.sumBy(_.or(0)) + style.cost(indices.length)
      
      Layout(slack, IArray.from(indices), IArray.from(widths.vouched), totalWidth)

    def bisect(min: Layout, max: Layout): (Layout, Layout) =
      if max.totalWidth - min.totalWidth <= 1 then (min, max) else
        val point = layout((min.slack + max.slack)/2)
        
        if point.totalWidth == width then (point, point)
        else if point.totalWidth > width then bisect(min, point)
        else bisect(point, max)
      
    val rowLayout = bisect(layout(0), layout(1))(0)

    // We may be able to increase the slack in some of the remaining columns

    def lines(data: Iterable[IArray[IArray[TextType]]]): LazyList[IArray[IArray[TextType]]] =
      data.to(LazyList).map: cells =>
        rowLayout.columnWidths.map: (index, column, width) =>
          column.sizing.confine(cells(index), width)
    
    lines(rows)
    


case class Table
    [RowType: ClassTag, TextType: ClassTag]
    (initColumns: Column[RowType, TextType]*)
    (using textual: Textual[TextType]):
  table =>
  
  val columns: IArray[Column[RowType, TextType]] = IArray.from(initColumns.filterNot(_.hide))
  val titles: IArray[(IArray[TextType], Int)] = IArray.from(columns.map(_.title.cut(t"\n") -> 1))

  def tabulate(data: Seq[RowType]): Tabulation[TextType] { type Row = RowType } =
    
    new Tabulation[TextType]:
      type Row = RowType
      val columns: IArray[Column[Row, TextType]] = table.columns
      val titles: IArray[(IArray[TextType], Int)] = table.titles
      val dataLength: Int = data.length
      
      val rows: Seq[IArray[IArray[TextType]]] =
        data.map { row => IArray.from(columns.map(_.get(row).cut(t"\n"))) }
      
    
package tableStyles:
  given default: TableStyle =
    TableStyle(1, '│', '│', '│', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '─', '─', '─')
 
  given horizontal: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ', ' ', '─', ' ', '─', '─', '─')
 
  given minimalist: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ')
 
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

  def cost(columns: Int): Int = columns*pad*2 + columns + 1

extension [RowType](data: Seq[RowType])
  def table[TextType]
      (using textual: Textual[TextType], tabulable: Tabulable[RowType, TextType])
      : Tabulation[TextType] =
    tabulable.tabulate(data)

trait Tabulable[RowType, TextType]:
  def table(): Table[RowType, TextType]
  private lazy val tableValue: Table[RowType, TextType] = table()
  def tabulate(data: Seq[RowType]): Tabulation[TextType] = tableValue.tabulate(data)

