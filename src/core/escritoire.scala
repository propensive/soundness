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
import contingency.*
import hieroglyph.*
import spectacular.*
import anticipation.*
import fulminate.*

import scala.collection.immutable as sci

import language.experimental.pureFunctions

case class TableError(minimumWidth: Int, availableWidth: Int)
extends Error(msg"The table required a minimum width of $minimumWidth, but only $availableWidth was available")

trait InsufficientSpaceHandler:
  def apply(minimumWidth: Int, availableWidth: Int): Unit

package insufficientSpaceHandling:
  given fail(using Raises[TableError]): InsufficientSpaceHandler =
    (minimum, available) => raise(TableError(minimum, available))(())
  
  given ignore: InsufficientSpaceHandler = (minimum, available) => ()

enum Breaks:
  case Never, Space, Zwsp, Character

enum Alignment:
  case Left, Right, Center

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

  def apply[RowType, CellType, TextType]
      (title:  TextType,
       width:  Optional[Int]       = Unset,
       align:  Optional[Alignment] = Unset,
       breaks: Breaks              = Breaks.Space,
       hide:   Boolean             = false,
       sizing: ColumnSizing        = columnSizing.Prose)
      (get: RowType -> CellType)
      (using textual: Textual[TextType], columnAlignment: ColumnAlignment[CellType] = ColumnAlignment.left)
      (using textual.ShowType[CellType])
          : Column[RowType, TextType] =

    def contents(row: RowType): TextType = textual.show(get(row))
    
    Column(title, contents, breaks, align.or(columnAlignment.alignment()), width, hide, sizing)

case class Column[RowType, TextType: Textual]
    (title:  TextType,
     get:    RowType => TextType,
     breaks: Breaks,
     align:  Alignment,
     width:  Optional[Int],
     hide:   Boolean,
     sizing: ColumnSizing)

object Table:
  @targetName("make")
  def apply[RowType](using classTag: ClassTag[RowType])[TextType: ClassTag: Textual]
      (initColumns: Column[RowType, TextType]*)
          : Table[RowType, TextType] =

    new Table(initColumns*)

abstract class Tabulation[TextType: ClassTag]():
  type Row
  
  def columns: IArray[Column[Row, TextType]]
  def titles: Seq[IArray[IArray[TextType]]]
  def rows: Seq[IArray[IArray[TextType]]]
  def dataLength: Int

  def layout(width: Int)(using style: TableStyle, metrics: TextMetrics, textual: Textual[TextType])
      (using insufficientSpace: InsufficientSpaceHandler)
          : TableLayout[TextType] =
    
    case class Layout(slack: Double, indices: IArray[Int], widths: IArray[Int], totalWidth: Int):
      lazy val include: sci.BitSet = indices.to(sci.BitSet)

      lazy val columnWidths: IArray[(Int, Column[Row, TextType], Int)] = IArray.from:
        indices.indices.map: index =>
          val columnIndex = indices(index)
          (columnIndex, columns(columnIndex), widths(index))

    def bisect(include: Int => Boolean): (Layout, Layout) =
      def shrink(slack: Double): Layout =
        val widths: IndexedSeq[Optional[Int]] =
          columns.indices.map: index =>
            val dataMax =
              if !include(index) then 0 else rows.map: cells =>
                columns(index).sizing.width[TextType](cells(index), width, slack).or(0)
              .max
    
            val titleMax =
              if !include(index) then 0 else titles.map: cells =>
                columns(index).sizing.width[TextType](cells(index), width, slack).or(0)
              .max
          
            dataMax.max(titleMax).puncture(0)
        
        val indices: IndexedSeq[Int] = widths.indices.map { index => widths(index).let(index.waive) }.compact
        val totalWidth = widths.sumBy(_.or(0)) + style.cost(indices.size)
      
        Layout(slack, IArray.from(indices), IArray.from(widths.compact), totalWidth)
      
      def recur(min: Layout, max: Layout, countdown: Int = 8): (Layout, Layout) =
        if countdown == 0 || max.totalWidth - min.totalWidth <= 1 then (min, max)
        else
          val point = shrink((min.slack + max.slack)/2)
          
          if point.totalWidth == width then (point, point)
          else if point.totalWidth > width then recur(min, point, countdown - 1)
          else recur(point, max, countdown - 1)
      
      recur(shrink(0), shrink(1), 8)

    val rowLayout = bisect(_ => true)(0)
    val rowLayout2 = bisect(rowLayout.include(_))(0)
    
    // We may be able to increase the slack in some of the remaining columns
    if rowLayout2.totalWidth > width then insufficientSpace(rowLayout2.totalWidth, width)

    def lines(data: Seq[IArray[IArray[TextType]]]): LazyList[TableRow[TextType]] =
      data.to(LazyList).map: cells =>
        val tableCells = rowLayout2.columnWidths.map: (index, column, width) =>
          val lines = column.sizing.fit(cells(index), width)
          TableCell(width, 1, lines, lines.length)
        
        val height = tableCells.maxBy(_.minHeight).minHeight

        TableRow(tableCells, false, height)
    
    val widths = rowLayout2.columnWidths.map(_(2))

    TableLayout(List(TableSection(widths, lines(titles)), TableSection(widths, lines(rows))))

case class TableCell[TextType](width: Int, span: Int, lines: IndexedSeq[TextType], minHeight: Int):
  def apply(line: Int): TextType = lines(line)

case class TableRow[TextType](cells: IArray[TableCell[TextType]], title: Boolean, height: Int):
  def apply(column: Int): TableCell[TextType] = cells(column)

case class TableSection[TextType](widths: IArray[Int], rows: LazyList[TableRow[TextType]])
case class TableLayout[TextType](sections: List[TableSection[TextType]]):

  def render(using metrics: TextMetrics, textual: Textual[TextType], style: TableStyle): LazyList[TextType] =
    val pad = t" "*style.padding
    val leftEdge = textual.make(t"${BoxDrawing(top = style.sideLines, bottom = style.sideLines)}$pad".s)
    val rightEdge = textual.make(t"$pad${BoxDrawing(top = style.sideLines, bottom = style.sideLines)}".s)
    val midEdge = textual.make(t"$pad${BoxDrawing(top = style.innerLines, bottom = style.innerLines)}$pad".s)
    
    def recur(widths: IArray[Int], rows: LazyList[TableRow[TextType]]): LazyList[TextType] =
      rows match
        case row #:: tail =>
          val lines = (0 until row.height).map: lineNumber =>
            widths.indices.map: index =>
              val cell = row(index)
              if cell.minHeight > lineNumber then cell(lineNumber).pad(widths(index))
              else textual.make((t" "*widths(index)).s)
            .join(leftEdge, midEdge, rightEdge)
          
          lines.to(LazyList) #::: recur(widths, tail)
        
        case _ =>
          LazyList()
    
    def rule(above: Optional[IArray[Int]], below: Optional[IArray[Int]]): TextType =
      val width = above.or(below).vouch(using Unsafe).pipe: widths =>
        widths.sum + style.cost(widths.length)

      val ascenders = above.let(_.scan(0)(_ + _ + style.padding*2 + 1).to(sci.BitSet)).or(sci.BitSet())
      val descenders = below.let(_.scan(0)(_ + _ + style.padding*2 + 1).to(sci.BitSet)).or(sci.BitSet())

      val horizontal =
        if above.absent then style.topLine else if below.absent then style.bottomLine else style.titleLine

      textual.make:
        Text.fill(width): index =>
          def vertical(bitSet: sci.BitSet, line: BoxLine): BoxLine =
            if bitSet.contains(index) then line else BoxLine.Blank

          if index == 0 then
            BoxDrawing
              (top    = vertical(ascenders, style.sideLines),
               right  = horizontal,
               bottom = vertical(descenders, style.sideLines),
               left   = BoxLine.Blank)
          else if index == (width - 1) then
            BoxDrawing
              (top    = vertical(ascenders, style.sideLines),
               right  = BoxLine.Blank,
               bottom = vertical(descenders, style.sideLines),
               left   = horizontal)
          else
            BoxDrawing
              (top    = vertical(ascenders, style.innerLines),
               right  = horizontal,
               bottom = vertical(descenders, style.innerLines),
               left   = horizontal)
        .s
  

    val topLine = rule(Unset, sections.head.widths)
    val midRule = rule(sections.head.widths, sections.head.widths)
    val bottomLine = rule(sections.head.widths, Unset)
    val body = sections.to(LazyList).flatMap { section => midRule #:: recur(section.widths, section.rows) }

    topLine #:: body.tail #::: LazyList(bottomLine)

case class Table[RowType: ClassTag, TextType: ClassTag](initColumns: Column[RowType, TextType]*)
    (using textual: Textual[TextType]):
  table =>
  
  val columns: IArray[Column[RowType, TextType]] = IArray.from(initColumns.filterNot(_.hide))
  val titles: Seq[IArray[IArray[TextType]]] = Seq(IArray.from(columns.map(_.title.cut(t"\n"))))

  def tabulate(data: Seq[RowType]): Tabulation[TextType] { type Row = RowType } = new Tabulation[TextType]:
    type Row = RowType

    val columns: IArray[Column[Row, TextType]] = table.columns
    val titles: Seq[IArray[IArray[TextType]]] = table.titles
    val dataLength: Int = data.length
    val rows: Seq[IArray[IArray[TextType]]] = data.map { row => columns.map(_.get(row).cut(t"\n")) }
