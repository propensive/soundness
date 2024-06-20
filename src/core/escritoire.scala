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

trait Attenuation:
  def apply(minimumWidth: Int, availableWidth: Int): Unit

package columnAttenuation:
  given fail(using Errant[TableError]): Attenuation =
    (minimum, available) => raise(TableError(minimum, available))(())

  given ignore: Attenuation = (minimum, available) => ()

enum Breaks:
  case Never, Space, Zwsp, Character

object TextAlignment:
  object Right extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean): TextType =
      Textual(t" "*(width - text.length))+text

  object Left extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean): TextType =
      text+Textual(t" "*(width - text.length))

  object Center extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean): TextType =
      val space = width - text.length
      val before = Textual(t" "*(space/2))
      val after = Textual(t" "*(space - space/2))

      before+text+after

  object Justify extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean): TextType =
      if last then text+Textual(t" "*(width - text.length))
      else
        val words = text.cut(t" ")
        val wordCount = words.length
        val spare = width - words.sumBy(_.length)

        def recur(spare: Int, count: Int, done: TextType): TextType =
          if count == 0 then done+Textual(t" "*spare) else
            val space = spare/count
            recur(spare - space, count - 1, done+Textual(t" "*space)+words(wordCount - count))

        recur(spare, wordCount - 1, words.head)


trait TextAlignment:
  def pad[TextType: Textual](text: TextType, width: Int, last: Boolean): TextType

enum VerticalAlignment:
  case Top, Bottom

object ColumnAlignment:
  val topLeft: ColumnAlignment[Any] = ColumnAlignment(TextAlignment.Left, VerticalAlignment.Top)
  given byte: ColumnAlignment[Byte] = ColumnAlignment(TextAlignment.Right, VerticalAlignment.Top)
  given short: ColumnAlignment[Short] = ColumnAlignment(TextAlignment.Right, VerticalAlignment.Top)
  given int: ColumnAlignment[Int] = ColumnAlignment(TextAlignment.Right, VerticalAlignment.Top)
  given long: ColumnAlignment[Long] = ColumnAlignment(TextAlignment.Right, VerticalAlignment.Top)
  given text: ColumnAlignment[Text] = ColumnAlignment(TextAlignment.Left, VerticalAlignment.Top)

case class ColumnAlignment[-ColumnType](text: TextAlignment, vertical: VerticalAlignment)

object Column:
  def apply[RowType, CellType, TextType: Textual]
      (title:         TextType,
       textAlign:     Optional[TextAlignment]     = Unset,
       verticalAlign: Optional[VerticalAlignment] = Unset,
       sizing:        Columnar                    = columnar.Prose)
      (get: RowType -> CellType)
      (using columnAlignment: ColumnAlignment[CellType] = ColumnAlignment.topLeft)
      (using TextType.Show[CellType])
          : Column[RowType, TextType] =

    def contents(row: RowType): TextType = TextType.show(get(row))

    Column
      (title, contents, textAlign.or(columnAlignment.text), verticalAlign.or(columnAlignment.vertical), sizing)

case class Column[RowType, TextType: Textual]
    (title:         TextType,
     get:           RowType => TextType,
     textAlign:     TextAlignment,
     verticalAlign: VerticalAlignment,
     sizing:        Columnar):

  def contramap[RowType2](lambda: RowType2 => RowType): Column[RowType2, TextType] =
    Column[RowType2, TextType](title, row => get(lambda(row)), textAlign, verticalAlign, sizing)

  def retitle(title: TextType): Column[RowType, TextType] = copy(title = title)

object Table:
  @targetName("make")
  def apply[RowType](using DummyImplicit)[TextType: ClassTag: Textual](columns0: Column[RowType, TextType]*)
          : Table[RowType, TextType] =

    new Table(columns0*)

object Tabulation:
  given [TextType: {Textual as textual, Printable as printable}](using TextMetrics, TableStyle, Attenuation) => Tabulation[TextType] is Printable =
    (tabulation, termcap) =>
      tabulation.layout(termcap.width.or(100)).render.map(printable.print(_, termcap)).join(t"\n")


abstract class Tabulation[TextType: ClassTag]():
  type Row

  def columns: IArray[Column[Row, TextType]]
  def titles: Seq[IArray[IArray[TextType]]]
  def rows: Seq[IArray[IArray[TextType]]]
  def dataLength: Int

  def layout(width: Int)(using style: TableStyle, metrics: TextMetrics, textual: TextType is Textual)
      (using attenuation: Attenuation)
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

      def recur(min: Layout, max: Layout, gas: Int = 8): (Layout, Layout) =
        if gas == 0 || max.totalWidth - min.totalWidth <= 1 then (min, max)
        else
          val point = shrink((min.slack + max.slack)/2)

          if point.totalWidth == width then (point, point)
          else if point.totalWidth > width then recur(min, point, gas - 1)
          else recur(point, max, gas - 1)

      recur(shrink(0), shrink(1), 8)

    val rowLayout = bisect(_ => true)(0)
    val rowLayout2 = bisect(rowLayout.include(_))(0)

    // We may be able to increase the slack in some of the remaining columns
    if rowLayout2.totalWidth > width then attenuation(rowLayout2.totalWidth, width)

    def lines(data: Seq[IArray[IArray[TextType]]]): LazyList[TableRow[TextType]] =
      data.to(LazyList).map: cells =>
        val tableCells = rowLayout2.columnWidths.map: (index, column, width) =>
          val lines = column.sizing.fit(cells(index), width, column.textAlign)
          TableCell(width, 1, lines, lines.length, column.textAlign)

        val height = tableCells.maxBy(_.minHeight).minHeight

        TableRow(tableCells, false, height)

    val widths = rowLayout2.columnWidths.map(_(2))

    TableLayout(List(TableSection(widths, lines(titles)), TableSection(widths, lines(rows))), style)

case class TableCell[TextType]
    (width: Int, span: Int, lines: IndexedSeq[TextType], minHeight: Int, textAlign: TextAlignment):
  def apply(line: Int): TextType = lines(line)

case class TableRow[TextType](cells: IArray[TableCell[TextType]], title: Boolean, height: Int):
  def apply(column: Int): TableCell[TextType] = cells(column)

case class TableSection[TextType](widths: IArray[Int], rows: LazyList[TableRow[TextType]])

object TableLayout:
  given [TextType: {Textual, Printable as printable}](using TextMetrics) => TableLayout[TextType] is Printable =
    (layout, termcap) =>
      layout.render.map(printable.print(_, termcap)).join(t"\n")

case class TableLayout[TextType](sections: List[TableSection[TextType]], style: TableStyle):

  def render(using metrics: TextMetrics, textual: TextType is Textual): LazyList[TextType] =
    val pad = t" "*style.padding
    val leftEdge = Textual(t"${style.charset(top = style.sideLines, bottom = style.sideLines)}$pad")
    val rightEdge = Textual(t"$pad${style.charset(top = style.sideLines, bottom = style.sideLines)}")
    val midEdge = Textual(t"$pad${style.charset(top = style.innerLines, bottom = style.innerLines)}$pad")

    def recur(widths: IArray[Int], rows: LazyList[TableRow[TextType]]): LazyList[TextType] =
      rows match
        case row #:: tail =>
          val lines = (0 until row.height).map: lineNumber =>
            widths.indices.map: index =>
              val cell = row(index)
              if cell.minHeight > lineNumber
              then
                cell.textAlign.pad(cell(lineNumber), widths(index), lineNumber == cell.minHeight - 1)
              else Textual((t" "*widths(index)))
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

      Textual:
        Text.fill(width): index =>
          def vertical(bitSet: sci.BitSet, line: BoxLine): BoxLine =
            if bitSet.contains(index) then line else BoxLine.Blank

          if index == 0 then
            style.charset
              (top    = vertical(ascenders, style.sideLines),
               right  = horizontal.or(BoxLine.Blank),
               bottom = vertical(descenders, style.sideLines),
               left   = BoxLine.Blank)
          else if index == (width - 1) then
            style.charset
              (top    = vertical(ascenders, style.sideLines),
               right  = BoxLine.Blank,
               bottom = vertical(descenders, style.sideLines),
               left   = horizontal.or(BoxLine.Blank))
          else
            style.charset
              (top    = vertical(ascenders, style.innerLines),
               right  = horizontal.or(BoxLine.Blank),
               bottom = vertical(descenders, style.innerLines),
               left   = horizontal.or(BoxLine.Blank))

    val topLine = if style.topLine.absent then LazyList() else LazyList(rule(Unset, sections.head.widths))
    val midRule = rule(sections.head.widths, sections.head.widths)
    val bottomLine = if style.bottomLine.absent then LazyList() else LazyList(rule(sections.head.widths, Unset))
    val body = sections.to(LazyList).flatMap { section => midRule #:: recur(section.widths, section.rows) }

    topLine #::: body.tail #::: bottomLine

case class Table[RowType, TextType: {ClassTag, Textual as textual}](columns0: Column[RowType, TextType]*):
  table =>

  val columns: IArray[Column[RowType, TextType]] = IArray.from(columns0)
  val titles: Seq[IArray[IArray[TextType]]] = Seq(IArray.from(columns.map(_.title.cut(t"\n"))))

  def tabulate(data: Seq[RowType]): Tabulation[TextType] { type Row = RowType } = new Tabulation[TextType]:
    type Row = RowType

    val columns: IArray[Column[Row, TextType]] = table.columns
    val titles: Seq[IArray[IArray[TextType]]] = table.titles
    val dataLength: Int = data.length
    val rows: Seq[IArray[IArray[TextType]]] = data.map { row => columns.map(_.get(row).cut(t"\n")) }
