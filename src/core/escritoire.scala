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
          breaks: Breaks = Breaks.Space, hide: Boolean = false, confinement: Confinement)
      (get: RowType -> CellType)
      (using textual: Textual[TextType], columnAlignment: ColumnAlignment[CellType] = ColumnAlignment.left)
      (using textual.ShowType[CellType])
      : Column[RowType, TextType] =

    def contents(row: RowType): TextType = textual.show(get(row))
    
    Column(title, contents, breaks, align.or(columnAlignment.alignment()), width, hide, confinement)

case class Column
    [RowType, TextType: Textual]
    (title: TextType, get: RowType -> TextType, breaks: Breaks, align: Alignment, width: Optional[Int],
        hide: Boolean, confinement: Confinement)

trait Confinement:
  def width[TextType: Textual](count: Int, get: Int -> IArray[TextType], maxWidth: Int, slack: Double): Optional[Int]
  def confine(text: Text, width: Int): List[Text]

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
  def cells: Seq[IArray[IArray[TextType]]] // rows of columns of lines
  def dataLength: Int

  def layout(width: Int)(using style: TableStyle, metrics: TextMetrics, textual: Textual[TextType]): Unit =
    
    def calculate(slack: Double): (Int, Int) =
      val widths = columns.indices.map: index =>
        columns(index).confinement.width[TextType](dataLength, cells(index)(_), width, slack)

      val shown: Int = widths.count(_.present)

      (widths.count(_.present), widths.sumBy(_.or(0)) + style.cost(shown))
    
    def bisect(minSlack: Double, minWidth: Int, maxSlack: Double, maxWidth: Int): Double =
      if maxWidth - minWidth > 1 then
        val slack2 = (maxSlack + minSlack)/2
        val width2 = calculate(slack2)(1)
        
        if width2 == width then slack2
        else if width2 > width then bisect(minSlack, minWidth, slack2, width2)
        else bisect(slack2, width2, maxSlack, maxWidth)
      
      else minSlack
    
    val minSlack = bisect(0.0, 0, 1.0, calculate(1.0)(1))

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
      
      val cells: Seq[IArray[IArray[TextType]]] =
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

