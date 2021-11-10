/*
    Escritoire, version 2.4.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire2

import escapade.*
import gossamer.*
import rudiments.*

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.reflect.ClassTag

enum Breaks:
  case Never, Word, Zwsp, Syllable, Character

enum Alignment:
  case Left, Right, Center, Justify

case class ColumnWidth(min: Int, max: Int)

enum BorderStyle:
  case DoubleSingle, SingleTight, SingleSpaced, Spaced

  def spacing(cols: Int): Int = this match
    case DoubleSingle => 3*cols + 1
    case SingleTight  => 3*cols + 1
    case SingleSpaced => 2*cols - 2
    case Spaced       => 2*cols - 2

object Column:
  def apply[T, S: AnsiShow, H: AnsiShow]
           (title: H, cell: T => S, breaks: Breaks = Breaks.Zwsp,
                alignment: Alignment = Alignment.Left,
                width: ColumnWidth = ColumnWidth(4, 100)): Column[T] =
    Column[T](title.ansi, t => cell(t).ansi, breaks, alignment, width)

case class Column[T](title: AnsiString, cell: T => AnsiString, breaks: Breaks, alignment: Alignment,
                         width: ColumnWidth)

case class TableWidthError(total: Int) extends Exception("escritoire: the table width is not in the valid range")

case class Tabulation[T: ClassTag](cols: Column[T]*):

  lazy val colsArray: IArray[Column[T]] = IArray.from(cols)

  @tailrec 
  final def widths(cols: List[Column[T]], flex: Int, map: Map[Column[T], Int]): Map[Column[T], Int] =
    cols match
      case Nil =>
        map
      
      case col :: rest =>
        val w = (flex.toDouble/rest.size + 0.5).toInt.min(col.width.max).max(col.width.min)
        widths(rest, flex - w, map.updated(col, (col.width.min + w)))

  @tailrec 
  final def rewidth(cols: List[Column[T]], proposed: Map[Column[T], Double], map: Map[Column[T], Int]): Map[Column[T], Int] =
    cols match
      case Nil =>
        map
      
      case col :: rest =>
        val w = (proposed(col) + 0.5).toInt.min(col.width.max).max(col.width.min)
        rewidth(rest, proposed, map.updated(col, (col.width.min + w)))

  def initialWidths(total: Int): IArray[Int] throws TableWidthError =
    val mins = colsArray.map(_.width.min)
    val maxs = colsArray.map(_.width.max)
    if total < mins.sum || total > maxs.sum then throw TableWidthError(total)

    val ws = widths(cols.sortBy { c => c.width.max - c.width.min }.to(List), total - mins.sum, Map())
    colsArray.map(ws(_))

  def tabulate(rows: Seq[T], width: Int, style: BorderStyle)
              : Unit throws TableWidthError =//IArray[AnsiString] throws TableWidthError =
    val cells: IArray[IArray[AnsiString]] =
      val rowsArray: IArray[T] = IArray.from(rows)
      rowsArray.map { row => colsArray.map { col => col.cell(row) } }

    val inits = initialWidths(width - style.spacing(cols.length))

    def height(content: Text, width: Int): Int throws TableWidthError =
      var idx = 0
      var lastSpace = -1
      var lastZwsp = -1
      var lineStart = 0
      var height = 0
      var skip = 0
      
      while idx < content.length
      do try
        content(idx) match
          case '\n'     => height += 1
                           lineStart = idx + 1
                           skip = 0
          case ' '      => lastSpace = idx
          case '\u200b' => lastZwsp = idx
                           skip += 1
          case char     => if idx - lineStart - skip > width then
                             if lastSpace < lineStart then throw TableWidthError(-1)
                             height += 1
                             lineStart = lastSpace + 1
                             skip = 0
        idx += 1
      catch case e: OutOfRangeError => throw Impossible("can't be out of range")

      height + 1
    
    def optimize(widths: IArray[Int], seen: TreeMap[Int, IArray[Int]] = TreeMap(), count: Int = 10)
                : IArray[Int] =
      if count == 0 then seen(seen.keys.min)
      else
        val heights: IArray[IArray[Int]] =
          cells.map { row => IArray.range(0, widths.length).map { i =>
            height(row(i).plain, widths(i))
          } }
        
        println("Heights: "+heights.to(List).map(_.to(List)))
  
        val maxHeights: IArray[Int] = heights.map(_.max)
        val totalHeight = maxHeights.sum
  
        val densities: IArray[IArray[Double]] =
          heights.zip(maxHeights).map { (row, h) => row.map(_.toDouble/h) }
        
        println("Densities: "+densities.to(List).map(_.to(List)))
        
        val meanDensities: IArray[Double] =
          densities.fold(widths.map(_ => 0.0))(_.zip(_).map(_ + _))
  
        val scaled: IArray[Double] = widths.zip(meanDensities).map(_*_)
        val scaleFactor = width/scaled.sum
        val approxWidths: IArray[Double] = scaled.map(_*scaleFactor)
        
        val optimized = rewidth(
          cols.sortBy { c => c.width.max - c.width.min }.to(List),
          colsArray.zip(approxWidths).to(Map),
          Map()
        )
        
        val newWidths = colsArray.map(optimized(_))
        val newSeen = seen.updated(totalHeight, newWidths)
  
        if newSeen.keys.min > totalHeight then optimize(newWidths, newSeen, count - 1)
        else newSeen(newSeen.keys.min)
    
    println(optimize(inits).to(List))
      

@main def run(): Unit =
  import unsafeExceptions.canThrowAny
  case class Person(name: Text, age: Int)
 
  val tab = Tabulation[Person](Column("Name", _.name), Column("Age", _.age))

  tab.tabulate(List(Person(t"Jonathan Luke Pretty", 38), Person(t"Emma", 78)), 17, BorderStyle.DoubleSingle)
    