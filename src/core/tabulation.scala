/*
    Escritoire, version 0.4.0. Copyright 2018-22 Jon Pretty, Propensive OÜ.

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
import wisteria.*

import annotation.*

enum Align:
  case Left
  case Right
  case Center

enum Width:
  case Exact(n: Int)
  case Flexible

object Column:
  def apply[Row, H: AnsiShow, V: AnsiShow](name: H, getter: Row => V, width: Width = Width.Flexible,
                                  align: Align = Align.Left): Column[Row] =
    new Column[Row](summon[AnsiShow[H]].ansiShow(name), width, align):
      def get(r: Row): AnsiText = summon[AnsiShow[V]].ansiShow(getter(r))

trait Column[Row](val name: AnsiText, val width: Width = Width.Flexible,
                       val align: Align = Align.Left):
  def get(r: Row): AnsiText

case class title(name: AnsiText) extends StaticAnnotation

object Tabulation:

  given [T: AnsiShow]: Tabulation[T] = Tabulation(Column(ansi"", summon[AnsiShow[T]].ansiShow(_)))

case class Tabulation[Row](columns: Column[Row]*):
  def padding: Int = 2

  def tabulate(maxWidth: Int, rows: Seq[Row], ansi: Option[AnsiText] = None): Seq[AnsiText] =
    val reset = ansi.fold(ansi"") { _ => ansi"${27.toChar}${escapes.Reset.on}" }
    val titleStrings = columns.to(List).map(_.name).map(List(_))
    
    val data: Seq[List[List[AnsiText]]] =
      titleStrings.map(_.map { str => ansi"$Bold($str)" }) +: (rows.map { row =>
        columns.to(List).map(_.get(row).cut(t"\n").map(_.ansi))
      })

    val tight = !data.exists(_.exists(_.length > 1))

    val maxWidths: Vector[Int] = data.foldLeft(Vector.fill(columns.size)(0)) {
      (widths, next) => widths.zip(next).map { (w, xs) =>
        xs.map { c => c.length }.max max w
      }
    }

    def rule(left: Char, mid: Char, cross: Char, right: Char) =
      maxWidths.map { w => mid.ansi*(w + 2): AnsiText }.join(ansi"${ansi.getOrElse(ansi"")}$left", ansi"$cross",
          ansi"$right${reset}")
    
    val hr = if tight then Nil else List(rule('╟', '─', '┼', '╢'))
    val endHr = if tight then rule('└', '─', '┴', '┘') else rule('╚', '═', '╧', '╝')
    val startHr = if tight then rule('┌', '─', '┬', '┐') else rule('╔', '═', '╤', '╗')
    val midHr = if tight then rule('├', '─', '┼', '┤') else rule('╠', '═', '╪', '╣')

    val totalWidth = maxWidths.sum + maxWidths.length*padding
    val flexibleWidths = columns.filter(_.width == Width.Flexible).size

    val initialWidths =
      columns.zip(maxWidths).foldLeft((Vector[Int](), maxWidth, flexibleWidths)) {
          case ((widths, off, todo), (head, maxCellWidth)) =>
            head.width match
              case Width.Exact(w) =>
                (widths :+ w, off, todo)
              case Width.Flexible =>
                val chosenWidth = off / todo min maxCellWidth
                (widths :+ chosenWidth, off - chosenWidth, todo - 1)
        }._1

    val spare = totalWidth - initialWidths.sum

    val (widths, _) = initialWidths.zip(maxWidths).foldLeft((Vector[Int](), spare)) {
      case ((agg, spare), (act, req)) =>
        if act == req then (agg :+ act, spare)
        else
          val allocated = spare min (req - act)
          (agg :+ (act + allocated), spare - allocated)
    }

    List(startHr) ++ data.flatMap { cells =>
      hr ++ cells.zip(widths).zip(columns).map { case ((lines, width), heading) =>
        lines.padTo(cells.map(_.length).max, ansi"").map(pad(_, width, heading.align, reset))
      }.transpose.map(_.join(ansi"${ansi.getOrElse(ansi"")}${if tight then ansi"│" else ansi"║"}${reset} ",
          ansi" ${ansi.getOrElse(ansi"")}│${reset} ",
          ansi" ${ansi.getOrElse(ansi"")}${if tight then ansi"│" else ansi"║"}${reset}"))

    }.drop(if tight then 0 else 1).patch(1, List(midHr), if tight then 0 else 1) ++ List(endHr)
  end tabulate

  private def pad(str: AnsiText, width: Int, alignment: Align, reset: AnsiText): AnsiText =
    val stripped = str.length
    alignment match
      case Align.Left =>
        if stripped > width then ansi"${(str.slice(stripped - width, str.length): AnsiText)}$reset"
        else ansi"$str${(ansi" "*(width - stripped))}"
      
      case Align.Right =>
        if stripped > width then ansi"${(str.drop(stripped - width): AnsiText)}$reset"
        else ansi"${(ansi" "*(width - stripped): AnsiText)}$str"
      
      case Align.Center =>
        if stripped > width
        then ansi"${pad(str.drop((stripped - width) / 2), width, Align.Left, reset)}$reset"
        else pad(ansi"$str${ansi" "*((width - stripped)/2)}", width, Align.Right, reset)
