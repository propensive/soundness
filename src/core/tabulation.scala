/*
    Escritoire, version 2.4.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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

object Ansi:
  val esc = 27.toChar
  val reset: String = s"$esc[39;49m"
  def bold(str: String): String = s"$esc[1m$str$esc[22m"
  def underline(str: String): String = s"$esc[4m$str$esc[24m"
  def strike(str: String): String = s"$esc[9m$str$esc[29m"
  def italic(str: String): String = s"$esc[3m$str$esc[23m"
  def reverse(str: String): String = s"$esc[7m$str$esc[27m"
  def up(n: Int): String = s"$esc[${n}A"
  def down(n: Int): String = s"$esc[${n}B"
  def right(n: Int): String = s"$esc[${n}C"
  def left(n: Int): String = s"$esc[${n}D"

  case class Color(red: Int, green: Int, blue: Int):
    def apply(str: String): String = s"${apply()}$str$reset"
    def fade(amount: Double) = Color((red*amount).toInt, (green*amount).toInt, (blue*amount).toInt)
    
    def brighten(amount: Double) = Color(
      (255 - (255 - red)*amount).toInt,
      (255 - (255 - green)*amount).toInt,
      (255 - (255 - blue)*amount).toInt
    )
    
    def apply(): String = s"$esc[38;2;$red;$green;${blue}m"
    def escaped: String = s"%{${apply()}%}"

  object Color:
    // Colors are taken from the solarized palette
    val base03: Color = Color(0, 43, 54)
    val base02: Color = Color(7, 54, 66)
    val base01: Color = Color(88, 110, 117)
    val base00: Color = Color(101, 123, 131)
    val base0: Color = Color(131, 148, 150)
    val base1: Color = Color(147, 161, 161)
    val base2: Color = Color(238, 232, 213)
    val base3: Color = Color(253, 246, 227)
    val yellow: Color = Color(181, 137, 0)
    val orange: Color = Color(203, 75, 22)
    val red: Color = Color(220, 50, 47)
    val magenta: Color = Color(211, 54, 130)
    val violet: Color = Color(108, 113, 196)
    val blue: Color = Color(38, 139, 210)
    val cyan: Color = Color(42, 161, 152)
    val green: Color = Color(133, 153, 0)
  
  def strip(string: String): String = string.replaceAll("""\e\[?.*?[\@-~]""", "").nn

enum Align:
  case Left
  case Right
  case Center

enum Width:
  case Exact(n: Int)
  case Flexible

object Heading:
  def apply[Row, V: AnsiShow](name: String, getter: Row => V, width: Width = Width.Flexible,
                                  align: Align = Align.Left): Heading[Row] =
    new Heading[Row](name, width, align):
      def get(r: Row): String = summon[AnsiShow[V]].show(getter(r))

trait Heading[Row](val name: String, val width: Width = Width.Flexible,
                       val align: Align = Align.Left):
  def get(r: Row): String

case class Tabulation[Row](headings: Heading[Row]*):
  def padding: Int = 2

  def tabulate(maxWidth: Int, rows: Seq[Row], ansi: Option[String] = None): Seq[String] =
    val reset = ansi.fold("") { _ => Ansi.reset }
    val titleStrings = headings.to(List).map(_.name).map(List(_))
    
    val data: Seq[List[List[String]]] = titleStrings.map(_.map(Ansi.bold(_))) +: (rows.map { row =>
      headings.to(List).map(_.get(row).cut("\n").to(List))
    })

    val tight = !data.exists(_.exists(_.length > 1))

    val maxWidths: Vector[Int] = data.foldLeft(Vector.fill(headings.size)(0)) {
      (widths, next) => widths.zip(next).map { (w, xs) =>
        xs.map { c => Ansi.strip(c).length }.max max w
      }
    }

    def rule(left: Char, mid: Char, cross: Char, right: Char) =
      maxWidths.map { w => s"$mid"*(w + 2) }.join(s"${ansi.getOrElse("")}$left", s"$cross",
          s"$right${reset}")
    
    val hr = if tight then Nil else List(rule('╟', '─', '┼', '╢'))
    val endHr = if tight then rule('└', '─', '┴', '┘') else rule('╚', '═', '╧', '╝')
    val startHr = if tight then rule('┌', '─', '┬', '┐') else rule('╔', '═', '╤', '╗')
    val midHr = if tight then rule('├', '─', '┼', '┤') else rule('╠', '═', '╪', '╣')

    val totalWidth = maxWidths.sum + maxWidths.length*padding
    val flexibleWidths = headings.filter(_.width == Width.Flexible).size

    val initialWidths =
      headings.zip(maxWidths).foldLeft((Vector[Int](), maxWidth, flexibleWidths)) {
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
      hr ++ cells.zip(widths).zip(headings).map { case ((lines, width), heading) =>
        lines.padTo(cells.map(_.length).max, "").map(pad(_, width, heading.align, reset))
      }.transpose.map(_.join(s"${ansi.getOrElse("")}${if tight then "│" else "║"}${reset} ",
          s" ${ansi.getOrElse("")}│${reset} ",
          s" ${ansi.getOrElse("")}${if tight then "│" else "║"}${reset}"))

    }.drop(if tight then 0 else 1).patch(1, List(midHr), if tight then 0 else 1) ++ List(endHr)
  end tabulate

  private def pad(str: String, width: Int, alignment: Align, reset: String): String =
    val stripped = Ansi.strip(str).length
    alignment match
      case Align.Left =>
        if stripped > width then str.dropRight(stripped - width)+reset
        else str + (" " * (width - stripped))
      
      case Align.Right =>
        if stripped > width then str.drop(stripped - width)+reset
        else (" " * (width - stripped)) + str
      
      case Align.Center =>
        if stripped > width
        then pad(str.drop((stripped - width) / 2), width, Align.Left, reset)+reset
        else pad(str+" "*((width - stripped)/2), width, Align.Right, reset)

object AnsiShow:
  private val decimalFormat =
    val df = new java.text.DecimalFormat()
    df.setMinimumFractionDigits(3)
    df.setMaximumFractionDigits(3)
    df

  given AnsiShow[String] = identity(_)
  given AnsiShow[Int] = _.toString
  given AnsiShow[Long] = _.toString
  given AnsiShow[Double] = decimalFormat.format(_).nn
  given AnsiShow[Seq[String]] = _.join("\n")

trait AnsiShow[T]:
  def show(value: T): String
