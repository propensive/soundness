/*
  
  Escritoire, version 1.0.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at
  
      http://www.apache.org/licenses/LICENSE-2.0
 
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

*/
package escritoire

import language.implicitConversions

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.ListMap

object Ansi {
  val esc = 27.toChar
  val reset: String = s"$esc[39;49m"
  def bold(str: String): String = s"$esc[1m$str$esc[22m"
  def underline(str: String): String = s"$esc[4m$str$esc[24m"
  def strike(str: String): String = s"$esc[9m$str$esc[29m"
  def italic(str: String): String = s"$esc[3m$str$esc[23m"
  def reverse(str: String): String = s"$esc[7m$str$esc[27m"

  case class Color(red: Int, green: Int, blue: Int) {
    def apply(str: String): String = s"$esc[38;2;$red;$green;${blue}m$str$reset"
    def fade(amount: Double) = Color((red*amount).toInt, (green*amount).toInt, (blue*amount).toInt)
    def brighten(amount: Double) = Color(
      (255 - (255 - red)*amount).toInt,
      (255 - (255 - green)*amount).toInt,
      (255 - (255 - blue)*amount).toInt
    )
  }

  object Color {
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
  }
  def strip(string: String): String = string.replaceAll("""\e\[?.*?[\@-~]""", "")
}

sealed trait Alignment
case object LeftAlign extends Alignment
case object RightAlign extends Alignment
case object CenterAlign extends Alignment

sealed trait Width
case class ExactWidth(n: Int) extends Width
case object FlexibleWidth extends Width

object Heading {
  def apply[Row, V: AnsiShow](name: String,
                              getter: Row => V,
                              width: Width = FlexibleWidth,
                              align: Alignment = LeftAlign): Heading[Row] =
    new Heading[Row](name, width, align) {
      def get(r: Row): String = implicitly[AnsiShow[V]].show(getter(r))
    }
}

abstract class Heading[Row](val name: String,
                            val width: Width = FlexibleWidth,
                            val align: Alignment = LeftAlign) {
  def get(r: Row): String
}

case class Tabulation[Row](headings: Heading[Row]*) {

  def padding: Int = 2

  def tabulate(maxWidth: Int, rows: Seq[Row]): Seq[String] = {
    val titleStrings = headings.to[List].map { h => List(h.name) }
    
    val data: Seq[List[List[String]]] = titleStrings.map(_.map(Ansi.underline(_))) +: (rows.map { row =>
      headings.to[List].map { _.get(row).split("\n").to[List] }
    })

    val tight = !data.exists(_.exists(_.length > 1))

    val paddedData = if(!tight) data.map(_.map("" :: _)) else data

    val maxWidths = paddedData.foldLeft(Vector.fill(headings.size)(0)) {
      case (widths, next) =>
        widths.zip(next).map { case (w, xs) => xs.map { c => Ansi.strip(c).length }.max max w }
    }

    val totalWidth = maxWidths.sum + maxWidths.length * padding
    val flexibleWidths = headings.filter(_.width == FlexibleWidth).size

    val initialWidths =
      headings
        .zip(maxWidths)
        .foldLeft((Vector[Int](), maxWidth, flexibleWidths)) {
          case ((widths, off, todo), (head, maxCellWidth)) =>
            head.width match {
              case ExactWidth(w) => (widths :+ w, off, todo)
              case FlexibleWidth =>
                val chosenWidth = off / todo min maxCellWidth
                (widths :+ chosenWidth, off - chosenWidth, todo - 1)
            }
        }
        ._1

    val spare = totalWidth - initialWidths.sum

    val (widths, _) = initialWidths.zip(maxWidths).foldLeft((Vector[Int](), spare)) {
      case ((agg, spare), (act, req)) =>
        if(act == req) (agg :+ act, spare)
        else {
          val allocated = spare min (req - act)
          (agg :+ (act + allocated), spare - allocated)
        }
    }

    paddedData.flatMap { cells =>
      cells
        .zip(widths)
        .zip(headings)
        .map {
          case ((lines, width), heading) =>
            lines.padTo(cells.map(_.length).max, "").map(pad(_, width, heading.align))
        }
        .transpose
        .map(_.mkString(" " * padding))
    } ++ (if(tight) Nil else List(""))
  }

  private def pad(str: String, width: Int, alignment: Alignment): String = {
    val stripped = Ansi.strip(str).length
    alignment match {
      case LeftAlign =>
        if (stripped > width) str.dropRight(stripped - width)+Ansi.reset
        else str + (" " * (width - stripped))
      case RightAlign =>
        if (stripped > width) str.drop(stripped - width)+Ansi.reset
        else (" " * (width - stripped)) + str
      case CenterAlign =>
        if (stripped > width) pad(str.drop((stripped - width) / 2), width, LeftAlign)+Ansi.reset
        else pad(str+" "*((width - stripped)/2), width, RightAlign)
    }
  }
}

object AnsiShow {

  private val decimalFormat = {
    val df = new java.text.DecimalFormat()
    df.setMinimumFractionDigits(3)
    df.setMaximumFractionDigits(3)
    df
  }

  implicit val string: AnsiShow[String] = identity
  implicit val int: AnsiShow[Int] = _.toString
  implicit val double: AnsiShow[Double] = decimalFormat.format(_)
  implicit val lines: AnsiShow[List[String]] = _.mkString("\n")
}

trait AnsiShow[T] { def show(value: T): String }
