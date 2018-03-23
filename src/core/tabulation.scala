package escritoire

import language.implicitConversions

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.ListMap
import annexation._

object Ansi {
  val esc = 27.toChar
  def rgb(red: Int, green: Int, blue: Int) = s"$esc[38;2;$red;$green;${blue}m"
  val reset: String = s"$esc[39;49m"

  object Color {
    // Colors are taken from the solarized palette
    val base03: String = rgb(0, 43, 54)
    val base02: String = rgb(7, 54, 66)
    val base01: String = rgb(88, 110, 117)
    val base00: String = rgb(101, 123, 131)
    val base0: String = rgb(131, 148, 150)
    val base1: String = rgb(147, 161, 161)
    val base2: String = rgb(238, 232, 213)
    val base3: String = rgb(253, 246, 227)
    val yellow: String = rgb(181, 137, 0)
    val orange: String = rgb(203, 75, 22)
    val red: String = rgb(220, 50, 47)
    val magenta: String = rgb(211, 54, 130)
    val violet: String = rgb(108, 113, 196)
    val blue: String = rgb(38, 139, 210)
    val cyan: String = rgb(42, 161, 152)
    val green: String = rgb(133, 153, 0)

    def strip(string: String): String = string.replaceAll("""\e\[?.*?[\@-~]""", "")
  }
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

  def lines(maxWidth: Int, rows: List[Row]): List[String] = {
    val titleStrings = headings.to[List].map { h =>
      List(h.name)
    }
    val underlines = titleStrings.map(_.map(_.replaceAll(".", "-")))
    val data: List[List[List[String]]] = titleStrings :: underlines :: (rows.map { row =>
      headings.to[List].map(_.get(row).split("\n").to[List])
    })

    val maxWidths = data.foldLeft(Vector.fill(headings.size)(0)) {
      case (widths, next) =>
        widths.zip(next).map { case (w, xs) => xs.map(_.length).max max w }
    }

    val totalWidth = maxWidths.sum + maxWidths.length * padding
    val flexibleWidths = headings.filter(_.width == FlexibleWidth).size

    val widths =
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

    data.flatMap { cells =>
      cells
        .zip(widths)
        .zip(headings)
        .map {
          case ((lines, width), heading) =>
            lines.padTo(cells.map(_.length).max, "").map(pad(_, width, heading.align))
        }
        .transpose
        .map(_.mkString(" " * padding))
    }
  }

  private def pad(str: String, width: Int, alignment: Alignment): String =
    alignment match {
      case LeftAlign =>
        if (str.length > width) str.dropRight(str.length - width)
        else str + (" " * (width - str.length))
      case RightAlign =>
        if (str.length > width) str.drop(str.length - width)
        else (" " * (width - str.length)) + str
      case CenterAlign =>
        if (str.length > width) pad(str.drop((str.length - width) / 2), width, LeftAlign)
        else pad(str+" "*((width - str.length)/2), width, RightAlign)
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
}

trait AnsiShow[T] { def show(value: T): String }
