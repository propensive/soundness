/*
    Caesura, version 0.4.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package caesura

import wisteria.*
import rudiments.*
import gossamer.*

import scala.annotation.*

trait Format:
  protected val separator: Char

  def parse(line: Txt): Row =
    @tailrec
    def parseLine(items: Vector[Txt], idx: Int, quoted: Boolean, start: Int, end: Int,
                      join: Boolean): Vector[Txt] =
      if line.length <= idx then
        if join then items.init :+ items.last + line.slice(start, if end < 0 then idx else end)
        else items :+ line.slice(start, if end < 0 then idx else end)
      else 
        val ch = try line(idx) catch case error@OutOfRangeError(_, _, _) => throw Impossible(error)
        
        (ch: @switch) match
          case `separator` =>
            if quoted then parseLine(items, idx + 1, quoted, start, end, join)
            else
              val elems: Vector[Txt] = if start < 0 then items :+ str"" else
                val suffix = line.slice(start, if end == -1 then idx else end)
                if join then items.init :+ items.last + suffix else items :+ suffix
  
              parseLine(elems, idx + 1, quoted = false, idx + 1, -1, join = false)
  
          case '"' =>
            if quoted then parseLine(items, idx + 1, quoted = false, start, idx, join = join)
            else if end != -1 then
              parseLine(items :+ line.slice(start, idx), idx + 1, quoted = true, idx + 1, -1,
                  join = true)
            else parseLine(items, idx + 1, quoted = true, idx + 1, -1, join = false)
  
          case ch: Char =>
            parseLine(items, idx + 1, quoted, start, end, join)

    Row(parseLine(Vector(), 0, quoted = false, 0, -1, join = false)*)

  def serialize(row: Row): Txt = row.elems.map(escape).join(separator.show)
  protected def escape(str: Txt): Txt

object Row:
  def from[T](value: T)(using writer: Csv.Writer[T]): Row = writer.write(value)

case class Row(elems: Txt*):
  def as[T](using decoder: Csv.Reader[T]): T = decoder.decode(this)

object Csv extends Format:

  given clairvoyant.HttpResponse[Csv, Txt] with
    def mimeType: String = "text/csv"
    def content(value: Csv): Txt = value.rows.map(Csv.serialize(_)).join(str"\n")

  given Reader[String] = _.elems.head.s
  given Reader[Txt] = _.elems.head
  given Reader[Int] = _.elems.head.s.toInt
  given Reader[Boolean] = _.elems.head == str"true"
  given Reader[Double] = _.elems.head.s.toDouble
  given Reader[Byte] = _.elems.head.s.toByte
  given Reader[Short] = _.elems.head.s.toShort
  given Reader[Float] = _.elems.head.s.toFloat
  given Reader[Char] = _.elems.head.s.head

  object Reader extends ProductDerivation[Reader]:
    def join[T](caseClass: CaseClass[Reader, T]): Reader[T] = Reader[T](
      fn = { row =>
        @annotation.tailrec
        def parseParams(row: Row, typeclasses: Seq[Reader[?]], params: Vector[Any]): T =
          if typeclasses.isEmpty then caseClass.rawConstruct(params)
          else
            val typeclass = typeclasses.head
            val appended = params :+ typeclass.decode(Row(row.elems.take(typeclass.width)*))
            parseParams(Row(row.elems.drop(typeclass.width)*), typeclasses.tail, appended)
        val typeclasses = caseClass.params.map(_.typeclass)
        parseParams(row, typeclasses, Vector())
      },
      width = caseClass.params.map(_.typeclass.width).sum
    )

    def apply[T](fn: Row => T, width: Int = 1): Reader[T] =
      val colWidth = width
      new Reader[T]:
        def decode(elems: Row): T = fn(elems)
        override def width: Int = colWidth

  trait Reader[T]:
    def decode(elems: Row): T
    def width: Int = 1

  given Writer[String] = s => Row(Txt(s))
  given Writer[Txt] = s => Row(s)
  given Writer[Int] = i => Row(i.show)
  given Writer[Boolean] = b => Row(b.show)
  given Writer[Byte] = b => Row(b.show)
  given Writer[Short] = s => Row(s.show)
  given Writer[Float] = f => Row(Txt(f.toString))
  given Writer[Double] = d => Row(Txt(d.toString))
  given Writer[Char] = c => Row(c.show)

  object Writer extends ProductDerivation[Writer]:
    def join[T](caseClass: CaseClass[Writer, T]): Writer[T] = (value: T) =>
      Row(caseClass.params.flatMap {
        param => param.typeclass.write(param.deref(value)).elems
      }*)

  trait Writer[T]:
    def write(value: T): Row

  override val separator = ','
  def escape(str: Txt): Txt =
    val c = str.s.count { ch => ch.isWhitespace || ch == '"' || ch == ',' }
    if c > 0 then str""""${str.s.replaceAll("\"", "\"\"").nn}"""" else str

extension [T](value: Seq[T])
  def csv(using Csv.Writer[T]): Csv = Csv(value.map(summon[Csv.Writer[T]].write(_))*)
  def tsv(using Csv.Writer[T]): Tsv = Tsv(value.map(summon[Csv.Writer[T]].write(_))*)

case class Csv(rows: Row*)
case class Tsv(rows: Row*)

object Tsv extends Format:
  override val separator = '\t'
  def escape(str: Txt): Txt = Txt(str.s.replaceAll("\t", "        ").nn)

  given clairvoyant.HttpResponse[Csv, Txt] with
    def mimeType: String = str"text/tab-separated-values".s
    def content(value: Csv): Txt =
      value.rows.map(Tsv.serialize(_)).join(str"\n")