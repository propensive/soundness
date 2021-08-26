/*
  
  Caesura, version 0.1.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

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
package caesura

import wisteria.*

import scala.annotation.*

trait Format:
  protected val separator: Char

  def parse(line: String): Row =
    @tailrec
    def parseLine(items: Vector[String], idx: Int, quoted: Boolean, start: Int, end: Int,
                      join: Boolean): Vector[String] =
      if line.length <= idx then
        if join then items.init :+ items.last + line.substring(start, if (end == -1) idx else end)
        else items :+ line.substring(start, if (end == -1) idx else end)
      else (line(idx): @switch) match
        case `separator` =>
          if quoted then parseLine(items, idx + 1, quoted, start, end, join)
          else
            val elems = if (start == -1) items :+ "" else
              val suffix = line.substring(start, if end == -1 then idx else end)
              if join then items.init :+ items.last + suffix else items :+ suffix

            parseLine(elems, idx + 1, quoted = false, idx + 1, -1, join = false)

        case '"' =>
          if quoted then parseLine(items, idx + 1, quoted = false, start, idx, join = join)
          else if end != -1 then
            parseLine(items :+ line.substring(start, idx), idx + 1, quoted = true, idx + 1, -1,
                join = true)
          else parseLine(items, idx + 1, quoted = true, idx + 1, -1, join = false)

        case _ =>
          parseLine(items, idx + 1, quoted, start, end, join)

    Row(parseLine(Vector(), 0, quoted = false, 0, -1, join = false)*)

  def apply(row: Row): String = row.elems.map(escape).mkString(separator.toString)
  protected def escape(str: String): String

object Row:
  def from[T](value: T)(using writer: Csv.Writer[T]): Row = writer.write(value)

case class Row(elems: String*):
  def as[T](using decoder: Csv.Reader[T]): T = decoder.decode(this)

object Csv extends Format:
  
  given Reader[String] = _.elems.head
  given Reader[Int] = _.elems.head.toInt
  given Reader[Boolean] = _.elems.head == "true"
  given Reader[Double] = _.elems.head.toDouble
  given Reader[Byte] = _.elems.head.toByte
  given Reader[Short] = _.elems.head.toShort
  given Reader[Float] = _.elems.head.toFloat
  given Reader[Char] = _.elems.head.head

  object Reader extends ProductDerivation[Reader]:
    def join[T](caseClass: CaseClass[Reader, T]): Reader[T] = Reader[T](
      fn = { row =>
        @annotation.tailrec
        def parseParams(row: Row, typeclasses: Seq[Reader[_]], params: Vector[Any]): T =
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

  given Writer[String] = s => Row(s)
  given Writer[Int] = i => Row(i.toString)
  given Writer[Boolean] = b => Row(b.toString)
  given Writer[Byte] = b => Row(b.toString)
  given Writer[Short] = s => Row(s.toString)
  given Writer[Float] = f => Row(f.toString)
  given Writer[Double] = d => Row(d.toString)
  given Writer[Char] = c => Row(c.toString)

  object Writer extends ProductDerivation[Writer]:
    def join[T](caseClass: CaseClass[Writer, T]): Writer[T] = (value: T) =>
      Row(caseClass.params.flatMap {
        param => param.typeclass.write(param.deref(value)).elems
      }*)

  trait Writer[T]:
    def write(value: T): Row

  override val separator = ','
  def escape(str: String): String =
    val c = str.count { ch => ch == ' ' || ch == '"' }
    if c > 0 then s""""${str.replaceAll("\"", "\"\"")}"""" else str

object Tsv extends Format:
  override val separator = '\t'
  def escape(str: String): String = str.replaceAll("\t", "        ")