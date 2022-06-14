/*
    Caesura, version 0.4.0. Copyright 2018-22 Jon Pretty, Propensive OÜ.

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
import turbulence.*

trait RowFormat:
  protected val separator: Char
  type Type
  def wrap(seq: List[Row]): Type
  
  def parse[T](input: T)(using source: Source[T], readable: Readable[LazyList[Line]])
           : Type throws StreamCutError | readable.E | source.E =
      wrap:
        readable.read(source.read(input)).to(List).map:
          line => parseLine(line.text)

  def parseLine(line: Text): Row =
    @tailrec
    def parseLine(items: Vector[Text], idx: Int, quoted: Boolean, start: Int, end: Int,
                      join: Boolean): Vector[Text] =
      if line.length <= idx then
        if join then items.init :+ t"${items.last}${line.slice(start, if end < 0 then idx else end)}"
        else items :+ line.slice(start, if end < 0 then idx else end)
      else 
        val ch = try line(idx) catch case error: OutOfRangeError => throw Impossible(error)
        
        (ch: @switch) match
          case `separator` =>
            if quoted then parseLine(items, idx + 1, quoted, start, end, join)
            else
              val elems: Vector[Text] = if start < 0 then items :+ t"" else
                val suffix = line.slice(start, if end == -1 then idx else end)
                if join then
                  val part: Text = t"${items.last}${suffix.s}"
                  items.init :+ part
                else items :+ suffix
  
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

  def serialize(row: Row): Text = row.elems.map(escape).join(separator.show)
  protected def escape(str: Text): Text

object Row:
  def from[T](value: T)(using writer: Csv.Writer[T]): Row = writer.write(value)
  given Show[Row] = _.elems.join(t",")

case class Row(elems: Text*):
  def as[T](using decoder: Csv.Reader[T]): T = decoder.decode(this)

object Csv extends RowFormat:
  type Type = Csv
  def wrap(seq: List[Row]): Csv = Csv(seq)
  given Show[Csv] = _.rows.map(serialize).join(t"\n")

  given clairvoyant.HttpResponse[Csv] with
    def mediaType: String = "text/csv"
    def content(value: Csv): LazyList[IArray[Byte]] =
      LazyList(value.rows.map(Csv.serialize(_)).join(t"\n").bytes)

  given Reader[String] = _.elems.head.s
  given Reader[Text] = _.elems.head
  given Reader[Int] = value => Int.unapply(value.elems.head).get
  given Reader[Boolean] = value => Boolean.unapply(value.elems.head).get
  given Reader[Double] = value => Double.unapply(value.elems.head).get
  given Reader[Byte] = value => Byte.unapply(value.elems.head).get
  given Reader[Short] = value => Short.unapply(value.elems.head).get
  given Reader[Float] = value => Float.unapply(value.elems.head).get
  given Reader[Char] = value => Char.unapply(value.elems.head).get

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

  given Writer[String] = s => Row(Text(s))
  given Writer[Text] = s => Row(s)
  given Writer[Int] = i => Row(i.show)
  given Writer[Boolean] = b => Row(b.show)
  given Writer[Byte] = b => Row(b.show)
  given Writer[Short] = s => Row(s.show)
  given Writer[Float] = f => Row(Showable(f).show)
  given Writer[Double] = d => Row(Showable(d).show)
  given Writer[Char] = c => Row(c.show)

  object Writer extends ProductDerivation[Writer]:
    def join[T](caseClass: CaseClass[Writer, T]): Writer[T] = (value: T) =>
      Row(caseClass.params.flatMap {
        param => param.typeclass.write(param.deref(value)).elems
      }*)

  trait Writer[T]:
    def write(value: T): Row

  override val separator = ','
  def escape(str: Text): Text =
    val c = str.count { ch => ch.isWhitespace || ch == '"' || ch == ',' }
    if c > 0 then t""""${str.s.replaceAll("\"", "\"\"").nn}"""" else str

extension [T](value: Seq[T])
  def csv(using Csv.Writer[T]): Csv = Csv(value.to(List).map(summon[Csv.Writer[T]].write(_)))
  def tsv(using Csv.Writer[T]): Tsv = Tsv(value.to(List).map(summon[Csv.Writer[T]].write(_)))

case class Csv(rows: List[Row]):
  def as[T: Csv.Reader]: List[T] = rows.map(_.as[T])

case class Tsv(rows: List[Row]):
  def as[T: Csv.Reader]: List[T] = rows.map(_.as[T])

object Tsv extends RowFormat:
  type Type = Tsv
  def wrap(seq: List[Row]): Tsv = Tsv(seq)
  override val separator = '\t'
  def escape(str: Text): Text = Text(str.s.replaceAll("\t", "        ").nn)
  given Show[Tsv] = _.rows.map(serialize).join(t"\n")

  given clairvoyant.HttpResponse[Tsv] with
    def mediaType: String = t"text/tab-separated-values".s
    
    def content(value: Tsv): LazyList[IArray[Byte]] =
      LazyList(value.rows.map(Tsv.serialize(_)).join(t"\n").bytes)

