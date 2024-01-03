/*
    Caesura, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import wisteria2.*
import rudiments.*
import gossamer.*
import fulminate.*
import turbulence.*
import spectacular.*
import hieroglyph.*
import anticipation.*

trait RowFormat:
  protected val separator: Char
  type Type
  def wrap(seq: List[Row]): Type
  
  def parse[SourceType](source: SourceType)(using readable: Readable[SourceType, Line])
           : Type = wrap:
      readable.read(source).to(List).map:
        line => parseLine(line.content)

  def parseLine(line: Text): Row =
    @tailrec
    def parseLine(items: Vector[Text], idx: Int, quoted: Boolean, start: Int, end: Int,
                      join: Boolean): Vector[Text] =
      if line.length <= idx then
        if join then items.init :+ t"${items.last}${line.slice(start, if end < 0 then idx else end)}"
        else items :+ line.slice(start, if end < 0 then idx else end)
      else 
        val ch = try line(idx) catch case error: OutOfRangeError => throw Mistake(error)
        
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
  def from[RowType](value: RowType)(using writer: Csv.Writer[RowType]): Row = writer.write(value)
  given Show[Row] = _.elems.join(t",")

case class Row(elems: Text*):
  def as[CellType: ColumnParser]: CellType = summon[ColumnParser[CellType]].decode(this)

object Csv extends RowFormat:
  type Type = Csv
  def wrap(seq: List[Row]): Csv = Csv(seq)
  given Show[Csv] = _.rows.map(serialize).join(t"\n")

  given (using CharEncoder): GenericHttpResponseStream[Csv] with
    def mediaType: Text = "text/csv".tt
    def content(value: Csv): LazyList[IArray[Byte]] =
      LazyList(value.rows.map(Csv.serialize(_)).join(t"\n").bytes)

  given Writer[String] = s => Row(Text(s))
  given Writer[Text] = s => Row(s)
  given Writer[Int] = i => Row(i.show)
  given (using BooleanStyle): Writer[Boolean] = b => Row(b.show)
  given Writer[Byte] = b => Row(b.show)
  given Writer[Short] = s => Row(s.show)
  given Writer[Float] = f => Row(f.toString.tt)
  given Writer[Double] = d => Row(d.toString.tt)
  given Writer[Char] = c => Row(c.show)

  object Writer extends ProductDerivation[Writer]:
    inline def join[DerivationType: ReflectiveProduct]: Writer[DerivationType] = value =>
      Row(params(value)(typeclass.write(param).elems).to(List).flatten*)
    
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
  def as[T: ColumnParser]: List[T] = rows.map(_.as[T])

case class Tsv(rows: List[Row]):
  def as[T: ColumnParser]: List[T] = rows.map(_.as[T])

object Tsv extends RowFormat:
  type Type = Tsv
  def wrap(seq: List[Row]): Tsv = Tsv(seq)
  override val separator = '\t'
  def escape(str: Text): Text = Text(str.s.replaceAll("\t", "        ").nn)
  given Show[Tsv] = _.rows.map(serialize).join(t"\n")

  given (using CharEncoder): GenericHttpResponseStream[Tsv] with
    def mediaType: Text = "text/tab-separated-values".tt
    
    def content(value: Tsv): LazyList[IArray[Byte]] =
      LazyList(value.rows.map(Tsv.serialize(_)).join(t"\n").bytes)


object ColumnParser extends ProductDerivation[ColumnParser]:
  
  inline def join[DerivationType: ReflectiveProduct]: ColumnParser[DerivationType] =
    new ColumnParser[DerivationType]:
      def decode(elems: Row): DerivationType =
        var count: Int = 0
        product:
          val row = Row(elems.elems.drop(count)*)
          count += typeclass.width
          typeclass.decode(row)
        
      override def width: Int =
        var count: Int = 0
        
        // FIXME: constructing the new product is unnecessary, but Wisteria does not currently provide a way
        // to iterate over the parameters
        product:
          count += typeclass.width
          null.asInstanceOf[Any]
        
        count
  
  given decoder[T: Decoder]: ColumnParser[T] = _.elems.head.decodeAs[T]

  def apply[T](fn: Row => T, width: Int = 1): ColumnParser[T] =
    val colWidth = width
    
    new ColumnParser[T]:
      def decode(row: Row): T = fn(row)
      override def width: Int = colWidth

trait ColumnParser[T]:
  def decode(elems: Row): T
  def width: Int = 1
