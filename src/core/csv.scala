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

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*

import scala.compiletime.*

case class DsvFormat(header: Boolean, delimiter: Char, quote: Char, escape: Char):
  val Delimiter: Char = delimiter
  val Quote: Char = quote
  val Escape: Char = escape

  def doublingEscapes: Boolean = quote == escape

case class Dsv(rows: LazyList[Row], head: Optional[Row] = Unset, format: Optional[DsvFormat] = Unset):
  lazy val columns: Map[Text, Int] = head.let(_.data.zipWithIndex.to(Map)).or(Map())
  override def toString(): String = rows.to(List).map(_.toString).mkString(" // ")
  def as[ValueType: DsvDecoder]: LazyList[ValueType] = rows.map(_.as[ValueType])

package dsvFormats:
  given DsvFormat as csv = DsvFormat(false, ',', '"', '"')
  given DsvFormat as csvWithHeadings = DsvFormat(true, ',', '"', '"')
  given DsvFormat as tsv = DsvFormat(false, '\t', '"', '"')
  given DsvFormat as tsvWithHeadings = DsvFormat(true, '\t', '"', '"')
  given DsvFormat as ssv = DsvFormat(false, ' ', '"', '"')
  given DsvFormat as ssvWithHeadings = DsvFormat(true, ' ', '"', '"')

case class Row(data: IArray[Text]):
  override def toString(): String = data.to(List).mkString("[", ";", "]")
  def as[CellType: DsvDecoder]: CellType = CellType.decode(this)

  override def hashCode: Int = data.indices.foldLeft(0): (aggregate, index) =>
    aggregate*31 + data(index).hashCode

  override def equals(that: Any): Boolean = that.asMatchable match
    case row: Row =>
      data.length == row.data.length && (data.indices.all { index => data(index) == row.data(index) })

    case _        => false

object Row:
  def apply(iterable: Iterable[Text]): Row = new Row(IArray.from(iterable))
  def apply(text: Text*): Row = new Row(IArray.from(text))

  given (using format: DsvFormat) => Row is Showable =
    _.data.map: cell =>
      if !cell.contains(format.Quote) then cell else
        Text.construct:
          append(format.quote)

          cell.s.foreach: char =>
            if char == format.quote then append(char)
            append(char)

          append(format.quote)
    .join(format.delimiter.show)

object Dsv:
  private enum State:
    case Fresh, Quoted, DoubleQuoted

  def parse[SourceType: Readable by Text](source: SourceType)(using format: DsvFormat): Dsv =
    val rows = recur(source.stream[Text])
    if format.header then Dsv(rows.tail, rows.head, format) else Dsv(rows, Unset, format)

  given (using DsvFormat) => Dsv is Showable = _.rows.map(_.show).join(t"\n")

  private def recur
      (content: LazyList[Text],
       index:   Ordinal        = Prim,
       column:  Int            = 0,
       cells:   Array[Text]    = new Array[Text](0),
       buffer:  TextBuffer     = TextBuffer(),
       state:   State          = State.Fresh)
      (using format: DsvFormat)
          : LazyList[Row] =

    inline def putCell(): Array[Text] =
      val cells2 = if cells.length <= column then cells :+ buffer() else
        cells(column) = buffer()
        cells

      cells2.also(buffer.clear())

    inline def advance() =
      val cells = putCell()
      recur(content, index + 1, column + 1, cells, buffer, State.Fresh)

    inline def next(char: Char): LazyList[Row] =
      buffer.put(char) yet recur(content, index + 1, column, cells, buffer, state)

    inline def quote(): LazyList[Row] = state match
      case State.Fresh  => recur(content, index + 1, column, cells, buffer, State.Quoted)
      case State.Quoted => recur(content, index + 1, column, cells, buffer, State.DoubleQuoted)
      case State.DoubleQuoted =>
        buffer.put(format.Quote)
        recur(content, index + 1, column, cells, buffer, State.Quoted)


    inline def fresh(): Array[Text] = new Array[Text](cells.length)

    inline def row(): LazyList[Row] =
      val cells = putCell()
      buffer.clear()
      Row(unsafely(cells.immutable)) #:: recur(content, index + 1, 0, fresh(), buffer, State.Fresh)

    content match
      case head #:: tail =>
        if !head.has(index) then recur(tail, Prim, column, cells, buffer, state)
        else
          head.s.charAt(index.n0) match
            case format.Delimiter => if state != State.Quoted then advance() else next(format.Delimiter)
            case format.Quote     => quote()
            case '\n' | '\r'      => if column == 0 && buffer.empty
                                     then recur(content, index + 1, 0, cells, buffer, State.Fresh)
                                     else if state != State.Quoted then row()
                                     else next(head.s.charAt(index.n0))
            case char             =>
              buffer.put(char)
              recur(content, index + 1, column, cells, buffer, state)

      case _ =>
        if column == 0 && buffer.empty then LazyList() else row()

trait RowFormat:
  protected val separator: Char
  type Format
  def wrap(list: List[Csv]): Format

  def serialize(row: Csv): Text = row.elems.map(escape).join(separator.show)
  protected def escape(str: Text): Text

object Csv:
  given Csv is Showable = _.elems.join(t",")

case class Csv(elems: Text*)

object DsvEncoder extends ProductDerivation[DsvEncoder]:
  inline def join[DerivationType <: Product: ProductReflection]: DsvEncoder[DerivationType] = value =>
    val cells = fields(value) { [FieldType] => field => context.encode(field).data }.to(List).flatten
    Row(cells)

  given encoder[ValueType](using encoder: Encoder[ValueType]): DsvEncoder[ValueType] = value =>
    Row(encoder.encode(value))

trait DsvEncoder[ValueType]:
  def encode(value: ValueType): Row

extension [ValueType](value: ValueType)
  def dsv(using encoder: DsvEncoder[ValueType]): Row = encoder.encode(value)

extension [ElementType](value: Seq[ElementType])
  def dsv(using encoder: DsvEncoder[ElementType]): Dsv =
    Dsv(value.to(LazyList).map(encoder.encode(_)))

case class CsvDoc(rows: List[Csv])
case class TsvDoc(rows: List[Csv])

object DsvDecoder extends ProductDerivation[DsvDecoder]:

  class DsvProductDecoder[DerivationType](count: Int, lambda: Row => DerivationType)
  extends DsvDecoder[DerivationType]:
    override def width: Int = count
    def decode(row: Row): DerivationType = lambda(row)

  inline def join[DerivationType <: Product: ProductReflection]: DsvDecoder[DerivationType] =
    val sum = contexts { [FieldType] => context => context.width }.sum
    var count = 0

    DsvProductDecoder[DerivationType](sum, elems => construct:
      [FieldType] => context =>
        val row = Row(elems.data.drop(count))
        count += context.width
        typeclass.decode(row))

  given decoder[ValueType: Decoder]: DsvDecoder[ValueType] = _.data.head.decodeAs[ValueType]

trait DsvDecoder[ValueType]:
  def decode(elems: Row): ValueType
  def width: Int = 1
