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

import wisteria.*
import rudiments.*
import gossamer.*
import fulminate.*
import prepositional.*
import denominative.*
import turbulence.*
import contingency.*
import spectacular.*
import hieroglyph.*
import anticipation.*


case class DsvFormat(delimiter: Char, quote: Char, escape: Char):
  val Delimiter: Char = delimiter
  val Quote: Char = quote
  val Escape: Char = escape

  def doublingEscapes: Boolean = quote == escape

case class Dsv(rows: LazyList[Dsv.Row]):
  override def toString(): String = rows.to(List).map(_.toString).mkString(" // ")

package dsvFormats:
  given DsvFormat as csv = DsvFormat(',', '"', '"')
  given DsvFormat as tsv = DsvFormat('\t', '"', '"')
  given DsvFormat as ssv = DsvFormat(' ', '"', '"')

object Dsv:
  case class Row(data: IArray[Text]):
    override def toString(): String = data.to(List).mkString("[", ";", "]")

  private enum State:
    case Fresh, Quoted, DoubleQuoted


  inline def parse(content: LazyList[Text])(using DsvFormat): Dsv = Dsv(recur(content))

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
      case LazyList()    => if column == 0 && buffer.empty then LazyList() else row()
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

trait RowFormat:
  protected val separator: Char
  type Format
  def wrap(list: List[Csv]): Format

  def serialize(row: Csv): Text = row.elems.map(escape).join(separator.show)
  protected def escape(str: Text): Text

object Csv:
  given Csv is Showable = _.elems.join(t",")

case class Csv(elems: Text*):
  def as[CellType: CsvDecoder]: CellType = summon[CsvDecoder[CellType]].decode(this)

object CsvDoc extends RowFormat:
  type Format = CsvDoc
  def wrap(seq: List[Csv]): CsvDoc = CsvDoc(seq)
  given CsvDoc is Showable = _.rows.map(serialize).join(t"\n")

  given (using CharEncoder) => CsvDoc is GenericHttpResponseStream:
    def mediaType: Text = t"text/csv"

    def content(value: CsvDoc): LazyList[IArray[Byte]] =
      LazyList(value.rows.map(CsvDoc.serialize(_)).join(t"\n").bytes)

  override val separator = ','

  def escape(text: Text): Text =
    val count = text.count { char => char.isWhitespace || char == '"' || char == ',' }
    if count > 0 then t""""${text.s.replaceAll("\"", "\"\"").nn}"""" else text

object CsvEncoder extends ProductDerivation[CsvEncoder]:
  inline def join[DerivationType <: Product: ProductReflection]: CsvEncoder[DerivationType] = value =>
    val cells = fields(value) { [FieldType] => field => context.encode(field).elems }.to(List).flatten
    Csv(cells*)

  given encoder[ValueType](using encoder: Encoder[ValueType]): CsvEncoder[ValueType] = value =>
    Csv(encoder.encode(value))

trait CsvEncoder[ValueType]:
  def encode(value: ValueType): Csv

extension [ValueType](value: ValueType)
  def csv(using encoder: CsvEncoder[ValueType]): Csv = encoder.encode(value)

extension [ElementType](value: Seq[ElementType])
  def csv(using encoder: CsvEncoder[ElementType]): CsvDoc = CsvDoc(value.to(List).map(encoder.encode(_)))
  def tsv(using encoder: CsvEncoder[ElementType]): TsvDoc = TsvDoc(value.to(List).map(encoder.encode(_)))

case class CsvDoc(rows: List[Csv]):
  def as[ValueType: CsvDecoder]: List[ValueType] = rows.map(_.as[ValueType])

case class TsvDoc(rows: List[Csv]):
  def as[ValueType: CsvDecoder]: List[ValueType] = rows.map(_.as[ValueType])

object TsvDoc extends RowFormat:
  type Format = TsvDoc
  def wrap(seq: List[Csv]): TsvDoc = TsvDoc(seq)
  override val separator = '\t'
  def escape(str: Text): Text = Text(str.s.replaceAll("\t", "        ").nn)
  given TsvDoc is Showable = _.rows.map(serialize).join(t"\n")

  given (using CharEncoder) => TsvDoc is GenericHttpResponseStream:
    def mediaType: Text = "text/tab-separated-values".tt

    def content(value: TsvDoc): LazyList[IArray[Byte]] =
      LazyList(value.rows.map(TsvDoc.serialize(_)).join(t"\n").bytes)

object CsvDecoder extends ProductDerivation[CsvDecoder]:
  inline def join[DerivationType <: Product: ProductReflection]: CsvDecoder[DerivationType] =
    new CsvDecoder[DerivationType]:
      def decode(elems: Csv): DerivationType =
        var count: Int = 0
        construct:
          [FieldType] => context =>
            val row = Csv(elems.elems.drop(count)*)
            count += context.width
            typeclass.decode(row)

      override def width: Int = contexts { [FieldType] => context => context.width }.sum

  given decoder[ValueType: Decoder]: CsvDecoder[ValueType] = _.elems.head.decodeAs[ValueType]

trait CsvDecoder[ValueType]:
  def decode(elems: Csv): ValueType
  def width: Int = 1
