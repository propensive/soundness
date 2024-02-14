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
import turbulence.*
import spectacular.*
import hieroglyph.*
import anticipation.*

trait RowFormat:
  protected val separator: Char
  type Format
  def wrap(list: List[Csv]): Format
  
  def parse[SourceType](source: SourceType)(using readable: Readable[SourceType, Line]): Format = wrap:
    readable.read(source).to(List).map: line =>
      parseLine(line.content)

  def parseLine(line: Text): Csv =
    @tailrec
    def parseLine
        (items: Vector[Text], index: Int, quoted: Boolean, start: Int, end: Int, join: Boolean)
        : Vector[Text] =
      if line.length <= index then
        if join then items.init :+ t"${items.last}${line.slice(start, if end < 0 then index else end)}"
        else items :+ line.slice(start, if end < 0 then index else end)
      else 
        val ch = try line(index) catch case error: OutOfRangeError => throw Panic(error)
        
        (ch: @switch) match
          case `separator` =>
            if quoted then parseLine(items, index + 1, quoted, start, end, join)
            else
              val elems: Vector[Text] = if start < 0 then items :+ t"" else
                val suffix = line.slice(start, if end == -1 then index else end)
                if join then
                  val part: Text = t"${items.last}${suffix.s}"
                  items.init :+ part
                else items :+ suffix
  
              parseLine(elems, index + 1, quoted = false, index + 1, -1, join = false)
  
          case '"' =>
            if quoted then parseLine(items, index + 1, quoted = false, start, index, join = join)
            else if end != -1 then
              parseLine(items :+ line.slice(start, index), index + 1, quoted = true, index + 1, -1, join = true)
            else parseLine(items, index + 1, quoted = true, index + 1, -1, join = false)
  
          case ch: Char =>
            parseLine(items, index + 1, quoted, start, end, join)

    Csv(parseLine(Vector(), 0, quoted = false, 0, -1, join = false)*)

  def serialize(row: Csv): Text = row.elems.map(escape).join(separator.show)
  protected def escape(str: Text): Text

object Csv:
  given show: Show[Csv] = _.elems.join(t",")

case class Csv(elems: Text*):
  def as[CellType: CsvDecoder]: CellType = summon[CsvDecoder[CellType]].decode(this)

object CsvDoc extends RowFormat:
  type Format = CsvDoc
  def wrap(seq: List[Csv]): CsvDoc = CsvDoc(seq)
  given show: Show[CsvDoc] = _.rows.map(serialize).join(t"\n")

  given httpResponseStream(using CharEncoder): GenericHttpResponseStream[CsvDoc] with
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
  given show: Show[TsvDoc] = _.rows.map(serialize).join(t"\n")

  given httpResponseStream(using CharEncoder): GenericHttpResponseStream[TsvDoc] with
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
