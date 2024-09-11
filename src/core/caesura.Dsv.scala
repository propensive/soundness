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

import scala.compiletime.*

case class Dsv(rows: LazyList[Row], head: Optional[Row] = Unset, format: Optional[DsvFormat] = Unset):
  lazy val columns: Map[Text, Int] = head.let(_.data.zipWithIndex.to(Map)).or(Map())
  override def toString(): String = rows.to(List).map(_.toString).mkString(" // ")
  def as[ValueType: DsvDecodable]: LazyList[ValueType] = rows.map(_.as[ValueType])

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
