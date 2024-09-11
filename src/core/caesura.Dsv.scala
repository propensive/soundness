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

case class Dsv(rows: LazyList[Row], format: Optional[DsvFormat] = Unset):
  def as[ValueType: DsvDecodable]: LazyList[ValueType] = rows.map(_.as[ValueType])

object Dsv:
  private enum State:
    case Fresh, Quoted, DoubleQuoted

  def parse[SourceType: Readable by Text](source: SourceType)(using format: DsvFormat): Dsv =
    Dsv(recur(source.stream[Text]), format)

  given (using DsvFormat) => Dsv is Showable = _.rows.map(_.show).join(t"\n")

  private def recur
      (content: LazyList[Text],
       index:   Ordinal                  = Prim,
       column:  Int                      = 0,
       cells:   Array[Text]              = new Array[Text](0),
       buffer:  TextBuffer               = TextBuffer(),
       state:   State                    = State.Fresh,
       head:    Optional[Map[Text, Int]] = Unset)
      (using format: DsvFormat)
          : LazyList[Row] =

    inline def putCell(): Array[Text] =
      val cells2 = if cells.length <= column then cells :+ buffer() else
        cells(column) = buffer()
        cells

      cells2.also(buffer.clear())

    inline def advance() =
      val cells = putCell()
      recur(content, index + 1, column + 1, cells, buffer, State.Fresh, head)

    inline def next(char: Char): LazyList[Row] =
      buffer.put(char) yet recur(content, index + 1, column, cells, buffer, state, head)

    inline def quote(): LazyList[Row] = state match
      case State.Fresh  => recur(content, index + 1, column, cells, buffer, State.Quoted, head)
      case State.Quoted => recur(content, index + 1, column, cells, buffer, State.DoubleQuoted, head)
      case State.DoubleQuoted =>
        buffer.put(format.Quote)
        recur(content, index + 1, column, cells, buffer, State.Quoted, head)

    inline def fresh(): Array[Text] = new Array[Text](cells.length)

    inline def putRow(): LazyList[Row] =
      val cells = putCell()
      buffer.clear()

      if format.header && head.absent then
        val map: Map[Text, Int] = cells.to(List).zipWithIndex.to(Map)
        recur(content, index + 1, 0, fresh(), buffer, State.Fresh, map)
      else
        val row = Row(unsafely(cells.immutable), head)
        row #:: recur(content, index + 1, 0, fresh(), buffer, State.Fresh, head)

    content match
      case row #:: tail =>
        if !row.has(index) then recur(tail, Prim, column, cells, buffer, state, head)
        else
          row.s.charAt(index.n0) match
            case format.Delimiter => if state != State.Quoted then advance() else next(format.Delimiter)
            case format.Quote     => quote()
            case '\n' | '\r'      => if column == 0 && buffer.empty
                                     then recur(content, index + 1, 0, cells, buffer, State.Fresh, head)
                                     else if state != State.Quoted then putRow()
                                     else next(row.s.charAt(index.n0))
            case char             =>
              buffer.put(char)
              recur(content, index + 1, column, cells, buffer, state, head)

      case _ =>
        if column == 0 && buffer.empty then LazyList() else putRow()
