                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package caesura

import java.util as ju

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import escritoire.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

object Sheet:
  private enum State:
    case Fresh, Quoted, DoubleQuoted


  given abstractable: (CharEncoder, DsvFormat)
  =>  Sheet is Abstractable across HttpStreams to HttpStreams.Content =

    new Abstractable:
      type Self = Sheet
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(dsv: Sheet): HttpStreams.Content =
        val mediaType: Text =
          dsv.format.let(_.delimiter) match
            case '\t' => t"text/tab-separated-values"
            case _    => t"text/csv"

        (mediaType, dsv.stream[Text].map(_.data))


  given tabular: Sheet is Tabular[Text]:
    type Element = Dsv

    def rows(value: Sheet) = value.rows

    def table(dsv: Sheet): Scaffold[Dsv, Text] =
      val columns: List[Text] =
        dsv.columns.let(_.to(List)).or:
          dsv.rows.prim.let: head =>
            (1 to head.data.length).to(List).map(_.toString.tt)

        . or(Nil)

      Scaffold[Dsv]
        ( ( columns.map: name =>
              Column[Dsv, Text, Text](name, sizing = columnar.Collapsible(0.5))
                ( _[Text](name).or(t"") ) )* )

  given aggregable: (format: DsvFormat) => Tactic[DsvError] => Sheet is Aggregable by Text = text =>
    val rows = recur(text)
    if format.header then Sheet(rows, format, rows.prim.let(_.header)) else Sheet(rows, format)

  given showable: DsvFormat => Sheet is Showable = _.rows.map(_.show).join(t"\n")
  given streamable: DsvFormat => Sheet is Streamable by Text = _.rows.to(Stream).map(_.show+t"\n")


  private def recur
    ( content:  Stream[Text],
      index:    Ordinal                  = Prim,
      column:   Int                      = 0,
      cells:    Array[Text]              = new Array[Text](0),
      builder:  TextBuilder              = TextBuilder(),
      state:    State                    = State.Fresh,
      headings: Optional[Map[Text, Int]] = Unset )
    ( using format: DsvFormat, tactic: Tactic[DsvError] )
  :   Stream[Dsv] =

    inline def putCell(): Array[Text] =
      val cells2 = if cells.length <= column then cells :+ builder() else
        cells(column) = builder()
        cells

      cells2.also(builder.clear())

    inline def advance() =
      val cells = putCell()
      recur(content, index + 1, column + 1, cells, builder, State.Fresh, headings)

    inline def proceed(char: Char): Stream[Dsv] =
      builder.put(char) yet recur(content, index + 1, column, cells, builder, state, headings)

    inline def quote(): Stream[Dsv] = state match
      case State.Fresh =>
        if !builder.nil then raise(DsvError(format, DsvError.Reason.MisplacedQuote))
        recur(content, index + 1, column, cells, builder, State.Quoted, headings)

      case State.Quoted =>
        recur(content, index + 1, column, cells, builder, State.DoubleQuoted, headings)

      case State.DoubleQuoted =>
        builder.put(format.Quote)
        recur(content, index + 1, column, cells, builder, State.Quoted, headings)

    inline def fresh(): Array[Text] = new Array[Text](cells.length)

    inline def putDsv(): Stream[Dsv] =
      val cells = putCell()

      if format.header && headings.absent then
        val map: Map[Text, Int] = cells.to(List).zipWithIndex.to(Map)
        recur(content, index + 1, 0, fresh(), builder, State.Fresh, map)
      else
        (column + 1).until(cells.length).each: index =>
          cells(index) = t""

        val row = Dsv(unsafely(cells.immutable), headings)
        row #:: recur(content, index + 1, 0, fresh(), builder, State.Fresh, headings)

    content.flow(if column == 0 && builder.nil then Stream() else putDsv()):
      if !next.has(index) then recur(more, Prim, column, cells, builder, state, headings) else
        next.s.charAt(index.n0) match
          case format.Delimiter =>
            if state != State.Quoted then advance() else proceed(format.Delimiter)

          case format.Quote =>
            quote()

          case '\n' | '\r' =>
            if column == 0 && builder.nil
            then recur(content, index + 1, 0, cells, builder, State.Fresh, headings)
            else if state != State.Quoted then putDsv()
            else proceed(next.s.charAt(index.n0))

          case char =>
            builder.put(char)
            recur(content, index + 1, column, cells, builder, state, headings)

case class Sheet
  ( rows:    Stream[Dsv],
    format:  Optional[DsvFormat]    = Unset,
    columns: Optional[IArray[Text]] = Unset ):

  def as[value: Decodable in Dsv]: Stream[value] tracks CellRef = rows.map(_.as[value])

  override def hashCode: Int =
    (rows.hashCode*31 + format.hashCode)*31 + columns.lay(-1): array =>
      ju.Arrays.hashCode(array.mutable(using Unsafe))

  override def equals(that: Any): Boolean = that.asMatchable match
    case dsv: Sheet =>
      dsv.rows == rows && dsv.format == format && columns.lay(dsv.columns == Unset): columns =>
        dsv.columns.lay(false)(columns.sameElements(_))

    case _ =>
      false
