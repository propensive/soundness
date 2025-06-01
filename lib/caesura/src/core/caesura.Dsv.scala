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
┃    Soundness, version 0.32.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import denominative.*
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

import scala.compiletime.*

import java.util as ju

case class Dsv
             ( rows:    Stream[Row],
               format:  Optional[DsvFormat]    = Unset,
               columns: Optional[IArray[Text]] = Unset ):

  def as[value: DsvDecodable]: Stream[value] tracks CellRef = rows.map(_.as[value])

  override def hashCode: Int =
    (rows.hashCode*31 + format.hashCode)*31 + columns.lay(-1): array =>
      ju.Arrays.hashCode(array.mutable(using Unsafe))

  override def equals(that: Any): Boolean = that.asMatchable match
    case dsv: Dsv =>
      dsv.rows == rows && dsv.format == format && columns.lay(dsv.columns == Unset): columns =>
        dsv.columns.lay(false)(columns.sameElements(_))

    case _ =>
      false

object Dsv:
  private enum State:
    case Fresh, Quoted, DoubleQuoted

  given abstractable: (CharEncoder, DsvFormat)
        => Dsv is Abstractable across HttpStreams into HttpStreams.Content =
    new Abstractable:
      type Self = Dsv
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(dsv: Dsv): HttpStreams.Content =
        val mediaType: Text =
          dsv.format.let(_.delimiter) match
            case '\t' => t"text/tab-separated-values"
            case _    => t"text/csv"

        (mediaType, dsv.stream[Text].map(_.bytes))


  given tabular: Dsv is Tabular[Text]:
    type Element = Row
    def rows(value: Dsv) = value.rows

    def table(dsv: Dsv): Table[Row, Text] =
      val columns: List[Text] =
        dsv.columns.let(_.to(List)).or:
          dsv.rows.prim.let: head =>
            (1 to head.data.length).to(List).map(_.toString.tt)

        . or(Nil)

      Table[Row]
        ( (columns.map: name =>
            Column[Row, Text, Text](name, sizing = columnar.Collapsible(0.5))
              ( _[Text](name).or(t"")) )* )


  def parse[source: Readable by Text](source: source)(using format: DsvFormat)
  : Dsv raises DsvError =

      val rows = recur(source.stream[Text])
      if format.header then Dsv(rows, format, rows.prim.let(_.header)) else Dsv(rows, format)


  given showable: DsvFormat => Dsv is Showable = _.rows.map(_.show).join(t"\n")
  given readable: DsvFormat => Dsv is Readable by Text = _.rows.to(Stream).map(_.show+t"\n")


  private def recur
                ( content:  Stream[Text],
                  index:    Ordinal                  = Prim,
                  column:   Int                      = 0,
                  cells:    Array[Text]              = new Array[Text](0),
                  builder:  TextBuilder              = TextBuilder(),
                  state:    State                    = State.Fresh,
                  headings: Optional[Map[Text, Int]] = Unset )
                ( using format: DsvFormat, tactic: Tactic[DsvError] )
  : Stream[Row] =

      inline def putCell(): Array[Text] =
        val cells2 = if cells.length <= column then cells :+ builder() else
          cells(column) = builder()
          cells

        cells2.also(builder.clear())

      inline def advance() =
        val cells = putCell()
        recur(content, index + 1, column + 1, cells, builder, State.Fresh, headings)

      inline def next(char: Char): Stream[Row] =
        builder.put(char) yet recur(content, index + 1, column, cells, builder, state, headings)

      inline def quote(): Stream[Row] = state match
        case State.Fresh =>
          if !builder.empty then raise(DsvError(format, DsvError.Reason.MisplacedQuote))
          recur(content, index + 1, column, cells, builder, State.Quoted, headings)

        case State.Quoted =>
          recur(content, index + 1, column, cells, builder, State.DoubleQuoted, headings)

        case State.DoubleQuoted =>
          builder.put(format.Quote)
          recur(content, index + 1, column, cells, builder, State.Quoted, headings)

      inline def fresh(): Array[Text] = new Array[Text](cells.length)

      inline def putRow(): Stream[Row] =
        val cells = putCell()

        if format.header && headings.absent then
          val map: Map[Text, Int] = cells.to(List).zipWithIndex.to(Map)
          recur(content, index + 1, 0, fresh(), builder, State.Fresh, map)
        else
          (column + 1).until(cells.length).each: index =>
            cells(index) = t""

          val row = Row(unsafely(cells.immutable), headings)
          row #:: recur(content, index + 1, 0, fresh(), builder, State.Fresh, headings)

      content.flow(if column == 0 && builder.empty then Stream() else putRow()):
        if !head.has(index) then recur(tail, Prim, column, cells, builder, state, headings) else
          head.s.charAt(index.n0) match
            case format.Delimiter =>
              if state != State.Quoted then advance() else next(format.Delimiter)

            case format.Quote =>
              quote()

            case '\n' | '\r' =>
              if column == 0 && builder.empty
              then recur(content, index + 1, 0, cells, builder, State.Fresh, headings)
              else if state != State.Quoted then putRow()
              else next(head.s.charAt(index.n0))

            case char =>
              builder.put(char)
              recur(content, index + 1, column, cells, builder, state, headings)
