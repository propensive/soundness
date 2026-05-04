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

import java.lang as jl
import java.util as ju

import scala.collection.mutable as scm
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
    val rows = parse(text)
    if format.header then Sheet(rows, format, rows.prim.let(_.header)) else Sheet(rows, format)

  given showable: DsvFormat => Sheet is Showable = _.rows.map(_.show).join(t"\n")
  given streamable: DsvFormat => Sheet is Streamable by Text = _.rows.to(Stream).map(_.show+t"\n")


  private def parse(content: Stream[Text])
    ( using format: DsvFormat, tactic: Tactic[DsvError] )
  :   Stream[Dsv] =

    new Parser(content).stream


  private class Parser(initial: Stream[Text])
    ( using format: DsvFormat, tactic: Tactic[DsvError] ):
    private var content: Stream[Text] = initial
    private var current: String = ""
    private var currentLen: Int = 0
    private var pos: Int = 0
    private val builder: jl.StringBuilder = new jl.StringBuilder(64)
    private val cellsBuf: scm.ArrayBuffer[Text] = new scm.ArrayBuffer[Text](16)
    private var state: State = State.Fresh
    private var headings: Optional[Map[Text, Int]] = Unset

    private val delim: Char = format.delimiter
    private val quoteChar: Char = format.quote
    private val isHeader: Boolean = format.header

    @scala.annotation.tailrec
    private def loadChunk(): Boolean =
      if pos < currentLen then true
      else content match
        case head #:: tail =>
          current = head.s
          currentLen = current.length
          pos = 0
          content = tail
          if currentLen == 0 then loadChunk() else true

        case _ =>
          false

    private def closeCell(): Unit =
      cellsBuf += Text(builder.toString.nn)
      builder.setLength(0)

    private def emitRow(): Dsv =
      val n = cellsBuf.length
      val arr = new Array[Text](n)
      cellsBuf.copyToArray(arr)
      cellsBuf.clear()
      Dsv(IArray.unsafeFromArray(arr), headings)

    private def parseRow(): Optional[Dsv] =
      while loadChunk() do
        val ch = current.charAt(pos)
        pos += 1
        state match
          case State.Fresh =>
            if ch == delim then closeCell()
            else if ch == quoteChar then
              if builder.length > 0 then
                raise(DsvError(format, DsvError.Reason.MisplacedQuote))
              state = State.Quoted
            else if ch == '\n' || ch == '\r' then
              if cellsBuf.isEmpty && builder.length == 0 then ()
              else
                closeCell()
                return emitRow()
            else
              builder.append(ch)

          case State.Quoted =>
            if ch == quoteChar then state = State.DoubleQuoted
            else builder.append(ch)

          case State.DoubleQuoted =>
            if ch == quoteChar then
              builder.append(quoteChar)
              state = State.Quoted
            else if ch == delim then
              closeCell()
              state = State.Fresh
            else if ch == '\n' || ch == '\r' then
              closeCell()
              state = State.Fresh
              return emitRow()
            else
              builder.append(ch)

      if cellsBuf.nonEmpty || builder.length > 0 then
        closeCell()
        emitRow()
      else
        Unset

    def stream: Stream[Dsv] = next()

    private def next(): Stream[Dsv] = parseRow() match
      case row: Dsv =>
        if isHeader && headings.absent then
          val data = row.data
          val mapBuilder = Map.newBuilder[Text, Int]
          var i = 0
          while i < data.length do
            mapBuilder += data(i) -> i
            i += 1
          headings = mapBuilder.result()
          next()
        else
          row #:: next()

      case _ =>
        Stream()

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
