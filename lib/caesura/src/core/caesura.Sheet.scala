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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*

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

        (mediaType, HttpStreams.Body(dsv.stream[Text].map(_.data).iterator))


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

  // Sealed per the codec-thunk pattern (see rep/DECISIONS.md): the resolution-scoped
  // tactic shares the instance's given-resolution lifetime.
  given aggregable: (format: DsvFormat) => (tactic: Tactic[DsvError])
  =>  Sheet is Aggregable by Text =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = Sheet
        type Operand = Text

        def aggregate(text: LazyList[Text]): Sheet = sheet(parse(text))
        override def accept(consume stream: (Stream[Text] over Credit)^): Sheet = sheet(parse(stream))

        private def sheet(rows: LazyList[Dsv]): Sheet =
          if format.header then Sheet(rows, format, rows.prim.let(_.header))
          else Sheet(rows, format)

  given showable: DsvFormat => Sheet is Showable = _.rows.map(_.show).join(t"\n")
  given streamable: DsvFormat => Sheet is Streamable by Text = _.rows.to(LazyList).map(_.show+t"\n")


  private def parse(content0: LazyList[Text])
    ( using format: DsvFormat, tactic: Tactic[DsvError] )
  :   LazyList[Dsv] =

    var content: LazyList[Text] = content0

    val load: () => Optional[Text] = () => content match
      case head #:: tail =>
        content = tail
        head

      case _ =>
        Unset

    new Parser(load).stream


  // Parse rows from a pull endpoint, one block-credit refill per chunk.
  private def parse(stream: Stream[Text] over Credit)
    ( using format: DsvFormat, tactic: Tactic[DsvError], buffering: Buffering )
  :   LazyList[Dsv] =

    val block = buffering.capacity(Substrate.Chars)

    val load: () => Optional[Text] = () => stream.refill(Credit(block)) match
      case count: Int =>
        val window = stream.window(using Unsafe)
        val text = stream.addressable.materialize(window, stream.start, count)
        stream.skip(count)
        text

      case _ =>
        Unset

    new Parser(load).stream


  private class Parser(load: () => Optional[Text])
    ( using format: DsvFormat, tactic: Tactic[DsvError] ):
    private var current: String = ""
    private var currentLen: Int = 0
    private var pos: Int = 0
    private var consumed: Int = 0
    private var rowOrdinal: Ordinal = Prim
    private val builder: jl.StringBuilder = new jl.StringBuilder(64)
    private val cellsBuf: scm.ArrayBuffer[Text] = new scm.ArrayBuffer[Text](16)
    private var state: State = State.Fresh
    private var headings: Optional[Map[Text, Int]] = Unset

    private val delim: Char = format.delimiter
    private val quoteChar: Char = format.quote
    private val isHeader: Boolean = format.header

    private def loadChunk(): Boolean =
      if pos < currentLen then true else
        var continue = true
        var result = false

        while continue do
          val next = load()

          if next.absent then continue = false else
            consumed += currentLen
            current = next.or(t"").s
            currentLen = current.length
            pos = 0

            if currentLen > 0 then
              continue = false
              result = true

        result

    private def closeCell(): Unit =
      cellsBuf += Text(builder.toString.nn)
      builder.setLength(0)

    private def emitRow(): Dsv =
      val n = cellsBuf.length
      val arr = new Array[Text](n)
      cellsBuf.copyToArray(arr)
      cellsBuf.clear()
      rowOrdinal = rowOrdinal.next
      Dsv(IArray.unsafeFromArray(arr), headings)

    // Scan ahead in Fresh state for the next delimiter, quote, or line-ending,
    // bulk-appending the run of regular characters in one operation, then
    // dispatch on the trigger char. Returns Unset to continue parsing or a
    // Dsv when a row has just been closed.
    private def scanFresh(): Optional[Dsv] =
      val str = current
      val len = currentLen
      val d = delim
      val q = quoteChar
      val s = pos
      var p = s

      while p < len do
        val ch = str.charAt(p)

        if ch == d || ch == q || ch == '\n' || ch == '\r' then
          if p > s then builder.append(str, s, p)
          pos = p + 1

          if ch == d then
            closeCell()
            return Unset
          else if ch == q then
            if builder.length > 0 then
              val reason = DsvError.Reason.MisplacedQuote
              raise(DsvError(format, reason, rowOrdinal, cellsBuf.length.z, consumed + p))

            state = State.Quoted
            return Unset
          else
            if cellsBuf.nil && builder.length == 0 then return Unset
            else
              closeCell()
              return emitRow()

        p += 1

      if p > s then builder.append(str, s, p)
      pos = p
      Unset

    // Scan ahead in Quoted state for the next quote, bulk-appending all other
    // characters (including delimiters and newlines) verbatim.
    private def scanQuoted(): Unit =
      val str = current
      val len = currentLen
      val q = quoteChar
      val s = pos
      var p = s
      while p < len && str.charAt(p) != q do p += 1
      if p > s then builder.append(str, s, p)

      if p < len then
        pos = p + 1
        state = State.DoubleQuoted
      else
        pos = p

    private def handleDoubleQuoted(): Optional[Dsv] =
      val ch = current.charAt(pos)
      pos += 1

      if ch == quoteChar then
        builder.append(quoteChar)
        state = State.Quoted
        Unset
      else if ch == delim then
        closeCell()
        state = State.Fresh
        Unset
      else if ch == '\n' || ch == '\r' then
        closeCell()
        state = State.Fresh
        emitRow()
      else
        builder.append(ch)
        Unset

    private def parseRow(): Optional[Dsv] =
      while loadChunk() do
        val emitted: Optional[Dsv] = state match
          case State.Fresh        => scanFresh()
          case State.Quoted       => scanQuoted(); Unset
          case State.DoubleQuoted => handleDoubleQuoted()

        emitted match
          case row: Dsv => return row
          case _        => ()

      if cellsBuf.nonEmpty || builder.length > 0 then
        closeCell()
        emitRow()
      else
        Unset

    def stream: LazyList[Dsv] = next()

    private def next(): LazyList[Dsv] = parseRow() match
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
        LazyList()

case class Sheet
  ( rows:    LazyList[Dsv],
    format:  Optional[DsvFormat]    = Unset,
    columns: Optional[IArray[Text]] = Unset ):

  def as[value: Decodable in Dsv]: LazyList[value] raises DsvError tracks CellRef =
    rows.map(_.as[value])

  override def hashCode: Int =
    (rows.hashCode*31 + format.hashCode)*31 + columns.lay(-1): array =>
      ju.Arrays.hashCode(array.mutable(using Unsafe))

  override def equals(that: Any): Boolean = that.asMatchable match
    case dsv: Sheet =>
      dsv.rows == rows && dsv.format == format && columns.lay(dsv.columns == Unset): columns =>
        dsv.columns.lay(false)(columns.sameElements(_))

    case _ =>
      false
