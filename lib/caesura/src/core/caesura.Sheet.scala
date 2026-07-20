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

import scala.caps

import proscenium.compat.*

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

        val stream: (Stream[Data] over Credit)^ =
          dsv.source[Text].via(summon[CharEncoder]).asInstanceOf[(Stream[Data] over Credit)^]

        (mediaType, HttpStreams.Body(stream.toProgression.iterator))


  given tabular: Sheet is Tabular[Text]:
    type Element = Dsv

    def rows(value: Sheet): List[Dsv] =
      List.from(scala.collection.immutable.ArraySeq.unsafeWrapArray(value.rows.mutable(using Unsafe)))

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

        def aggregate(text: Progression[Text]): Sheet = sheet(parseRows(Stream(text.iterator)))
        override def accept(stream: (Stream[Text] over Credit)^): Sheet =
          // The non-consume `accept` crosses to the consuming parser as a
          // neutral reference; each accept delivers a single-use stream.
          sheet(parseRows(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]))

        private def sheet(iterator: Iterator[Dsv]^): Sheet =
          val rows = IArray.from(iterator)
          if format.header then Sheet(rows, format, rows.prim.let(_.header))
          else Sheet(rows, format)

  given showable: DsvFormat => Sheet is Showable = _.rows.to(List).map(_.show).join(t"\n")
  given streamable: DsvFormat => Sheet is Streamable by Text over Credit = sheet =>
    Stream(sheet.rows.iterator.map(_.show+t"\n"))


  // Parse rows from a pull endpoint as a single-consumer iterator, one
  // block-credit refill per chunk. Each call builds a fresh parser over the
  // stream; the iterator owns both for its lifetime.
  private[caesura] def parseRows(consume stream: (Stream[Text] over Credit)^)
    ( using format: DsvFormat, tactic: Tactic[DsvError], buffering: Buffering )
  :   Iterator[Dsv]^ =

    val block = buffering.capacity(Substrate.Chars)

    // The load closure is built inline in the constructor call: a local
    // binding of it would hide the stream from the subsequent statement (the
    // statement rule). The resolution-scoped tactic is sealed at the rim (the
    // codec-thunk pattern, as in `aggregable` above) so the fresh parser can
    // cross into the consume position without referring to the parameter.
    given sealedTactic: Tactic[DsvError] = caps.unsafe.unsafeAssumePure(tactic)

    rowIterator:
      new Parser(() => stream.refill(Credit(block)) match
        case count: Int =>
          val window = stream.window(using Unsafe)
          val text = stream.addressable.materialize(window, stream.start, count)
          stream.skip(count)
          text

        case _ =>
          Unset)

  // Constructed in a helper: a local binding of the fresh parser would hide it
  // from the anonymous class (the statement rule); consume parameters carry
  // explicit capture sets and hide nothing.
  // A token reader for direct (AST-free) parsing, over a fresh parser: the
  // `Dsv.Parsable` counterpart of `parseRows`. Same rim discipline as above.
  private[caesura] def directReader(consume stream: (Stream[Text] over Credit)^)
    ( using format: DsvFormat, tactic: Tactic[DsvError], buffering: Buffering )
  :   DsvReader^ =

    val block = buffering.capacity(Substrate.Chars)
    given sealedTactic: Tactic[DsvError] = caps.unsafe.unsafeAssumePure(tactic)

    DsvReader(format = format, tactic = caps.unsafe.unsafeAssumePure(tactic), parser =
      new Parser(() => stream.refill(Credit(block)) match
        case count: Int =>
          val window = stream.window(using Unsafe)
          val text = stream.addressable.materialize(window, stream.start, count)
          stream.skip(count)
          text

        case _ =>
          Unset))

  private def rowIterator(consume parser: Parser^): Iterator[Dsv]^ =
    new Iterator[Dsv]:
      // Untracked: a stdlib `Iterator` cannot extend `Stateful`; the fields
      // are reached only through this iterator's own methods.
      @caps.unsafe.untrackedCaptures
      private var pending: Optional[Dsv] = Unset
      @caps.unsafe.untrackedCaptures
      private var finished: Boolean = false

      def hasNext: Boolean =
        if pending.present then true
        else if finished then false
        else
          pending = parser.nextRow()
          if pending.absent then finished = true
          pending.present

      def next(): Dsv =
        if !hasNext then Iterator.empty.next()
        val row = pending.or(Iterator.empty.next())
        pending = Unset
        row


  private[caesura] class Parser(load: () => Optional[Text])
    ( using format: DsvFormat, tactic: Tactic[DsvError] )
  extends caps.ExclusiveCapability, caps.Stateful:
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

    update private def loadChunk(): Boolean =
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

    // The completed row's cells remain in `cellsBuf` (reused across rows):
    // the direct parser reads them in place; `materialize()` copies them out
    // for the `Dsv` row path.
    update private def endRow(): Unit =
      rowOrdinal = rowOrdinal.next

    private[caesura] def cellCount: Int = cellsBuf.length

    private[caesura] def cellAt(index: Int): Optional[Text] =
      if index >= 0 && index < cellsBuf.length then cellsBuf(index) else Unset

    private[caesura] def columnIndex(name: Text): Optional[Int] =
      headings.let(_.at(name))

    private[caesura] def materialize(): Dsv =
      val n = cellsBuf.length
      val arr = new Array[Text](n)
      cellsBuf.copyToArray(arr)
      Dsv(IArray.unsafeFromArray(arr), headings)

    // Scan ahead in Fresh state for the next delimiter, quote, or line-ending,
    // bulk-appending the run of regular characters in one operation, then
    // dispatch on the trigger char. Returns true when a row has just been
    // closed (its cells left in `cellsBuf`).
    update private def scanFresh(): Boolean =
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
            return false
          else if ch == q then
            if builder.length > 0 then
              val reason = DsvError.Reason.MisplacedQuote
              // Pre-read into locals: the error's context-function argument may
              // not read the parser's state from inside `raise`'s
              // capture-polymorphic parameter.
              val row = rowOrdinal
              val cell = cellsBuf.length.z
              val position = consumed + p
              raise(DsvError(format, reason, row, cell, position))

            state = State.Quoted
            return false
          else
            if cellsBuf.nil && builder.length == 0 then return false
            else
              closeCell()
              endRow()
              return true

        p += 1

      if p > s then builder.append(str, s, p)
      pos = p
      false

    // Scan ahead in Quoted state for the next quote, bulk-appending all other
    // characters (including delimiters and newlines) verbatim.
    update private def scanQuoted(): Unit =
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

    update private def handleDoubleQuoted(): Boolean =
      val ch = current.charAt(pos)
      pos += 1

      if ch == quoteChar then
        builder.append(quoteChar)
        state = State.Quoted
        false
      else if ch == delim then
        closeCell()
        state = State.Fresh
        false
      else if ch == '\n' || ch == '\r' then
        closeCell()
        state = State.Fresh
        endRow()
        true
      else
        builder.append(ch)
        false

    // Parse the next row into `cellsBuf`, returning false at the end of the
    // input. The previous row's cells are discarded on entry.
    private[caesura] update def advance(): Boolean =
      cellsBuf.clear()

      while loadChunk() do
        val complete: Boolean = state match
          case State.Fresh        => scanFresh()
          case State.Quoted       => scanQuoted(); false
          case State.DoubleQuoted => handleDoubleQuoted()

        if complete then return true

      if cellsBuf.nonEmpty || builder.length > 0 then
        closeCell()
        endRow()
        true
      else
        false

    // Advance to the next DATA row: when the format has a header, the first
    // parsed row is consumed into `headings` so data rows can be addressed by
    // column name.
    private[caesura] update def advanceData(): Boolean =
      if advance() then
        if isHeader && headings.absent then
          val mapBuilder = scala.collection.immutable.Map.newBuilder[Text, Int]
          var i = 0

          while i < cellsBuf.length do
            mapBuilder += cellsBuf(i) -> i
            i += 1

          headings = Map.of(mapBuilder.result())
          advanceData()
        else true
      else false

    // The next data row, or `Unset` at the end of the input.
    update def nextRow(): Optional[Dsv] =
      if advanceData() then materialize() else Unset

// A fully-instantiated sheet of DSV data: every row is in memory, replayable
// and indexable. The streaming counterpart is `stream.rows`, an
// `Iterator[Dsv]` over a live pull endpoint, which never builds a `Sheet`.
case class Sheet
  ( rows:    IArray[Dsv],
    format:  Optional[DsvFormat]    = Unset,
    columns: Optional[IArray[Text]] = Unset ):

  def as[value: Decodable in Dsv]: List[value] raises DsvError tracks CellRef =
    rows.to(List).map(_.as[value])

  override def hashCode: Int =
    (ju.Arrays.hashCode(rows.mutable(using Unsafe).asInstanceOf[Array[Object | Null]])*31
        + format.hashCode)*31
    + columns.lay(-1): array =>
        ju.Arrays.hashCode(array.mutable(using Unsafe))

  override def equals(that: Any): Boolean = that.asMatchable match
    case dsv: Sheet =>
      dsv.rows.sameElements(rows) && dsv.format == format
      && columns.lay(dsv.columns == Unset): columns =>
           dsv.columns.lay(false)(columns.sameElements(_))

    case _ =>
      false
