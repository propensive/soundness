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
package turbulence

import scala.caps

import scala.language.adhocExtensions

import java.io as ji
import java.lang as jl

import anticipation.*
import capricious.*
import contingency.*
import denominative.*
import hieroglyph.*
import hypotenuse.*
import parasite.*
import prepositional.*
import proscenium.compat.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.*

import LineSeparation.*
import abstractables.instantAbstractable
import probates.awaitProbate

inline def more[value](using value: value aka "more"): value = value()

extension [value](value: value)
  // A streamable value's pull endpoint: the fluent form of
  // `Streamable.stream` (`file.source`, `entry.source`). (Named `source`
  // because zephyrine's `.stream` constructor already owns that name under
  // the flat `soundness.*` re-export.)
  def source[element](using streamable: (value is Streamable by element over Credit)^)
  :   (Stream[element] over Credit)^ =

    streamable.stream(value)

  inline def read[result](using readable: (value is Readable to result)^): result =
    readable.read(value)


  def writeTo[target](target: target)[element]
    // Capture-polymorphic evidence: writers built from an `Emit[StreamError]` capture it; the
    // write completes within this call, so nothing is retained and the result stays `Unit`.
    ( using streamable: (value is Streamable by element over Credit)^,
            writable:   (target is Writable by element)^ )
  :   Unit =

    writable.write(target, streamable.stream(value))

extension [value](value: value)
  def load[result <: Documentary]
    ( using streamable: (value is Streamable by Text over Credit)^,
            loadable:   (result is Loadable by Text)^ )
  :   Document[result] =
    loadable.load(streamable.stream(value))

extension [medium, transport](consume stream: (Stream[medium] over transport)^)
  // The detached pump: `pump` on its own parasite task, for fire-and-forget
  // transfers and genuinely concurrent pipeline halves. This is the "one
  // pumping thread" of a pipeline with an asynchronous boundary.
  def flow(consume intake: (Intake[medium] over transport)^)(using Monitor, Probate): Task[Unit] =
    // Both endpoints move onto the pump fiber as neutral carriers (a consume parameter
    // cannot be consumed from inside the spawned closure); single ownership transfers
    // with the spawn.
    val streamRef: AnyRef = stream.asInstanceOf[AnyRef]
    val intakeRef: AnyRef = intake.asInstanceOf[AnyRef]

    async:
      streamRef.asInstanceOf[(Stream[medium] over transport)^]
      . pump(intakeRef.asInstanceOf[(Intake[medium] over transport)^])

extension (consume stream: (Stream[Text] over Credit)^)
  // Split a character stream into a record stream of its lines (each `Text`,
  // without its terminator), under the ambient `LineSeparation` policy. Drain
  // with `.records`/`.memoize`, or compose onward like any record stream.
  // (Named to steer clear of gossamer's value-level `lines` — same-named
  // extensions from different modules shadow, not overload, under the flat
  // `soundness.*` re-export.)
  def delineate(using lineSeparation: LineSeparation, buffering: Buffering)
  :   (Stream[IArray[Text]] over Credit)^ =

    stream.via(lineSeparation).asInstanceOf[(Stream[IArray[Text]] over Credit)^]

extension (text: Text)
  // Split a whole `Text` into its lines through the SAME `LineSeparation`
  // duct as the streaming form, driven directly over the value as a single
  // window (`Duct.feed`) — no stream endpoint, no credit machinery. One
  // implementation, two drivers.
  @targetName("delineateText")
  def delineate(using lineSeparation: LineSeparation, buffering: Buffering): IArray[Text] =
    Duct.feed(text, LineSeparation.lines.duct(lineSeparation))

extension (data: Data)
  // Split whole bytes into lines: whole-value character decoding, then the
  // lines duct as above.
  @targetName("delineateWholeData")
  def delineate
    ( using decoder: CharDecoder, lineSeparation: LineSeparation, buffering: Buffering )
  :   IArray[Text] =

    decoder.decoded(data).delineate

extension (consume stream: (Stream[Data] over Credit)^)
  // Split a byte stream into its lines: character decoding under the ambient
  // `CharDecoder`, then line splitting as above.
  @targetName("delineateData")
  def delineate
    ( using decoder: CharDecoder, lineSeparation: LineSeparation, buffering: Buffering )
  :   (Stream[IArray[Text]] over Credit)^ =

    stream.via(decoder).asInstanceOf[(Stream[Text] over Credit)^].delineate

extension (consume stream: (Stream[Data] over Credit)^)
  // View a pull endpoint as a `java.io.InputStream` for handing to JDK APIs:
  // each `read` pulls through the window (one block of credit at a time) and
  // never materializes a chunk; `close` closes the endpoint. The kernel-native
  // replacement for `lazyList.inputStream`.
  def inputStream(using buffering: Buffering): ji.InputStream^ =
    val block = buffering.capacity(Substrate.Bytes)

    new ji.InputStream:
      // A JDK class cannot extend `Stateful`; the flag is this adapter's only state.
      @caps.unsafe.untrackedCaptures
      private var ended: Boolean = false

      // The count of bytes now readable in the window: refills (skipping
      // zero-credit grants) until data arrives or the stream ends.
      private def ensure(): Int =
        if ended then -1 else stream.refill(Credit(block)) match
          case count: Int => if count == 0 then ensure() else count
          case _          =>
            ended = true
            stream.close()
            -1

      override def read(): Int =
        val available = ensure()

        if available < 0 then -1 else
          val byte = stream.window(using Unsafe).asInstanceOf[Array[Byte]](stream.start) & 0xff
          stream.skip(1)
          byte

      override def read(target: Array[Byte] | Null, offset: Int, length: Int): Int =
        if length == 0 then 0 else
          val available = ensure()

          if available < 0 then -1 else
            val take = available.min(length)
            System.arraycopy(stream.window(using Unsafe), stream.start, target, offset, take)
            stream.skip(take)
            take

      override def close(): Unit = if !ended then
        ended = true
        stream.close()

extension (consume stream: (Stream[Data] over Credit)^)
  // Adapt a pull endpoint to the HTTP-body interchange protocol: each `next`
  // refills with `limit` credit and materializes the delivered window.
  def httpBody: HttpStreams.Body^ = limit =>
    stream.refill(Credit(limit)) match
      case count: Int =>
        val take = count.min(limit)
        val chunk = stream.addressable.materialize(stream.window(using Unsafe), stream.start, take)
        stream.skip(take)
        chunk

      case _ =>
        null

extension (body: HttpStreams.Body)
  // View an HTTP-body interchange value as a pull endpoint, one chunk per
  // block-sized request.
  def stream(using buffering: Buffering): (Stream[Data] over Credit)^ =
    new Stream[Data]:
      type Transport = Credit

      private val block: Int = buffering.capacity(Substrate.Bytes)
      private var chunk: Data = IArray.empty[Byte].asInstanceOf[Data]
      private var start0: Int = 0
      private var limit0: Int = 0
      private var ended: Boolean = false

      // The chunk is immutable and only ever read through the window.
      protected def window0: AnyRef = chunk.asInstanceOf[AnyRef]
      def start: Int = start0
      def limit: Int = limit0
      update def skip(count: Int): Unit = start0 += count

      update def refill(demand: Credit): Optional[Int] =
        if limit0 > start0 then limit0 - start0
        else if ended then Unset
        else
          val granted = summon[Credit is Regulation].grant(demand)

          if granted == 0 then 0 else
            body.next(block.min(granted)) match
              case null =>
                ended = true
                Unset

              case data: Data =>
                chunk = data
                start0 = 0
                limit0 = data.length
                if limit0 == 0 then refill(demand) else limit0


package stdios:
  given muteStdio: Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)

  given systemStdio: (termcap: Termcap) => Stdio =
    Stdio
      ( jl.System.out.nn,
        jl.System.err.nn,
        jl.System.in.nn,
        termcap )

  given virtualMachineStdio: (termcap: Termcap) => Stdio =
    Stdio
      ( ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.out)),
        ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.err)),
        ji.FileInputStream(ji.FileDescriptor.in),
        termcap )

package lineSeparation:
  given carriageReturnLineSeparation
  :   LineSeparation(NewlineSeq.Cr, Action.Nl, Action.Skip, Action.Nl, Action.Nl)

  given strictCarriageReturnLineSeparation
  :   LineSeparation(NewlineSeq.Cr, Action.Nl, Action.Lf, Action.NlLf, Action.LfNl)

  given linefeedLineSeparation
  :   LineSeparation(NewlineSeq.Lf, Action.Skip, Action.Nl, Action.Nl, Action.Nl)

  given strictLinefeedsLineSeparation
  :   LineSeparation(NewlineSeq.Lf, Action.Nl, Action.Lf, Action.NlLf, Action.LfNl)

  given carriageReturnLinefeedLineSeparation
  :   LineSeparation(NewlineSeq.CrLf, Action.Skip, Action.Lf, Action.Nl, Action.LfNl)

  given adaptiveLinefeedLineSeparation
  :   LineSeparation(NewlineSeq.Lf, Action.Nl, Action.Nl, Action.Nl, Action.Nl)

  given virtualMachineLineSeparation: LineSeparation = jl.System.lineSeparator.nn match
    case "\r\n"    => carriageReturnLinefeedLineSeparation
    case "\r"      => carriageReturnLineSeparation
    case "\n"      => linefeedLineSeparation
    case _: String => adaptiveLinefeedLineSeparation

extension [element](stream: Progression[element])
  def deduplicate: Progression[element] =
    def recur(last: element, stream: Progression[element]): Progression[element] =
      stream.flow(Progression()):
        if last == next then recur(last, more) else next #:: recur(next, more)

    stream.flow(Progression())(next #:: recur(next, more))


  // `next`/`more` are bound with `aka`-label refinements; under capture checking the
  // labelled singleton type does not simplify away in every position (the aka-Tagged/
  // castbox class), so use sites strip it with `next.asInstanceOf[element]`.
  inline def flow[result](inline termination: => result)
    ( inline proceed: (element aka "next", Progression[element] aka "more") ?=> result )
  :   result =

    stream match
      case next #:: more => proceed(using next.aka["next"], more.aka["more"])
      case _             => termination


  def strict: Progression[element] = stream.stdlib.length yet stream

extension (obj: Progression.type)
  // Defers evaluation of `stream` until the result is forced. `empty.lazyAppendedAll(=> stream)`
  // keeps the by-name suffix unforced — equivalent to (and cheaper than) the old
  // `(dummy #:: stream).tail`, and it sidesteps the captured-by-name cons under cc.
  def defer[element](stream: => Progression[element]): Progression[element] =
    Progression().lazyAppendedAll(stream)


extension (stream: Progression[Data])
  def discard(bytes: Bytes): Progression[Data] =
    def recur(stream: Progression[Data], count: Bytes): Progression[Data] = stream.flow(Progression()):
      if next.bytes < count
      then recur(more, count - next.bytes)
      else
        // hoisted: a fresh array built inside `#::`'s by-name operand (which would
        // capture the enclosing context) could not escape it
        val head: Data = next.drop(count.long.toInt).asInstanceOf[Data]
        head #:: more

    recur(stream, bytes)

  def shred(mean: Double, variance: Double)(using Random): Progression[Data] =
    given gamma: Distribution = Gamma.approximate(mean, variance)

    def newArray(): Array[Byte]^ = new Array[Byte](arbitrary[Double]().toInt.max(1))

    def recur(stream: Progression[Data], sourcePos: Int, dest: Array[Byte]^, destPos: Int)
    :   Progression[Data] =

      stream match
        case source #:: more =>
          val ready = source.length - sourcePos
          val free = dest.length - destPos

          if ready < free then
            jl.System.arraycopy(source, sourcePos, dest, destPos, ready)
            recur(more, 0, dest, destPos + ready)
          else if free < ready then
            jl.System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(stream, sourcePos + free, newArray(), 0)
          else // free == ready
            jl.System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(more, 0, newArray(), 0)

        case _ =>
          if destPos == 0 then Progression()
          else
            // arraycopy, not `.slice`: the ArrayOps conversion demands a pure array
            val out = new Array[Byte](destPos)
            jl.System.arraycopy(dest, 0, out, 0, destPos)
            Progression(out.immutable(using Unsafe).asInstanceOf[Data])

    recur(stream, 0, newArray(), 0)

  def chunked(size: Int, zeroPadding: Boolean = false): Progression[Data] =
    def newArray(): Array[Byte]^ = new Array[Byte](size)


    def recur(stream: Progression[Data], sourcePos: Int, dest: Array[Byte]^, destPos: Int)
    :   Progression[Data] =

      stream match
        case source #:: more =>
          val ready = source.length - sourcePos
          val free = dest.length - destPos

          if ready < free then
            jl.System.arraycopy(source, sourcePos, dest, destPos, ready)
            recur(more, 0, dest, destPos + ready)
          else if free < ready then
            jl.System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(stream, sourcePos + free, newArray(), 0)
          else // free == ready
            jl.System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(more, 0, newArray(), 0)

        case _ =>
          if destPos == 0 then Progression()
          else
            // arraycopy, not `.slice`: the ArrayOps conversion demands a pure array
            val length = if zeroPadding then dest.length else destPos
            val out = new Array[Byte](length)
            jl.System.arraycopy(dest, 0, out, 0, length)
            Progression(out.immutable(using Unsafe).asInstanceOf[Data])


    recur(stream, 0, newArray(), 0)

  def take(bytes: Bytes): Progression[Data] =
    def recur(stream: Progression[Data], count: Bytes): Progression[Data] =
      stream.flow(Progression()):
        if next.bytes < count then
          val head: Data = next
          head #:: recur(more, count - next.bytes)
        else Progression(next.take(count.long.toInt).asInstanceOf[Data])

    recur(stream, bytes)

  def inputStream: ji.InputStream = new ji.InputStream:
    // A JDK adapter, not a capability class: its staging slots are untracked.
    @caps.unsafe.untrackedCaptures private var current: Progression[Data] = stream
    @caps.unsafe.untrackedCaptures private var offset: Int = 0
    @caps.unsafe.untrackedCaptures private var focus: Data = IArray.empty[Byte].asInstanceOf[Data]

    override def available(): Int =
      val diff = focus.length - offset

      if diff > 0 then diff
      else if current.nil then 0
      else
        focus = current.head
        current = current.tail
        offset = 0
        available()

    override def close(): Unit = ()

    def read(): Int = if available() == 0 then -1 else (focus(offset) & 0xff).also(offset += 1)

    override def read(array: Array[Byte] | Null, arrayOffset: Int, length: Int): Int =
      if length == 0 then 0 else
        val count = length.min(available())

        if count == 0 then -1 else
          if count > 0 then jl.System.arraycopy(focus, offset, array, arrayOffset, count)
          offset += count
          count
