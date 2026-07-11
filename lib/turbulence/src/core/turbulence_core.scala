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

import language.experimental.separationChecking

import language.adhocExtensions

import java.io as ji
import java.lang as jl

import anticipation.*
import capricious.*
import contingency.*
import denominative.*
import hypotenuse.*
import parasite.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.*

import LineSeparation.*
import abstractables.instantAbstractable
import probates.awaitProbate

inline def more[value](using value: value aka "more"): value = value()

extension [value](value: value)
  inline def stream[element]: LazyList[element] =
    ${turbulence.internal.stream[value, element]('value)}

  inline def read[result](using readable: (value is Readable to result)^): result =
    readable.read(value)


  def writeTo[target](target: target)[element]
    // Capture-polymorphic evidence: writers built from an `Emit[StreamError]` capture it; the
    // write completes within this call, so nothing is retained and the result stays `Unit`.
    ( using streamable: (value is Streamable by element)^,
            writable:   (target is Writable by element)^ )
  :   Unit =

    writable.write(target, streamable.stream(value))

extension [value: Streamable by Text](value: value)
  def load[result <: Documentary](using loadable: (result is Loadable by Text)^)
  :   Document[result] =
    loadable.load(value.stream[Text])

extension [medium, transport](consume stream: (Stream[medium] over transport)^)
  // The detached pump: `flowTo` on its own parasite task, for fire-and-forget
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
      . flowTo(intakeRef.asInstanceOf[(Intake[medium] over transport)^])

extension (consume stream: (Stream[Data] over Credit)^)
  def compress[format <: Compressor](using compression: format is Compression, buffering: Buffering)
  :   (Stream[Data] over Credit)^ =

    stream.through(compression.compressor())

  def decompress[format <: Compressor]
    ( using compression: format is Compression, buffering: Buffering )
  :   (Stream[Data] over Credit)^ =

    stream.through(compression.decompressor())

extension [medium](consume stream: (Stream[medium] over Credit)^)
  // Legacy view: drain a pull endpoint as a lazy list of materialized chunks,
  // pulling one block per forced cell. For consumers not yet converted to the
  // streaming kernel; conversion holds only one block at a time, but the
  // resulting cells are immutable and GC-managed like any lazy list.
  def lazyList(using buffering: Buffering): LazyList[medium] =
    val block = buffering.capacity(stream.addressable.substrate)

    def recur(): LazyList[medium] =
      stream.refill(Credit(block)) match
        case Unset =>
          LazyList()

        case count: Int =>
          val window = stream.window(using Unsafe)
          val chunk = stream.addressable.materialize(window, stream.start, count)
          stream.skip(count)
          chunk #:: recur()

    // The transitional bridge: a LazyList is pure, so it cannot carry the consumed
    // stream's capture — the well-known confinement gap that motivated the capability
    // kernel. Sealed here, at the single bridge point (scheduled for removal).
    caps.unsafe.unsafeAssumePure(LazyList.defer(recur()))

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

  // Legacy view of an HTTP body as a lazy list of chunks.
  def lazyList(using buffering: Buffering): LazyList[Data] =
    val block = buffering.capacity(Substrate.Bytes)

    def recur(): LazyList[Data] = body.next(block) match
      case null       => LazyList()
      case data: Data => data #:: recur()

    // The transitional bridge: a LazyList is pure, so it cannot carry the consumed
    // stream's capture — the well-known confinement gap that motivated the capability
    // kernel. Sealed here, at the single bridge point (scheduled for removal).
    caps.unsafe.unsafeAssumePure(LazyList.defer(recur()))

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

def spool[item](using erased void: Void)[result](lambda: Spool[item] => result): result =
  val spool: Spool[item] = Spool()
  try lambda(spool) finally spool.stop()

extension [element](stream: LazyList[element])
  def deduplicate: LazyList[element] =
    def recur(last: element, stream: LazyList[element]): LazyList[element] =
      stream.flow(LazyList()):
        if last == next then recur(last, more) else next #:: recur(next, more)

    stream.flow(LazyList())(next #:: recur(next, more))


  // `next`/`more` are bound with `aka`-label refinements; under capture checking the
  // labelled singleton type does not simplify away in every position (the aka-Tagged/
  // castbox class), so use sites strip it with `next.asInstanceOf[element]`.
  inline def flow[result](inline termination: => result)
    ( inline proceed: (element aka "next", LazyList[element] aka "more") ?=> result )
  :   result =

    stream match
      case next #:: more => proceed(using next.aka["next"], more.aka["more"])
      case _             => termination


  def strict: LazyList[element] = stream.length yet stream

  def multiplex(that: LazyList[element])(using Monitor): LazyList[element] =
    LazyList.multiplex(stream, that)

  def regulate(tap: Tap)(using Monitor): LazyList[element] =
    def defer
      ( active: Boolean,
        stream: LazyList[Some[element] | Tap.Regulation],
        buffer: List[element] )
    :   LazyList[element] =

      recur(active, stream, buffer)


    @tailrec
    def recur
      ( active: Boolean, stream: LazyList[Some[element] | Tap.Regulation], buffer: List[element] )
    :   LazyList[element] =

      if active && buffer.nonEmpty then buffer.head #:: defer(true, stream, buffer.tail)
      else if stream.nil then LazyList()
      else stream.head match
        case Tap.Regulation.Start => recur(true, stream.tail, buffer)
        case Tap.Regulation.Stop  => recur(false, stream.tail, Nil)

        case Some(other) =>
          if active then other.nn #:: defer(true, stream.tail, Nil)
          else recur(false, stream.tail, other.nn :: buffer)

    LazyList.defer(recur(true, stream.map(Some(_)).multiplex(tap.stream), Nil))

  def cluster[duration: Abstractable across Durations to Long]
    ( duration: duration, maxSize: Optional[Int] = Unset )
    ( using Monitor )
  :   LazyList[List[element]] =

    val Limit = maxSize.or(Int.MaxValue)

    def recur(stream: LazyList[element], list: List[element], count: Int): LazyList[List[element]] =
      count match
        case 0 =>
          safely(async(stream.nil).await()) match
            case false => recur(stream.tail, stream.head :: list, count + 1)
            case true  => LazyList()
            case _     => recur(stream, Nil, 0)

        case Limit =>
          list.reverse #:: recur(stream, Nil, 0)

        case _ =>
          safely(async(stream.nil).await(duration)) match
            case false => recur(stream.tail, stream.head :: list, count + 1)
            case true  => LazyList(list.reverse)
            case _     => list.reverse #:: recur(stream, Nil, 0)

    LazyList.defer(recur(stream, Nil, 0))


  def parallelMap[element2](lambda: element => element2)(using Monitor): LazyList[element2] =

    val out: Spool[element2] = Spool()

    async:
      stream.each: element =>
        // Fan out: each element is mapped on its own supervised worker that puts the result
        // into the
        // spool. (`each`, not `map`: the source must be drained to actually spawn the workers.)
        async(out.put(lambda(element)))
        ()

    out.stream

extension (obj: LazyList.type)
  def multiplex[element](streams: LazyList[element]*)(using Monitor): LazyList[element] =
    multiplexer(streams*).stream

  def multiplexer[element](streams: LazyList[element]*)(using monitor: Monitor): Multiplexer[Any, element]^{monitor} =
    Multiplexer[Any, element]().tap: multiplexer =>
      streams.zipWithIndex.each: (stream, index) =>
        multiplexer.add(index, stream)

  def defer[element](stream: => LazyList[element]): LazyList[element] =
    (null.asInstanceOf[element] #:: stream).tail


  def metronome[generic: Abstractable across Durations to Long](duration: generic)(using Monitor)
  :   LazyList[Unit] =

    val spool: Spool[Unit] = Spool()
    val startTime: Long = jl.System.currentTimeMillis

    // Ticking requires the `Monitor` (to `sleep`), which a pure lazy `Stream` cannot itself
    // capture,
    // so it runs as a supervised task that pushes ticks into a `Spool`. When the scope is torn
    // down a
    // cancelled `sleep` raises `AsyncError`; the spool is then stopped so the consumer's stream ends.
    async:
      @tailrec
      def recur(iteration: Int): Unit =
        sleep(startTime + duration.generic/1_000_000L*iteration)
        spool.put(())
        recur(iteration + 1)

      try recur(0) catch case _: AsyncError => spool.stop()

    spool.stream


extension (stream: LazyList[Data])
  def discard(bytes: Bytes): LazyList[Data] =
    def recur(stream: LazyList[Data], count: Bytes): LazyList[Data] = stream.flow(LazyList()):
      if next.bytes < count
      then recur(more, count - next.bytes)
      else
        // hoisted: a fresh array built inside `#::`'s by-name operand (which would
        // capture the enclosing context) could not escape it
        val head: Data = next.drop(count.long.toInt).asInstanceOf[Data]
        head #:: more

    recur(stream, bytes)

  def compress[compression <: Compressor: Compression]: LazyList[Data] =
    compression.compress(stream)

  def decompress[compression <: Compressor: Compression]: LazyList[Data] =
    compression.decompress(stream)

  def shred(mean: Double, variance: Double)(using Random): LazyList[Data] =
    given gamma: Distribution = Gamma.approximate(mean, variance)

    def newArray(): Array[Byte]^ = new Array[Byte](arbitrary[Double]().toInt.max(1))

    def recur(stream: LazyList[Data], sourcePos: Int, dest: Array[Byte]^, destPos: Int)
    :   LazyList[Data] =

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
          if destPos == 0 then LazyList()
          else
            // arraycopy, not `.slice`: the ArrayOps conversion demands a pure array
            val out = new Array[Byte](destPos)
            jl.System.arraycopy(dest, 0, out, 0, destPos)
            LazyList(out.immutable(using Unsafe).asInstanceOf[Data])

    recur(stream, 0, newArray(), 0)

  def chunked(size: Int, zeroPadding: Boolean = false): LazyList[Data] =
    def newArray(): Array[Byte]^ = new Array[Byte](size)


    def recur(stream: LazyList[Data], sourcePos: Int, dest: Array[Byte]^, destPos: Int)
    :   LazyList[Data] =

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
          if destPos == 0 then LazyList()
          else
            // arraycopy, not `.slice`: the ArrayOps conversion demands a pure array
            val length = if zeroPadding then dest.length else destPos
            val out = new Array[Byte](length)
            jl.System.arraycopy(dest, 0, out, 0, length)
            LazyList(out.immutable(using Unsafe).asInstanceOf[Data])


    recur(stream, 0, newArray(), 0)

  def take(bytes: Bytes): LazyList[Data] =
    def recur(stream: LazyList[Data], count: Bytes): LazyList[Data] =
      stream.flow(LazyList()):
        if next.bytes < count then
          val head: Data = next
          head #:: recur(more, count - next.bytes)
        else LazyList(next.take(count.long.toInt).asInstanceOf[Data])

    recur(stream, bytes)

  def inputStream: ji.InputStream = new ji.InputStream:
    // A JDK adapter, not a capability class: its staging slots are untracked.
    @caps.unsafe.untrackedCaptures private var current: LazyList[Data] = stream
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
