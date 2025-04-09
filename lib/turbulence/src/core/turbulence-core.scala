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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import language.adhocExtensions

import java.io as ji
import java.lang as jl
import java.util.zip as juz

import anticipation.*
import capricious.*
import contingency.*
import hypotenuse.*
import parasite.*, asyncTermination.await
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

extension [value](value: value)
  def stream[element](using readable: value is Readable by element): Stream[element] =
    readable.stream(value)

  inline def read[result]: result =
    compiletime.summonFrom:
      case aggregable: (`result` is Aggregable by Bytes) =>
        compiletime.summonInline[value is Readable by Bytes].give:
          aggregable.aggregate(value.stream[Bytes])

      case aggregable: (`result` is Aggregable by Text) =>
        compiletime.summonInline[value is Readable by Text].give:
          aggregable.aggregate(value.stream[Text])

  def writeTo[target](target: target)[element]
     (using readable: value is Readable by element, writable: target is Writable by element)
  :     Unit =

    writable.write(target, readable.stream(value))

package stdioSources:
  given mute: Stdio = Stdio(null, null, null, termcapDefinitions.basic)

  package system:
    given textOnly: Stdio =
      Stdio(jl.System.out.nn, jl.System.err.nn, jl.System.in.nn, termcapDefinitions.basic)

    given ansi: Stdio =
      Stdio(jl.System.out.nn, jl.System.err.nn, jl.System.in.nn, termcapDefinitions.xterm256)

  package virtualMachine:
    given textOnly: Stdio =
      val stdout = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.out))
      val stderr = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.err))
      val stdin = ji.FileInputStream(ji.FileDescriptor.in)

      Stdio(stdout, stderr, stdin, termcapDefinitions.basic)

    given ansi: Stdio =
      val stdout = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.out))
      val stderr = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.err))
      val stdin = ji.FileInputStream(ji.FileDescriptor.in)

      Stdio(stdout, stderr, stdin, termcapDefinitions.xterm256)

extension [element](stream: Stream[element])
  def deduplicate: Stream[element] =
    def recur(last: element, stream: Stream[element]): Stream[element] =
      stream.flow(Stream()):
        if last == head then recur(last, tail) else head #:: recur(head, tail)

    stream.flow(Stream())(head #:: recur(head, tail))

  inline def flow[result](inline termination: => result)
     (inline proceed: (head: element, tail: Stream[element]) ?=> result)
  :     result =
    stream match
      case head #:: tail => proceed(using head, tail)
      case _             => termination

  def strict: Stream[element] = stream.length yet stream

  def rate[generic: {GenericDuration, SpecificDuration}](duration: generic)(using Monitor)
  :     Stream[element] raises AsyncError =

    def recur(stream: Stream[element], last: Long): Stream[element] =
      stream.flow(Stream()):
        val duration2 =
          SpecificDuration(generic.milliseconds(duration) - (jl.System.currentTimeMillis - last))

        if generic.milliseconds(duration2) > 0 then snooze(duration2)
        stream

    async(recur(stream, jl.System.currentTimeMillis)).await()

  def multiplexWith(that: Stream[element])(using Monitor): Stream[element] =
    unsafely(Stream.multiplex(stream, that))

  def regulate(tap: Tap)(using Monitor): Stream[element] =
    def defer
       (active: Boolean,
        stream: Stream[Some[element] | Tap.Regulation],
        buffer: List[element])
    :     Stream[element] =

      recur(active, stream, buffer)

    @tailrec
    def recur
       (active: Boolean,
        stream: Stream[Some[element] | Tap.Regulation],
        buffer: List[element])
    :     Stream[element] =

      if active && buffer.nonEmpty then buffer.head #:: defer(true, stream, buffer.tail)
      else if stream.isEmpty then Stream()
      else stream.head match
        case Tap.Regulation.Start =>
          recur(true, stream.tail, buffer)

        case Tap.Regulation.Stop =>
          recur(false, stream.tail, Nil)

        case Some(other) =>
          if active then other.nn #:: defer(true, stream.tail, Nil)
          else recur(false, stream.tail, other.nn :: buffer)

    Stream.defer(recur(true, stream.map(Some(_)).multiplexWith(tap.stream), Nil))

  def cluster[duration: GenericDuration](duration: duration, maxSize: Optional[Int] = Unset)
     (using Monitor)
  :     Stream[List[element]] =

    val Limit = maxSize.or(Int.MaxValue)

    def recur(stream: Stream[element], list: List[element], count: Int): Stream[List[element]] =
      count match
        case 0 => safely(async(stream.isEmpty).await()) match
          case Unset => recur(stream, Nil, 0)
          case false => recur(stream.tail, stream.head :: list, count + 1)
          case true  => Stream()

        case Limit =>
          list.reverse #:: recur(stream, Nil, 0)

        case _ => safely(async(stream.isEmpty).await(duration)) match
          case Unset => list.reverse #:: recur(stream, Nil, 0)
          case false => recur(stream.tail, stream.head :: list, count + 1)
          case true  => Stream(list.reverse)

    Stream.defer(recur(stream, Nil, 0))

  def parallelMap[element2](lambda: element => element2)(using Monitor): Stream[element2] =

    val out: Spool[element2] = Spool()

    async:
      stream.map: elem =>
        async(out.put(lambda(elem)))

    out.stream

package lineSeparation:
  import LineSeparation.Action.*
  import LineSeparation.NewlineSeq

  given carriageReturn: LineSeparation(NewlineSeq.Cr, Nl, Skip, Nl, Nl)
  given strictCarriageReturn: LineSeparation(NewlineSeq.Cr, Nl, Lf, NlLf, LfNl)
  given linefeed: LineSeparation(NewlineSeq.Lf, Skip, Nl, Nl, Nl)
  given strictLinefeeds: LineSeparation(NewlineSeq.Lf, Nl, Lf, NlLf, LfNl)
  given carriageReturnLinefeed: LineSeparation(NewlineSeq.CrLf, Skip, Lf, Nl, LfNl)
  given adaptiveLinefeed: LineSeparation(NewlineSeq.Lf, Nl, Nl, Nl, Nl)

  given virtualMachine: LineSeparation = jl.System.lineSeparator.nn match
    case "\r\n"    => carriageReturnLinefeed
    case "\r"      => carriageReturn
    case "\n"      => linefeed
    case _: String => adaptiveLinefeed

extension (obj: Stream.type)
  def multiplex[element](streams: Stream[element]*)(using Monitor): Stream[element] =

    multiplexer(streams*).stream

  def multiplexer[element](streams: Stream[element]*)(using Monitor): Multiplexer[Any, element] =

    val multiplexer = Multiplexer[Any, element]()
    streams.zipWithIndex.map(_.swap).each(multiplexer.add)
    multiplexer

  def defer[element](stream: => Stream[element]): Stream[element] =
    (null.asInstanceOf[element] #:: stream).tail

  def pulsar[generic: GenericDuration](duration: generic)(using Monitor): Stream[Unit] =
    val startTime: Long = jl.System.currentTimeMillis

    def recur(iteration: Int): Stream[Unit] =
      try
        snooze(startTime + generic.milliseconds(duration)*iteration)
        () #:: pulsar(duration)
      catch case err: AsyncError => Stream()

    recur(0)

extension (bytes: Bytes)
  def gzip: Bytes =
    val out = ji.ByteArrayOutputStream()
    val out2 = juz.GZIPOutputStream(out)
    out2.write(bytes.mutable(using Unsafe))
    out2.close()
    out.toByteArray.nn.immutable(using Unsafe)

  def gunzip: Bytes =
    val in = ji.ByteArrayInputStream(bytes.mutable(using Unsafe))
    val in2 = juz.GZIPInputStream(in)
    val out = ji.ByteArrayOutputStream()
    val buffer: Array[Byte] = new Array(1024)

    def recur(): Unit = in2.read(buffer).tap: length =>
      if length > 0 then
        out.write(buffer, 0, length)
        recur()

    recur()

    out.toByteArray.nn.immutable(using Unsafe)

extension (stream: Stream[Bytes])
  def discard(memory: Memory): Stream[Bytes] =
    def recur(stream: Stream[Bytes], count: Memory): Stream[Bytes] = stream.flow(Stream()):
      if head.memory < count
      then recur(tail, count - head.memory) else head.drop(count.long.toInt) #:: tail

    recur(stream, memory)

  def compress[compression <: CompressionAlgorithm: Compression]: Stream[Bytes] =
    compression.compress(stream)

  def decompress[compression <: CompressionAlgorithm: Compression]: Stream[Bytes] =
    compression.decompress(stream)

  def shred(mean: Double, variance: Double)(using Random): Stream[Bytes] =
    given gamma: Distribution = Gamma.approximate(mean, variance)

    def newArray(): Array[Byte] = new Array[Byte](arbitrary[Double]().toInt.max(1))

    def recur(stream: Stream[Bytes], sourcePos: Int, dest: Array[Byte], destPos: Int)
    :     Stream[Bytes] =

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
          if destPos == 0 then Stream()
          else Stream(dest.slice(0, destPos).immutable(using Unsafe))

    recur(stream, 0, newArray(), 0)

  def chunked(size: Int, zeroPadding: Boolean = false): Stream[Bytes] =
    def newArray(): Array[Byte] = new Array[Byte](size)

    def recur(stream: Stream[Bytes], sourcePos: Int, dest: Array[Byte], destPos: Int)
    :     Stream[Bytes] =

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
          if destPos == 0 then Stream()
          else Stream:
            (if zeroPadding then dest else dest.slice(0, destPos)).immutable(using Unsafe)

    recur(stream, 0, newArray(), 0)

  def take(memory: Memory): Stream[Bytes] =
    def recur(stream: Stream[Bytes], count: Memory): Stream[Bytes] =
      stream.flow(Stream()):
        if head.memory < count then head #:: recur(tail, count - head.memory)
        else Stream(head.take(count.long.toInt))

    recur(stream, memory)

  def inputStream: ji.InputStream = new ji.InputStream:
    private var current: Stream[Bytes] = stream
    private var offset: Int = 0
    private var focus: Bytes = IArray.empty[Byte]

    override def available(): Int =
      val diff = focus.length - offset
      if diff > 0 then diff
      else if current.isEmpty then 0
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

def spool[item](using erased Void)[result](lambda: Spool[item] => result): result =
  val spool: Spool[item] = Spool()
  try lambda(spool) finally spool.stop()
