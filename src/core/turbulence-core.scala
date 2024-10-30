/*
    Turbulence, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import language.adhocExtensions

import java.io as ji
import java.util.zip as juz

import anticipation.*
import capricious.*
import contingency.*
import hypotenuse.*
import parasite.*, orphanDisposal.await
import rudiments.*
import symbolism.*
import prepositional.*
import vacuous.*

extension [ValueType](value: ValueType)
  def stream[ElementType](using readable: ValueType is Readable by ElementType): LazyList[ElementType] =
    readable.stream(value)

  inline def read[ResultType]: ResultType =
    compiletime.summonFrom:
      case aggregable: (ResultType is Aggregable by Bytes) =>
        compiletime.summonInline[ValueType is Readable by Bytes].give:
          aggregable.aggregate(value.stream[Bytes])

      case aggregable: (ResultType is Aggregable by Text) =>
        compiletime.summonInline[ValueType is Readable by Text].give:
          aggregable.aggregate(value.stream[Text])

  def writeTo[TargetType](target: TargetType)[ElementType]
      (using readable: ValueType is Readable by ElementType, writable: TargetType is Writable by ElementType)
          : Unit =

    writable.write(target, readable.stream(value))

package stdioSources:
  given Stdio as mute = Stdio(null, null, null, termcapDefinitions.basic)

  package system:
    given Stdio as textOnly =
      Stdio(System.out.nn, System.err.nn, System.in.nn, termcapDefinitions.basic)

    given Stdio as ansi =
      Stdio(System.out.nn, System.err.nn, System.in.nn, termcapDefinitions.xterm256)

  package virtualMachine:
    given Stdio as textOnly =
      val stdout = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.out))
      val stderr = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.err))
      val stdin = ji.FileInputStream(ji.FileDescriptor.in)

      Stdio(stdout, stderr, stdin, termcapDefinitions.basic)

    given Stdio as ansi =
      val stdout = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.out))
      val stderr = ji.PrintStream(ji.FileOutputStream(ji.FileDescriptor.err))
      val stdin = ji.FileInputStream(ji.FileDescriptor.in)

      Stdio(stdout, stderr, stdin, termcapDefinitions.xterm256)

extension [ElementType](stream: LazyList[ElementType])
  def deduplicate: LazyList[ElementType] =
    def recur(last: ElementType, stream: LazyList[ElementType]): LazyList[ElementType] =
      stream.flow(LazyList())(if last == head then recur(last, tail) else head #:: recur(head, tail))

    stream.flow(LazyList())(head #:: recur(head, tail))

  inline def flow[ResultType](inline termination: => ResultType)
      (inline proceed: (head: ElementType, tail: LazyList[ElementType]) ?=> ResultType)
          : ResultType =
    stream match
      case head #:: tail => proceed(using head, tail)
      case _             => termination

  def strict: LazyList[ElementType] = stream.length yet stream

  def rate[DurationType: GenericDuration: SpecificDuration](duration: DurationType)
      (using Monitor, Tactic[ConcurrencyError])
          : LazyList[ElementType] =

    def recur(stream: LazyList[ElementType], last: Long): LazyList[ElementType] =
      stream.flow(LazyList()):
        val delay = SpecificDuration(duration.milliseconds - (System.currentTimeMillis - last))
        if delay.milliseconds > 0 then sleep(delay)
        stream

    async(recur(stream, System.currentTimeMillis)).await()

  def multiplexWith(that: LazyList[ElementType])(using Monitor): LazyList[ElementType] =
    unsafely(LazyList.multiplex(stream, that))

  def regulate(tap: Tap)(using Monitor): LazyList[ElementType] =
    def defer
        (active: Boolean,
         stream: LazyList[Some[ElementType] | Tap.Regulation],
         buffer: List[ElementType])
            : LazyList[ElementType] =

      recur(active, stream, buffer)

    @tailrec
    def recur
        (active: Boolean,
         stream: LazyList[Some[ElementType] | Tap.Regulation],
         buffer: List[ElementType])
            : LazyList[ElementType] =

      if active && buffer.nonEmpty then buffer.head #:: defer(true, stream, buffer.tail)
      else if stream.isEmpty then LazyList()
      else stream.head match
        case Tap.Regulation.Start =>
          recur(true, stream.tail, buffer)

        case Tap.Regulation.Stop =>
          recur(false, stream.tail, Nil)

        case Some(other) =>
          if active then other.nn #:: defer(true, stream.tail, Nil)
          else recur(false, stream.tail, other.nn :: buffer)

    LazyList.defer(recur(true, stream.map(Some(_)).multiplexWith(tap.stream), Nil))

  def cluster[DurationType: GenericDuration](duration: DurationType, maxSize: Optional[Int] = Unset)
      (using Monitor)
          : LazyList[List[ElementType]] =

    val Limit = maxSize.or(Int.MaxValue)

    def recur(stream: LazyList[ElementType], list: List[ElementType], count: Int)
            : LazyList[List[ElementType]] =

      count match
        case 0 => safely(async(stream.isEmpty).await()) match
          case Unset => recur(stream, Nil, 0)
          case false => recur(stream.tail, stream.head :: list, count + 1)
          case true  => LazyList()

        case Limit =>
          list.reverse #:: recur(stream, Nil, 0)

        case _ => safely(async(stream.isEmpty).await(duration)) match
          case Unset => list.reverse #:: recur(stream, Nil, 0)
          case false => recur(stream.tail, stream.head :: list, count + 1)
          case true  => LazyList(list.reverse)

    LazyList.defer(recur(stream, Nil, 0))

  def parallelMap[ElementType2](lambda: ElementType => ElementType2)(using Monitor)
          : LazyList[ElementType2] =

    val out: Spool[ElementType2] = Spool()

    async:
      stream.map: elem =>
        async(out.put(lambda(elem)))

    out.stream

package lineSeparation:
  import LineSeparation.Action.*
  import LineSeparation.NewlineSeq

  given LineSeparation(NewlineSeq.Cr, Nl, Skip, Nl, Nl) as carriageReturn
  given LineSeparation(NewlineSeq.Cr, Nl, Lf, NlLf, LfNl) as strictCarriageReturn
  given LineSeparation(NewlineSeq.Lf, Skip, Nl, Nl, Nl) as linefeed
  given LineSeparation(NewlineSeq.Lf, Nl, Lf, NlLf, LfNl) as strictLinefeeds
  given LineSeparation(NewlineSeq.CrLf, Skip, Lf, Nl, LfNl) as carriageReturnLinefeed
  given LineSeparation(NewlineSeq.Lf, Nl, Nl, Nl, Nl) as adaptiveLinefeed

  given LineSeparation as virtualMachine = System.lineSeparator.nn match
    case "\r\n"    => carriageReturnLinefeed
    case "\r"      => carriageReturn
    case "\n"      => linefeed
    case _: String => adaptiveLinefeed

extension (obj: LazyList.type)
  def multiplex[ElemType](streams: LazyList[ElemType]*)(using Monitor)
          : LazyList[ElemType] =

    multiplexer(streams*).stream

  def multiplexer[ElemType](streams: LazyList[ElemType]*)(using Monitor)
          : Multiplexer[Any, ElemType] =

    val multiplexer = Multiplexer[Any, ElemType]()
    streams.zipWithIndex.map(_.swap).each(multiplexer.add)
    multiplexer

  def defer[ElemType](lazyList: => LazyList[ElemType]): LazyList[ElemType] =
    (null.asInstanceOf[ElemType] #:: lazyList).tail

  def pulsar[DurationType: GenericDuration](duration: DurationType)(using Monitor): LazyList[Unit] =
    val startTime: Long = System.currentTimeMillis

    def recur(iteration: Int): LazyList[Unit] =
      try
        snooze(startTime + duration.milliseconds*iteration)
        () #:: pulsar(duration)
      catch case err: ConcurrencyError => LazyList()

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

extension (lazyList: LazyList[Bytes])
  def discard(byteSize: ByteSize): LazyList[Bytes] =
    def recur(stream: LazyList[Bytes], count: ByteSize): LazyList[Bytes] = stream.flow(LazyList()):
      if head.byteSize < count
      then recur(tail, count - head.byteSize) else head.drop(count.long.toInt) #:: tail

    recur(lazyList, byteSize)

  def compress[CompressionType <: CompressionAlgorithm: Compression]: LazyList[Bytes] =
    summon[Compression].compress(lazyList)

  def decompress[CompressionType <: CompressionAlgorithm: Compression]: LazyList[Bytes] =
    summon[Compression].decompress(lazyList)

  def shred(mean: Double, variance: Double)(using Randomization): LazyList[Bytes] =
    stochastic:
      given Distribution = Gamma.approximate(mean, variance)

      def newArray(): Array[Byte] = new Array[Byte](arbitrary[Double]().toInt.max(1))

      def recur(stream: LazyList[Bytes], sourcePos: Int, dest: Array[Byte], destPos: Int)
              : LazyList[Bytes] =

        stream match
          case source #:: more =>
            val ready = source.length - sourcePos
            val free = dest.length - destPos

            if ready < free then
              System.arraycopy(source, sourcePos, dest, destPos, ready)
              recur(more, 0, dest, destPos + ready)
            else if free < ready then
              System.arraycopy(source, sourcePos, dest, destPos, free)
              dest.immutable(using Unsafe) #:: recur(stream, sourcePos + free, newArray(), 0)
            else // free == ready
              System.arraycopy(source, sourcePos, dest, destPos, free)
              dest.immutable(using Unsafe) #:: recur(more, 0, newArray(), 0)

          case _ =>
            if destPos == 0 then LazyList()
            else LazyList(dest.slice(0, destPos).immutable(using Unsafe))

      recur(lazyList, 0, newArray(), 0)

  def chunked(size: Int, zeroPadding: Boolean = false): LazyList[Bytes] =
    def newArray(): Array[Byte] = new Array[Byte](size)

    def recur(stream: LazyList[Bytes], sourcePos: Int, dest: Array[Byte], destPos: Int)
            : LazyList[Bytes] =

      stream match
        case source #:: more =>
          val ready = source.length - sourcePos
          val free = dest.length - destPos

          if ready < free then
            System.arraycopy(source, sourcePos, dest, destPos, ready)
            recur(more, 0, dest, destPos + ready)
          else if free < ready then
            System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(stream, sourcePos + free, newArray(), 0)
          else // free == ready
            System.arraycopy(source, sourcePos, dest, destPos, free)
            dest.immutable(using Unsafe) #:: recur(more, 0, newArray(), 0)

        case _ =>
          if destPos == 0 then LazyList()
          else LazyList:
            (if zeroPadding then dest else dest.slice(0, destPos)).immutable(using Unsafe)

    recur(lazyList, 0, newArray(), 0)

  def take(byteSize: ByteSize): LazyList[Bytes] =
    def recur(stream: LazyList[Bytes], count: ByteSize): LazyList[Bytes] =
      stream.flow(LazyList()):
        if head.byteSize < count then head #:: recur(tail, count - head.byteSize)
        else LazyList(head.take(count.long.toInt))

    recur(lazyList, byteSize)

def spool[ItemType](using DummyImplicit)[ResultType](lambda: Spool[ItemType] => ResultType)
        : ResultType =
  val spool: Spool[ItemType] = Spool()
  try lambda(spool) finally spool.stop()
