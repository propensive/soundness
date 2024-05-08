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

import rudiments.*
import hypotenuse.*
import vacuous.*
import parasite.*
import capricious.*
import anticipation.*

import scala.collection.mutable as scm

import java.io as ji
import java.util.zip as juz

import language.experimental.captureChecking

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

erased trait CompressionAlgorithm
erased trait Gzip extends CompressionAlgorithm
erased trait Zlib extends CompressionAlgorithm

trait Compression:
  type Self <: CompressionAlgorithm
  def compress(lazyList: LazyList[Bytes]): LazyList[Bytes]
  def decompress(lazyList: LazyList[Bytes]): LazyList[Bytes]

object Compression:
  given Gzip is Compression:
    def compress(lazyList: LazyList[Bytes]): LazyList[Bytes] =
      val out = ji.ByteArrayOutputStream()
      val out2 = juz.GZIPOutputStream(out)

      def recur(stream: LazyList[Bytes]): LazyList[Bytes] = stream match
        case head #:: tail =>
          out2.write(head.mutable(using Unsafe))
          if out.size == 0 then recur(tail) else
            val data = out.toByteArray().nn.immutable(using Unsafe)
            out.reset()
            data #:: recur(tail)

        case _ =>
          out2.close()
          if out.size == 0 then LazyList() else LazyList(out.toByteArray().nn.immutable(using Unsafe))

      recur(lazyList)

    def decompress(lazyList: LazyList[Bytes]): LazyList[Bytes] =
      juz.GZIPInputStream(LazyListInputStream(lazyList)).stream[Bytes]

extension (lazyList: LazyList[Bytes])
  def drop(byteSize: ByteSize): LazyList[Bytes] =
    def recur(stream: LazyList[Bytes], skip: ByteSize): LazyList[Bytes] = stream match
      case head #:: tail =>
        if head.byteSize < skip
        then recur(tail, skip - head.byteSize) else head.drop(skip.long.toInt) #:: tail

      case _ =>
        LazyList()

    recur(lazyList, byteSize)

  def compress[CompressionType <: CompressionAlgorithm: Compression]: LazyList[Bytes] =
    summon[Compression].compress(lazyList)

  def decompress[CompressionType <: CompressionAlgorithm: Compression]: LazyList[Bytes] =
    summon[Compression].decompress(lazyList)

  def shred(mean: Double, variance: Double)(using RandomNumberGenerator): LazyList[Bytes] =
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
    def recur(stream: LazyList[Bytes], count: ByteSize): LazyList[Bytes] = stream match
      case head #:: tail =>
        if head.byteSize < count then head #:: recur(tail, count - head.byteSize)
        else LazyList(head.take(count.long.toInt))

      case _ =>
        LazyList()

    recur(lazyList, byteSize)

class LazyListOutputStream() extends ji.OutputStream:
  private val buffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
  private val chunks: Funnel[Bytes] = Funnel()

  def stream: LazyList[Bytes] = chunks.stream
  def write(int: Int): Unit = buffer.append(int.toByte)

  override def close(): Unit = flush().also(chunks.stop())
  override def write(bytes: Array[Byte]): Unit = chunks.put(bytes.immutable(using Unsafe))

  override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
    chunks.put(bytes.slice(offset, offset + length).immutable(using Unsafe))

  override def flush(): Unit = if !buffer.isEmpty then
    chunks.put(buffer.toArray.immutable(using Unsafe))
    buffer.clear()

object LazyListInputStream:
  def apply(input: => LazyList[Bytes]): LazyListInputStream = new LazyListInputStream(input)

class LazyListInputStream(input: LazyList[Bytes]) extends ji.InputStream:
  private var stream: LazyList[Bytes] = input
  private var offset: Int = 0
  private var focus: Bytes = IArray.empty[Byte]

  override def available(): Int =
    val diff = focus.length - offset
    if diff > 0 then diff
    else if stream.isEmpty then 0
    else
      focus = stream.head
      stream = stream.tail
      offset = 0
      available()

  override def close(): Unit = ()

  def read(): Int = if available() == 0 then -1 else (focus(offset) & 0xff).also(offset += 1)

  override def read(array: Array[Byte], arrayOffset: Int, length: Int): Int =
    if length == 0 then 0 else
      val count = length.min(available())

      if count == 0 then -1 else
        if count > 0 then System.arraycopy(focus, offset, array, arrayOffset, count)
        offset += count
        count

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
        sleepUntil(startTime + duration.milliseconds*iteration)
        () #:: pulsar(duration)
      catch case err: ConcurrencyError => LazyList()

    recur(0)
