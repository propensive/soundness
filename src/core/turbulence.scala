/*
    Turbulence, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import fulminate.*
import perforate.*
import anticipation.*
import parasite.*
import hieroglyph.*

import java.io as ji
import java.nio as jn
import java.util.concurrent.atomic as juca

import scala.collection.mutable as scm

import language.dynamics

import java.util.concurrent as juc

extension (value: LazyList[Bytes])
  def slurp(): Bytes =
    val bld: scm.ArrayBuilder[Byte] = scm.ArrayBuilder.ofByte()
    value.foreach { bs => bld.addAll(bs.mutable(using Unsafe)) }
    
    bld.result().immutable(using Unsafe)

extension (obj: LazyList.type)
  def multiplex[ElemType](streams: LazyList[ElemType]*)(using Monitor): LazyList[ElemType] =
    multiplexer(streams*).stream
  
  def multiplexer[ElemType](streams: LazyList[ElemType]*)(using Monitor): Multiplexer[Any, ElemType] =
    val multiplexer = Multiplexer[Any, ElemType]()
    streams.zipWithIndex.map(_.swap).foreach(multiplexer.add)
    multiplexer
  
  def pulsar[DurationType: GenericDuration](duration: DurationType)(using Monitor): LazyList[Unit] =
    val startTime: Long = System.currentTimeMillis
    
    def recur(iteration: Int): LazyList[Unit] =
      try
        sleepUntil(startTime + duration.milliseconds*iteration)
        () #:: pulsar(duration)
      catch case err: CancelError => LazyList()
    
    recur(0)

class Pulsar[DurationType: GenericDuration](duration: DurationType):
  private var continue: Boolean = true
  def stop(): Unit = continue = false

  def stream(using Monitor): LazyList[Unit] =
    if !continue then LazyList() else try
      sleep(duration)
      () #:: stream
    catch case err: CancelError => LazyList()

class StreamBuffer[ElemType]():
  private val primary: juc.LinkedBlockingQueue[Option[ElemType]] = juc.LinkedBlockingQueue()
  private val secondary: juc.LinkedBlockingQueue[Option[ElemType]] = juc.LinkedBlockingQueue()
  private var buffer: Boolean = true
  private var closed: Boolean = false

  def useSecondary() = primary.put(None)
  def usePrimary() = secondary.put(None)

  def close(): Unit =
    closed = true
    primary.put(None)
    secondary.put(None)

  def put(value: ElemType): Unit = primary.put(Some(value))
  def putSecondary(value: ElemType): Unit = secondary.put(Some(value))
  
  def stream: LazyList[ElemType] =
    def recur(): LazyList[ElemType] =
      if buffer then primary.take().nn match
        case None =>
          buffer = false
          if closed then LazyList() else recur()
        case Some(value) =>
          value #:: recur()
      else secondary.take().nn match
        case None =>
          buffer = true
          if closed then LazyList() else recur()
        case Some(value) =>
          value #:: recur()
    
    recur()

object Tap:
  enum Regulation:
    case Start, Stop

class Tap(initial: Boolean = true):
  private val flowing: juca.AtomicBoolean = juca.AtomicBoolean(initial)
  private val funnel: Funnel[Tap.Regulation] = Funnel()
  
  def open(): Unit = if !flowing.getAndSet(true) then funnel.put(Tap.Regulation.Start)
  def pause(): Unit = if flowing.getAndSet(false) then funnel.put(Tap.Regulation.Stop)
  def stop(): Unit = funnel.stop()
  def state(): Boolean = flowing.get
  def stream: LazyList[Tap.Regulation] = funnel.stream

object Writable:
  given outputStreamBytes
      (using streamCut: Raises[StreamCutError])
      : SimpleWritable[ji.OutputStream, Bytes] =
    (outputStream, bytes) => outputStream.write(bytes.mutable(using Unsafe))
  
  given outputStreamText
      (using streamCut: Raises[StreamCutError], encoder: CharEncoder)
      : SimpleWritable[ji.OutputStream, Text] =
    (outputStream, text) => outputStream.write(encoder.encode(text).mutable(using Unsafe))

trait SimpleWritable[-TargetType, -ChunkType] extends Writable[TargetType, ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit = stream match
    case head #:: tail => writeChunk(target, head)
                          write(target, tail)
    case _             => ()

  def writeChunk(target: TargetType, chunk: ChunkType): Unit

@missingContext(contextMessage(module = "turbulence", typeclass = "Writable", param = "${TargetType}")())

@capability
trait Writable[-TargetType, -ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit

  def contraMap[TargetType2](fn: TargetType2 => TargetType): Writable[TargetType2, ChunkType] =
    (target, stream) => write(fn(target), stream)

object Appendable:
  given stdoutBytes(using io: Stdio): SimpleAppendable[Stdout.type, Bytes] =
    (stderr, bytes) => io.putOutBytes(bytes)
  
  given stdoutText(using io: Stdio, enc: CharEncoder): SimpleAppendable[Stdout.type, Text] =
    (stderr, text) => io.putOutText(text)

  given stderrBytes(using io: Stdio): SimpleAppendable[Stderr.type, Bytes] =
    (stderr, bytes) => io.putErrBytes(bytes)
  
  given stderrText(using io: Stdio, enc: CharEncoder): SimpleAppendable[Stderr.type, Text] =
    (stderr, text) => io.putErrText(text)

  given outputStreamBytes(using streamCut: Raises[StreamCutError])
                         : SimpleAppendable[ji.OutputStream, Bytes] =
    (outputStream, bytes) => outputStream.write(bytes.mutable(using Unsafe))
  
  given outputStreamText(using streamCut: Raises[StreamCutError], encoder: CharEncoder)
                        : SimpleWritable[ji.OutputStream, Text] =
    (outputStream, text) => outputStream.write(encoder.encode(text).mutable(using Unsafe))

trait Appendable[-TargetType, -ChunkType]:
  def append(target: TargetType, stream: LazyList[ChunkType]): Unit
  
  def contraMap[TargetType2](fn: TargetType2 => TargetType): Appendable[TargetType2, ChunkType] =
    (target, stream) => append(fn(target), stream)
  
  def asWritable: Writable[TargetType, ChunkType] = append(_, _)

trait SimpleAppendable[-TargetType, -ChunkType] extends Appendable[TargetType, ChunkType]:
  def append(target: TargetType, stream: LazyList[ChunkType]): Unit = stream match
    case head #:: tail => appendChunk(target, head)
                          append(target, tail)
    case _             => ()

  def appendChunk(target: TargetType, chunk: ChunkType): Unit

object Readable:
  given bytes: Readable[Bytes, Bytes] = LazyList(_)
  given text: Readable[Text, Text] = LazyList(_)
  
  given textToBytes(using encoder: CharEncoder): Readable[Text, Bytes] =
    text => LazyList(encoder.encode(text))

  given bytesToText[SourceType]
                   (using readable: Readable[SourceType, Bytes], decoder: CharDecoder,
                        handler: BadEncodingHandler)
                   : Readable[SourceType, Text] =
    value => decoder.decode(readable.read(value))
  
  given lazyList[ChunkType]: Readable[LazyList[ChunkType], ChunkType] = identity(_)

  given bufferedReader(using streamCut: Raises[StreamCutError])
                      : Readable[ji.BufferedReader, Line] =
    reader =>
      def recur(count: ByteSize): LazyList[Line] =
        try reader.readLine match
          case null         => LazyList()
          case line: String => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
        catch case err: ji.IOException => raise(StreamCutError(count))(LazyList())
        finally reader.close()
      
      recur(0L.b)

  given inputStream(using streamCut: Raises[StreamCutError]): (Readable[ji.InputStream, Bytes]) =
    in =>
      val channel: jn.channels.ReadableByteChannel = jn.channels.Channels.newChannel(in).nn
      val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](65536)).nn
  
      def recur(total: Long): LazyList[Bytes] =
        try
          channel.read(buf) match
            case -1 => LazyList().tap(_ => try channel.close() catch case err: Exception => ())
            case 0  => recur(total)
            
            case count =>
              buf.flip()
              val size: Int = count.min(65536)
              val array: Array[Byte] = new Array[Byte](size)
              buf.get(array)
              buf.clear()

              LazyList.cons(array.immutable(using Unsafe), recur(total + count))
            
        catch case e: Exception => LazyList(raise(StreamCutError(total.b))(Bytes()))
        
      recur(0)

@capability
trait Readable[-SourceType, +ChunkType]:
  def read(value: SourceType): LazyList[ChunkType]
  
  def contraMap[SourceType2](fn: SourceType2 => SourceType): Readable[SourceType2, ChunkType] =
    source => read(fn(source))

object Aggregable:
  given bytesBytes: Aggregable[Bytes, Bytes] = source =>
    def recur(buf: ji.ByteArrayOutputStream, source: LazyList[Bytes]): Bytes = source match
      case head #:: tail => buf.write(head.mutable(using Unsafe))
                            recur(buf, tail)
      case _             => buf.toByteArray().nn.immutable(using Unsafe)
    
    recur(ji.ByteArrayOutputStream(), source)
  
  given bytesText(using decoder: CharDecoder): Aggregable[Bytes, Text] = bytesBytes.map(decoder.decode)

  given lazyList
      [ChunkType, ChunkType2]
      (using aggregable: Aggregable[ChunkType, ChunkType2])
      : Aggregable[ChunkType, LazyList[ChunkType2]] = chunk => LazyList(aggregable.aggregate(chunk))

  given functor[ChunkType]: Functor[[ValueType] =>> Aggregable[ChunkType, ValueType]] = new Functor:
    def map
        [ResultType, ResultType2]
        (aggregable: Aggregable[ChunkType, ResultType], fn: ResultType => ResultType2)
        : Aggregable[ChunkType, ResultType2] =
      new Aggregable:
        def aggregate(value: LazyList[ChunkType]): ResultType2 = fn(aggregable.aggregate(value))
      
@capability
trait Aggregable[-ChunkType, +ResultType]:
  def aggregate(source: LazyList[ChunkType]): ResultType

extension [ValueType](value: ValueType)
  def stream[ChunkType](using readable: Readable[ValueType, ChunkType]): LazyList[ChunkType] =
    readable.read(value)
  
  def readAs
      [ResultType]
      (using readable: Readable[ValueType, Bytes], aggregable: Aggregable[Bytes, ResultType])
      : ResultType =
    aggregable.aggregate(readable.read(value))
  
  def writeTo
      [TargetType]
      (target: TargetType)
      [ChunkType]
      (using readable: Readable[ValueType, ChunkType], writable: Writable[TargetType, ChunkType])
      : Unit =
    writable.write(target, readable.read(value))
  
  def appendTo
      [TargetType]
      (target: TargetType)
      [ChunkType]
      (using readable: Readable[ValueType, ChunkType], appendable: Appendable[TargetType, ChunkType])
      : Unit =
    appendable.append(target, readable.read(value))

case class Line(content: Text)

case class StreamCutError(total: ByteSize) extends Error(msg"the stream was cut prematurely after $total bytes")
