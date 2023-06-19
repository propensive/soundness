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
import digression.*
import anticipation.*
import parasitism.*
import hieroglyph.*

import java.io as ji
import java.nio as jn
import java.util.concurrent.atomic as juca

import scala.collection.mutable as scm

import language.dynamics

import java.util.concurrent as juc

type DataStream = LazyList[IArray[Byte] throws StreamCutError]

extension (value: DataStream)
  def slurp(): Bytes throws StreamCutError =
    val bld: scm.ArrayBuilder[Byte] = scm.ArrayBuilder.ofByte()
    value.foreach { bs => bld.addAll(bs.mutable(using Unsafe)) }
    
    bld.result().immutable(using Unsafe)

case class StreamUnavailableError() extends Error(err"the stream is unavailable")

extension (obj: LazyList.type)
  def multiplex
      [ElemType](streams: LazyList[ElemType]*)(using Monitor): LazyList[ElemType] =
    multiplexer(streams*).stream
  
  def multiplexer
      [ElemType](streams: LazyList[ElemType]*)(using Monitor): Multiplexer[Any, ElemType] =
    val multiplexer = Multiplexer[Any, ElemType]()
    streams.zipWithIndex.map(_.swap).foreach(multiplexer.add)
    multiplexer
  
  def pulsar[DurationType: GenericDuration](interval: DurationType)(using Monitor): LazyList[Unit] =
    try
      sleep(interval)
      () #:: pulsar(interval)
    catch case err: CancelError => LazyList()

class Pulsar[DurationType: GenericDuration](interval: DurationType):
  private var continue: Boolean = true
  def stop(): Unit = continue = false

  def stream(using Monitor): LazyList[Unit] =
    if !continue then LazyList() else try
      sleep(interval)
      () #:: stream
    catch case err: CancelError => LazyList()

case class Multiplexer[KeyType, ElemType]()(using monitor: Monitor):
  private val tasks: TrieMap[KeyType, /*{monitor}*/ Task[Unit]] = TrieMap()
  private val queue: juc.LinkedBlockingQueue[Option[ElemType]] = juc.LinkedBlockingQueue()

  def close(): Unit = tasks.keys.foreach(remove(_))

  @tailrec
  private def pump(key: KeyType, stream: LazyList[ElemType]): Unit =
    if stream.isEmpty then remove(key) else
      //ctx.terminate(())
      relinquish()
      queue.put(Some(stream.head))
      pump(key, stream.tail)

  def add(key: KeyType, stream: LazyList[ElemType]): Unit = tasks(key) = Task(Text("pump"))(pump(key, stream))
 
  private def remove(key: KeyType): Unit = synchronized:
    tasks -= key
    if tasks.isEmpty then queue.put(None)
  
  def stream: LazyList[ElemType] =
    def recur(): LazyList[ElemType] = queue.take() match
      case null | None => LazyList()
      case Some(item)  => item #:: recur()
    
    // FIXME: This should be identical to recur(), but recur is not tail-recursive so
    // it can lead to stack overflow. It may still be a memory leak, though.
    LazyList() #::: recur()


extension [ElemType](stream: LazyList[ElemType])
  def rate
      [DurationType: GenericDuration](interval: DurationType)
      (using monitor: Monitor, cancel: CanThrow[CancelError])
      : /*{monitor, cancel}*/ LazyList[ElemType] =
    def recur(stream: LazyList[ElemType], last: Long): LazyList[ElemType] = stream match
      case head #:: tail =>
        val delay = makeDuration(readDuration(interval) - (System.currentTimeMillis - last))
        if readDuration(delay) > 0 then sleep(delay)
        stream
      case _ =>
        LazyList()

    Task(Text("ratelimiter"))(recur(stream, System.currentTimeMillis)).await()

  def multiplexWith(that: LazyList[ElemType])(using Monitor): LazyList[ElemType] =
    unsafely(LazyList.multiplex(stream, that))

  def regulate(tap: Tap)(using Monitor): LazyList[ElemType] =
    def defer
        (active: Boolean, stream: LazyList[Some[ElemType] | Tap.Regulation],
            buffer: List[ElemType])
        : LazyList[ElemType] =
      recur(active, stream, buffer)

    @tailrec
    def recur
        (active: Boolean, stream: LazyList[Some[ElemType] | Tap.Regulation],
            buffer: List[ElemType])

        : LazyList[ElemType] =
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

    LazyList() #::: recur(true, stream.map(Some(_)).multiplexWith(tap.stream), Nil)

  def cluster
      [DurationType: GenericDuration]
      (interval: DurationType, maxSize: Maybe[Int] = Unset, maxDelay: Maybe[DurationType] = Unset)
      (using Monitor)
      : LazyList[List[ElemType]] =
    
    def defer(stream: LazyList[ElemType], list: List[ElemType], expiry: Long): LazyList[List[ElemType]] =
      recur(stream, list, expiry)

    @tailrec
    def recur(stream: LazyList[ElemType], list: List[ElemType], expiry: Long): LazyList[List[ElemType]] =
      if list.isEmpty then
        val newExpiry: Long = maxDelay.option.map(readDuration).fold(Long.MaxValue)(_ + System.currentTimeMillis)
        if stream.isEmpty then LazyList() else recur(stream.tail, List(stream.head), newExpiry)
      else
        val hasMore: Task[Boolean] = Task(Text("cluster"))(!stream.isEmpty)

        val recurse: Option[Boolean] = try
          val deadline: Long = readDuration(interval).min(expiry - System.currentTimeMillis).max(0)
          if hasMore.await(deadline)(using timeApi.long) then Some(true) else None
        catch case err: (TimeoutError | CancelError) => Some(false)

        // The try/catch above seems to fool tail-call identification
        if recurse.isEmpty then LazyList(list)
        else if recurse.get then recur(stream.tail, stream.head :: list, expiry)
        else list.reverse #:: defer(stream, Nil, Long.MaxValue)
    
    LazyList() #::: recur(stream, Nil, Long.MaxValue)

  def parallelMap
      [ElemType2](fn: ElemType => ElemType2)(using monitor: Monitor): /*{monitor, fn}*/ LazyList[ElemType2] =
    val out: Funnel[ElemType2] = Funnel()
    Task(Text("parallelMap")):
      stream.map: elem =>
        Task(Text("elem")):
          out.put(fn(elem))
    
    out.stream

// object StreamBuffer:
//   given Writable[StreamBuffer[Bytes throws StreamCutError]] with
//     def write(buffer: StreamBuffer[Bytes throws StreamCutError], stream: DataStream) =
//       stream.foreach(buffer.put(_))

object Io:
  def put(bytes: Bytes)(using io: Stdio): Unit =
    io.putOutBytes(bytes)

  def print[TextType](text: TextType)(using io: Stdio)(using printable: Printable[TextType]): Unit =
    io.putOutText(printable.print(text))
  
  def printErr[TextType](text: TextType)(using io: Stdio)(using printable: Printable[TextType]): Unit =
    io.putErrText(printable.print(text))
  
  def println
      [TextType](text: TextType)(using io: Stdio, printable: Printable[TextType])
      : /*{io, printable, lines}*/ Unit =
    io.putOutText(printable.print(text))
    io.putOutText(Text("\n"))

  def println()(using io: Stdio): Unit = io.putOutText(Text("\n"))
  def printlnErr()(using io: Stdio): Unit = io.putErrText(Text("\n"))
  
  def printlnErr
      [TextType](text: TextType)(using io: Stdio, printable: Printable[TextType])
      : /*{io, printable, lines}*/ Unit =
    io.putErrText(printable.print(text))
    io.putErrText(Text("\n"))
  
@capability
trait Stdio:
  def putErrBytes(bytes: Bytes): Unit
  def putErrText(text: Text): Unit
  def putOutBytes(bytes: Bytes): Unit
  def putOutText(text: Text): Unit

object Stderr
object Stdout

package basicIo:
  given jvm(using streamCut: CanThrow[StreamCutError]): Stdio = new Stdio:
    val encoder = CharEncoder.system
    def putOutText(text: Text): Unit = putOutBytes(encoder.encode(text))
    def putErrText(text: Text): Unit = putErrBytes(encoder.encode(text))
    
    def putOutBytes(bytes: Bytes): Unit =
      if System.out == null then throw StreamCutError(0.b)
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))
    
    def putErrBytes(bytes: Bytes): Unit =
      if System.out == null then throw StreamCutError(0.b)
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))
    
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
      if buffer then primary.take() match
        case null | None =>
          buffer = false
          if closed then LazyList() else recur()
        case Some(value) =>
          value #:: recur()
      else secondary.take() match
        case null | None =>
          buffer = true
          if closed then LazyList() else recur()
        case Some(value) =>
          value #:: recur()
    
    recur()


class Funnel[ItemType]():
  private val queue: juc.LinkedBlockingQueue[Option[ItemType]] = juc.LinkedBlockingQueue()
  
  def put(item: ItemType): Unit = queue.put(Some(item))
  def stop(): Unit = queue.put(None)
  
  def stream: LazyList[ItemType] =
    LazyList.continually(queue.take()).takeWhile(_ != None).collect:
      case Some(item) => item

class Gun() extends Funnel[Unit]():
  def fire(): Unit = put(())

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

case class IoFailure() extends Exception("I/O Failure")

object Writable:
  given outputStreamBytes
      (using streamCut: CanThrow[StreamCutError])
      : (/*{streamCut}*/ SimpleWritable[ji.OutputStream, Bytes]) =
    (outputStream, bytes) => outputStream.write(bytes.mutable(using Unsafe))
  
  given outputStreamText
    (using streamCut: CanThrow[StreamCutError], encoder: /*{*}*/ CharEncoder)
    : (/*{streamCut, encoder}*/ SimpleWritable[ji.OutputStream, Text]) =
    (outputStream, text) => outputStream.write(encoder.encode(text).mutable(using Unsafe))

trait SimpleWritable[-TargetType, -ChunkType] extends Writable[TargetType, ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit = stream match
    case head #:: tail => writeChunk(target, head)
                          write(target, tail)
    case _             => ()

  def writeChunk(target: TargetType, chunk: ChunkType): Unit

@missingContext(contextMessage(module = "turbulence", typeclass = "Writable", param = "${TargetType}")())
// @missingContext(multiline"""
// An contextual turbulence.Writable instance is required to write to a $${TargetType} instance.
  
// If writing Text (rather than Bytes), it may be necessary to have a character encoding in scope, for example
// with,
  
//     import characterEncodings.utf8, or,
//     import characterEncodings.ascii,
  
// plus an appropriate way of handling bad encodings, such as:

//     import badEncodingHandlers.strict, or,
//     import badEncodingHandlers.skip
// """)
@capability
trait Writable[-TargetType, -ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit

  def contraMap[TargetType2](fn: TargetType2 => TargetType): /*{fn}*/ Writable[TargetType2, ChunkType] =
    (target, stream) => write(fn(target), stream)

object Appendable:
  given stdoutBytes(using io: Stdio): (/*{io}*/ SimpleAppendable[Stdout.type, Bytes]) =
    (stderr, bytes) => io.putOutBytes(bytes)
  
  given stdoutText(using io: Stdio, enc: CharEncoder): (/*{io}*/ SimpleAppendable[Stdout.type, Text]) =
    (stderr, text) => io.putOutText(text)

  given stderrBytes(using io: Stdio): (/*{io}*/ SimpleAppendable[Stderr.type, Bytes]) =
    (stderr, bytes) => io.putErrBytes(bytes)
  
  given stderrText(using io: Stdio, enc: CharEncoder): (/*{io}*/ SimpleAppendable[Stderr.type, Text]) =
    (stderr, text) => io.putErrText(text)

  given outputStreamBytes(using streamCut: CanThrow[StreamCutError])
                         : (/*{streamCut}*/ SimpleAppendable[ji.OutputStream, Bytes]) =
    (outputStream, bytes) => outputStream.write(bytes.mutable(using Unsafe))
  
  given outputStreamText(using streamCut: CanThrow[StreamCutError], encoder: /*{*}*/ CharEncoder)
                        : (/*{streamCut, encoder}*/ SimpleWritable[ji.OutputStream, Text]) =
    (outputStream, text) => outputStream.write(encoder.encode(text).mutable(using Unsafe))

trait Appendable[-TargetType, -ChunkType]:
  def append(target: TargetType, stream: LazyList[ChunkType]): Unit
  
  def contraMap[TargetType2](fn: TargetType2 => TargetType): /*{fn}*/ Appendable[TargetType2, ChunkType] =
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
  given text: (/*{}*/ Readable[Text, Text]) = LazyList(_)
  
  given textToBytes(using encoder: /*{*}*/ CharEncoder): (/*{enc}*/ Readable[Text, Bytes]) =
    text => LazyList(encoder.encode(text))

  given bytesToText[SourceType]
                   (using readable: /*{*}*/ Readable[SourceType, Bytes], decoder: /*{*}*/ CharDecoder,
                        handler: /*{*}*/ BadEncodingHandler)
                   : (/*{readable, decoder, handler}*/ Readable[SourceType, Text]) =
    value => decoder.decode(readable.read(value))
  
  given lazyList[ChunkType]: Readable[LazyList[ChunkType], ChunkType] = identity(_)

  given bufferedReader(using streamCut: CanThrow[StreamCutError])
                      : (/*{streamCut}*/ Readable[ji.BufferedReader, Line]) =
    reader =>
      def recur(count: ByteSize): LazyList[Line] =
        try reader.readLine match
          case null         => LazyList()
          case line: String => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
        catch case err: ji.IOException => throw StreamCutError(count)
        finally reader.close()
      
      recur(0L.b)


  given inputStream(using streamCut: CanThrow[StreamCutError]): (/*{streamCut}*/ Readable[ji.InputStream, Bytes]) =
    in =>
      val channel: jn.channels.ReadableByteChannel = jn.channels.Channels.newChannel(in).nn
      val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](65536)).nn
  
      def recur(total: Long): /*{streamCut}*/ LazyList[Bytes] =
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
            
        catch case e: Exception => LazyList(throw StreamCutError(total.b))
        
      recur(0)

@capability
trait Readable[-SourceType, +ChunkType]:
  def read(value: SourceType): LazyList[ChunkType]
  
  def contraMap[SourceType2](fn: SourceType2 => SourceType): /*{fn}*/ Readable[SourceType2, ChunkType] =
    source => read(fn(source))

object Aggregable:
  given bytesBytes: Aggregable[Bytes, Bytes] = source =>
    def recur(buf: ji.ByteArrayOutputStream, source: LazyList[Bytes]): Bytes = source match
      case head #:: tail => buf.write(head.mutable(using Unsafe))
                            recur(buf, tail)
      case _             => buf.toByteArray().nn.immutable(using Unsafe)
    
    recur(ji.ByteArrayOutputStream(), source)
  
  given bytesText(using decoder: CharDecoder): Aggregable[Bytes, Text] = bytesBytes.map(decoder.decode)

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
  def stream[ChunkType](using readable: /*{*}*/ Readable[ValueType, ChunkType]): /*{readable}*/ LazyList[ChunkType] =
    readable.read(value)
  
  def read[ResultType]
          (using readable: /*{*}*/ Readable[ValueType, Bytes], aggregable: /*{*}*/ Aggregable[Bytes, ResultType])
            : /*{readable, aggregable}*/ ResultType =
    aggregable.aggregate(readable.read(value))
  
  def writeTo[TargetType, ChunkType](target: TargetType)
             (using readable: /*{*}*/ Readable[ValueType, ChunkType], writable: /*{*}*/ Writable[TargetType, ChunkType])
             : /*{readable, writable}*/ Unit =
    writable.write(target, readable.read(value))
  
  def appendTo[TargetType, ChunkType](target: TargetType)
              (using readable: /*{*}*/ Readable[ValueType, ChunkType],
                    appendable: /*{*}*/ Appendable[TargetType, ChunkType])
              : /*{readable, appendable}*/ Unit =
    appendable.append(target, readable.read(value))

case class Line(content: Text)

