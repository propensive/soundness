/*
    Turbulence, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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
import deviation.*
import anticipation.*
import parasitism.*

import scala.collection.IterableFactory
import scala.collection.mutable.HashMap
import scala.compiletime.*, ops.int.*

import java.util.regex.*
import java.util as ju
import java.io as ji
import java.nio as jn
import java.lang.ref as jlr

import scala.collection.mutable as scm

import language.dynamics
import language.experimental.captureChecking

import scala.util.{Try, Success, Failure}

import java.util.concurrent as juc

type DataStream = LazyList[IArray[Byte] throws StreamCutError]

extension (value: DataStream)
  def slurp(): Bytes throws StreamCutError =
    val bld: scm.ArrayBuilder[Byte] = scm.ArrayBuilder.ofByte()
    value.foreach { bs => bld.addAll(bs.mutable(using Unsafe)) }
    
    bld.result().immutable(using Unsafe)

case class StreamCutError(total: ByteSize) extends Error(err"the stream was cut prematurely after $total")
case class StreamUnavailableError() extends Error(err"the stream is unavailable")

extension (obj: LazyList.type)
  def multiplex[T](streams: LazyList[T]*)(using Monitor): LazyList[T] =
    multiplexer(streams*).stream
  
  def multiplexer[T](streams: LazyList[T]*)(using Monitor): Multiplexer[Any, T] =
    val multiplexer = Multiplexer[Any, T]()
    streams.zipWithIndex.map(_.swap).foreach(multiplexer.add)
    multiplexer
  
  def pulsar(using time: GenericDuration)(interval: time.Duration)(using Monitor): LazyList[Unit] =
    try
      sleep(interval)
      () #:: pulsar(interval)
    catch case err: CancelError => LazyList()

class Pulsar(using time: GenericDuration)(interval: time.Duration):
  private var continue: Boolean = true
  def stop(): Unit = continue = false

  def stream(using Monitor): LazyList[Unit] =
    if !continue then LazyList() else try
      sleep(interval)
      () #:: stream
    catch case err: CancelError => LazyList()

case class Multiplexer[K, T]()(using monitor: Monitor):
  private val tasks: HashMap[K, Task[Unit]] = HashMap()
  private val queue: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()

  def close(): Unit = tasks.keys.foreach(remove(_))

  @tailrec
  private def pump(key: K, stream: LazyList[T]): Unit =
    if stream.isEmpty then remove(key) else
      //ctx.terminate(())
      monitor.accede()
      queue.put(stream.head)
      pump(key, stream.tail)

  def add(key: K, stream: LazyList[T]): Unit = synchronized:
    tasks(key) = Task(Text("pump"))(pump(key, stream))
 
  private def remove(key: K): Unit = synchronized:
    tasks -= key
    if tasks.isEmpty then queue.put(Unset)
  
  def stream: LazyList[T] =
    def recur(): LazyList[T] = queue.take() match
      case null | Unset       => LazyList()
      case item: T @unchecked => item.nn #:: recur()
    
    // FIXME: This should be identical to recur(), but recur is not tail-recursive so
    // it can lead to stack overflow. It may still be a memory leak, though.
    LazyList() #::: recur()


extension [T](stream: LazyList[T])
  def rate(using time: GenericDuration)(interval: time.Duration)(using Monitor)
          : LazyList[T] throws CancelError =
    def recur(stream: LazyList[T], last: Long): LazyList[T] = stream match
      case head #:: tail =>
        val delay = makeDuration(readDuration(interval) - (System.currentTimeMillis - last))
        if time.readDuration(delay) > 0 then sleep(delay)
        stream
      case _ =>
        LazyList()

    Task(Text("ratelimiter"))(recur(stream, System.currentTimeMillis)).await()

  def multiplexWith(that: LazyList[T])(using Monitor): LazyList[T] =
    unsafely(LazyList.multiplex(stream, that))

  def regulate(tap: Tap)(using Monitor): LazyList[T] =
    def defer(active: Boolean, stream: LazyList[T | Tap.Regulation], buffer: List[T]): LazyList[T] =
      recur(active, stream, buffer)

    @tailrec
    def recur(active: Boolean, stream: LazyList[T | Tap.Regulation], buffer: List[T]): LazyList[T] =
      if active && buffer.nonEmpty then buffer.head #:: defer(true, stream, buffer.tail)
      else if stream.isEmpty then LazyList()
      else stream.head match
        case Tap.Regulation.Start => recur(true, stream.tail, buffer)
        case Tap.Regulation.Stop  => recur(false, stream.tail, Nil)
        case other: T @unchecked  => if active then other.nn #:: defer(true, stream.tail, Nil)
                                     else recur(false, stream.tail, other.nn :: buffer)

    LazyList() #::: recur(true, stream.multiplexWith(tap.stream), Nil)

  def cluster(using time: GenericDuration)
             (interval: time.Duration, maxSize: Maybe[Int] = Unset, maxDelay: Maybe[Long] = Unset)
             (using Monitor): LazyList[List[T]] =
    
    def defer(stream: LazyList[T], list: List[T], expiry: Long): LazyList[List[T]] =
      recur(stream, list, expiry)

    @tailrec
    def recur(stream: LazyList[T], list: List[T], expiry: Long): LazyList[List[T]] =
      if list.isEmpty then
        val newExpiry: Long = maxDelay.option.fold(Long.MaxValue)(_ + System.currentTimeMillis)
        if stream.isEmpty then LazyList() else recur(stream.tail, List(stream.head), newExpiry)
      else
        val hasMore: Task[Boolean] = Task(Text("cluster"))(!stream.isEmpty)

        val recurse: Option[Boolean] = try
          val deadline: Long = readDuration(interval).min(expiry - System.currentTimeMillis).max(0)
          if hasMore.await(deadline)(using timeRepresentation.long) then Some(true) else None
        catch case err: (TimeoutError | CancelError) => Some(false)

        // The try/catch above seems to fool tail-call identification
        if recurse.isEmpty then LazyList(list)
        else if recurse.get then recur(stream.tail, stream.head :: list, expiry)
        else list.reverse #:: defer(stream, Nil, Long.MaxValue)
    
    LazyList() #::: recur(stream, Nil, Long.MaxValue)

// object StreamBuffer:
//   given Writable[StreamBuffer[Bytes throws StreamCutError]] with
//     def write(buffer: StreamBuffer[Bytes throws StreamCutError], stream: DataStream) =
//       stream.foreach(buffer.put(_))

@capability
trait BasicIo:
  def writeStdoutText(text: Text): Unit
  def writeStderrText(text: Text): Unit
  def writeStdoutBytes(bytes: Bytes): Unit
  def writeStderrBytes(bytes: Bytes): Unit

object Stderr
object Stdout:
  def print(text: Text)(using basicIo: BasicIo): Unit =
    basicIo.writeStdoutText(text)
  
  def println(text: Text)(using basicIo: BasicIo): Unit =
    basicIo.writeStdoutText(text)
    basicIo.writeStdoutText(Text("\n"))

package basicIo:
  given jvm(using streamCut: CanThrow[StreamCutError]): BasicIo = new BasicIo:
    def writeStdoutBytes(bytes: Bytes): Unit =
      if System.out == null then throw StreamCutError(0.b)
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))
    
    def writeStdoutText(text: Text): Unit =
      if System.out == null then throw StreamCutError(0.b) else System.err.nn.print(text.s)
    
    def writeStderrBytes(bytes: Bytes): Unit =
      if System.out == null then throw StreamCutError(0.b)
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))
    
    def writeStderrText(text: Text): Unit =
      if System.out == null then throw StreamCutError(0.b) else System.err.nn.print(text.s)

class StreamBuffer[T]():
  private val primary: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()
  private val secondary: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()
  private var buffer: Boolean = true
  private var closed: Boolean = false

  def useSecondary() = primary.put(Unset)
  def usePrimary() = secondary.put(Unset)

  def close(): Unit =
    closed = true
    primary.put(Unset)
    secondary.put(Unset)

  def put(value: T): Unit = primary.put(value)
  def putSecondary(value: T): Unit = secondary.put(value)
  
  def stream: LazyList[T] =
    def recur(): LazyList[T] =
      if buffer then primary.take() match
        case Unset =>
          buffer = false
          if closed then LazyList() else recur()
        case value: T @unchecked =>
          value #:: recur()
        case _ =>
          throw Mistake("Should never match")
      else secondary.take() match
        case Unset =>
          buffer = true
          if closed then LazyList() else recur()
        case value: T @unchecked =>
          value #:: recur()
        case _ =>
          throw Mistake("Should never match")
    
    recur()


class Funnel[T]():
  private val queue: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()
  def put(value: T): Unit = queue.put(value)
  def stop(): Unit = queue.put(Unset)
  def stream: LazyList[T] =
    LazyList.continually(queue.take()).takeWhile(_ != Unset).collect { case t: T => t }

class Gun() extends Funnel[Unit]():
  def fire(): Unit = put(())

object Tap:
  enum Regulation:
    case Start, Stop

class Tap(initial: Boolean = true):
  private var on: Boolean = initial
  private val funnel: Funnel[Tap.Regulation] = Funnel()
  
  def pause(): Unit = synchronized:
    if on then
      on = false
      funnel.put(Tap.Regulation.Stop)

  def open(): Unit = synchronized:
    if !on then
      on = true
      funnel.put(Tap.Regulation.Start)
  
  def stop(): Unit = synchronized(funnel.stop())
  def state(): Boolean = on
  def stream: LazyList[Tap.Regulation] = funnel.stream

case class IoFailure() extends Exception("I/O Failure")

object Writable:
  given outputStreamBytes(using streamCut: CanThrow[StreamCutError])
        : ({streamCut} SimpleWritable[ji.OutputStream, Bytes]) =
    (outputStream, bytes) => outputStream.write(bytes.mutable(using Unsafe))
  
  given outputStreamText(using streamCut: CanThrow[StreamCutError], enc: {*} Encoding)
        : ({streamCut, enc} SimpleWritable[ji.OutputStream, Text]) =
    (outputStream, text) => outputStream.write(text.s.getBytes(enc.name.s).nn)

trait SimpleWritable[-TargetType, -ChunkType] extends Writable[TargetType, ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit = stream match
    case head #:: tail => writeChunk(target, head)
                          write(target, tail)
    case _             => ()

  def writeChunk(target: TargetType, chunk: ChunkType): Unit

@implicitNotFound("An contextual turbulence.Writable instance is required to write to a ${TargetType} instance"+
                  ".\nIf writing Text (rather than Bytes), it may be necessary to have a character encoding in"+
                  " scope, for example with,\n    import characterEncodings.utf8, or,\n    import characterEnc"+
                  "odings.ascii,\nplus an appropriate way of handling bad encodings, such as:\n    import badE"+
                  "ncodingHandlers.strict, or,\n    import badEncodingHandlers.skip")
trait Writable[-TargetType, -ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit

  def contraMap[TargetType2](fn: TargetType2 => TargetType): {fn} Writable[TargetType2, ChunkType] =
    (target, stream) => write(fn(target), stream)

object Appendable:
  given stdoutBytes(using basicIo: BasicIo): ({basicIo} SimpleAppendable[Stdout.type, Bytes]) =
    (stderr, bytes) => basicIo.writeStdoutBytes(bytes)
  
  given stdoutText(using basicIo: BasicIo): ({basicIo} SimpleAppendable[Stdout.type, Text]) =
    (stderr, text) => basicIo.writeStdoutText(text)

  given stderrBytes(using basicIo: BasicIo): ({basicIo} SimpleAppendable[Stderr.type, Bytes]) =
    (stderr, bytes) => basicIo.writeStderrBytes(bytes)
  
  given stderrText(using basicIo: BasicIo): ({basicIo} SimpleAppendable[Stderr.type, Text]) =
    (stderr, text) => basicIo.writeStderrText(text)

  given outputStreamBytes(using streamCut: CanThrow[StreamCutError])
                         : ({streamCut} SimpleAppendable[ji.OutputStream, Bytes]) =
    (outputStream, bytes) => outputStream.write(bytes.mutable(using Unsafe))
  
  given outputStreamText(using streamCut: CanThrow[StreamCutError], enc: {*} Encoding)
                        : ({streamCut, enc} SimpleWritable[ji.OutputStream, Text]) =
    (outputStream, text) => outputStream.write(text.s.getBytes(enc.name.s).nn)

trait Appendable[-TargetType, -ChunkType]:
  def append(target: TargetType, stream: LazyList[ChunkType]): Unit
  
  def contraMap[TargetType2](fn: TargetType2 => TargetType): {fn} Appendable[TargetType2, ChunkType] =
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
  given text: ({} Readable[Text, Text]) = LazyList(_)
  given textToBytes(using enc: {*} Encoding): ({enc} Readable[Text, Bytes]) =
    text => LazyList(text.s.getBytes(enc.name.s).nn.immutable(using Unsafe))

  given bytesToText[SourceType]
                   (using readable: {*} Readable[SourceType, Bytes], enc: {*} Encoding,
                        handler: {*} BadEncodingHandler)
                   : ({readable, enc, handler} Readable[SourceType, Text]) =
    value => enc.convertStream(readable.read(value))
  
  given lazyList[ChunkType]: Readable[LazyList[ChunkType], ChunkType] = identity(_)

  given bufferedReader(using streamCut: CanThrow[StreamCutError])
                      : ({streamCut} Readable[ji.BufferedReader, Line]) =
    reader =>
      def recur(count: ByteSize): LazyList[Line] =
        try reader.readLine match
          case null         => LazyList()
          case line: String => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
        catch case err: ji.IOException => throw StreamCutError(count)
        finally reader.close()
      
      recur(0L.b)


  given inputStream(using streamCut: CanThrow[StreamCutError]): ({streamCut} Readable[ji.InputStream, Bytes]) =
    in =>
      val channel: jn.channels.ReadableByteChannel = jn.channels.Channels.newChannel(in).nn
      try
        val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](65536)).nn
  
        def recur(total: Long): {streamCut} LazyList[Bytes] =
          channel.read(buf) match
            case -1 => LazyList().tap(_ => try channel.close() catch case err: Exception => ())
            case 0  => recur(total)
            
            case count =>
              try
                buf.flip()
                val size: Int = count.min(65536)
                val array: Array[Byte] = new Array[Byte](size)
                buf.get(array)
                buf.clear()
  
                LazyList.cons(array.immutable(using Unsafe), recur(total + count))
              
              catch case e: Exception => LazyList(throw StreamCutError(total.b))
              finally try channel.close() catch case err: Exception => ()
        
        recur(0)
      catch case err: Exception => LazyList(throw StreamCutError(0.b)): {streamCut} LazyList[Bytes]
      finally try channel.close() catch case err: Exception => ()

trait Readable[-SourceType, +ChunkType]:
  def read(value: SourceType): LazyList[ChunkType]
  
  def contraMap[SourceType2](fn: SourceType2 => SourceType): {fn} Readable[SourceType2, ChunkType] =
    source => read(fn(source))

object Aggregable:
  given bytes: Aggregable[Bytes, Bytes] = source =>
    def recur(buf: ji.ByteArrayOutputStream, source: LazyList[Bytes]): Bytes = source match
      case head #:: tail => buf.write(head.mutable(using Unsafe))
                            recur(buf, tail)
      case _             => buf.toByteArray().nn.immutable(using Unsafe)
    
    recur(ji.ByteArrayOutputStream(), source)

trait Aggregable[-ChunkType, +ResultType]:
  def aggregate(source: LazyList[ChunkType]): ResultType

extension [ValueType](value: ValueType)
  def read[ChunkType](using readable: {*} Readable[ValueType, ChunkType]): {readable} LazyList[ChunkType] =
    readable.read(value)
  
  def readAs[ResultType, ChunkType]
            (using readable: {*} Readable[ValueType, ChunkType],
                  aggregable: {*} Aggregable[ChunkType, ResultType])
            : {readable, aggregable} ResultType =
    aggregable.aggregate(readable.read(value))
  
  def writeTo[TargetType, ChunkType](target: TargetType)
             (using readable: {*} Readable[ValueType, ChunkType], writable: {*} Writable[TargetType, ChunkType])
             : {readable, writable} Unit =
    writable.write(target, readable.read(value))
  
  def appendTo[TargetType, ChunkType](target: TargetType)
              (using readable: {*} Readable[ValueType, ChunkType],
                    appendable: {*} Appendable[TargetType, ChunkType])
              : {readable, appendable} Unit =
    appendable.append(target, readable.read(value))

case class Line(content: Text)
