/*
    Turbulence, version 0.4.0. Copyright 2022-22 Jon Pretty, Propensive OÜ.

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
import clairvoyant.*

import scala.collection.IterableFactory
import scala.collection.mutable.HashMap
import scala.compiletime.*, ops.int.*
import scala.concurrent.*

import java.util.regex.*
import java.util as ju
import java.io as ji

import scala.util.CommandLineParser

import language.dynamics

import scala.util.{Try, Success, Failure}

import java.util.concurrent as juc

type DataStream = LazyList[IArray[Byte] throws StreamCutError]

extension (value: DataStream)
  def slurp(limit: ByteSize): Bytes throws ExcessDataError | StreamCutError =
    value.foldLeft(IArray[Byte]()):
      (acc, next) =>
        if acc.length + next.length > limit.long
        then throw ExcessDataError((acc.length + next.length).b, limit)
        else acc ++ next

case class ExcessDataError(size: ByteSize, limit: ByteSize)
extends Error((Text("the amount of data in the stream (at least "), size,
    Text("B) exceeds the limit ("), limit, Text("B)"))):
  def message: Text =
    Text(s"the amount of data in the stream (at least ${size}B) exceeds the limit (${limit}B)")

case class StreamCutError() extends Error(Text("the stream was cut prematurely") *: EmptyTuple):
  def message: Text = Text("the stream was cut prematurely")

object Util:
  def readInputStream(in: ji.InputStream, limit: ByteSize): DataStream = in match
    case in: ji.BufferedInputStream =>
      def read(): DataStream =
        try
          val avail = in.available
          
          val buf = new Array[Byte]((if avail == 0 then limit.long.toInt else avail min
              limit.long.toInt))
          
          val count = in.read(buf, 0, buf.length)
          if count < 0 then LazyList()
          else if avail == 0 then buf.slice(0, count).unsafeImmutable #:: read()
          else buf.unsafeImmutable #:: read()
        catch case error: ji.IOException => LazyList(throw StreamCutError())

      read()
    
    case in: ji.InputStream =>
      readInputStream(ji.BufferedInputStream(in), limit)
  
  def write(stream: DataStream, out: ji.OutputStream): Unit throws StreamCutError =
    stream.map(_.unsafeMutable).foreach(out.write(_))

object Source:
  given Source[SystemIn.type] with
    type E = StreamCutError
    def read(value: SystemIn.type): DataStream throws StreamCutError =
      if System.in == null then throw StreamCutError() else Util.readInputStream(System.in, 10.mb)

trait Source[T]:
  type E <: Exception
  def read(value: T): DataStream throws E

object Sink:
  given Sink[SystemOut.type] with
    type E = StreamCutError
    def write(value: SystemOut.type, stream: DataStream) =
      if System.out == null then throw StreamCutError() else Util.write(stream, System.out)
  
  given Sink[SystemErr.type] with
    type E = StreamCutError
    def write(value: SystemErr.type, stream: DataStream) =
      if System.err == null then throw StreamCutError() else Util.write(stream, System.err)
  
  given Sink[ji.OutputStream] with
    type E = StreamCutError
    def write(sink: ji.OutputStream, stream: DataStream) =
      val out = sink match
        case out: ji.BufferedOutputStream => out
        case out: ji.OutputStream         => ji.BufferedOutputStream(out)
      
      Util.write(stream, out)

trait Sink[T]:
  type E <: Exception
  def write(value: T, stream: DataStream): Unit throws E | StreamCutError

object SafeStreamable:
  given SafeStreamable[LazyList[Bytes]] = identity(_)

object Streamable:
  given Streamable[DataStream] = identity(_)
  given Streamable[Bytes] = LazyList(_)

  given Streamable[Text] = value => LazyList(value.s.getBytes("UTF-8").nn.unsafeImmutable)

trait Streamable[T]:
  def stream(value: T): DataStream

trait SafeStreamable[T] extends Streamable[T]:
  def safeStream(value: T): LazyList[Bytes]
  def stream(value: T): DataStream = safeStream(value).map(identity(_))

object Readable:
  given Readable[DataStream] with
    type E = Nothing
    def read(stream: DataStream): DataStream throws E | StreamCutError = stream
  
  given Readable[Bytes] with
    type E = ExcessDataError
    def read(stream: DataStream): Bytes throws ExcessDataError | StreamCutError =
      stream.slurp(10.mb)

  given (using enc: Encoding): Readable[Text] with
    type E = ExcessDataError
    def read(value: DataStream) = Text(String(value.slurp(1.mb).unsafeMutable, enc.name.s))

  given textReader(using enc: Encoding): Readable[LazyList[Text]] with
    type E = ExcessDataError
    def read(stream: DataStream) =
      
      def read(stream: DataStream, carried: Array[Byte] = Array.empty[Byte]): LazyList[Text] =
        if stream.isEmpty then LazyList()
        else
          // FIXME: constructing this new array may be unnecessarily costly.
          val buf = carried ++ stream.head.unsafeMutable
          val carry = enc.carry(buf)
          
          Text(String(buf, 0, buf.length - carry, enc.name.s)) #::
              read(stream.tail, buf.takeRight(carry))
      
      read(stream)

trait Readable[T]:
  readable =>
    type E <: Exception
    def read(stream: DataStream): T throws E | StreamCutError
    def map[S](fn: T => S): turbulence.Readable[S] { type E = readable.E } =
      new turbulence.Readable[S]:
        type E = readable.E
        def read(stream: DataStream): S throws E | StreamCutError =
          fn(readable.read(stream))

object SystemIn
object SystemOut
object SystemErr

extension (obj: LazyList.type)
  def multiplex[T](streams: LazyList[T]*): LazyList[T] = multiplexer(streams*).stream
  
  def multiplexer[T](streams: LazyList[T]*): Multiplexer[Any, T] =
    val multiplexer = Multiplexer[Any, T]()
    streams.zipWithIndex.map(_.swap).foreach(multiplexer.add)
    multiplexer
  
  def pulsar(using time: Timekeeper)(interval: time.Type): LazyList[Unit] =
    Thread.sleep(time.to(interval))
    () #:: pulsar(interval)

class Pulsar(using time: Timekeeper)(interval: time.Type):
  private var continue: Boolean = true
  def stop(): Unit = continue = false

  def stream: LazyList[Unit] = if !continue then LazyList() else
    Thread.sleep(time.to(interval))
    () #:: stream

extension [T](value: T)
  def dataStream(using src: Source[T]): DataStream throws src.E = src.read(value)
  
  def writeStream(stream: DataStream)(using sink: Sink[T]): Unit throws sink.E | StreamCutError =
    sink.write(value, stream)
  
  def writeTo[S](destination: S)(using sink: Sink[S], streamable: Streamable[T])
                : Unit throws sink.E | StreamCutError =
    sink.write(destination, streamable.stream(value))

  def read[S](using readable: Readable[S], src: Source[T])
      : S throws readable.E | src.E | StreamCutError =
    readable.read(dataStream)

case class Multiplexer[K, T]():
  private val tasks: HashMap[K, Task[Unit]] = HashMap()
  private val queue: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()

  def close(): Unit = tasks.keys.foreach(remove(_))

  @tailrec
  private def pump(key: K, stream: => LazyList[T]): Unit =
    if stream.isEmpty then remove(key)
    else
      queue.put(stream.head)
      pump(key, stream.tail)

  def add(key: K, stream: LazyList[T]): Unit =
    synchronized:
      val task: Task[Unit] = Task(pump(key, stream))
      task()
      tasks(key) = task
 
  private def remove(key: K): Unit = synchronized:
    tasks -= key
    if tasks.isEmpty then queue.put(Unset)
  
  def stream: LazyList[T] =
    def recur(): LazyList[T] =
      queue.take() match
        case Unset   => LazyList()
        case null    => LazyList()
        case item: T => item.nn #:: recur()
    
    LazyList() #::: recur()

extension [T](stream: LazyList[T])
  def ready(using ExecutionContext): Future[LazyList[T]] = Future:
    blocking:
      stream.isEmpty
      stream

  def multiplexWith(that: LazyList[T]): LazyList[T] = LazyList.multiplex(stream, that)

  def regulate(tap: Tap): LazyList[T] =
    def defer(active: Boolean, stream: LazyList[T | Tap.Regulation], buffer: List[T]): LazyList[T] =
      recur(active, stream, buffer)

    @tailrec
    def recur(active: Boolean, stream: LazyList[T | Tap.Regulation], buffer: List[T]): LazyList[T] =
      if active && buffer.nonEmpty then buffer.head #:: defer(true, stream, buffer.tail)
      else if stream.isEmpty then LazyList()
      else stream.head match
        case Tap.Regulation.Start => recur(true, stream.tail, buffer)
        case Tap.Regulation.Stop  => recur(false, stream.tail, Nil)
        case other: T             => if active then other.nn #:: defer(true, stream.tail, Nil)
                                     else recur(false, stream.tail, other.nn :: buffer)

    LazyList() #::: recur(true, stream.multiplexWith(tap.stream), Nil)

  def cluster(using time: Timekeeper)
             (interval: time.Type, maxSize: Maybe[Int] = Unset, maxDelay: Maybe[Long] = Unset)
             (using ExecutionContext): LazyList[List[T]] =
    
    def defer(stream: LazyList[T], list: List[T], expiry: Long): LazyList[List[T]] =
      recur(stream, list, expiry)

    @tailrec
    def recur(stream: LazyList[T], list: List[T], expiry: Long): LazyList[List[T]] =
      if list.isEmpty then
        val newExpiry: Long = maxDelay.option.fold(Long.MaxValue)(_ + System.currentTimeMillis)
        if stream.isEmpty then LazyList() else recur(stream.tail, List(stream.head), newExpiry)
      else
        val hasMore: Future[Boolean] = Future(!stream.isEmpty)

        val recurse: Option[Boolean] = try
          val deadline = time.to(interval).min(expiry - System.currentTimeMillis).max(0)
          if hasMore.timeout(deadline).await() then Some(true) else None
        catch case err: TimeoutError => Some(false)

        // The try/catch above fools tail-call identification
        if recurse.isEmpty then LazyList(list)
        else if recurse.get then recur(stream.tail, stream.head :: list, expiry)
        else list.reverse #:: defer(stream, Nil, Long.MaxValue)
    
    LazyList() #::: recur(stream, Nil, Long.MaxValue)

object StreamBuffer:
  given Sink[StreamBuffer[Bytes throws StreamCutError]] with
    type E = StreamCutError
    
    def write(buffer: StreamBuffer[Bytes throws StreamCutError], stream: DataStream) =
      stream.foreach(buffer.put(_))
  

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
    if buffer then primary.take() match
      case Unset =>
        buffer = false
        if closed then LazyList() else stream
      case value: T =>
        value #:: stream
    else secondary.take() match
      case Unset =>
        buffer = true
        if closed then LazyList() else stream
      case value: T =>
        value #:: stream
    

class Funnel[T]():
  private val queue: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()
  def put(value: T): Unit = queue.put(value)
  def stop(): Unit = queue.put(Unset)
  def stream: LazyList[T] = LazyList.continually(queue.take()).takeWhile(_ != Unset).sift[T]

class Gun() extends Funnel[Unit]():
  def fire(): Unit = put(())

object Tap:
  enum Regulation:
    case Start, Stop

class Tap(initial: Boolean = true):
  private var on: Boolean = initial
  private val funnel: Funnel[Tap.Regulation] = Funnel()
  def stream: LazyList[Tap.Regulation] = funnel.stream
  
  def pause(): Unit = synchronized:
    if on then
      on = false
      funnel.put(Tap.Regulation.Stop)

  def open(): Unit = synchronized:
    if !on then
      on = true
      funnel.put(Tap.Regulation.Start)
  
  def stop(): Unit = synchronized(funnel.stop())
    
  
