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
import anticipation.*
import tetromino.*
import parasitism.*

import scala.collection.IterableFactory
import scala.collection.mutable.HashMap
import scala.compiletime.*, ops.int.*

import java.util.regex.*
import java.util as ju
import java.io as ji
import java.nio as jn, java.nio.channels as jnc
import java.lang.ref as jlr

import scala.collection.mutable as scm

import language.dynamics

import scala.util.{Try, Success, Failure}

import java.util.concurrent as juc

type DataStream = LazyList[IArray[Byte] throws StreamCutError]

extension (value: DataStream)
  def slurp(rubrics: Rubric*): Bytes throws StreamCutError =
    value.foldLeft(IArray[Byte]()): (acc, next) =>
      // FIXME: Use Rubric
      acc ++ next

case class StreamCutError() extends Error(err"the stream was cut prematurely")

object Util:
  def readInputStream(in: ji.InputStream, rubrics: Rubric*)(using allocator: Allocator): DataStream =
    val channel = jnc.Channels.newChannel(in).nn
    try
      val buf = jn.ByteBuffer.wrap(allocator.allocate(64.kb, rubrics*)).nn

      def recur(): DataStream =
        channel.read(buf) match
          case -1 =>
            channel.close()
            LazyList()
          case 0 =>
            recur()
          case count =>
            try
              buf.flip()
              val size = count min 65536
              val array = allocator.allocate(size.b, rubrics*)
              buf.get(array)
              buf.clear()
              array.immutable(using Unsafe) #:: recur()
            catch case e: ExcessDataError =>
              LazyList(throw StreamCutError()): DataStream
      
      recur()
    catch case err: ExcessDataError =>
      LazyList(throw StreamCutError()): DataStream

  def write(stream: DataStream, out: ji.OutputStream): Unit throws StreamCutError =
    stream.map(_.mutable(using Unsafe)).foreach(out.write(_))

// object Source:
//   given (using Allocator): Source[SystemIn.type] with
//     def read(value: SystemIn.type): DataStream throws StreamCutError =
//       if System.in == null then throw StreamCutError() else Util.readInputStream(System.in)

trait Source[T]:
  type E <: Exception
  def read(value: T, rubrics: Rubric*): DataStream throws E

object Appendable:
  given Appendable[SystemOut.type] with
    def write(value: SystemOut.type, stream: DataStream) =
      if System.out == null then throw StreamCutError() else Util.write(stream, System.out)
  
  given Appendable[SystemErr.type] with
    def write(value: SystemErr.type, stream: DataStream) =
      if System.err == null then throw StreamCutError() else Util.write(stream, System.err)
  
  given Appendable[ji.OutputStream] with
    def write(writable: ji.OutputStream, stream: DataStream) =
      val out = writable match
        case out: ji.BufferedOutputStream => out
        case out: ji.OutputStream         => ji.BufferedOutputStream(out)
      
      Util.write(stream, out)

trait Appendable[T]:
  type E <: Exception
  def write(value: T, stream: DataStream): Unit throws StreamCutError | E

trait Writable[T]:
  type E <: Exception
  def write(value: T, stream: DataStream): Unit throws StreamCutError | E

object SafeStreamable:
  given SafeStreamable[LazyList[Bytes]] = identity(_)

object Streamable:
  given Streamable[DataStream] = identity(_)
  given Streamable[Bytes] = LazyList(_)

  given Streamable[Text] = value => LazyList(value.s.getBytes("UTF-8").nn.immutable(using Unsafe))
  given Streamable[LazyList[Text]] = _.map(_.s.getBytes("UTF-8").nn.immutable(using Unsafe))

trait Streamable[T]:
  def stream(value: T): DataStream

trait SafeStreamable[T] extends Streamable[T]:
  def safeStream(value: T): LazyList[Bytes]
  def stream(value: T): DataStream = safeStream(value).map(identity(_))

object Readable:
  given Readable[DataStream] with
    type E = StreamCutError
    def read(stream: DataStream, rubrics: Rubric*): DataStream throws StreamCutError | E = stream
  
  given Readable[Bytes] with
    type E = StreamCutError
    def read(stream: DataStream, rubrics: Rubric*): Bytes throws StreamCutError | E =
      stream.slurp(rubrics*)

  given (using enc: Encoding): Readable[Text] with
    type E = StreamCutError
    def read(value: DataStream, rubrics: Rubric*) =
      Text(String(value.slurp(rubrics*).mutable(using Unsafe), enc.name.s))

  given textReader(using enc: Encoding): Readable[LazyList[Text]] with
    type E = StreamCutError
    def read(stream: DataStream, rubrics: Rubric*) =
      
      def read(stream: DataStream, carried: Array[Byte] = Array.empty[Byte]): LazyList[Text] =
        if stream.isEmpty then LazyList()
        else
          // FIXME: constructing this new array may be unnecessarily costly.
          val buf = carried ++ stream.head.mutable(using Unsafe)
          val carry = enc.carry(buf)
          
          Text(String(buf, 0, buf.length - carry, enc.name.s)) #::
              read(stream.tail, buf.takeRight(carry))
      
      read(stream)

trait Readable[T]:
  readable =>
  type E <: Exception
  def read(stream: DataStream, rubrics: Rubric*): T throws StreamCutError | E
  
  def map[S](fn: T => S): turbulence.Readable[S] { type E = readable.E } =
    new turbulence.Readable[S]:
      type E = readable.E
      def read(stream: DataStream, rubrics: Rubric*): S throws StreamCutError | readable.E =
        fn(readable.read(stream))

object SystemIn
object SystemOut
object SystemErr

extension (obj: LazyList.type)
  def multiplex[T](streams: LazyList[T]*)(using Monitor, Threading): LazyList[T] =
    multiplexer(streams*).stream
  
  def multiplexer[T](streams: LazyList[T]*)(using Monitor, Threading): Multiplexer[Any, T] =
    val multiplexer = Multiplexer[Any, T]()
    streams.zipWithIndex.map(_.swap).foreach(multiplexer.add)
    multiplexer
  
  def pulsar(using time: Timekeeper)(interval: time.Type)(using Monitor): LazyList[Unit] =
    try
      sleep(interval)
      () #:: pulsar(interval)
    catch case err: CancelError => LazyList()

class Pulsar(using time: Timekeeper)(interval: time.Type):
  private var continue: Boolean = true
  def stop(): Unit = continue = false

  def stream(using Monitor): LazyList[Unit] = if !continue then LazyList() else
    try
      sleep(interval)
      () #:: stream
    catch case err: CancelError => LazyList()

extension [T](value: T)
  def dataStream(using src: Source[T]): DataStream throws src.E = src.read(value)
  
  def writeStream(stream: DataStream)(using writable: Writable[T]): Unit throws StreamCutError | writable.E =
    writable.write(value, stream)
  
  def appendTo[S](destination: S)(using appendable: Appendable[S], streamable: Streamable[T])
                : Unit throws StreamCutError | appendable.E =
    appendable.write(destination, streamable.stream(value))

  def writeTo[S](destination: S)(using writable: Writable[S], streamable: Streamable[T])
                : Unit throws StreamCutError | writable.E =
    writable.write(destination, streamable.stream(value))

  def read[S](rubrics: Rubric*)(using readable: Readable[S], src: Source[T])
          : S throws StreamCutError | src.E | readable.E =
    readable.read(dataStream, rubrics*)

case class Multiplexer[K, T]()(using monitor: Monitor, threading: Threading):
  private val tasks: HashMap[K, Task[Unit]] = HashMap()
  private val queue: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()

  def close(): Unit = tasks.keys.foreach(remove(_))

  @tailrec
  private def pump(key: K, stream: LazyList[T]): Unit =
    if stream.isEmpty then remove(key) else
      //ctx.terminate(())
      monitor.bail()
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
    
    LazyList() #::: recur()


extension [T](stream: LazyList[T])

  def rate(using time: Timekeeper)(interval: time.Type)(using Monitor, Threading)
          : LazyList[T] throws CancelError =
    def recur(stream: LazyList[T], last: Long): LazyList[T] = stream match
      case LazyList() =>
        LazyList()
      case head #:: tail =>
        val delay = time.from(time.to(interval) - (System.currentTimeMillis - last))
        if time.to(delay) > 0 then sleep(delay)
        stream

    Task(Text("ratelimiter"))(recur(stream, System.currentTimeMillis)).await()

  def multiplexWith(that: LazyList[T])(using Monitor, Threading): LazyList[T] = LazyList.multiplex(stream, that)

  def regulate(tap: Tap)(using Monitor, Threading): LazyList[T] =
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

  def cluster(using time: Timekeeper)
             (interval: time.Type, maxSize: Maybe[Int] = Unset, maxDelay: Maybe[Long] = Unset)
             (using Monitor, Threading): LazyList[List[T]] =
    
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
          val deadline = time.from(time.to(interval).min(expiry - System.currentTimeMillis).max(0))
          if hasMore.await(deadline) then Some(true) else None
        catch case err: (TimeoutError | CancelError) => Some(false)

        // The try/catch above seems to fool tail-call identification
        if recurse.isEmpty then LazyList(list)
        else if recurse.get then recur(stream.tail, stream.head :: list, expiry)
        else list.reverse #:: defer(stream, Nil, Long.MaxValue)
    
    LazyList() #::: recur(stream, Nil, Long.MaxValue)

object StreamBuffer:
  given Writable[StreamBuffer[Bytes throws StreamCutError]] with
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
      case value: T @unchecked =>
        value #:: stream
      case _ =>
        throw Mistake("Should never match")
    else secondary.take() match
      case Unset =>
        buffer = true
        if closed then LazyList() else stream
      case value: T @unchecked =>
        value #:: stream
      case _ =>
        throw Mistake("Should never match")
    

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
