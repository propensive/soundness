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
import anticipation.*
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
  def slurp(): Bytes throws StreamCutError =
    val bld: scm.ArrayBuilder[Byte] = scm.ArrayBuilder.ofByte()
    
    value.foreach: bytes =>
      bld.addAll(bytes.mutable(using Unsafe))
    
    bld.result().immutable(using Unsafe)

case class StreamCutError() extends Error(err"the stream was cut prematurely")
case class AlreadyStreamingError() extends Error(err"the stream was accessed twice, which is not permitted")

object Source:
  given Source[DataStream] with
    def read(value: DataStream) = value

trait Source[T]:
  def read(value: T): DataStream

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
  def write(value: T, stream: DataStream): Unit throws StreamCutError

trait Writable[T]:
  def write(value: T, stream: DataStream): Unit throws StreamCutError

object Streamable:
  given Streamable[DataStream] = identity(_)
  given Streamable[Bytes] = LazyList(_)

  given Streamable[Text] = value => LazyList(value.s.getBytes("UTF-8").nn.immutable(using Unsafe))
  given Streamable[LazyList[Text]] = _.map(_.s.getBytes("UTF-8").nn.immutable(using Unsafe))

trait Streamable[T]:
  def stream(value: T): DataStream

object Readable:
  given Readable[DataStream] with
    def read(stream: DataStream): DataStream throws StreamCutError = stream
  
  given Readable[Bytes] with
    def read(stream: DataStream): Bytes throws StreamCutError =
      stream.slurp()

  given (using enc: Encoding): Readable[Text] with
    def read(value: DataStream) =
      Text(String(value.slurp().mutable(using Unsafe), enc.name.s))

  given textReader(using enc: Encoding): Readable[LazyList[Text]] with
    private final val empty = Array.empty[Byte]

    def read(stream: DataStream) =
      def read(stream: DataStream, carried: Array[Byte] = empty, skip: Int = 0): LazyList[Text] = stream match
        case head #:: tail =>
          val buf = head.mutable(using Unsafe)
          if carried.length > 0 then
            val need = enc.run(carried(0))
            val got = buf.length + carried.length
            if got < need then read(tail, carried ++ buf)
            else if got == need then Text(String(carried ++ buf, enc.name.s)) #:: read(tail, empty)
            else Text(String(carried ++ buf.take(need - carried.length), enc.name.s)) #:: read(stream, empty, need - carried.length)
          else
            val carry = enc.carry(buf)
            Text(String(buf, skip, buf.length - carry - skip, enc.name.s)) #:: read(tail, buf.takeRight(carry))
        
        case _ =>
          LazyList()
        
      
      read(stream)

trait Readable[T]:
  readable =>
  def read(stream: DataStream): T throws StreamCutError
  
  def map[S](fn: T => S): turbulence.Readable[S] =
    new turbulence.Readable[S]:
      def read(stream: DataStream): S throws StreamCutError =
        fn(readable.read(stream))

object SystemIn
object SystemOut
object SystemErr

extension (obj: LazyList.type)
  def multiplex[T](streams: LazyList[T]*)(using Monitor, Threading): LazyList[T] throws AlreadyStreamingError =
    multiplexer(streams*).stream
  
  def multiplexer[T](streams: LazyList[T]*)(using Monitor, Threading): Multiplexer[Any, T] =
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

  def stream(using Monitor): LazyList[Unit] throws AlreadyStreamingError =
    if !continue then LazyList() else try
      sleep(interval)
      () #:: stream
    catch case err: CancelError => LazyList()

extension [T](value: T)
  def dataStream(using src: Source[T]): DataStream = src.read(value)
  
  def writeStream(stream: DataStream)(using writable: Writable[T]): Unit throws StreamCutError =
    writable.write(value, stream)
  
  def appendTo[S](destination: S)(using appendable: Appendable[S], streamable: Streamable[T])
                : Unit throws StreamCutError =
    appendable.write(destination, streamable.stream(value))

  def writeTo[S](destination: S)(using writable: Writable[S], streamable: Streamable[T])
                : Unit throws StreamCutError =
    writable.write(destination, streamable.stream(value))

  def read[S]()(using readable: Readable[S], src: Source[T]): S throws StreamCutError =
    readable.read(dataStream)

case class Multiplexer[K, T]()(using monitor: Monitor, threading: Threading):
  private val tasks: HashMap[K, Task[Unit]] = HashMap()
  private val queue: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()
  private var streaming: Boolean = false

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
  
  def stream: LazyList[T] throws AlreadyStreamingError =
    def recur(): LazyList[T] = queue.take() match
      case null | Unset       => LazyList()
      case item: T @unchecked => item.nn #:: recur()
    
    if streaming then throw AlreadyStreamingError() else LazyList() #::: recur()


extension [T](stream: LazyList[T])

  def rate(using time: GenericDuration)(interval: time.Duration)(using Monitor, Threading)
          : LazyList[T] throws CancelError =
    def recur(stream: LazyList[T], last: Long): LazyList[T] = stream match
      case head #:: tail =>
        val delay = makeDuration(readDuration(interval) - (System.currentTimeMillis - last))
        if time.readDuration(delay) > 0 then sleep(delay)
        stream
      case _ =>
        LazyList()

    Task(Text("ratelimiter"))(recur(stream, System.currentTimeMillis)).await()

  def multiplexWith(that: LazyList[T])(using Monitor, Threading): LazyList[T] =
    unsafely(LazyList.multiplex(stream, that))

  def regulate(tap: Tap)(using Monitor, Threading): LazyList[T] throws AlreadyStreamingError =
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
             (using Monitor, Threading, GenericInstant { type Instant = Long }): LazyList[List[T]] =
    
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
  private var streaming: Boolean = false

  def useSecondary() = primary.put(Unset)
  def usePrimary() = secondary.put(Unset)

  def close(): Unit =
    closed = true
    primary.put(Unset)
    secondary.put(Unset)

  def put(value: T): Unit = primary.put(value)
  def putSecondary(value: T): Unit = secondary.put(value)

  
  def stream: LazyList[T] throws AlreadyStreamingError =
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
    
    if streaming then throw AlreadyStreamingError() else recur()


class Funnel[T]():
  private val queue: juc.LinkedBlockingQueue[Maybe[T]] = juc.LinkedBlockingQueue()
  def put(value: T): Unit = queue.put(value)
  def stop(): Unit = queue.put(Unset)
  def stream: LazyList[T] throws AlreadyStreamingError =
    LazyList.continually(queue.take()).takeWhile(_ != Unset).sift[T]

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
  def stream: LazyList[Tap.Regulation] throws AlreadyStreamingError = funnel.stream
