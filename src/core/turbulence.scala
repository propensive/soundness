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

import scala.collection.IterableFactory
import scala.compiletime.*, ops.int.*
import scala.concurrent.*

import java.util.regex.*
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

case class ExcessDataError(size: ByteSize, limit: ByteSize) extends Error:
  def message: Text =
    Text(s"the amount of data is the stream (at least ${size}B) exceeds the limit (${limit}B)")

case class StreamCutError() extends Error:
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

extension (obj: LazyList.type)
  def multiplex[T](streams: LazyList[T]*): LazyList[T] =
    import scala.concurrent.*
    given ExecutionContext = ExecutionContext.Implicits.global

    def future(stream: LazyList[T], idx: Int): Future[Int] = Future:
      blocking:
        stream.isEmpty
        idx
    
    var vector: Vector[LazyList[T]] = streams.to(Vector)
    var futuresVector: Vector[Future[Int]] = vector.zipWithIndex.map(future(_, _))

    def recur(futures: Set[Future[Int]]): LazyList[T] =
      if futures.isEmpty then LazyList()
      else
        val idx = Future.firstCompletedOf(futures).await()
        val stream = vector(idx)
        if stream.isEmpty then recur(futures - futuresVector(idx))
        else
          vector = vector.updated(idx, stream.tail)
          val oldFuture = futuresVector(idx)
          val newFuture = future(stream.tail, idx)
          futuresVector = futuresVector.updated(idx, newFuture)
          stream.head #:: recur(futures - oldFuture + newFuture)
    
    recur(futuresVector.to(Set))

case class EventStream[T](interval: Int = 10):
  private val queue: juc.ConcurrentLinkedQueue[T] = juc.ConcurrentLinkedQueue[T]()
  def put(value: T): Unit = queue.offer(value)
  
  final def stream(): LazyList[T] =
    if !queue.isEmpty then queue.poll.nn #:: stream() else
      Thread.sleep(interval)
      stream()

case class Multiplexer[K, T]()(using ExecutionContext):
  private enum Change:
    case AddStream(key: K, stream: LazyList[T])
    case RemoveStream(key: K)
  
  private enum Update:
    case Yield(key: K, stream: LazyList[T])
    case Ready(stream: LazyList[Change])
  
  import Change.*
  import Update.*

  private val changes: EventStream[Change] = EventStream()
  def add(key: K, stream: LazyList[T]): Unit = changes.put(AddStream(key, stream))
  def remove(key: K): Unit = changes.put(RemoveStream(key))

  private def future(key: K, stream: LazyList[T]): Future[Update] = Future:
    blocking:
      stream.isEmpty
      Yield(key, stream)

  private def event(stream: LazyList[Change]): Future[Update] = stream.ready.map(Ready(_))

  def stream()(using ExecutionContext): LazyList[T] =
    def recur(futures: Map[Maybe[K], Future[Update]]): LazyList[T] =
      if futures.isEmpty then LazyList()
      else
        Future.firstCompletedOf(futures.values).await() match
          case Yield(key, stream) =>
            if stream.isEmpty then recur(futures - key)
            else stream.head #:: recur(futures.updated(key, future(key, stream.tail)))
          
          case Ready(RemoveStream(key) #:: tail) =>
            recur(futures.updated(Unset, event(tail)) - key)
          
          case Ready(AddStream(key, stream) #:: tail) =>
            recur(futures.updated(key, future(key, stream)).updated(Unset, event(tail)))
    
    recur(Map(Unset -> event(changes.stream())))

extension [T](stream: LazyList[T])
  def ready(using ExecutionContext): Future[LazyList[T]] = Future:
    blocking:
      stream.isEmpty
      stream

  def cluster(interval: Long, maxSize: Maybe[Int] = Unset)
             (using ExecutionContext)
             : LazyList[List[T]] =
    import scala.concurrent.*

    class Timer():
      private var exp: Long = System.currentTimeMillis + interval
      
      val future: Future[None.type] =
        Future:
          blocking:
            @tailrec
            def waitUntil(time: Long): Unit =
              Thread.sleep(time - System.currentTimeMillis)
              if time != exp then waitUntil(exp)
            
            waitUntil(exp)
            None
      
      def postpone(): Unit = exp = System.currentTimeMillis + interval

    def rerecur(stream: LazyList[T], list: List[T], timer: Option[Timer]): LazyList[List[T]] =
      recur(stream, list, timer)

    @tailrec
    def recur(stream: LazyList[T], list: List[T], timer: Option[Timer]): LazyList[List[T]] =
      if list.length == maxSize then list #:: rerecur(stream, Nil, None)
      else timer match
        case None =>
          if stream.isEmpty then LazyList[List[T]]()
          else recur(stream.tail, stream.head :: list, Some(Timer()))
        case Some(t) =>
          val timeout = t.future
          val nextReady = Future(blocking(Some(stream.isEmpty)))
          
          Future.firstCompletedOf(Iterable(timeout, nextReady)).await() match
            case None =>
              list #:: rerecur(stream, Nil, None)
            
            case Some(empty) =>
              if empty then LazyList() else
                t.postpone()
                recur(stream.tail, stream.head :: list, Some(t))

    recur(stream, Nil, None)
