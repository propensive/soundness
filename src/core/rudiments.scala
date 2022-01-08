/*
    Rudiments, version 0.6.0. Copyright 2020-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import scala.collection.IterableFactory
import scala.compiletime.*, ops.int.*
import scala.concurrent.*

import java.util.regex.*
import java.io as ji

import scala.util.CommandLineParser

import language.dynamics

export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}

export Predef.{nn, genericArrayOps, identity, summon, charWrapper, $conforms, ArrowAssoc,
    intWrapper, longWrapper, shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper,
    classOf, locally}

export scala.concurrent.{Future, ExecutionContext}
export scala.util.control.NonFatal
export scala.jdk.CollectionConverters.*
export scala.annotation.{tailrec, implicitNotFound, targetName, switch, StaticAnnotation}

type Bytes = IArray[Byte]
type DataStream = LazyList[IArray[Byte] throws StreamCutError]

opaque type Text = String

object Text:
  def apply(string: String): Text = string
  
  extension (string: Text)
    def s: String = string

  given CommandLineParser.FromString[Text] = identity(_)

  given Ordering[Text] = Ordering.String.on[Text](_.s)
  
  given typeTest: Typeable[Text] with
    def unapply(value: Any): Option[value.type & Text] = value match
      case str: String => Some(str.asInstanceOf[value.type & Text])
      case _           => None

object Bytes:
  def apply(xs: Byte*): Bytes = IArray(xs*)
  def empty: Bytes = IArray()

case class ExcessDataError(size: ByteSize, limit: ByteSize) extends Error:
  def message: Text =
    Text(s"the amount of data in the stream (at least ${size}B) exceeds the limit (${limit}B)")

extension (value: DataStream)
  def slurp(limit: ByteSize): Bytes throws ExcessDataError | StreamCutError =
    value.foldLeft(IArray[Byte]()):
      (acc, next) =>
        if acc.length + next.length > limit
        then throw ExcessDataError(acc.length + next.length, limit)
        else acc ++ next

extension [T](value: T)
  def only[S](pf: PartialFunction[T, S]): Option[S] = Some(value).collect(pf)
  def unit: Unit = ()
  def twin: (T, T) = (value, value)
  def triple: (T, T, T) = (value, value, value)
  def unsafeMatchable: T & Matchable = value.asInstanceOf[T & Matchable]

extension [T](value: IArray[T])
  def unsafeMutable: Array[T] = value.asInstanceOf[Array[T]]

extension [T](value: Array[T])
  def unsafeImmutable: IArray[T] = value.asInstanceOf[IArray[T]]
  
  def snapshot(using ClassTag[T]): IArray[T] =
    val newArray = new Array[T](value.length)
    System.arraycopy(value, 0, newArray, 0, value.length)
    newArray.unsafeImmutable

extension [K, V](map: Map[K, V])
  def upsert(key: K, operation: Option[V] => V) = map.updated(key, operation(map.get(key)))
  
  def collate(otherMap: Map[K, V])(merge: (V, V) => V): Map[K, V] =
    otherMap.foldLeft(map) { case (acc, (k, v)) => acc.updated(k, acc.get(k).fold(v)(merge(v, _))) }

class Recur[T](fn: => T => T):
  def apply(value: T): T = fn(value)

def fix[T](func: Recur[T] ?=> (T => T)): (T => T) = func(using Recur(fix(func)))
def recur[T: Recur](value: T): T = summon[Recur[T]](value)

case class Property(name: Text) extends Dynamic:
  def apply(): Text throws KeyNotFoundError =
    Text(Option(System.getProperty(name.s)).getOrElse(throw KeyNotFoundError(name)).nn)
  
  def selectDynamic(key: String): Property = Property(Text(s"$name.$key"))
  def applyDynamic(key: String)(): Text throws KeyNotFoundError = selectDynamic(key).apply()

object Sys extends Dynamic:
  def selectDynamic(key: String): Property = Property(Text(key))
  def applyDynamic(key: String)(): Text throws KeyNotFoundError = selectDynamic(key).apply()
  def bigEndian: Boolean = java.nio.ByteOrder.nativeOrder == java.nio.ByteOrder.BIG_ENDIAN

case class KeyNotFoundError(name: Text) extends Error:
  def message: Text = Text(s"key $name not found")

object Impossible:
  def apply(error: Exception): Impossible =
    Impossible(s"rudiments: an ${error.getClass.getName} exception was thrown when this was not "+
        s"believed to be possible; the error was '${error.getMessage}'")

case class Impossible(message: String) extends java.lang.Error(message)

object Unset

type Maybe[T] = Unset.type | T

extension [T](opt: Maybe[T])
  def otherwise(value: => T): T = opt match
    case Unset => value
    case other => other.asInstanceOf[T]
  
  def option: Option[T] = opt match
    case Unset => None
    case other => Some(other.asInstanceOf[T])

extension (iarray: IArray.type)
  def init[T: ClassTag](size: Int)(fn: Array[T] => Unit): IArray[T] =
    val array = new Array[T](size)
    fn(array)
    array.unsafeImmutable


extension [T](opt: Option[T]) def maybe: Unset.type | T = opt.getOrElse(Unset)

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
  given Source[Stdin.type] with
    type E = StreamCutError
    def read(value: Stdin.type): DataStream throws StreamCutError =
      if System.in == null then throw StreamCutError() else Util.readInputStream(System.in, 10.mb)

trait Source[T]:
  type E <: Exception
  def read(value: T): DataStream throws E

object Sink:
  given Sink[Stdout.type] with
    type E = StreamCutError
    def write(value: Stdout.type, stream: DataStream) =
      if System.out == null then throw StreamCutError() else Util.write(stream, System.out)
  
  given Sink[Stderr.type] with
    type E = StreamCutError
    def write(value: Stderr.type, stream: DataStream) =
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

trait Encoding:
  def name: Text
  def carry(array: Array[Byte]): Int

trait Readable[T]:
  readable =>
    type E <: Exception
    def read(stream: DataStream): T throws E | StreamCutError
    def map[S](fn: T => S): rudiments.Readable[S] { type E = readable.E } =
      new rudiments.Readable[S]:
        type E = readable.E
        def read(stream: DataStream): S throws E | StreamCutError =
          fn(readable.read(stream))

object Stdin
object Stdout
object Stderr

def safely[T](value: => CanThrow[Exception] ?=> T): Maybe[T] =
  try value(using unsafeExceptions.canThrowAny) catch NonFatal => Unset

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

extension [T](xs: Iterable[T])
  transparent inline def mtwin: Iterable[(T, T)] = xs.map { x => (x, x) }
  transparent inline def mtriple: Iterable[(T, T, T)] = xs.map { x => (x, x, x) }

extension [T](future: Future[T])
  def await(): T = Await.result(future, duration.Duration.Inf)

extension[T](xs: Seq[T])
  def random: T = xs(util.Random().nextInt(xs.length))
  transparent inline def shuffle: Seq[T] = util.Random().shuffle(xs)

extension (bs: Int)
  def b: ByteSize = bs
  def kb: ByteSize = bs*1024
  def mb: ByteSize = bs*1024*1024
  def gb: ByteSize = bs*1024*1024*1024
  def tb: ByteSize = bs*1024*1024*1024*1024

opaque type ByteSize = Long

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
        val finished = Future.firstCompletedOf(futures)
        val idx: Int = Await.result(finished, duration.Duration.Inf)
        val stream = vector(idx)
        if stream.isEmpty then recur(futures - futuresVector(idx))
        else
          vector = vector.updated(idx, stream.tail)
          val oldFuture = futuresVector(idx)
          val newFuture = future(stream.tail, idx)
          futuresVector = futuresVector.updated(idx, newFuture)
          stream.head #:: recur(futures - oldFuture + newFuture)
    
    recur(futuresVector.to(Set))

object ByteSize:
  extension (bs: ByteSize)
    def long: Long = bs
    
    @targetName("plus")
    infix def +(that: ByteSize): ByteSize = bs + that
    
    @targetName("gt")
    infix def >(that: ByteSize): Boolean = bs > that

    @targetName("lt")
    infix def <(that: ByteSize): Boolean = bs < that

    @targetName("lte")
    infix def <=(that: ByteSize): Boolean = bs <= that

    @targetName("gte")
    infix def >=(that: ByteSize): Boolean = bs >= that

    @targetName("minus")
    infix def -(that: ByteSize): ByteSize = bs - that

    @targetName("times")
    infix def *(that: Int): ByteSize = bs*that

    @targetName("div")
    infix def /(that: Int): ByteSize = bs/that

enum ExitStatus:
  case Ok
  case Fail(status: Int)

  def apply(): Int = this match
    case Ok           => 0
    case Fail(status) => status

object StackTrace:
  case class Frame(className: Text, method: Text, file: Text, line: Int, native: Boolean)

  def apply(exception: Throwable): StackTrace =
    val frames = List(exception.getStackTrace.nn.map(_.nn)*).map:
      frame =>
        StackTrace.Frame(
          Text(frame.getClassName.nn),
          Text(frame.getMethodName.nn),
          Text(frame.getFileName.nn),
          frame.getLineNumber,
          frame.isNativeMethod
        )
    
    val cause = Option(exception.getCause)
    val fullClassName: String = exception.getClass.nn.getName.nn
    val fullClass: List[Text] = List(fullClassName.split("\\.").nn.map(_.nn).map(Text(_))*)
    val className: Text = fullClass.last
    val component: Text = Text(fullClassName.substring(0 max (fullClassName.length - className.s.length - 1)).nn)
    val message = Text(Option(exception.getMessage).map(_.nn).getOrElse(""))
    
    StackTrace(component, className, message, frames, cause.map(_.nn).map(StackTrace(_)).maybe)

case class StackTrace(component: Text, className: Text, message: Text,
    frames: List[StackTrace.Frame], cause: Maybe[StackTrace])

abstract class Error(cause: Maybe[Error] = Unset) extends Exception():
  def fullClass: List[Text] = List(getClass.nn.getName.nn.split("\\.").nn.map(_.nn).map(Text(_))*)
  def className: Text = fullClass.last
  def component: Text = fullClass.head

  override def getMessage: String = component.s+": "+message
  override def getCause: Exception | Null = cause.option.getOrElse(null)

  def message: Text
  def explanation: Maybe[Text] = Unset

  def stackTrace: StackTrace = StackTrace(this)