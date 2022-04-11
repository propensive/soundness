/*
    Rudiments, version 0.4.0. Copyright 2020-22 Jon Pretty, Propensive OÜ.

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
import java.util as ju
import java.util.concurrent as juc

import scala.util.CommandLineParser

import language.dynamics

import scala.util.{Try, Success, Failure}

import java.util.concurrent as juc
import scala.util.chaining.scalaUtilChainingOps

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

case class Counter(first: Int = 0):
  private var id: Int = first - 1
  
  def apply(): Int = synchronized(id.tap { _ => id += 1 })

object Task:
  private val count: Counter = Counter()
  private def nextName(): String = s"turbulance-${count()}"

  @targetName("make")
  def apply[T](fn: => T): Task[T] = Task(() => fn)

case class Task[T] private(fn: () => T):
  private val promise: Promise[T] = Promise()
  private val runnable: Runnable = () => promise.complete(Success(fn()))
  private val thread: Thread = Thread(runnable, Task.nextName())
  
  def apply(): Promise[T] =
    thread.start()
    promise

trait Encoding:
  def name: Text
  def carry(array: Array[Byte]): Int

def safely[T](value: => CanThrow[Exception] ?=> T): Maybe[T] =
  try value(using unsafeExceptions.canThrowAny) catch NonFatal => Unset

def unsafely[T](value: => CanThrow[Exception] ?=> T): T = value(using unsafeExceptions.canThrowAny)

extension [T](xs: Iterable[T])
  transparent inline def mtwin: Iterable[(T, T)] = xs.map { x => (x, x) }
  transparent inline def mtriple: Iterable[(T, T, T)] = xs.map { x => (x, x, x) }
  transparent inline def sift[S]: Iterable[S] = xs.collect { case x: S => x }

object Timer extends ju.Timer(true)

case class TimeoutError() extends Error:
  def message: Text = Text("An operation did not complete in the time it was given")

extension [T](future: Future[T])
  def await(): T = Await.result(future, duration.Duration.Inf)
  
  def timeout(timeout: Long)(using ExecutionContext): Future[T] =
    val p = Promise[T]
    val timerTask: ju.TimerTask = () => p.tryFailure(TimeoutError())
    
    Timer.schedule(timerTask, timeout)

    future.map:
      a => if p.trySuccess(a) then timerTask.cancel()
    .recover:
      case e: Exception =>
        if p.tryFailure(e) then timerTask.cancel()

    p.future

extension[T](xs: Seq[T])
  def random: T = xs(util.Random().nextInt(xs.length))
  transparent inline def shuffle: Seq[T] = util.Random().shuffle(xs)

extension (bs: Int)
  def b: ByteSize = bs
  def kb: ByteSize = bs*1024L
  def mb: ByteSize = bs*1024L*1024
  def gb: ByteSize = bs*1024L*1024*1024
  def tb: ByteSize = bs*1024L*1024*1024*1024

extension (bs: Long)
  def b: ByteSize = bs
  def kb: ByteSize = bs*1024
  def mb: ByteSize = bs*1024*1024
  def gb: ByteSize = bs*1024*1024*1024
  def tb: ByteSize = bs*1024*1024*1024*1024

opaque type ByteSize = Long

class Trigger():
  private val promise: Promise[Trigger] = Promise()
  def pull(): Unit = synchronized(if !promise.isCompleted then promise.complete(Success(this)))
  def future: Future[Trigger] = promise.future

object ByteSize:

  given Ordering[ByteSize] = Ordering.Long.on(_.long)

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

object ExitStatus:
  def apply(value: Int): ExitStatus = if value == 0 then Ok else Fail(value)

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
    val component: Text = Text(fullClassName.substring(0, 0 max (fullClassName.length - className.s.length - 1)).nn)
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

case class Pid(value: Long)