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

import java.util.regex.*
import java.io as ji
import java.util as ju
import java.util.concurrent as juc

import scala.util.CommandLineParser

import language.dynamics

import scala.util.{Try, Success, Failure}

import java.util.concurrent as juc
export scala.util.chaining.scalaUtilChainingOps

export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}

export Predef.{nn, genericArrayOps, identity, summon, charWrapper, $conforms, ArrowAssoc,
    intWrapper, longWrapper, shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper,
    classOf, locally}

export scala.util.control.NonFatal
export scala.jdk.CollectionConverters.*
export scala.annotation.{tailrec, implicitNotFound, targetName, switch, StaticAnnotation}

type Bytes = IArray[Byte]
opaque type Text = String

object Text:
  def apply(string: String): Text = string
  extension (string: Text) def s: String = string

  given CommandLineParser.FromString[Text] = identity(_)
  given Ordering[Text] = Ordering.String.on[Text](_.s)
 
  given Conversion[String, Text] = Text(_)

  given typeTest: Typeable[Text] with
    def unapply(value: Any): Option[value.type & Text] = value match
      case str: String => Some(str.asInstanceOf[value.type & Text])
      case _           => None

object Bytes:
  def apply(xs: Byte*): Bytes = IArray(xs*)
  def apply(long: Long): Bytes = IArray((56 to 0 by -8).map(long >> _).map(_.toByte)*)
  def empty: Bytes = IArray()

extension [T](value: T)
  def only[S](pf: PartialFunction[T, S]): Option[S] = Some(value).collect(pf)
  def unit: Unit = ()
  def waive: Any => T = _ => value
  def twin: (T, T) = (value, value)
  def triple: (T, T, T) = (value, value, value)
  
  transparent inline def matchable(using erased Unsafe.type): T & Matchable =
    value.asInstanceOf[T & Matchable]

extension [T](value: IArray[T])
  transparent inline def mutable(using erased Unsafe.type): Array[T] = value match
    case array: Array[T] @unchecked => array
    case _                          => throw Mistake("Should never match")

extension [T](value: Array[T])
  transparent inline def immutable(using erased Unsafe.type): IArray[T] = value match
    case array: IArray[T] @unchecked => array
    case _                           => throw Mistake("Should never match")
  
  def snapshot(using ClassTag[T]): IArray[T] =
    val newArray = new Array[T](value.length)
    System.arraycopy(value, 0, newArray, 0, value.length)
    newArray.immutable(using Unsafe)

extension [K, V](map: Map[K, V])
  def upsert(key: K, operation: Option[V] => V) = map.updated(key, operation(map.get(key)))
  
  def collate(otherMap: Map[K, V])(merge: (V, V) => V): Map[K, V] =
    otherMap.foldLeft(map): (acc, kv) =>
      acc.updated(kv(0), acc.get(kv(0)).fold(kv(1))(merge(kv(1), _)))

class Recur[T](fn: => T => T):
  def apply(value: T): T = fn(value)

def fix[T](func: Recur[T] ?=> (T => T)): (T => T) = func(using Recur(fix(func)))
def recur[T: Recur](value: T): T = summon[Recur[T]](value)

case class Property(name: Text) extends Dynamic:
  def apply(): Text throws KeyNotFoundError =
    Text(Option(System.getProperty(name.s)).getOrElse(throw KeyNotFoundError(name)).nn)
  
  def update(value: Text): Unit = System.setProperty(name.s, value.s)
  def selectDynamic(key: String): Property = Property(Text(s"$name.$key"))
  def applyDynamic(key: String)(): Text throws KeyNotFoundError = selectDynamic(key).apply()

object Sys extends Dynamic:
  def selectDynamic(key: String): Property = Property(Text(key))
  def applyDynamic(key: String)(): Text throws KeyNotFoundError = selectDynamic(key).apply()
  def bigEndian: Boolean = java.nio.ByteOrder.nativeOrder == java.nio.ByteOrder.BIG_ENDIAN

case class KeyNotFoundError(name: Text)(using Codepoint) extends Error(err"key $name not found")(pos)

object Mistake:
  def apply(error: Exception): Mistake =
    Mistake(s"rudiments: an ${error.getClass.getName} exception was thrown when this was not "+
        s"believed to be possible; the error was '${error.getMessage}'")

case class Mistake(message: String) extends java.lang.Error(message)

object Unset

type Maybe[T] = Unset.type | T

extension [T](opt: Maybe[T])
  def otherwise(value: => T): T = opt match
    case Unset               => value
    case other: T @unchecked => other
  
  def presume(using default: Default[T]): T = otherwise(default())
  
  def option: Option[T] = opt match
    case Unset               => None
    case other: T @unchecked => Some(other)

extension (iarray: IArray.type)
  def init[T: ClassTag](size: Int)(fn: Array[T] => Unit): IArray[T] =
    val array = new Array[T](size)
    fn(array)
    array.immutable(using Unsafe)

extension [T](opt: Option[T])
  def maybe: Unset.type | T = opt.getOrElse(Unset)
  def presume(using default: Default[T]) = opt.getOrElse(default())

case class Counter(first: Int = 0):
  private var id: Int = first
  def apply(): Int = synchronized(id.tap((id += 1).waive))

trait Encoding:
  def name: Text
  def carry(array: Array[Byte]): Int

object AndExtractor:
  @targetName("And")
  object `&`:
    def unapply[T](value: T): Some[(T, T)] = Some((value, value))

export AndExtractor.&

extension (xs: Iterable[Text])
  transparent inline def ss: Iterable[String] = xs

extension [T](xs: Iterable[T])
  transparent inline def mtwin: Iterable[(T, T)] = xs.map { x => (x, x) }
  transparent inline def mtriple: Iterable[(T, T, T)] = xs.map { x => (x, x, x) }
  transparent inline def sift[S]: Iterable[S] = xs.collect { case x: S @unchecked => x }
  
  def indexBy[S](fn: T => S): Map[S, T] throws DuplicateIndexError =
    val map = xs.map { value => fn(value) -> value }
    if xs.size != map.size then throw DuplicateIndexError() else map.to(Map)

object Timer extends ju.Timer(true)

case class DuplicateIndexError()(using Codepoint)
extends Error(err"the sequence contained more than one element that mapped to the same index")(pos)

case class TimeoutError()(using Codepoint)
extends Error(err"an operation did not complete in the time it was given")(pos)

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

// class Trigger():
//   private val promise: Promise[Trigger] = Promise()
//   def pull(): Unit = synchronized(safely(promise.supply(this)))

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

case class Pid(value: Long)

object Uuid:
  def unapply(text: Text): Option[Uuid] =
    safely:
      val uuid = ju.UUID.fromString(text.s).nn
      Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)
    .option

  def apply(): Uuid =
    val uuid = ju.UUID.randomUUID().nn
    Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)
  
case class Uuid(msb: Long, lsb: Long):
  def javaUuid: ju.UUID = ju.UUID(msb, lsb)
  def bytes: Bytes = Bytes(msb) ++ Bytes(lsb)

object Unsafe

object Default:
  given Default[Int](0)
  given Default[Long](0L)
  given Default[Text](Text(""))
  given Default[String]("")
  given [T]: Default[List[T]](Nil)
  given [T]: Default[Set[T]](Set())
  given [T]: Default[Vector[T]](Vector())

trait Default[+T](default: T):
  def apply(): T = default

package environments:
  given system: Environment(v => Option(System.getenv(v.s)).map(_.nn).map(Text(_)))
  given empty: Environment(v => None)

@implicitNotFound("rudiments: a contextual Environment is required, for example\n    given Environment()\nor,\n"+
                      "    given Envronment = environments.system")
class Environment(getEnv: Text => Option[Text]):
  def apply(variable: Text): Option[Text] = getEnv(variable)

case class EnvError(variable: Text)(using Codepoint)
extends Error(err"the environment variable $variable was not found")(pos)

sealed class Internet()

def internet[T](fn: Internet ?=> T): T =
  val inet: Internet = Internet()
  fn(using inet)
