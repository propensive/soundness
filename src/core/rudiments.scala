/*
    Rudiments, version 0.4.0. Copyright 2020-23 Jon Pretty, Propensive OÜ.

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

import anticipation.*

import scala.collection.IterableFactory
import scala.compiletime.*, ops.int.*

import java.util.regex.*
import java.io as ji
import java.util as ju
import java.util.concurrent as juc
import java.nio.charset as jnc

import scala.util.CommandLineParser

import language.dynamics

import scala.util.{Try, Success, Failure}
import scala.deriving.*
import scala.quoted.*

export scala.util.chaining.scalaUtilChainingOps

export scala.reflect.{ClassTag, Typeable}
export scala.collection.immutable.{Set, List, ListMap, Map, TreeSet, TreeMap}

export Predef.{nn, genericArrayOps, identity, summon, charWrapper, $conforms, ArrowAssoc,
    intWrapper, longWrapper, shortWrapper, byteWrapper, valueOf, doubleWrapper, floatWrapper,
    classOf, locally}

export scala.util.control.NonFatal

export scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala, MapHasAsScala, SeqHasAsJava,
    MapHasAsJava, EnumerationHasAsScala}

export scala.annotation.{tailrec, implicitNotFound, targetName, switch, StaticAnnotation, capability}

import language.experimental.captureChecking

object Unset:
  override def toString(): String = "——"

type Maybe[ValueType] = Unset.type | ValueType

type Bytes = IArray[Byte]

opaque type Text = String

object Text:
  def apply(string: String): Text = string
  extension (string: Text) def s: String = string

  given CommandLineParser.FromString[Text] = identity(_)
  given Ordering[Text] = Ordering.String.on[Text](_.s)
  given GenericHttpRequestParam[String, Text] = _.s

  given (using fromExpr: FromExpr[String]): FromExpr[Text] with
    def unapply(expr: Expr[Text])(using Quotes): Option[Text] = fromExpr.unapply(expr).map(Text(_))
  
  given (using toExpr: ToExpr[String]): ToExpr[Text] with
    def apply(txt: Text)(using Quotes): Expr[Text] = toExpr(txt.s)

  given Conversion[String, Text] = Text(_)

  given typeTest: Typeable[Text] with
    def unapply(value: Any): Option[value.type & Text] = value match
      case str: String => Some(str.asInstanceOf[value.type & Text])
      case _           => None

object Bytes:
  def apply(xs: Byte*): Bytes = IArray(xs*)
  def apply(long: Long): Bytes = IArray((56 to 0 by -8).map(long >> _).map(_.toByte)*)
  def empty: Bytes = IArray()

extension [ValueType](value: ValueType)
  def only[ValueType2](fn: PartialFunction[ValueType, ValueType2]): Option[ValueType2] = Some(value).collect(fn)
  def unit: Unit = ()
  def waive: Any => ValueType = _ => value
  def twin: (ValueType, ValueType) = (value, value)
  def triple: (ValueType, ValueType, ValueType) = (value, value, value)
  def puncture(point: ValueType): Maybe[ValueType] = if value == point then Unset else point
  inline def is[ValueSubtype <: ValueType]: Boolean = value.isInstanceOf[ValueSubtype]

  transparent inline def matchable(using erased Unsafe.type): ValueType & Matchable =
    value.asInstanceOf[ValueType & Matchable]

extension [ElemType](value: IArray[ElemType])
  inline def mutable(using erased Unsafe.type): Array[ElemType] = value match
    case array: Array[ElemType] @unchecked => array
    case _                                  => throw Mistake("Should never match")

extension [ElemType](value: Array[ElemType])
  inline def immutable(using erased Unsafe.type): IArray[ElemType] = value match
    case array: IArray[ElemType] @unchecked => array
    case _                                  => throw Mistake("Should never match")

  def snapshot(using ClassTag[ElemType]): IArray[ElemType] =
    val newArray = new Array[ElemType](value.length)
    System.arraycopy(value, 0, newArray, 0, value.length)
    newArray.immutable(using Unsafe)

extension [KeyType, ValueType](map: Map[KeyType, ValueType])
  def upsert(key: KeyType, op: Maybe[ValueType] => ValueType): Map[KeyType, ValueType] =
    map.updated(key, op(if map.contains(key) then map(key) else Unset))

  def collate(otherMap: Map[KeyType, ValueType])(merge: (ValueType, ValueType) => ValueType)
             : Map[KeyType, ValueType] =
    otherMap.foldLeft(map): (acc, kv) =>
      acc.updated(kv(0), acc.get(kv(0)).fold(kv(1))(merge(kv(1), _)))

extension [K, V](map: Map[K, List[V]])
  def plus(key: K, value: V): Map[K, List[V]] = map.updated(key, map.get(key).fold(List(value))(value :: _))

class Recur[ValueType](fn: => ValueType => ValueType):
  def apply(value: ValueType): ValueType = fn(value)

def fix[ValueType](fn: Recur[ValueType] ?-> (ValueType => ValueType)): (ValueType => ValueType) =
  fn(using Recur(fix(fn)))

def recur[ValueType: Recur](value: ValueType): ValueType = summon[Recur[ValueType]](value)

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

case class KeyNotFoundError(name: Text)
extends Error(ErrorMessage[Text *: EmptyTuple](List(Text("key "), Text(" not found")), name *: EmptyTuple))

extension (iarray: IArray.type)
  def create[ElemType: ClassTag](size: Int)(fn: Array[ElemType] => Unit): IArray[ElemType] =
    val array = new Array[ElemType](size)
    fn(array)
    array.immutable(using Unsafe)

case class Counter(first: Int = 0):
  private var id: Int = first
  def apply(): Int = synchronized(id.tap((id += 1).waive))

object AndExtractor:
  @targetName("And")
  object `&`:
    def unapply[ValueType](value: ValueType): Some[(ValueType, ValueType)] = Some((value, value))

export AndExtractor.&

extension (xs: Iterable[Text])
  transparent inline def ss: Iterable[String] = xs

extension [ValueType](xs: Iterable[ValueType])
  transparent inline def mtwin: Iterable[(ValueType, ValueType)] = xs.map { x => (x, x) }
  transparent inline def mtriple: Iterable[(ValueType, ValueType, ValueType)] = xs.map { x => (x, x, x) }

  def indexBy[ValueType2](fn: ValueType -> ValueType2): Map[ValueType2, ValueType] throws DuplicateIndexError =
    val map = xs.map: value =>
      (fn(value), value)
    
    if xs.size != map.size then throw DuplicateIndexError() else map.to(Map)

object Timer extends ju.Timer(true)

case class DuplicateIndexError()
extends Error(ErrorMessage[EmptyTuple](
  List(Text("the sequence contained more than one element that mapped to the same index")), EmptyTuple
))

extension[ElemType](xs: Seq[ElemType])
  def random: ElemType = xs(util.Random().nextInt(xs.length))
  transparent inline def shuffle: Seq[ElemType] = util.Random().shuffle(xs)
  
  def runs(fn: ElemType => Any): List[List[ElemType]] =
    
    @tailrec
    def recur(current: Any, todo: Seq[ElemType], run: List[ElemType], done: List[List[ElemType]])
             : List[List[ElemType]] =
      if todo.isEmpty then (run.reverse :: done).reverse
      else
        val focus = fn(todo.head)
        if current == focus then recur(current, todo.tail, todo.head :: run, done)
        else recur(focus, todo.tail, List(todo.head), run.reverse :: done)

    if xs.isEmpty then Nil else recur(fn(xs.head), xs.tail, List(xs.head), Nil)

object Cursor:
  opaque type Cursor = Int
  opaque type CursorSeq[T] = IndexedSeq[T]

  extension [T](xs: CursorSeq[T])
    def apply(idx: Cursor): T = xs(idx)
    def apply(idx: Cursor, offset: Int): Maybe[T] =
      if (idx + offset) >= 0 && (idx + offset) < xs.length then apply(idx + offset) else Unset
    
  extension (cursor: Cursor) def index: Int = cursor

  transparent inline def curse[T, S](xs: IndexedSeq[T])(inline fn: (CursorSeq[T], Cursor) ?=> S)
                              : IndexedSeq[S] =
    xs.indices.map { i => fn(using xs, i) }

inline def cursor[ElemType](using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor): ElemType =
  xs(cur)

inline def precursor[ElemType](using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor)
                    : Maybe[ElemType] =
  xs(cur, -1)

inline def postcursor[ElemType](using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor)
                     : Maybe[ElemType] =
  xs(cur, 1)

inline def cursorIndex(using inline cur: Cursor.Cursor): Int = cur.index

inline def cursor[ElemType](n: Int)(using inline xs: Cursor.CursorSeq[ElemType], inline cur: Cursor.Cursor)
                 : Maybe[ElemType] =
  xs(cur, n)


extension [ElemType](xs: IndexedSeq[ElemType])
  transparent inline def curse[ElemType2](inline fn: (Cursor.CursorSeq[ElemType], Cursor.Cursor) ?=> ElemType2)
                              : IndexedSeq[ElemType2] =
    Cursor.curse(xs)(fn)

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

object ByteSize:
  given GenericHttpRequestParam["content-length", ByteSize] = _.long.toString
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

case class Pid(value: Long):
  override def toString(): String = "ᴾᴵᴰ｢"+value+"｣"

object Uuid:
  def unapply(text: Text): Option[Uuid] =
    try Some:
      val uuid = ju.UUID.fromString(text.s).nn
      Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)
    catch case err: Exception => None

  def apply(): Uuid =
    val uuid = ju.UUID.randomUUID().nn
    Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

case class Uuid(msb: Long, lsb: Long):
  def javaUuid: ju.UUID = ju.UUID(msb, lsb)
  def bytes: Bytes = Bytes(msb) ++ Bytes(lsb)

inline def env(using env: Environment): Environment = env

package environments:
  given system: Environment(
    v => Option(System.getenv(v.s)).map(_.nn).map(Text(_)),
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_))
  )

  given restricted: Environment(
    v => None,
    v => Option(System.getProperty(v.s)).map(_.nn).map(Text(_))
  )

  given empty: Environment(v => None, v => None)

sealed class LocalhostAccess()
sealed class Internet() extends LocalhostAccess()

def internet[T](fn: Internet ?=> T): T =
  val inet: Internet = Internet()
  fn(using inet)

def localhostAccess[T](fn: LocalhostAccess ?=> T): T =
  val access: LocalhostAccess = LocalhostAccess()
  fn(using access)

extension [ProductType <: Product](product: ProductType)(using mirror: Mirror.ProductOf[ProductType])
  def tuple: mirror.MirroredElemTypes = Tuple.fromProductTyped(product)

extension [TupleType <: Tuple](tuple: TupleType)
  def to[ProductType](using mirror: Mirror.ProductOf[ProductType]): ProductType = mirror.fromProduct(tuple)

object Unsafe

object Default:
  given Default[Int](0)
  given Default[Long](0L)
  given Default[Text](Text(""))
  given Default[String]("")
  given [ElemType]: Default[List[ElemType]](Nil)
  given [ElemType]: Default[Set[ElemType]](Set())
  given [ElemType]: Default[Vector[ElemType]](Vector())

trait Default[+ValueType](default: ValueType):
  def apply(): ValueType = default
