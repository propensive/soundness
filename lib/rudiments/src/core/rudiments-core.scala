                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package rudiments

import language.dynamics

import java.io as ji

import scala.collection as sc
import scala.collection.mutable as scm
import scala.compiletime.*
import scala.deriving.*

import anticipation.*
import denominative.*
import fulminate.*
import prepositional.*
import proscenium.*
import vacuous.*

def fixpoint[ValueType](initial: ValueType)
   (fn: (recur: (ValueType => ValueType)) ?=> (ValueType => ValueType)): ValueType =

  def recurrence(fn: (recur: ValueType => ValueType) ?=> ValueType => ValueType)
  :       ValueType => ValueType =
    fn(using recurrence(fn(_)))

  recurrence(fn)(initial)

extension [ValueType](value: ValueType)
  def unit: Unit = ()
  def waive: Any => ValueType = _ => value
  def twin: (ValueType, ValueType) = (value, value)
  def triple: (ValueType, ValueType, ValueType) = (value, value, value)

  inline def iff(inline predicate: Boolean)(inline lambda: ValueType => ValueType): ValueType =
    if predicate then lambda(value) else value

  inline def iff(inline predicate: ValueType => Boolean)(inline lambda: ValueType => ValueType)
  :       ValueType =
    if predicate(value) then lambda(value) else value

  inline def is[ValueSubtype <: ValueType]: Boolean = value.isInstanceOf[ValueSubtype]

  transparent inline def matchable(using Unsafe): ValueType & Matchable =
    value.asInstanceOf[ValueType & Matchable]

  def give[ResultType](block: ValueType ?=> ResultType): ResultType = block(using value)

extension [InputType, ResultType](inline lambda: (=> InputType) => ResultType)
  inline def upon(inline value: => InputType): ResultType = lambda(value)

extension [ValueType](inline value: => ValueType)
  inline def pipe[ResultType](inline lambda: ValueType => ResultType): ResultType = lambda(value)

  inline def tap(inline block: ValueType => Unit): ValueType =
    val result: ValueType = value
    block(result)
    result

  inline def also(inline block: => Unit): ValueType =
    val result: ValueType = value
    block
    result

extension (inline statement: => Unit)
  inline infix def yet [ResultType](inline block: => ResultType): ResultType =
    statement
    block

def loop(block: => Unit): Loop = Loop({ () => block })

export Rudiments.&

extension (inline context: StringContext)
  transparent inline def bin(): AnyVal = ${Rudiments.bin('context)}
  transparent inline def hex(): IArray[Byte] = ${Rudiments.hex('context)}

@targetName("erasedValue")
erased def !![ErasedType] : ErasedType = scala.compiletime.erasedValue

extension [ValueType <: Matchable](iterable: Iterable[ValueType])
  transparent inline def sift[FilterType <: ValueType: Typeable]: Iterable[FilterType] =
    iterable.flatMap(FilterType.unapply(_))

  inline def has(value: ValueType): Boolean = iterable.exists(_ == value)

  inline def where(inline predicate: ValueType => Boolean): Optional[ValueType] =
    iterable.find(predicate).getOrElse(Unset)

  transparent inline def interleave(right: Iterable[ValueType]): Iterable[ValueType] =
    iterable.zip(right).flatMap(Iterable(_, _))

extension [ValueType](iterator: Iterator[ValueType])
  transparent inline def each(predicate: (ordinal: Ordinal) ?=> ValueType => Unit): Unit =
    var ordinal: Ordinal = Prim
    iterator.foreach: value =>
      predicate(using ordinal)(value)
      ordinal += 1

  inline def all(predicate: ValueType => Boolean): Boolean = iterator.forall(predicate)

extension [ValueType](iterable: Iterable[ValueType])
  transparent inline def each(lambda: (ordinal: Ordinal) ?=> ValueType => Unit): Unit =
    var ordinal: Ordinal = Prim
    iterable.foreach: value =>
      lambda(using ordinal)(value)
      ordinal += 1

  inline def fuse[StateType](base: StateType)
     (lambda: (state: StateType, next: ValueType) ?=> StateType)
  :     StateType =

    val iterator: Iterator[ValueType] = iterable.iterator
    var state: StateType = base
    while iterator.hasNext do state = lambda(using state, iterator.next)

    state

  def sumBy[NumberType: Numeric](lambda: ValueType => NumberType): NumberType =
    var count = NumberType.zero

    iterable.foreach: value =>
      count = NumberType.plus(count, lambda(value))

    count

  inline def all(predicate: ValueType => Boolean): Boolean = iterable.forall(predicate)
  transparent inline def bi: Iterable[(ValueType, ValueType)] = iterable.map { x => (x, x) }

  transparent inline def tri: Iterable[(ValueType, ValueType, ValueType)] =
    iterable.map { x => (x, x, x) }

  def indexBy[ValueType2](lambda: ValueType => ValueType2): Map[ValueType2, ValueType] =
    iterable.map: value =>
      (lambda(value), value)

    . to(Map)

  def longestTrain(predicate: ValueType => Boolean): (Int, Int) =
    @tailrec
    def recur
       (index: Int, iterable: Iterable[ValueType], bestStart: Int, bestLength: Int, length: Int)
    :       (Int, Int) =

      if iterable.isEmpty then (bestStart, bestLength) else
        if predicate(iterable.head) then
          if length >= bestLength
          then recur(index + 1, iterable.tail, index - length, length + 1, length + 1)
          else recur(index + 1, iterable.tail, bestStart, bestLength, length + 1)
        else recur(index + 1, iterable.tail, bestStart, bestLength, 0)

    recur(0, iterable, 0, 0, 0)

extension [ElemType](value: IArray[ElemType])
  inline def mutable(using Unsafe): Array[ElemType] = value.asMatchable.runtimeChecked match
    case array: (Array[ElemType] @unchecked) => array

extension [ElemType](array: Array[ElemType])
  def immutable(using Unsafe): IArray[ElemType] = array.runtimeChecked match
    case array: (IArray[ElemType] @unchecked) => array

  def snapshot(using ClassTag[ElemType]): IArray[ElemType] =
    val newArray = new Array[ElemType](array.length)
    System.arraycopy(array, 0, newArray, 0, array.length)
    newArray.immutable(using Unsafe)

  inline def place(value: IArray[ElemType], ordinal: Ordinal = Prim): Unit =
    System.arraycopy(value.asInstanceOf[Array[ElemType]], 0, array, ordinal.n0, value.length)

extension [KeyType, ValueType](map: sc.Map[KeyType, ValueType])
  inline def has(key: KeyType): Boolean = map.contains(key)
  inline def bijection: Bijection[KeyType, ValueType] = Bijection(map.to(Map))

extension [KeyType, ValueType](map: Map[KeyType, ValueType])
  def upsert(key: KeyType, op: Optional[ValueType] => ValueType): Map[KeyType, ValueType] =
    map.updated(key, op(if map.contains(key) then map(key) else Unset))

  def collate(right: Map[KeyType, ValueType])(merge: (ValueType, ValueType) => ValueType)
  :       Map[KeyType, ValueType] =

    right.fuse(map)(state.updated(next(0), state.get(next(0)).fold(next(1))(merge(_, next(1)))))

extension [KeyType, ValueType](map: scm.Map[KeyType, ValueType])
  inline def establish(key: KeyType)(evaluate: => ValueType): ValueType =
    map.getOrElseUpdate(key, evaluate)

extension [KeyType, ValueType](map: Map[KeyType, List[ValueType]])
  def plus(key: KeyType, value: ValueType): Map[KeyType, List[ValueType]] =
    map.updated(key, map.get(key).fold(List(value))(value :: _))

extension [ValueType](list: List[ValueType])
  def unwind(tail: List[ValueType]): List[ValueType] = tail.reverse_:::(list)

extension [ElemType](seq: Seq[ElemType])
  def runs: List[List[ElemType]] = runsBy(identity)

  inline def prim: Optional[ElemType] = if seq.isEmpty then Unset else seq.head
  inline def sec: Optional[ElemType] = if seq.length < 2 then Unset else seq(1)
  inline def ter: Optional[ElemType] = if seq.length < 3 then Unset else seq(2)
  inline def unique: Optional[ElemType] = if seq.length == 1 then seq.head else Unset

  def runsBy(lambda: ElemType => Any): List[List[ElemType]] =
    @tailrec
    def recur(current: Any, todo: Seq[ElemType], run: List[ElemType], done: List[List[ElemType]])
    :       List[List[ElemType]] =
      if todo.isEmpty then (run.reverse :: done).reverse
      else
        val focus = lambda(todo.head)
        if current == focus then recur(current, todo.tail, todo.head :: run, done)
        else recur(focus, todo.tail, List(todo.head), run.reverse :: done)

    if seq.isEmpty then Nil else recur(lambda(seq.head), seq.tail, List(seq.head), Nil)

inline def cursor[ElemType]
   (using inline seq: Cursor.CursorSeq[ElemType], inline cursor: Cursor.Cursor)
:       ElemType =

  cursor.of(seq)

inline def precursor[ElemType]
   (using inline seq: Cursor.CursorSeq[ElemType], inline cursor: Cursor.Cursor)
:       Optional[ElemType] =

  cursor.of(seq, -1)

inline def postcursor[ElemType]
   (using inline seq: Cursor.CursorSeq[ElemType], inline cursor: Cursor.Cursor)
:       Optional[ElemType] =

  cursor.of(seq, 1)

inline def cursorIndex(using inline cursor: Cursor.Cursor): Int = cursor.index

inline def cursorOffset[ElemType](offset: Int)
   (using inline seq: Cursor.CursorSeq[ElemType], inline cursor: Cursor.Cursor)
:       Optional[ElemType] =
  cursor.of(seq, offset)

extension [ElemType](seq: IndexedSeq[ElemType])
  transparent inline def curse[ElemType2]
     (inline block: (Cursor.CursorSeq[ElemType], Cursor.Cursor) ?=> ElemType2)
  :       IndexedSeq[ElemType2] =
    Cursor.curse(seq)(block)

  transparent inline def has(index: Int): Boolean = index >= 0 && index < seq.length

extension (iarray: IArray.type)
  def create[ElemType: ClassTag](size: Int)(lambda: Array[ElemType] => Unit): IArray[ElemType] =
    val array: Array[ElemType] = new Array[ElemType](size)
    lambda(array)
    array.immutable(using Unsafe)

extension (bytes: Bytes)
  def javaInputStream: ji.InputStream = new ji.ByteArrayInputStream(bytes.mutable(using Unsafe))

extension [ValueType: Indexable](inline value: ValueType)
  inline def has(index: ValueType.Operand): Boolean = ValueType.contains(value, index)

  inline def at(index: ValueType.Operand): Optional[ValueType.Result] =
    optimizable[ValueType.Result]: default =>
      if ValueType.contains(value, index) then ValueType.access(value, index) else default

extension [ValueType: Segmentable as segmentable](inline value: ValueType)
  inline def segment(interval: Interval): ValueType = segmentable.segment(value, interval)

extension (bs: Int)
  def b: Memory = Memory(bs)
  def kib: Memory = Memory(bs*1024L)
  def mib: Memory = Memory(bs*1024L*1024)
  def gib: Memory = Memory(bs*1024L*1024*1024)
  def tib: Memory = Memory(bs*1024L*1024*1024*1024)

extension (bs: Long)
  def b: Memory = Memory(bs)
  def kib: Memory = Memory(bs*1024)
  def mib: Memory = Memory(bs*1024*1024)
  def gib: Memory = Memory(bs*1024*1024*1024)
  def tib: Memory = Memory(bs*1024*1024*1024*1024)

extension (bytes: Bytes)
  def memory: Memory = Memory(bytes.size)

def workingDirectory[PathType: Instantiable across Paths from Text]
   (using directory: WorkingDirectory)
:     PathType =

  directory.path[PathType]

def homeDirectory[PathType: Instantiable across Paths from Text]
   (using directory: HomeDirectory)
:     PathType =

  directory.path[PathType]

def temporaryDirectory[PathType: Instantiable across Paths from Text]
   (using directory: TemporaryDirectory)
:     PathType =

  directory.path[PathType]

package workingDirectories:
  given systemProperty: WorkingDirectory = () =>
    Optional(System.getProperty("user.dir")).let(_.tt).or:
      panic(m"the `user.dir` system property is not set")

  given default: WorkingDirectory = () => java.nio.file.Paths.get("").nn.toAbsolutePath.toString

package homeDirectories:
  given systemProperty: HomeDirectory = () =>
    Optional(System.getProperty("user.home")).let(_.tt).or:
      panic(m"the `user.home` system property is not set")

  given environment: HomeDirectory = () =>
    List("HOME", "USERPROFILE", "HOMEPATH").map(System.getenv(_)).map(Optional(_)).compact.prim
    . let(_.tt)
    . or:
        panic(m"none of `HOME`, `USERPROFILE` or `HOMEPATH` environment variables is set")

package temporaryDirectories:
  given systemProperty: TemporaryDirectory = () =>
    Optional(System.getProperty("java.io.tmpdir")).let(_.tt).or:
      panic(m"the `java.io.tmpdir` system property is not set")

  given environment: TemporaryDirectory = () =>
    List("TMPDIR", "TMP", "TEMP").map(System.getenv(_)).map(Optional(_)).compact.prim.let(_.tt).or:
      panic(m"none of `TMPDIR`, `TMP` or `TEMP` environment variables is set")

extension [ValueType: Countable](inline value: ValueType)
  inline def ult: Optional[Ordinal] =
    if ValueType.size(value) >= 1 then (ValueType.size(value) - 1).z else Unset

  inline def pen: Optional[Ordinal] =
    if ValueType.size(value) >= 1 then (ValueType.size(value) - 2).z else Unset

  inline def ant: Optional[Ordinal] =
    if ValueType.size(value) >= 1 then (ValueType.size(value) - 3).z else Unset

extension [ProductType <: Product: Mirror.ProductOf](product: ProductType)
  def tuple: ProductType.MirroredElemTypes = Tuple.fromProductTyped(product)

extension [TupleType <: Tuple](tuple: TupleType)
  def to[ProductType: Mirror.ProductOf]: ProductType = ProductType.fromProduct(tuple)
