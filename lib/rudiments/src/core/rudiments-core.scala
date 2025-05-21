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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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
import symbolism.*
import vacuous.*

def fixpoint[value](initial: value)(fn: (recur: (value => value)) ?=> (value => value)): value =
  def recurrence(fn: (recur: value => value) ?=> value => value): value => value =
    fn(using recurrence(fn(_)))

  recurrence(fn)(initial)

extension [value](value: value)
  def unit: Unit = ()
  def waive: Any => value = _ => value
  def twin: (value, value) = (value, value)
  def triple: (value, value, value) = (value, value, value)

  inline def iff(inline predicate: Boolean)(inline lambda: value => value): value =
    if predicate then lambda(value) else value

  inline def iff(inline predicate: value => Boolean)(inline lambda: value => value): value =
    if predicate(value) then lambda(value) else value

  inline def is[ValueSubtype <: value]: Boolean = value.isInstanceOf[ValueSubtype]

  transparent inline def matchable(using Unsafe): value & Matchable =
    value.asInstanceOf[value & Matchable]

  def give[result](block: value ?=> result): result = block(using value)

extension [input, result](inline lambda: (=> input) => result)
  inline def upon(inline value: => input): result = lambda(value)

extension [value](inline value: => value)
  inline def pipe[result](inline lambda: value => result): result = lambda(value)

  inline def tap(inline block: value => Unit): value =
    val result: value = value
    block(result)
    result

  inline def also(inline block: => Unit): value =
    val result: value = value
    block
    result

extension (inline statement: => Unit)
  inline infix def yet [result](inline block: => result): result =
    statement
    block

def loop(block: => Unit): Loop = Loop({ () => block })

export Rudiments.&

@targetName("erasedValue")
erased def !![erasure] : erasure = scala.compiletime.erasedValue

extension [value <: Matchable](iterable: Iterable[value])
  transparent inline def sift[filter <: value: Typeable]: Iterable[filter] =
    iterable.flatMap(filter.unapply(_))

  inline def has(value: value): Boolean = iterable.exists(_ == value)

  inline def where(inline predicate: value => Boolean): Optional[value] =
    iterable.find(predicate).getOrElse(Unset)

  transparent inline def weave(right: Iterable[value]): Iterable[value] =
    iterable.zip(right).flatMap(Iterable(_, _))

extension [value](iterator: Iterator[value])
  transparent inline def each(predicate: (ordinal: Ordinal) ?=> value => Unit): Unit =
    var ordinal: Ordinal = Prim
    iterator.foreach: value =>
      predicate(using ordinal)(value)
      ordinal += 1

  inline def all(predicate: value => Boolean): Boolean = iterator.forall(predicate)

extension [value](iterables: Iterable[Iterable[value]])
  def intercalate(between: Iterable[value] = Iterable()): Iterable[value] =
    if iterables.isEmpty then Iterable() else iterables.reduceLeft(_ ++ between ++ _)

extension [value](iterable: Iterable[value])
  transparent inline def total
                          (using addable:  value is Addable by value,
                                 equality: addable.Result =:= value)
  :     Optional[value] =
    compiletime.summonFrom:
      case zeroic: ((? <: value) is Zeroic) =>
        iterable.foldLeft(zeroic.zero)(addable.add)

      case _ =>
        if iterable.isEmpty then Unset else iterable.tail.foldLeft(iterable.head)(addable.add)

  transparent inline def mean
                          (using addable:   value is Addable by value,
                                 equality:  addable.Result =:= value,
                                 divisible: value is Divisible by Double)
  :     Optional[divisible.Result] =
   compiletime.summonFrom:
     case zeroic: ((? <: value) is Zeroic) =>
       iterable.foldLeft[value](zeroic.zero)(addable.add)/iterable.size.toDouble

     case _ =>
       iterable.total.let(_/iterable.size.toDouble)

  def variance
       (using zeroic:        value is Zeroic,
              addable:       value is Addable by value,
              equality:      addable.Result =:= value,
              divisible:     value is Divisible by Double,
              subtractable:  value is Subtractable by divisible.Result,
              multiplicable: subtractable.Result is Multiplicable by subtractable.Result,
              addable2:      multiplicable.Result is Addable by multiplicable.Result,
              zeroic2:       multiplicable.Result is Zeroic,
              equality2:     addable2.Result =:= multiplicable.Result,
              divisible2:    multiplicable.Result is Divisible by Double)
  :     divisible2.Result =
    val mean: divisible.Result = iterable.mean
    iterable.map(_ - mean).map { value => value*value }.total/iterable.size.toDouble

  def standardDeviation
       (using zeroic:        value is Zeroic,
              addable:       value is Addable by value,
              equality:      addable.Result =:= value,
              divisible:     value is Divisible by Double,
              subtractable:  value is Subtractable by divisible.Result,
              multiplicable: subtractable.Result is Multiplicable by subtractable.Result,
              addable2:      multiplicable.Result is Addable by multiplicable.Result,
              zeroic2:       multiplicable.Result is Zeroic,
              equality2:     addable2.Result =:= multiplicable.Result,
              divisible2:    multiplicable.Result is Divisible by Double,
              rootable:      divisible2.Result is Rootable[2])
  :     rootable.Result =
    val mean: divisible.Result = iterable.mean
    (iterable.map(_ - mean).map { value => value*value }.total/iterable.size.toDouble).sqrt

  def product
       (using unital:        value is Unital,
              multiplicable: value is Multiplicable by value,
              equality:      multiplicable.Result =:= value)
  :     value =
    iterable.foldLeft(unital.one)(multiplicable.multiply)

  transparent inline def each(lambda: (ordinal: Ordinal) ?=> value => Unit): Unit =
    var ordinal: Ordinal = Prim
    iterable.iterator.foreach: value =>
      lambda(using ordinal)(value)
      ordinal += 1

  transparent inline def annex[right](lambda: value => right) = iterable.map: item =>
    inline !![value] match
      case tuple: Tuple => tuple :* lambda(tuple)
      case other        => (other, lambda(other))

  inline def fuse[state](base: state)(lambda: (state: state, next: value) ?=> state): state =
    val iterator: Iterator[value] = iterable.iterator
    var state: state = base
    while iterator.hasNext do state = lambda(using state, iterator.next)

    state

  def sumBy[number: Numeric](lambda: value => number): number =
    var count = number.zero

    iterable.foreach: value =>
      count = number.plus(count, lambda(value))

    count

  inline def all(predicate: value => Boolean): Boolean = iterable.forall(predicate)
  transparent inline def bi: Iterable[(value, value)] = iterable.map { x => (x, x) }

  transparent inline def tri: Iterable[(value, value, value)] = iterable.map { x => (x, x, x) }

  def indexBy[value2](lambda: value => value2): Map[value2, value] =
    iterable.map: value =>
      (lambda(value), value)

    . to(Map)

  def longestTrain(predicate: value => Boolean): (Int, Int) =
    @tailrec
    def recur(index: Int, iterable: Iterable[value], bestStart: Int, bestLength: Int, length: Int)
    :       (Int, Int) =

      if iterable.isEmpty then (bestStart, bestLength) else
        if predicate(iterable.head) then
          if length >= bestLength
          then recur(index + 1, iterable.tail, index - length, length + 1, length + 1)
          else recur(index + 1, iterable.tail, bestStart, bestLength, length + 1)
        else recur(index + 1, iterable.tail, bestStart, bestLength, 0)

    recur(0, iterable, 0, 0, 0)

extension [element](value: IArray[element])
  inline def mutable(using Unsafe): Array[element] = value.asInstanceOf[Array[element]]

extension [element](array: Array[element])
  inline def immutable(using Unsafe): IArray[element] = array.asInstanceOf[IArray[element]]

  def snapshot(using ClassTag[element]): IArray[element] =
    val newArray = new Array[element](array.length)
    System.arraycopy(array, 0, newArray, 0, array.length)
    newArray.immutable(using Unsafe)

  inline def place(value: IArray[element], ordinal: Ordinal = Prim): Unit =
    System.arraycopy(value.asInstanceOf[Array[element]], 0, array, ordinal.n0, value.length)

extension [key, value](map: sc.Map[key, value])
  inline def has(key: key): Boolean = map.contains(key)
  inline def bijection: Bijection[key, value] = Bijection(map.to(Map))

extension [key, value](map: Map[key, value])
  def upsert(key: key, op: Optional[value] => value): Map[key, value] =
    map.updated(key, op(if map.contains(key) then map(key) else Unset))

  def collate(right: Map[key, value])(merge: (value, value) => value): Map[key, value] =
    right.fuse(map)(state.updated(next(0), state.get(next(0)).fold(next(1))(merge(_, next(1)))))

extension [key, value](map: scm.Map[key, value])
  inline def establish(key: key)(evaluate: => value): value = map.getOrElseUpdate(key, evaluate)

extension [key, value](map: Map[key, List[value]])
  def plus(key: key, value: value): Map[key, List[value]] =
    map.updated(key, map.get(key).fold(List(value))(value :: _))

extension [value](list: List[value])
  def unwind(tail: List[value]): List[value] = tail.reverse_:::(list)

extension [element](seq: Seq[element])
  def runs: List[List[element]] = runsBy(identity)

  inline def prim: Optional[element] = if seq.isEmpty then Unset else seq.head
  inline def sec: Optional[element] = if seq.length < 2 then Unset else seq(1)
  inline def ter: Optional[element] = if seq.length < 3 then Unset else seq(2)
  inline def unique: Optional[element] = if seq.length == 1 then seq.head else Unset

  def runsBy(lambda: element => Any): List[List[element]] =
    @tailrec
    def recur(current: Any, todo: Seq[element], run: List[element], done: List[List[element]])
    :       List[List[element]] =
      if todo.isEmpty then (run.reverse :: done).reverse
      else
        val focus = lambda(todo.head)
        if current == focus then recur(current, todo.tail, todo.head :: run, done)
        else recur(focus, todo.tail, List(todo.head), run.reverse :: done)

    if seq.isEmpty then Nil else recur(lambda(seq.head), seq.tail, List(seq.head), Nil)

extension [element](seq: IndexedSeq[element])
  transparent inline def has(index: Int): Boolean = index >= 0 && index < seq.length

extension (iarray: IArray.type)
  def create[element: ClassTag](size: Int)(lambda: Array[element] => Unit): IArray[element] =
    val array: Array[element] = new Array[element](size)
    lambda(array)
    array.immutable(using Unsafe)

extension (bytes: Bytes)
  def javaInputStream: ji.InputStream = new ji.ByteArrayInputStream(bytes.mutable(using Unsafe))

extension [indexable: Indexable](inline value: indexable)
  inline def has(index: indexable.Operand): Boolean = indexable.contains(value, index)

  inline def at(index: indexable.Operand): Optional[indexable.Result] =
    optimizable[indexable.Result]: default =>
      if indexable.contains(value, index) then indexable.access(value, index) else default

extension [value: Segmentable as segmentable](inline value: value)
  inline def segment(interval: Interval): value = segmentable.segment(value, interval)

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

def workingDirectory[path: Instantiable across Paths from Text](using directory: WorkingDirectory)
:     path =

  directory.path[path]

def homeDirectory[path: Instantiable across Paths from Text](using directory: HomeDirectory)
:     path =

  directory.path[path]

def temporaryDirectory[path: Instantiable across Paths from Text]
   (using directory: TemporaryDirectory)
:     path =

  directory.path[path]

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

extension [countable: Countable](inline value: countable)
  inline def ult: Optional[Ordinal] =
    if countable.size(value) >= 1 then (countable.size(value) - 1).z else Unset

  inline def pen: Optional[Ordinal] =
    if countable.size(value) >= 1 then (countable.size(value) - 2).z else Unset

  inline def ant: Optional[Ordinal] =
    if countable.size(value) >= 1 then (countable.size(value) - 3).z else Unset

extension [product <: Product: Mirror.ProductOf](value: product)
  def tuple: product.MirroredElemTypes = Tuple.fromProductTyped(value)

extension [tuple <: Tuple](tuple: tuple)
  def to[product: Mirror.ProductOf]: product = product.fromProduct(tuple)

extension (erased tuple: Tuple)
  inline def keep[nat <: Nat] = !![Tuple.Take[tuple.type, nat]]
  inline def skip[nat <: Nat] = !![Tuple.Drop[tuple.type, nat]]
  inline def contains[element]: Boolean = indexOf[element] >= 0
  inline def indexOf[element]: Int = recurIndex[tuple.type, element](0)

  transparent inline def subtypes[supertype]: Tuple = recurSubtypes[tuple.type, supertype, Zero]

  private transparent inline def recurSubtypes[tuple <: Tuple, supertype, done <: Tuple]: Tuple =
    inline !![tuple] match
      case _: Zero           => !![Tuple.Reverse[done]]
      case _: (head *: tail) => inline !![head] match
        case _: `supertype`     => recurSubtypes[tail, supertype, head *: done]
        case _                  => recurSubtypes[tail, supertype, done]

  private inline def recurIndex[tuple <: Tuple, element](index: Int): Int =
    inline !![tuple] match
      case _: Zero              => -1
      case _: (element *: tail) => index
      case _: (other *: tail)   => recurIndex[tail, element](index + 1)
