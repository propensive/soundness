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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import scala.quoted.*

import anticipation.*
import denominative.*
import prepositional.*
import symbolism.*
import vacuous.*

export rudiments.internal.{Bytes, Digit}

// TODO(capture-checking, #1412): `fixpoint` does not yet type-check under capture checking — the
// recursive context function `fn(using recurrence(fn(_)))` trips "Reference `recur` is not
// included in the allowed capture set". It is currently unused anywhere in the codebase, so it is
// commented out while a capture-correct formulation is found.
//def fixpoint[value](initial: value)(fn: (recur: (value => value)) ?=> (value => value)): value =
//  def recurrence(fn: (recur: value => value) ?=> value => value): value => value =
//    fn(using recurrence(fn(_)))
//
//  recurrence(fn)(initial)

inline def probe[target]: Nothing = ${rudiments.internal.probe[target]}
inline def typeName[target]: Text = ${rudiments.internal.name[target]}
inline def reflectClass[target]: Class[target] = ${rudiments.internal.reflectClass}
inline def that[result](inline block: => result): result = block
inline def state[value](using value: value aka "state"): value = value()
inline def next[value](using value: value aka "next"): value = value()
inline def prior[value](using value: value aka "prior"): value = value()
inline def ordinal(using value: Ordinal aka "ordinal"): Ordinal = value()

inline def repeat(count: Int)(inline action: => Unit): Unit =
  var i = 0

  while i < count do
    action
    i += 1

extension [value](value: value)
  def unit: Unit = ()
  def waive: Any => value = _ => value
  def twin: (value, value) = (value, value)
  def triple: (value, value, value) = (value, value, value)

  inline def when(inline predicate: Boolean)(inline lambda: value => value): value =
    if predicate then lambda(value) else value

  inline def when(inline predicate: value => Boolean)(inline lambda: value => value): value =
    if predicate(value) then lambda(value) else value

  inline def typed[ValueSubtype <: value]: Boolean = value.isInstanceOf[ValueSubtype]

  transparent inline def matchable(using erased Unsafe): value & Matchable =
    value.asInstanceOf[value & Matchable]

  def give[result](block: value ?=> result): result = block(using value)

extension [value: Countable](value: value)
  inline def occupied: Optional[value] = if value.nil then Unset else value

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

extension [input, result](inline lambda: input => result)
  inline infix def and (inline input2: => input): result = lambda(input2)
  inline def context: input ?=> result = (input: input) ?=> lambda(input)

def loop(block: => Unit): Loop^{block} =
  def lambda(): Unit = block
  Loop(lambda)

export rudiments.internal.&

@targetName("erasedValue")
inline def !! [erasure]: erasure = caps.unsafe.unsafeErasedValue

extension [value <: Matchable](iterable: Iterable[value])
  transparent inline def sift[filter <: value: Typeable]: Iterable[filter] =
    iterable.flatMap(filter.unapply(_))

  transparent inline def weave(right: Iterable[value]): Iterable[value] =
    iterable.zip(right).flatMap(Iterable(_, _))

// `collection.has(value)` (value membership) for any `collection` that is
// `Inclusive`; the queried value type is fixed by the instance's `Operand`.
// Whether a *key/index* is present is `Indexable`'s `defines` instead.
extension [self](self: self)(using inclusive: self is Inclusive)
  def has(value: inclusive.Operand): Boolean = inclusive.has(self, value)

// Element-oriented queries over any `Traversable` value. `seek` returns the first
// element satisfying a predicate; `where` returns the *index* (`Ordinal`) of the
// first element satisfying a predicate. (A value-taking overload of `where` is
// deliberately omitted: a path-dependent `traversable.Operand` makes it ambiguous
// with the predicate form for any lambda argument, so `where(_ == value)` is the
// idiom for locating a specific element.)
extension [self](self: self)(using traversable: self is Traversable)
  def seek(predicate: traversable.Operand => Boolean): Optional[traversable.Operand] =
    traversable.traverse(self).find(predicate) match
      case Some(element) => element
      case None          => Unset

  def where(predicate: traversable.Operand => Boolean): Optional[Ordinal] =
    traversable.traverse(self).zipWithIndex.find { (element, _) => predicate(element) } match
      case Some((_, index)) => index.z
      case None             => Unset

  // `subsumes` tests whether `subsequence` occurs as a contiguous run of elements
  // within `self` — a substring, for `Text`. The empty subsequence is always
  // present. (Element membership, by contrast, is `has` via `Inclusive`.)
  def subsumes(subsequence: self): Boolean =
    val whole = Vector.from(traversable.traverse(self))
    val part  = Vector.from(traversable.traverse(subsequence))
    val last  = whole.length - part.length

    part.isEmpty || whole.indices.exists: start =>
      start <= last && part.indices.forall: offset =>
        whole(start + offset) == part(offset)

extension [value](iterator: Iterator[value])
  transparent inline def each(predicate: Ordinal aka "ordinal" ?=> value => Unit): Unit =
    var ordinal: Ordinal = Prim

    iterator.foreach: value =>
      predicate(using ordinal.aka["ordinal"])(value)
      ordinal += 1

  inline def all(predicate: value => Boolean): Boolean = iterator.forall(predicate)

extension [value](iterables: Iterable[Iterable[value]])
  def intercalate(between: Iterable[value] = Iterable()): Iterable[value] =
    if iterables.nil then Iterable() else iterables.reduceLeft(_ ++ between ++ _)

extension [value](iterable: Iterable[value])
  // The `Result` of each numeric typeclass below is bound as an inferred type parameter (e.g.
  // `to result` + `result =:= value`) rather than referenced path-dependently (`addable.Result =:=
  // value`). Under capture checking the path-dependent form fails at the cross-package `export`
  // forwarder (#1411); binding it as a type parameter keeps call sites unchanged.
  transparent inline def total[result]
    ( using addable:  value is Addable by value to result,
            equality: result =:= value )
  :   Optional[value] =

    compiletime.summonFrom:
      case zeroic: ((? <: value) is Zeroic) => iterable.foldLeft(zeroic.zero)(addable.add)

      case _ =>
        if iterable.nil then Unset else iterable.tail.foldLeft(iterable.head)(addable.add)


  transparent inline def mean[addResult, divResult]
    ( using addable:   value is Addable by value to addResult,
            equality:  addResult =:= value,
            divisible: value is Divisible by Double to divResult,
            eqality2:  divResult =:= value )
  :   Optional[value] =

    iterable.total.let(_/iterable.size.toDouble)

  inline def mean2[subResult, addResult, divResult, add2Result]
    ( using subtractable: value is Subtractable by value to subResult,
            addable:      subResult is Addable by subResult to addResult,
            equality:     addResult =:= subResult,
            divisible:    subResult is Divisible by Double to divResult,
            equality2:    divResult =:= subResult,
            addable2:     value is Addable by divResult to add2Result,
            equality3:    add2Result =:= value )
  :   Optional[value] =

    if iterable.nil then Unset else
      val arbitrary = iterable.head

      iterable.map(_ - arbitrary).total.let: total =>
        arbitrary + total/iterable.size.toDouble

  def variance[addResult, divResult, subResult, mulResult, add2Result, div2Result]
    ( using addable:       value is Addable by value to addResult,
            equality:      addResult =:= value,
            divisible:     value is Divisible by Double to divResult,
            equality2:     divResult =:= value,
            subtractable:  value is Subtractable by value to subResult,
            multiplicable: subResult is Multiplicable by subResult to mulResult,
            addable2:      mulResult is Addable by mulResult to add2Result,
            zeroic2:       mulResult is Zeroic,
            equality3:     add2Result =:= mulResult,
            divisible2:    mulResult is Divisible by Double to div2Result )
  :   Optional[div2Result] =

    iterable.mean.let: mean =>
      iterable.map(_ - mean).map { value => value*value }.total/iterable.size.toDouble


  def std[addResult, divResult, div2Result, mulResult]
    ( using addable:       value is Addable by value to addResult,
            equality:      addResult =:= value,
            divisible:     value is Divisible by Double to divResult,
            equality2:     divResult =:= value,
            divisible2:    value is Divisible by value to div2Result,
            equality3:     div2Result =:= Double,
            multiplicable: value is Multiplicable by Double to mulResult,
            equality4:     mulResult =:= value )
  :   Optional[value] =

    iterable.mean.let: mean0 =>
      val mean: value = mean0
      val divisor: value = iterable.head
      var sum: Double = 0.0
      val mean2: Double = mean/divisor

      for item <- iterable do
        val x: Double = item/divisor
        val y: Double = x - mean2
        sum += y*y

      divisor*math.sqrt(sum/iterable.size.toDouble)


  def product[mulResult]
    ( using unital:        value is Unital,
            multiplicable: value is Multiplicable by value to mulResult,
            equality:      mulResult =:= value )
  :   value =

    iterable.foldLeft(unital.one)(multiplicable.multiply)


  transparent inline def each(lambda: Ordinal aka "ordinal" ?=> value => Unit): Unit =
    var ordinal: Ordinal = Prim

    iterable.iterator.foreach: value =>
      lambda(using ordinal.aka["ordinal"])(value)
      ordinal += 1

  // The explicit `Iterable[Tuple]` return type is load-bearing under capture checking: without it
  // the inferred type pickled into the cross-package `export` forwarder carries a malformed capture
  // set and crashes the compiler (#1410). `transparent` still refines the type at each call site.
  transparent inline def annex[right](lambda: value => right): Iterable[Tuple] =
    iterable.map: item =>
      inline !![value] match
        case tuple: Tuple => tuple :* lambda(tuple)
        case other        => (other, lambda(other))


  inline def fuse[state](base: state)(lambda: (state aka "state", value aka "next") ?=> state)
  :   state =

    val iterator: Iterator[value] = iterable.iterator
    var state: state = base

    while iterator.hasNext
    do state = lambda(using state.aka["state"], iterator.next().aka["next"])

    state


  def sumBy[number: Numeric](lambda: value => number): number =
    var count = number.zero

    iterable.foreach: value =>
      count = number.plus(count, lambda(value))

    count

  inline def all(predicate: value => Boolean): Boolean = iterable.forall(predicate)
  transparent inline def bi: Iterable[(value, value)] = iterable.map: value => (value, value)

  transparent inline def tri: Iterable[(value, value, value)] =
    iterable.map: value => (value, value, value)

  def indexBy[value2](lambda: value => value2): Map[value2, value] =
    iterable.map: value =>
      (lambda(value), value)

    . to(Map)

  def longestTrain(predicate: value => Boolean): (Int, Int) =
    @tailrec
    def recur(index: Int, iterable: Iterable[value], bestStart: Int, bestLength: Int, length: Int)
    :   (Int, Int) =

      if iterable.nil then (bestStart, bestLength) else
        if predicate(iterable.head) then
          if length >= bestLength
          then recur(index + 1, iterable.tail, index - length, length + 1, length + 1)
          else recur(index + 1, iterable.tail, bestStart, bestLength, length + 1)
        else
          recur(index + 1, iterable.tail, bestStart, bestLength, 0)

    recur(0, iterable, 0, 0, 0)

extension [element](value: IArray[element])
  inline def mutable(using erased Unsafe): Array[element] = value.asInstanceOf[Array[element]]

extension [element](array: Array[element])
  inline def immutable(using erased Unsafe): IArray[element] = array.asInstanceOf[IArray[element]]

  def snapshot(using ClassTag[element]): IArray[element] =
    val newArray = new Array[element](array.length)
    System.arraycopy(array, 0, newArray, 0, array.length)
    newArray.immutable(using Unsafe)

  inline def place(value: IArray[element], ordinal: Ordinal = Prim): Unit =
    System.arraycopy(value.asInstanceOf[Array[element]], 0, array, ordinal.n0, value.length)

extension [key, value](map: sc.Map[key, value])
  inline def defines(key: key): Boolean = map.contains(key)
  inline def bijection: Bijection[key, value] = Bijection(map.to(Map))

extension [key, value](map: Map[key, value])
  def upsert(key: key, optional: Optional[value] => value): Map[key, value] =
    map.updated(key, optional(if map.defines(key) then map(key) else Unset))

  def collate(right: Map[key, value])(merge: (value, value) => value): Map[key, value] =
    right.fuse(map)(state.updated(next(0), state.get(next(0)).fold(next(1))(merge(_, next(1)))))

extension [key, value](map: scm.Map[key, value])
  inline def establish(key: key)(evaluate: => value): value = map.getOrElseUpdate(key, evaluate)

extension [key, value](map: Map[key, List[value]])
  def plus(key: key, value: value): Map[key, List[value]] =
    map.updated(key, map.get(key).fold(List(value))(value :: _))

extension [value](list: List[value])
  def unwind(tail: List[value]): List[value] = tail.reverse_:::(list)

extension [element](sequence: Seq[element])
  def runs: List[List[element]] = runsBy(identity)

  inline def prim: Optional[element] = if sequence.nil then Unset else sequence.head
  inline def sec: Optional[element] = if sequence.length < 2 then Unset else sequence(1)
  inline def ter: Optional[element] = if sequence.length < 3 then Unset else sequence(2)
  inline def unique: Optional[element] = if sequence.length == 1 then sequence.head else Unset

  def runsBy(lambda: element => Any): List[List[element]] =
    @tailrec
    def recur(current: Any, todo: Seq[element], run: List[element], done: List[List[element]])
    :   List[List[element]] =

      if todo.nil then (run.reverse :: done).reverse
      else
        val focus = lambda(todo.head)

        if current == focus then recur(current, todo.tail, todo.head :: run, done)
        else recur(focus, todo.tail, List(todo.head), run.reverse :: done)

    if sequence.nil then Nil
    else recur(lambda(sequence.head), sequence.tail, List(sequence.head), Nil)

extension (bytes: Data)
  def javaInputStream: ji.InputStream = new ji.ByteArrayInputStream(bytes.mutable(using Unsafe))

extension [indexable: Indexable](value: indexable)
  inline def defines(index: indexable.Operand): Boolean = indexable.contains(value, index)

  // A single `at` that dispatches at compile time on the index type: an index statically known to
  // be confined to *this* `value` (an `Operand in value.type`, hence in range) returns a bare
  // `Result`; any other index is bounds-checked and returns `Optional`. The declared return type is
  // `Optional`, so non-reducing (e.g. generic) call sites are safe; a confined index narrows to a
  // bare `Result`.
  transparent inline def at[index](ordinal: index)(using sub: index <:< indexable.Operand)
  :   Optional[indexable.Result] =

    summonFrom:
      case _: (`index` <:< (indexable.Operand in value.type)) =>
        indexable.access(value, sub(ordinal))

      case _ =>
        val key: indexable.Operand = sub(ordinal)

        optimizable[indexable.Result]: default =>
          if indexable.contains(value, key) then indexable.access(value, key) else default

extension [indexable: Indexable by Ordinal](inline value: indexable)
  inline def prim: Optional[indexable.Result] = value.at(Prim)
  inline def sec: Optional[indexable.Result] = value.at(Sec)
  inline def ter: Optional[indexable.Result] = value.at(Ter)

extension [value: Segmentable as segmentable](inline value: value)
  inline def segment(interval: Interval): value = segmentable.segment(value, interval)

extension (bs: Int)
  def b: Bytes = Bytes(bs)
  def kib: Bytes = Bytes(bs*1024L)
  def mib: Bytes = Bytes(bs*1024L*1024)
  def gib: Bytes = Bytes(bs*1024L*1024*1024)
  def tib: Bytes = Bytes(bs*1024L*1024*1024*1024)

extension (bs: Long)
  def b: Bytes = Bytes(bs)
  def kib: Bytes = Bytes(bs*1024)
  def mib: Bytes = Bytes(bs*1024*1024)
  def gib: Bytes = Bytes(bs*1024*1024*1024)
  def tib: Bytes = Bytes(bs*1024*1024*1024*1024)

extension (data: Data)
  def bytes: Bytes = Bytes(data.size)


extension [countable: Countable](inline value: countable)
  inline def limit: Ordinal = countable.size(value).z

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
  @unexported
  inline def contains[element]: Boolean = indexOf[element] >= 0
  @unexported
  inline def indexOf[element]: Int = recurIndex[tuple.type, element](0)

  @unexported
  transparent inline def subtypes[supertype]: Tuple = recurSubtypes[tuple.type, supertype, Zero]

  private transparent inline def recurSubtypes[tuple <: Tuple, supertype, done <: Tuple]: Tuple =
    inline !![tuple] match
      case _: Zero => !![Tuple.Reverse[done]]

      case _: (head *: tail) =>
        inline !![head] match
          case _: `supertype`     => recurSubtypes[tail, supertype, head *: done]
          case _                  => recurSubtypes[tail, supertype, done]

  private inline def recurIndex[tuple <: Tuple, element](index: Int): Int =
    inline !![tuple] match
      case _: Zero              => -1
      case _: (element *: tail) => index
      case _: (other *: tail)   => recurIndex[tail, element](index + 1)

extension (using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  @unexported
  inline def literal
    [ primitive
      <:  Boolean | Byte | Short | Int | Long | Float | Double | Char | String | Unit | Null ]
  :   Optional[primitive] =

    import quotes.reflect.*

    repr.match
      case constant: ConstantType => constant
      case _                      => Unset

    . let: value =>

        inline !![primitive] match
          case _: Boolean =>
            value.constant.match
              case BooleanConstant(value) => value
              case _                      => Unset

          case _: Byte =>
            value.constant match
              case ByteConstant(value)    => value
              case _                      => Unset

          case _: Short =>
            value.constant match
              case ShortConstant(value)   => value
              case _                      => Unset

          case _: Int =>
            value.constant match
              case IntConstant(value)     => value
              case _                      => Unset

          case _: Long =>
            value.constant match
              case LongConstant(value)    => value
              case _                      => Unset

          case _: Float =>
            value.constant match
              case FloatConstant(value)   => value
              case _                      => Unset

          case _: Double =>
            value.constant match
              case DoubleConstant(value)  => value
              case _                      => Unset

          case _: Char =>
            value.constant match
              case CharConstant(value)    => value
              case _                      => Unset

          case _: String =>
            value.constant match
              case StringConstant(value)  => value
              case _                      => Unset

          case _: Unit =>
            value.constant match
              case UnitConstant()         => ()
              case _                      => Unset

          case _: Null =>
            value.constant match
              case NullConstant()         => null
              case _                      => Unset

    . asInstanceOf[Optional[primitive]]
