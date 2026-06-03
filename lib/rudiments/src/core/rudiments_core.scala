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

import scala.language.dynamics

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

def fixpoint[value](initial: value)(fn: (recur: (value => value)) ?=> (value => value)): value =
  def recurrence(fn: (recur: value => value) ?=> value => value): value => value =
    fn(using recurrence(fn(_)))

  recurrence(fn)(initial)

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

def loop(block: => Unit): Loop =
  Loop({ () => block })


export rudiments.internal.&

@targetName("erasedValue")
inline def !! [erasure]: erasure = scala.caps.unsafe.unsafeErasedValue

extension [traversable: Traversable](value: traversable)
  inline def sift[filter: Typeable]: List[filter] =
    List.from(traversable.iterator(value).flatMap(filter.unapply(_)))

  inline def where(inline predicate: traversable.Operand => Boolean)
  :   Optional[traversable.Operand] =

    traversable.iterator(value).find(predicate).getOrElse(Unset)

  inline def weave(right: List[traversable.Operand]): List[traversable.Operand] =
    val pairs = traversable.iterator(value).zip(right.scala.iterator)
    List.from(pairs.flatMap { (a, b) => Iterator(a, b) })

// `has` (element membership) stays on concrete receivers: a typeclass-generic version would clash
// irreconcilably with the equally-generic `Indexable`-based `has` (index validity, used by `Map`).
extension [value <: Matchable](list: List[value])
  @targetName("hasList")
  inline def has(value: value): Boolean = list.scala.contains(value)

extension [element <: Matchable](set: Set[element])
  @targetName("hasSet")
  inline def has(value: element): Boolean = set.scala.contains(value)

extension [value <: Matchable](series: Series[value])
  @targetName("hasSeries")
  inline def has(value: value): Boolean = series.scala.contains(value)

extension [element <: Matchable](iarray: IArray[element])
  @targetName("hasIArray")
  inline def has(value: element): Boolean = iarray.exists(_ == value)

extension [value <: Matchable](iterable: scala.collection.Iterable[value])
  @targetName("hasSeq")
  inline def has(value: value): Boolean = iterable.exists(_ == value)

extension [element](array: IArray[element])
  transparent inline def to[Form <: AnyKind](using transformable: IArray[element] is Transformable in Form)
  :   transformable.Result =
    transformable.transform(array)

extension [element <: Matchable](array: Array[element])
  @targetName("hasArray")
  inline def has(value: element): Boolean = array.exists(_ == value)

extension [value](lists: List[List[value]])
  def intercalate(between: List[value] = List()): List[value] =
    if lists.nil then List() else lists.reduceLeft(_ ++ between ++ _)

// Type-directed conversion: `coll.to(List)`, `coll.to(Set)`, `coll.to(Map)`, … resolved through the
// `Transformable` typeclass, provided for every iterable via `Traversable`. `IArray` has its own,
// more specific, block above.
extension [traversable: Traversable](value: traversable)
  transparent inline def to[Form <: AnyKind](using transformable: traversable is Transformable in Form)
  :   transformable.Result =
    transformable.transform(value)

// `size` is provided via the `Sizable` typeclass (so a single `size` extension serves both
// collections and other sizable things like `Path`, avoiding same-name extension clashes).
extension [sizable: Sizable](value: sizable)
  inline def size: Int = sizable.size(value)

extension [traversable: Traversable](value: traversable)
  transparent inline def total
    ( using addable:  traversable.Operand is Addable by traversable.Operand,
            equality: addable.Result =:= traversable.Operand )
  :   Optional[traversable.Operand] =

    scala.compiletime.summonFrom:
      case zeroic: ((? <: traversable.Operand) is Zeroic) =>
        value.foldLeft(zeroic.zero)(addable.add)

      case _ =>
        val iterator = traversable.iterator(value)
        if !iterator.hasNext then Unset else iterator.foldLeft(iterator.next())(addable.add)


  transparent inline def mean
    ( using addable:   traversable.Operand is Addable by traversable.Operand,
            equality:  addable.Result =:= traversable.Operand,
            divisible: traversable.Operand is Divisible by Double,
            eqality2:  divisible.Result =:= traversable.Operand )
  :   Optional[traversable.Operand] =

    value.total.let(_/traversable.iterator(value).size.toDouble)

  inline def mean2
    ( using subtractable: traversable.Operand is Subtractable by traversable.Operand,
            addable:      subtractable.Result is Addable by subtractable.Result,
            equality:     addable.Result =:= subtractable.Result,
            divisible:    subtractable.Result is Divisible by Double,
            equality2:    divisible.Result =:= subtractable.Result,
            addable2:     traversable.Operand is Addable by divisible.Result,
            equality3:    addable2.Result =:= traversable.Operand )
  :   Optional[traversable.Operand] =

    if !traversable.iterator(value).hasNext then Unset else
      val arbitrary = value.head

      List.from(traversable.iterator(value).map(_ - arbitrary)).total.let: total =>
        arbitrary + total/traversable.iterator(value).size.toDouble

  def variance
    ( using addable:       traversable.Operand is Addable by traversable.Operand,
            equality:      addable.Result =:= traversable.Operand,
            divisible:     traversable.Operand is Divisible by Double,
            equality2:     divisible.Result =:= traversable.Operand,
            subtractable:  traversable.Operand is Subtractable by traversable.Operand,
            multiplicable: subtractable.Result is Multiplicable by subtractable.Result,
            addable2:      multiplicable.Result is Addable by multiplicable.Result,
            zeroic2:       multiplicable.Result is Zeroic,
            equality3:     addable2.Result =:= multiplicable.Result,
            divisible2:    multiplicable.Result is Divisible by Double )
  :   Optional[divisible2.Result] =

    value.mean.let: mean =>
      val deviations = traversable.iterator(value).map(_ - mean)
      List.from(deviations.map { item => item*item }).total/traversable.iterator(value).size.toDouble


  def std
    ( using addable:       traversable.Operand is Addable by traversable.Operand,
            equality:      addable.Result =:= traversable.Operand,
            divisible:     traversable.Operand is Divisible by Double,
            equality2:     divisible.Result =:= traversable.Operand,
            divisible2:    traversable.Operand is Divisible by traversable.Operand,
            equality3:     divisible2.Result =:= Double,
            multiplicable: traversable.Operand is Multiplicable by Double,
            equality4:     multiplicable.Result =:= traversable.Operand )
  :   Optional[traversable.Operand] =

    value.mean.let: mean0 =>
      val mean: traversable.Operand = mean0
      val divisor: traversable.Operand = value.head
      var sum: Double = 0.0
      val mean2: Double = mean/divisor

      value.each: item =>
        val x: Double = item/divisor
        val y: Double = x - mean2
        sum += y*y

      divisor*scala.math.sqrt(sum/traversable.iterator(value).size.toDouble)


  def product
    ( using unital:        traversable.Operand is Unital,
            multiplicable: traversable.Operand is Multiplicable by traversable.Operand,
            equality:      multiplicable.Result =:= traversable.Operand )
  :   traversable.Operand =

    value.foldLeft(unital.one)(multiplicable.multiply)


  transparent inline def each(lambda: Ordinal aka "ordinal" ?=> traversable.Operand => Unit): Unit =
    var ordinal: Ordinal = Prim

    traversable.iterator(value).foreach: item =>
      lambda(using ordinal.aka["ordinal"])(item)
      ordinal += 1

  transparent inline def annex[right](lambda: traversable.Operand => right) =
    List.from(traversable.iterator(value)).map: item =>
      inline !![traversable.Operand] match
        case tuple: Tuple => tuple :* lambda(tuple)
        case other        => (other, lambda(other))


  inline def fuse[state](base: state)
    ( lambda: (state aka "state", traversable.Operand aka "next") ?=> state )
  :   state =

    val iterator: Iterator[traversable.Operand] = traversable.iterator(value)
    var state: state = base

    while iterator.hasNext
    do state = lambda(using state.aka["state"], iterator.next().aka["next"])

    state


  inline def all(predicate: traversable.Operand => Boolean): Boolean =
    traversable.iterator(value).forall(predicate)

  inline def bi: List[(traversable.Operand, traversable.Operand)] =
    List.from(traversable.iterator(value)).map: item => (item, item)

  inline def tri: List[(traversable.Operand, traversable.Operand, traversable.Operand)] =
    List.from(traversable.iterator(value)).map: item => (item, item, item)

  def indexBy[value2](lambda: traversable.Operand => value2): Map[value2, traversable.Operand] =
    List.from(traversable.iterator(value)).map { item => (lambda(item), item) }.to[Map]

  def longestTrain(predicate: traversable.Operand => Boolean): (Int, Int) =
    val iterator = traversable.iterator(value)
    var index = 0
    var length = 0
    var bestStart = 0
    var bestLength = 0

    while iterator.hasNext do
      if predicate(iterator.next()) then
        if length >= bestLength then
          bestStart = index - length
          bestLength = length + 1

        length += 1
      else
        length = 0

      index += 1

    (bestStart, bestLength)

// `sumBy` names the element type explicitly (via the `Operand = element` refinement) so a lambda
// like `_.length` resolves member access even when the element type is itself abstract.
extension [self, element](value: self)(using traversable: self is Traversable { type Operand = element })
  def sumBy[number: Numeric](lambda: element => number): number =
    var count = number.zero

    traversable.iterator(value).foreach: item =>
      count = number.plus(count, lambda(item))

    count

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
  inline def has(key: key): Boolean = map.contains(key)
  inline def bijection: Bijection[key, value] = Bijection(map.to(Map))

extension [key, value](map: Map[key, value])
  def upsert(key: key, optional: Optional[value] => value): Map[key, value] =
    map.updated(key, optional(if map.has(key) then map(key) else Unset))

  def collate(right: Map[key, value])(merge: (value, value) => value): Map[key, value] =
    List.from(right.scala).fuse(map)(state.updated(next(0), state.get(next(0)).fold(next(1))(merge(_, next(1)))))

extension [key, value](map: scm.Map[key, value])
  inline def establish(key: key)(evaluate: => value): value = map.getOrElseUpdate(key, evaluate)

extension [key, value](map: Map[key, List[value]])
  def plus(key: key, value: value): Map[key, List[value]] =
    map.updated(key, map.get(key).fold(List(value))(value :: _))

extension [value](list: List[value])
  def unwind(tail: List[value]): List[value] = tail.reverse_:::(list)

extension [traversable: Traversable](value: traversable)
  inline def prim: Optional[traversable.Operand] =
    val iterator = traversable.iterator(value)
    if iterator.hasNext then iterator.next() else Unset

  inline def sec: Optional[traversable.Operand] =
    val iterator = traversable.iterator(value).drop(1)
    if iterator.hasNext then iterator.next() else Unset

  inline def ter: Optional[traversable.Operand] =
    val iterator = traversable.iterator(value).drop(2)
    if iterator.hasNext then iterator.next() else Unset

  inline def unique: Optional[traversable.Operand] =
    val iterator = traversable.iterator(value)

    if iterator.hasNext then
      val first = iterator.next()
      if iterator.hasNext then Unset else first
    else
      Unset

extension [element](sequence: List[element])
  def runs: List[List[element]] = runsBy(identity)

  def runsBy(lambda: element => Any): List[List[element]] =
    @tailrec
    def recur(current: Any, todo: List[element], run: List[element], done: List[List[element]])
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

extension [indexable: Indexable](inline value: indexable)
  inline def has(index: indexable.Operand): Boolean = indexable.contains(value, index)

  inline def at(index: indexable.Operand): Optional[indexable.Result] =
    optimizable[indexable.Result]: default =>
      if indexable.contains(value, index) then indexable.access(value, index) else default


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
  inline def keep[nat <: Nat] = !![Tuple.Take[tuple.type, nat]]
  inline def skip[nat <: Nat] = !![Tuple.Drop[tuple.type, nat]]
  inline def contains[element]: Boolean = indexOf[element] >= 0
  inline def indexOf[element]: Int = recurIndex[tuple.type, element](0)

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
