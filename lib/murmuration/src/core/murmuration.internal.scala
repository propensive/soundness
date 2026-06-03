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
package murmuration

// Standard-library types are imported under aliases so that nothing in this file
// references the bare `scala` package identifier — otherwise the `.scala` escape-hatch
// extension method (below) would shadow the package throughout `object internal`.
import scala.PartialFunction
import scala.annotation.targetName
import scala.collection.Factory
import scala.collection.Iterable as ScalaIterable
import scala.collection.IterableFactory
import scala.collection.IterableOnce as ScalaIterableOnce
import scala.collection.Iterator as ScalaIterator
import scala.collection.MapFactory
import scala.collection.immutable.List as ScalaList
import scala.collection.immutable.Map as ScalaMap
import scala.collection.immutable.Range
import scala.collection.immutable.ArraySeq as ScalaArraySeq
import scala.collection.immutable.IndexedSeq as ScalaIndexedSeq
import scala.collection.immutable.LazyList as ScalaLazyList
import scala.collection.immutable.Seq as ScalaSeq
import scala.collection.immutable.Set as ScalaSet
import scala.collection.immutable.Vector as ScalaVector
import scala.collection.mutable.Builder
import scala.IArray as ScalaIArray
import scala.Array as ScalaArray
import scala.Predef.is
import scala.Predef.genericArrayOps
import scala.math.Numeric
import scala.math.Ordering
import scala.reflect.ClassTag

object internal:
  // Murmuration's own `IterableOnce` — the root of our collection hierarchy. It erases to scala's
  // `IterableOnce` (only `iterator`/`knownSize`; the rich API lives in `IterableOnceOps`, which
  // `IterableOnce` does not extend), so our own extensions still win and no scala collection
  // methods leak. Interop with scala collections is always explicit, via `.scala`.
  opaque type IterableOnce[+value] = ScalaIterableOnce[value]

  object IterableOnce:
    extension [value](iterableOnce: IterableOnce[value])
      inline def scala: ScalaIterableOnce[value] = iterableOnce
      inline def iterator: ScalaIterator[value] = iterableOnce.iterator

  // `List <: IterableOnce`. Each Murmuration collection (`List`/`Set`/`Series`) carries its own
  // full, self-contained building vocabulary on its companion rather than inheriting a shared one;
  // the iteration/consume vocabulary is provided generically by the `Traversable` typeclass
  // (rudiments). Extending `IterableFactory` also makes the companion a valid target for the
  // standard library's `xs.to(List)` (via scala's implicit `IterableFactory.toFactory`), and
  // supplies `newBuilder`.
  opaque type List[+value] = ScalaList[value]

  object List extends IterableFactory[List]:
    override inline def apply[value](elements: value*): List[value] = ScalaList(elements*)
    override inline def empty[value]: List[value] = ScalaList.empty

    // The inbound counterpart to the `.scala` escape hatch: adopt a standard-library collection
    // as ours. Used where stdlib operations produce a `scala.collection.immutable.List` etc.
    def from[value](source: ScalaIterableOnce[value]): List[value] = ScalaList.from(source)

    def newBuilder[value]: Builder[value, List[value]] = ScalaList.newBuilder[value]

    override inline def fill[value](count: Int)(element: => value): List[value] =
      ScalaList.fill(count)(element)

    override inline def tabulate[value](count: Int)(lambda: Int => value): List[value] =
      ScalaList.tabulate(count)(lambda)

    inline def range(start: Int, end: Int): List[Int] = ScalaList.range(start, end)

    // Equality across our `List` and the standard library's, in both directions (they share a
    // runtime representation). Mirrors the multiversal-equality permissiveness `scala.List` has.
    given canEqual: [left, right] => CanEqual[List[left], List[right]] = CanEqual.derived

    given canEqualScalaRight: [left, right] => CanEqual[List[left], ScalaList[right]] =
      CanEqual.derived

    given canEqualScalaLeft: [left, right] => CanEqual[ScalaList[left], List[right]] =
      CanEqual.derived

    // Enables `case List(a, b, c)`. Returns the underlying List, which the compiler treats as
    // the sequence of bound elements.
    def unapplySeq[value](list: List[value]): ScalaList[value] = list

    extension [value](list: List[value])
      // The escape hatch to the standard library (overrides `Iterable.scala` for the precise
      // `scala.collection.immutable.List` type). Use only where genuine scala interop is required.
      inline def scala: ScalaList[value] = list
      inline def iterator: ScalaIterator[value] = list.iterator
      inline def count(predicate: value => Boolean): Int = list.scala.count(predicate)

      // Collection-returning overrides — same operations as `Iterable`, but returning `List`.
      inline def map[value2](lambda: value => value2): List[value2] = list.scala.map(lambda)

      inline def flatMap[functor[_], value2](lambda: value => functor[value2])
          (using inline expandable: functor is Expandable)
      :   List[value2] =

        list.scala.flatMap { value => expandable.expand(lambda(value)) }

      inline def collect[value2](lambda: PartialFunction[value, value2]): List[value2] =
        list.scala.collect(lambda)

      inline def filter(predicate: value => Boolean): List[value] = list.scala.filter(predicate)
      inline def filterNot(predicate: value => Boolean): List[value] = list.scala.filterNot(predicate)
      inline def withFilter(predicate: value => Boolean): List[value] = list.filter(predicate)

      inline def scanLeft[total](initial: total)(lambda: (total, value) => total): List[total] =
        list.scala.scanLeft(initial)(lambda)

      inline def zip[value2, collection[_]](that: collection[value2])
          (using inline expandable: collection is Expandable)
      :   List[(value, value2)] =
        list.scala.zip(expandable.expand(that))
      inline def zipWithIndex: List[(value, Int)] = list.scala.zipWithIndex

      inline def unzip[left, right](using asPair: value => (left, right))
      :   (List[left], List[right]) =

        list.scala.unzip

      inline def take(count: Int): List[value] = list.scala.take(count)
      inline def takeRight(count: Int): List[value] = list.scala.takeRight(count)
      inline def takeWhile(predicate: value => Boolean): List[value] = list.scala.takeWhile(predicate)
      inline def drop(count: Int): List[value] = list.scala.drop(count)
      inline def dropRight(count: Int): List[value] = list.scala.dropRight(count)
      inline def dropWhile(predicate: value => Boolean): List[value] = list.scala.dropWhile(predicate)
      inline def slice(from: Int, until: Int): List[value] = list.scala.slice(from, until)

      inline def span(predicate: value => Boolean): (List[value], List[value]) =
        list.scala.span(predicate)

      inline def splitAt(index: Int): (List[value], List[value]) = list.scala.splitAt(index)

      inline def partition(predicate: value => Boolean): (List[value], List[value]) =
        list.scala.partition(predicate)

      inline def groupBy[key](lambda: value => key): Map[key, List[value]] =
        list.groupBy(lambda)

      inline def grouped(size: Int): ScalaIterator[List[value]] = list.grouped(size)
      inline def sliding(size: Int): ScalaIterator[List[value]] = list.sliding(size)

      inline def flatten[value2, inner[_]]
          (using ev: value =:= inner[value2], inline expandable: inner is Expandable)
      :   List[value2] =
        List.from(list.scala.iterator.flatMap { item => expandable.expand(ev(item)) })

      inline def tail: List[value] = list.scala.tail
      inline def init: List[value] = list.scala.init

      // Sequence-specific operations (no `Iterable` equivalent), plus the cons operators.
      inline def length: Int = list.length
      inline def apply(index: Int): value = list(index)
      inline def indices: Range = list.indices
      inline def reverse: List[value] = list.scala.reverse
      inline def distinct: List[value] = list.scala.distinct

      inline def sorted[value2 >: value](using ordering: Ordering[value2]): List[value] =
        list.scala.sorted[value2]

      inline def sortBy[key](lambda: value => key)(using ordering: Ordering[key]): List[value] =
        list.scala.sortBy(lambda)

      inline def sortWith(lessThan: (value, value) => Boolean): List[value] =
        list.scala.sortWith(lessThan)

      inline def contains[value2 >: value](element: value2): Boolean = list.contains(element)
      inline def indexOf[value2 >: value](element: value2): Int = list.indexOf(element)
      inline def indexWhere(predicate: value => Boolean): Int = list.indexWhere(predicate)

      inline def startsWith[value2 >: value, collection[_]](that: collection[value2])
          (using inline expandable: collection is Expandable)
      :   Boolean =
        list.startsWith(expandable.expand(that))

      inline def endsWith[value2 >: value](that: ScalaIterable[value2]): Boolean =
        list.endsWith(that)

      // `endsWith` needs an `Iterable` (it traverses twice); passing one of ours requires this
      // overload (no ambiguity — our `List` does not subtype scala's `Iterable`).
      inline def endsWith[value2 >: value](that: List[value2]): Boolean =
        list.endsWith(that: ScalaList[value2])

      inline def `++`[value2 >: value, collection[_]](suffix: collection[value2])
          (using inline expandable: collection is Expandable)
      :   List[value2] =
        list.scala ++ expandable.expand(suffix)
      inline def `:+`[value2 >: value](element: value2): List[value2] = list.scala :+ element

      inline def padTo[value2 >: value](length: Int, element: value2): List[value2] =
        list.scala.padTo(length, element)

      inline def updated[value2 >: value](index: Int, element: value2): List[value2] =
        list.scala.updated(index, element)

      inline def patch[value2 >: value, collection[_]]
          (from: Int, other: collection[value2], replaced: Int)
          (using inline expandable: collection is Expandable)
      :   List[value2] =

        list.scala.patch(from, expandable.expand(other), replaced)

      // `a ::: b` is right-associative, so Scala 3 dispatches it on the LEFT operand (here the
      // receiver), unlike the standard library where `:::` dispatches on the right one.
      inline def `:::`[value2 >: value](rest: List[value2]): List[value2] =
        (list: ScalaList[value2]) ++ rest

      inline def `reverse_:::`[value2 >: value](prefix: List[value2]): List[value2] =
        (list: ScalaList[value2]).reverse_:::(prefix)

    // `::` and `+:` are right-associative too, so they must dispatch on the element — the left
    // operand — which is therefore the extension receiver. A single type parameter (rather than
    // `[value2 >: value]`) still widens correctly — `value` is inferred from the list and the
    // element widens into it — and keeps these to ONE type-parameter clause, which the inliner
    // needs (a second clause produces "illegal repeated type application" in `transparent inline`
    // code such as Fulminate's `Message.apply`). The ascription pins each to the standard
    // library's operator rather than recursing back into these methods.
    extension [value](element: value)
      inline def `::`(list: List[value]): List[value] = element :: (list: ScalaList[value])
      inline def `+:`(list: List[value]): List[value] = element +: (list: ScalaList[value])

  // Enables `case Nil`: matched by equality against the singleton empty list.
  val Nil: List[Nothing] = ScalaList.empty

  // Enables `case head :: tail` via a name-based extractor. `Nel` (non-empty list) is an `AnyVal`
  // whose underlying is the List reference, so it erases completely: a match compiles to static
  // isEmpty/head/tail reads on the bare List — no `Option`, no `Tuple2`, no carrier object. (The
  // accessors must be plain members, not `inline`: the pattern-matcher calls them indirectly and
  // cannot inline them.)
  final class Nel[+value](val underlying: ScalaList[value]) extends scala.AnyVal:
    def isEmpty: Boolean = underlying.isEmpty
    def get: Nel[value] = this
    def _1: value = underlying.head
    def _2: List[value] = underlying.tail

  object `::`:
    inline def unapply[value](list: List[value]): Nel[value] = Nel(list)

  // Extractors for `case init :+ last` and `case head +: tail` (the `::` extractor above covers
  // `head :: tail`). These coexist with the `:+`/`+:` cons extension methods — different owners.
  object `:+`:
    def unapply[value](list: List[value]): Option[(List[value], value)] =
      val scalaList = (list: ScalaList[value])
      if scalaList.isEmpty then None else Some((scalaList.init, scalaList.last))

  object `+:`:
    def unapply[value](list: List[value]): Option[(value, List[value])] =
      val scalaList = (list: ScalaList[value])
      if scalaList.isEmpty then None else Some((scalaList.head, scalaList.tail))

  // `Set <: IterableOnce`. As with `List`, extending `IterableFactory` makes the
  // companion a valid `xs.to(Set)` target and supplies `newBuilder`. Unlike `List`, `Set` is
  // INVARIANT (as the standard library's immutable `Set` is), and it carries no cons or
  // positional-unapply machinery; instead it has the set-algebra operations and a membership
  // `apply` (the standard library's `Set` is also a `value => Boolean`).
  opaque type Set[element] = ScalaSet[element]

  object Set extends IterableFactory[Set]:
    override inline def apply[element](elements: element*): Set[element] = ScalaSet(elements*)
    override inline def empty[element]: Set[element] = ScalaSet.empty
    def from[element](source: ScalaIterableOnce[element]): Set[element] = ScalaSet.from(source)
    def newBuilder[element]: Builder[element, Set[element]] = ScalaSet.newBuilder[element]

    // Equality across our `Set` and the standard library's, in both directions (shared runtime
    // representation), mirroring the multiversal-equality permissiveness `scala.Set` has.
    given canEqual: [left, right] => CanEqual[Set[left], Set[right]] = CanEqual.derived

    given canEqualScalaRight: [left, right] => CanEqual[Set[left], ScalaSet[right]] =
      CanEqual.derived

    given canEqualScalaLeft: [left, right] => CanEqual[ScalaSet[left], Set[right]] =
      CanEqual.derived

    extension [element](set: Set[element])
      // The escape hatch to the standard library (overrides `Iterable.scala` for the precise type).
      inline def scala: ScalaSet[element] = set
      inline def iterator: ScalaIterator[element] = set.scala.iterator
      inline def count(predicate: element => Boolean): Int = set.scala.count(predicate)

      // Membership — `Set` is used both as a collection and (via `apply`) as a `element => Boolean`.
      inline def contains(element: element): Boolean = set.scala.contains(element)
      inline def apply(element: element): Boolean = set.scala.contains(element)

      // Set algebra. Single-element `set + x` / `set - x` are provided by symbolism's universal
      // `+`/`-` extensions, backed by the `Set is Addable`/`Subtractable` instances in symbolism
      // (the collection lives below symbolism, so those instances can't sit in this companion).
      // `incl`/`excl` remain as the direct single-element forms; `++`/`--` are collection union/diff.
      inline def `++`[collection[_]](that: collection[element])
          (using inline expandable: collection is Expandable)
      :   Set[element] =
        set.scala ++ expandable.expand(that)

      inline def `--`[collection[_]](that: collection[element])
          (using inline expandable: collection is Expandable)
      :   Set[element] =
        set.scala -- expandable.expand(that)
      inline def incl(element: element): Set[element] = set.scala + element
      inline def excl(element: element): Set[element] = set.scala - element
      inline def intersect(that: Set[element]): Set[element] = set.scala.intersect(that.scala)
      inline def union(that: Set[element]): Set[element] = set.scala.union(that.scala)
      inline def diff(that: Set[element]): Set[element] = set.scala.diff(that.scala)
      inline def subsetOf(that: Set[element]): Boolean = set.scala.subsetOf(that.scala)

      // Collection-returning overrides — same operations as `Iterable`, but returning `Set`.
      inline def map[element2](lambda: element => element2): Set[element2] = set.scala.map(lambda)

      inline def flatMap[functor[_], element2](lambda: element => functor[element2])
          (using inline expandable: functor is Expandable)
      :   Set[element2] =
        set.scala.flatMap { value => expandable.expand(lambda(value)) }

      inline def collect[element2](lambda: PartialFunction[element, element2]): Set[element2] =
        set.scala.collect(lambda)

      inline def filter(predicate: element => Boolean): Set[element] = set.scala.filter(predicate)

      inline def filterNot(predicate: element => Boolean): Set[element] =
        set.scala.filterNot(predicate)

      inline def flatten[element2, inner[_]]
          (using ev: element =:= inner[element2], inline expandable: inner is Expandable)
      :   Set[element2] =
        Set.from(set.scala.iterator.flatMap { item => expandable.expand(ev(item)) })

      inline def withFilter(predicate: element => Boolean): Set[element] = set.scala.filter(predicate)
      inline def take(count: Int): Set[element] = set.scala.take(count)
      inline def takeWhile(predicate: element => Boolean): Set[element] = set.scala.takeWhile(predicate)
      inline def drop(count: Int): Set[element] = set.scala.drop(count)
      inline def dropWhile(predicate: element => Boolean): Set[element] = set.scala.dropWhile(predicate)
      inline def slice(from: Int, until: Int): Set[element] = set.scala.slice(from, until)

      inline def span(predicate: element => Boolean): (Set[element], Set[element]) =
        set.scala.span(predicate)

      inline def partition(predicate: element => Boolean): (Set[element], Set[element]) =
        set.scala.partition(predicate)

      inline def grouped(size: Int): ScalaIterator[Set[element]] = set.scala.grouped(size)
      inline def zipWithIndex: Set[(element, Int)] = set.scala.zipWithIndex

      inline def zip[element2, collection[_]](that: collection[element2])
          (using inline expandable: collection is Expandable)
      :   Set[(element, element2)] =
        set.scala.zip(expandable.expand(that))

      inline def groupBy[key](lambda: element => key): Map[key, Set[element]] =
        set.scala.groupBy(lambda)

  // `Map` — our opaque mirror of `scala.collection.immutable.Map`, the project's binary (key/value)
  // collection. Fully opaque like `List`/`Set`/`Series`; it iterates `(key, value)` pairs (via its
  // `Traversable` instance) and gets `map + (k -> v)` / `map - k` from symbolism's `Addable`/
  // `Subtractable`. `.get` returns scala `Option`; the positional `at` (Indexable) gives `Optional`.
  opaque type Map[key, +value] = ScalaMap[key, value]

  object Map extends MapFactory[Map]:
    override inline def empty[key, value]: Map[key, value] = ScalaMap.empty
    override inline def apply[key, value](pairs: (key, value)*): Map[key, value] = ScalaMap(pairs*)

    def from[key, value](source: ScalaIterableOnce[(key, value)]): Map[key, value] =
      ScalaMap.from(source)

    def newBuilder[key, value]: Builder[(key, value), Map[key, value]] = ScalaMap.newBuilder

    given canEqual: [key, value, key2, value2] => CanEqual[Map[key, value], Map[key2, value2]] =
      CanEqual.derived

    given canEqualScalaRight: [key, value, key2, value2]
    =>  CanEqual[Map[key, value], ScalaMap[key2, value2]] = CanEqual.derived

    given canEqualScalaLeft: [key, value, key2, value2]
    =>  CanEqual[ScalaMap[key, value], Map[key2, value2]] = CanEqual.derived

    extension [key, value](map: Map[key, value])
      inline def scala: ScalaMap[key, value] = map
      inline def iterator: ScalaIterator[(key, value)] = map.scala.iterator

      inline def apply(key: key): value = map.scala(key)
      inline def get(key: key): Option[value] = map.scala.get(key)

      inline def getOrElse[value2 >: value](key: key, default: => value2): value2 =
        map.scala.getOrElse(key, default)

      inline def contains(key: key): Boolean = map.scala.contains(key)
      inline def isEmpty: Boolean = map.scala.isEmpty
      inline def nonEmpty: Boolean = map.scala.nonEmpty
      inline def size: Int = map.scala.size
      inline def keys: List[key] = List.from(map.scala.keys)
      inline def values: List[value] = List.from(map.scala.values)
      inline def keySet: Set[key] = Set.from(map.scala.keySet)

      inline def updated[value2 >: value](key: key, value: value2): Map[key, value2] =
        map.scala.updated(key, value)

      inline def removed(key: key): Map[key, value] = map.scala.removed(key)

      // `map ++ otherMap` (right-biased merge); `map ++ pairs` adds a collection of `(key, value)`s.
      inline def `++`[value2 >: value](that: Map[key, value2]): Map[key, value2] =
        map.scala ++ that.scala

      @targetName("concatPairs")
      inline def `++`[collection[_]](pairs: collection[(key, value)])
          (using inline expandable: collection is Expandable)
      :   Map[key, value] =
        map.scala ++ expandable.expand(pairs)

      inline def `--`[collection[_]](keys: collection[key])
          (using inline expandable: collection is Expandable)
      :   Map[key, value] =
        map.scala -- expandable.expand(keys)

      inline def map[key2, value2](lambda: ((key, value)) => (key2, value2)): Map[key2, value2] =
        map.scala.map(lambda)

      inline def mapValues[value2](lambda: value => value2): Map[key, value2] =
        map.scala.view.mapValues(lambda).toMap

      inline def filter(predicate: ((key, value)) => Boolean): Map[key, value] =
        map.scala.filter(predicate)

      inline def filterNot(predicate: ((key, value)) => Boolean): Map[key, value] =
        map.scala.filterNot(predicate)

      inline def collect[key2, value2](lambda: PartialFunction[(key, value), (key2, value2)])
      :   Map[key2, value2] =
        map.scala.collect(lambda)

      inline def foreach[result](lambda: ((key, value)) => result): Unit =
        map.scala.foreach(lambda)

      inline def foldLeft[total](initial: total)(lambda: (total, (key, value)) => total): total =
        map.scala.foldLeft(initial)(lambda)

      inline def groupBy[key2](lambda: ((key, value)) => key2): Map[key2, Map[key, value]] =
        map.scala.groupBy(lambda)

      inline def toList: List[(key, value)] = List.from(map.scala)

  // `Series <: IterableOnce` — our opaque mirror of `scala.Vector`: an immutable indexed
  // sequence with efficient random access and both-ended append. Mirrors `List`'s companion (building
  // overrides returning `Series`, the sequence/positional ops, and `:+`/`+:`), but without the
  // linked-list cons (`::`/`:::`) machinery, which is not idiomatic for a `Vector`.
  opaque type Series[+value] = ScalaVector[value]

  object Series extends IterableFactory[Series]:
    override inline def apply[value](elements: value*): Series[value] = ScalaVector(elements*)
    override inline def empty[value]: Series[value] = ScalaVector.empty
    def from[value](source: ScalaIterableOnce[value]): Series[value] = ScalaVector.from(source)
    def newBuilder[value]: Builder[value, Series[value]] = ScalaVector.newBuilder[value]

    override inline def fill[value](count: Int)(element: => value): Series[value] =
      ScalaVector.fill(count)(element)

    override inline def tabulate[value](count: Int)(lambda: Int => value): Series[value] =
      ScalaVector.tabulate(count)(lambda)

    inline def range(start: Int, end: Int): Series[Int] = ScalaVector.range(start, end)

    // Equality across our `Series` and the standard library's `Vector`, in both directions (shared
    // runtime representation), mirroring `List`/`Set`.
    given canEqual: [left, right] => CanEqual[Series[left], Series[right]] = CanEqual.derived

    given canEqualScalaRight: [left, right] => CanEqual[Series[left], ScalaVector[right]] =
      CanEqual.derived

    given canEqualScalaLeft: [left, right] => CanEqual[ScalaVector[left], Series[right]] =
      CanEqual.derived

    // Enables `case Series(a, b, c)`: the underlying vector is a valid `Seq`-shaped `unapplySeq`.
    def unapplySeq[value](series: Series[value]): ScalaIndexedSeq[value] = series

    extension [value](series: Series[value])
      // The escape hatch to the standard library (overrides `Iterable.scala` for the precise type).
      inline def scala: ScalaVector[value] = series
      inline def iterator: ScalaIterator[value] = series.scala.iterator
      inline def count(predicate: value => Boolean): Int = series.scala.count(predicate)

      // Collection-returning overrides — same operations as `Iterable`, but returning `Series`.
      inline def map[value2](lambda: value => value2): Series[value2] = series.scala.map(lambda)

      inline def flatMap[functor[_], value2](lambda: value => functor[value2])
          (using inline expandable: functor is Expandable)
      :   Series[value2] =

        series.scala.flatMap { value => expandable.expand(lambda(value)) }

      inline def collect[value2](lambda: PartialFunction[value, value2]): Series[value2] =
        series.scala.collect(lambda)

      inline def filter(predicate: value => Boolean): Series[value] = series.scala.filter(predicate)

      inline def filterNot(predicate: value => Boolean): Series[value] =
        series.scala.filterNot(predicate)

      inline def withFilter(predicate: value => Boolean): Series[value] = series.filter(predicate)

      inline def scanLeft[total](initial: total)(lambda: (total, value) => total): Series[total] =
        series.scala.scanLeft(initial)(lambda)

      inline def zip[value2, collection[_]](that: collection[value2])
          (using inline expandable: collection is Expandable)
      :   Series[(value, value2)] =
        series.scala.zip(expandable.expand(that))

      inline def zipWithIndex: Series[(value, Int)] = series.scala.zipWithIndex

      inline def unzip[left, right](using asPair: value => (left, right))
      :   (Series[left], Series[right]) =

        series.scala.unzip

      inline def take(count: Int): Series[value] = series.scala.take(count)
      inline def takeRight(count: Int): Series[value] = series.scala.takeRight(count)

      inline def takeWhile(predicate: value => Boolean): Series[value] =
        series.scala.takeWhile(predicate)

      inline def drop(count: Int): Series[value] = series.scala.drop(count)
      inline def dropRight(count: Int): Series[value] = series.scala.dropRight(count)

      inline def dropWhile(predicate: value => Boolean): Series[value] =
        series.scala.dropWhile(predicate)

      inline def slice(from: Int, until: Int): Series[value] = series.scala.slice(from, until)

      inline def span(predicate: value => Boolean): (Series[value], Series[value]) =
        series.scala.span(predicate)

      inline def splitAt(index: Int): (Series[value], Series[value]) = series.scala.splitAt(index)

      inline def partition(predicate: value => Boolean): (Series[value], Series[value]) =
        series.scala.partition(predicate)

      inline def groupBy[key](lambda: value => key): Map[key, Series[value]] =
        series.groupBy(lambda)

      inline def grouped(size: Int): ScalaIterator[Series[value]] = series.grouped(size)
      inline def sliding(size: Int): ScalaIterator[Series[value]] = series.sliding(size)

      inline def flatten[value2, inner[_]]
          (using ev: value =:= inner[value2], inline expandable: inner is Expandable)
      :   Series[value2] =
        Series.from(series.scala.iterator.flatMap { item => expandable.expand(ev(item)) })

      inline def tail: Series[value] = series.scala.tail
      inline def init: Series[value] = series.scala.init

      // Sequence-specific / positional operations.
      inline def length: Int = series.length
      inline def apply(index: Int): value = series(index)
      inline def indices: Range = series.indices
      inline def reverse: Series[value] = series.scala.reverse
      inline def distinct: Series[value] = series.scala.distinct

      inline def sorted[value2 >: value](using ordering: Ordering[value2]): Series[value] =
        series.scala.sorted[value2]

      inline def sortBy[key](lambda: value => key)(using ordering: Ordering[key]): Series[value] =
        series.scala.sortBy(lambda)

      inline def sortWith(lessThan: (value, value) => Boolean): Series[value] =
        series.scala.sortWith(lessThan)

      inline def contains[value2 >: value](element: value2): Boolean = series.contains(element)
      inline def indexOf[value2 >: value](element: value2): Int = series.indexOf(element)
      inline def indexWhere(predicate: value => Boolean): Int = series.indexWhere(predicate)

      inline def startsWith[value2 >: value, collection[_]](that: collection[value2])
          (using inline expandable: collection is Expandable)
      :   Boolean =
        series.startsWith(expandable.expand(that))

      inline def `++`[value2 >: value, collection[_]](suffix: collection[value2])
          (using inline expandable: collection is Expandable)
      :   Series[value2] =
        series.scala ++ expandable.expand(suffix)

      inline def `:+`[value2 >: value](element: value2): Series[value2] = series.scala :+ element

      inline def padTo[value2 >: value](length: Int, element: value2): Series[value2] =
        series.scala.padTo(length, element)

      inline def updated[value2 >: value](index: Int, element: value2): Series[value2] =
        series.scala.updated(index, element)

      inline def patch[value2 >: value, collection[_]]
          (from: Int, other: collection[value2], replaced: Int)
          (using inline expandable: collection is Expandable)
      :   Series[value2] =

        series.scala.patch(from, expandable.expand(other), replaced)

    // `+:` (prepend) is right-associative, so it dispatches on the element (the left operand) — the
    // extension receiver. `@targetName` avoids the erasure clash with `List`'s element-`+:`.
    extension [value](element: value)
      @targetName("prependSeries")
      inline def `+:`(series: Series[value]): Series[value] = element +: (series: ScalaVector[value])

  // `IArray` — our opaque immutable array, aliasing the standard library's. Unlike `List`/`Set` it
  // is NOT `<: Iterable` (the standard library's `IArray` erases to `Array`, not to a
  // `scala.collection.Iterable`), so it carries its own full, self-contained method set. The opaque
  // alias is transparent inside `object internal`, so each `inline` member delegates to the real
  // `scala.IArray` op; array-rebuilding ops carry a `ClassTag`, exactly as `scala.IArray` does.
  // `type Data = IArray[Byte]` (anticipation) builds on this.
  // `IArray` — our opaque immutable array, aliasing the standard library's. Unlike `List`/`Set` it
  // is NOT `<: Iterable`. Because the standard library's own `IArray` ops are *extensions* on the same
  // erased `Array` type, delegating via the public `scala` accessor would re-resolve to these very
  // methods (infinite inline recursion); so each op instead casts to the underlying `Array` and uses
  // its `ArrayOps`, re-wrapping array-returning results as `IArray`. `type Data = IArray[Byte]`.
  opaque type IArray[+element] = ScalaIArray[element]

  object IArray:
    inline def apply[element: ClassTag](elements: element*): IArray[element] = ScalaIArray(elements*)
    inline def empty[element: ClassTag]: IArray[element] = ScalaIArray.empty
    def from[element: ClassTag](source: ScalaIterableOnce[element]): IArray[element] =
      ScalaIArray.from(source)
    inline def fill[element: ClassTag](count: Int)(value: => element): IArray[element] =
      ScalaIArray.fill(count)(value)
    inline def tabulate[element: ClassTag](count: Int)(lambda: Int => element): IArray[element] =
      ScalaIArray.tabulate(count)(lambda)
    inline def unsafeFromArray[element](array: ScalaArray[element]): IArray[element] =
      ScalaIArray.unsafeFromArray(array)

    // Enables `case IArray(a, b, c)`. The result must be `Seq`-shaped (a bare `Array` is not a valid
    // `unapplySeq` product), so wrap the underlying array as an immutable `ArraySeq`.
    def unapplySeq[element](array: IArray[element]): ScalaSeq[element] =
      ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]])

    given canEqual: [left, right] => CanEqual[IArray[left], IArray[right]] = CanEqual.derived

    extension [element](array: IArray[element])
      // The escape hatch: the standard library's immutable `IArray` (with its own ops), for interop.
      inline def scala: ScalaIArray[element] = array

      inline def length: Int = array.asInstanceOf[ScalaArray[element]].length
      inline def size: Int = array.asInstanceOf[ScalaArray[element]].length
      inline def apply(index: Int): element = array.asInstanceOf[ScalaArray[element]](index)
      inline def isEmpty: Boolean = array.asInstanceOf[ScalaArray[element]].isEmpty
      inline def nonEmpty: Boolean = array.asInstanceOf[ScalaArray[element]].nonEmpty
      inline def head: element = array.asInstanceOf[ScalaArray[element]].head
      inline def headOption: Option[element] = array.asInstanceOf[ScalaArray[element]].headOption
      inline def last: element = array.asInstanceOf[ScalaArray[element]].last
      inline def lastOption: Option[element] = array.asInstanceOf[ScalaArray[element]].lastOption
      inline def indices: Range = array.asInstanceOf[ScalaArray[element]].indices
      inline def iterator: ScalaIterator[element] = array.asInstanceOf[ScalaArray[element]].iterator

      inline def foreach[result](lambda: element => result): Unit = array.asInstanceOf[ScalaArray[element]].foreach(lambda)
      inline def forall(predicate: element => Boolean): Boolean = array.asInstanceOf[ScalaArray[element]].forall(predicate)
      inline def exists(predicate: element => Boolean): Boolean = array.asInstanceOf[ScalaArray[element]].exists(predicate)
      inline def find(predicate: element => Boolean): Option[element] = array.asInstanceOf[ScalaArray[element]].find(predicate)
      inline def count(predicate: element => Boolean): Int = array.asInstanceOf[ScalaArray[element]].count(predicate)
      inline def contains[element2 >: element](value: element2): Boolean =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).contains(value)
      inline def indexOf[element2 >: element](value: element2): Int =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).indexOf(value)
      inline def indexWhere(predicate: element => Boolean): Int = array.asInstanceOf[ScalaArray[element]].indexWhere(predicate)
      inline def indexOfSlice[element2 >: element](that: IArray[element2]): Int =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]])
        . indexOfSlice(ScalaArraySeq.unsafeWrapArray(that.asInstanceOf[ScalaArray[element2]]))

      inline def foldLeft[total](initial: total)(lambda: (total, element) => total): total =
        array.asInstanceOf[ScalaArray[element]].foldLeft(initial)(lambda)
      inline def foldRight[total](initial: total)(lambda: (element, total) => total): total =
        array.asInstanceOf[ScalaArray[element]].foldRight(initial)(lambda)
      inline def fold[element2 >: element](initial: element2)(lambda: (element2, element2) => element2)
      :   element2 =
        array.asInstanceOf[ScalaArray[element]].fold(initial)(lambda)
      inline def reduce[element2 >: element](lambda: (element2, element2) => element2): element2 =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).reduce(lambda)
      inline def reduceLeft[element2 >: element](lambda: (element2, element) => element2): element2 =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).reduceLeft(lambda)
      inline def sum[element2 >: element](using Numeric[element2]): element2 =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).sum[element2]
      inline def product[element2 >: element](using Numeric[element2]): element2 =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).product[element2]
      inline def min[element2 >: element](using Ordering[element2]): element =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).min[element2]
      inline def max[element2 >: element](using Ordering[element2]): element =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).max[element2]
      inline def minBy[key](lambda: element => key)(using Ordering[key]): element =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).minBy(lambda)
      inline def maxBy[key](lambda: element => key)(using Ordering[key]): element =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).maxBy(lambda)
      inline def mkString: String =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).mkString
      inline def mkString(separator: String): String =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).mkString(separator)
      inline def mkString(start: String, separator: String, end: String): String =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).mkString(start, separator, end)

      inline def map[element2: ClassTag](lambda: element => element2): IArray[element2] =
        array.asInstanceOf[ScalaArray[element]].map(lambda).asInstanceOf[IArray[element2]]
      inline def flatMap[functor[_], element2: ClassTag](lambda: element => functor[element2])
          (using inline expandable: functor is Expandable)
      :   IArray[element2] =
          array.asInstanceOf[ScalaArray[element]].flatMap { value => expandable.expand(lambda(value)) }
        . asInstanceOf[IArray[element2]]
      inline def collect[element2: ClassTag](lambda: PartialFunction[element, element2]): IArray[element2] =
        array.asInstanceOf[ScalaArray[element]].collect(lambda).asInstanceOf[IArray[element2]]
      inline def zip[element2](that: IArray[element2]): IArray[(element, element2)] =
        array.asInstanceOf[ScalaArray[element]].zip(that.scala).asInstanceOf[IArray[(element, element2)]]
      inline def zipWithIndex: IArray[(element, Int)] =
        array.asInstanceOf[ScalaArray[element]].zipWithIndex.asInstanceOf[IArray[(element, Int)]]

      inline def filter(predicate: element => Boolean): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].filter(predicate).asInstanceOf[IArray[element]]
      inline def filterNot(predicate: element => Boolean): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].filterNot(predicate).asInstanceOf[IArray[element]]
      inline def take(count: Int): IArray[element] = array.asInstanceOf[ScalaArray[element]].take(count).asInstanceOf[IArray[element]]
      inline def takeRight(count: Int): IArray[element] = array.asInstanceOf[ScalaArray[element]].takeRight(count).asInstanceOf[IArray[element]]
      inline def drop(count: Int): IArray[element] = array.asInstanceOf[ScalaArray[element]].drop(count).asInstanceOf[IArray[element]]
      inline def dropRight(count: Int): IArray[element] = array.asInstanceOf[ScalaArray[element]].dropRight(count).asInstanceOf[IArray[element]]
      inline def takeWhile(predicate: element => Boolean): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].takeWhile(predicate).asInstanceOf[IArray[element]]
      inline def dropWhile(predicate: element => Boolean): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].dropWhile(predicate).asInstanceOf[IArray[element]]
      inline def slice(from: Int, until: Int): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].slice(from, until).asInstanceOf[IArray[element]]
      inline def splitAt(index: Int): (IArray[element], IArray[element]) =
        array.asInstanceOf[ScalaArray[element]].splitAt(index).asInstanceOf[(IArray[element], IArray[element])]
      inline def span(predicate: element => Boolean): (IArray[element], IArray[element]) =
        array.asInstanceOf[ScalaArray[element]].span(predicate).asInstanceOf[(IArray[element], IArray[element])]
      inline def tail: IArray[element] = array.asInstanceOf[ScalaArray[element]].tail.asInstanceOf[IArray[element]]
      inline def init: IArray[element] = array.asInstanceOf[ScalaArray[element]].init.asInstanceOf[IArray[element]]
      inline def reverse: IArray[element] = array.asInstanceOf[ScalaArray[element]].reverse.asInstanceOf[IArray[element]]
      inline def distinct: IArray[element] = array.asInstanceOf[ScalaArray[element]].distinct.asInstanceOf[IArray[element]]
      inline def sorted[element2 >: element](using Ordering[element2]): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].sorted[element2].asInstanceOf[IArray[element]]
      inline def sortBy[key](lambda: element => key)(using Ordering[key]): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].sortBy(lambda).asInstanceOf[IArray[element]]
      inline def sortWith(lessThan: (element, element) => Boolean): IArray[element] =
        array.asInstanceOf[ScalaArray[element]].sortWith(lessThan).asInstanceOf[IArray[element]]
      inline def updated[element2 >: element: ClassTag](index: Int, value: element2): IArray[element2] =
        array.asInstanceOf[ScalaArray[element]].updated(index, value).asInstanceOf[IArray[element2]]
      inline def padTo[element2 >: element: ClassTag](length: Int, value: element2): IArray[element2] =
        array.asInstanceOf[ScalaArray[element]].padTo(length, value).asInstanceOf[IArray[element2]]
      inline def grouped(size: Int): ScalaIterator[IArray[element]] =
        array.asInstanceOf[ScalaArray[element]].grouped(size).map(_.asInstanceOf[IArray[element]])
      inline def scan[element2 >: element: ClassTag](initial: element2)(lambda: (element2, element2) => element2)
      :   IArray[element2] =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).scan(initial)(lambda).toArray
        . asInstanceOf[IArray[element2]]
      inline def scanLeft[total: ClassTag](initial: total)(lambda: (total, element) => total): IArray[total] =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).scanLeft(initial)(lambda).toArray
        . asInstanceOf[IArray[total]]

      inline def sameElements[element2 >: element](that: IArray[element2]): Boolean =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]])
        . sameElements(ScalaArraySeq.unsafeWrapArray(that.asInstanceOf[ScalaArray[element2]]))
      inline def lastIndexWhere(predicate: element => Boolean): Int =
        ScalaArraySeq.unsafeWrapArray(array.asInstanceOf[ScalaArray[element]]).lastIndexWhere(predicate)
      inline def groupBy[key](lambda: element => key): Map[key, IArray[element]] =
        array.asInstanceOf[ScalaArray[element]].groupBy(lambda)
        . view.mapValues(_.asInstanceOf[IArray[element]]).toMap

      inline def `++`[element2 >: element: ClassTag](suffix: ScalaIterableOnce[element2]): IArray[element2] =
        (array.asInstanceOf[ScalaArray[element]] ++ suffix).asInstanceOf[IArray[element2]]
      inline def `++`[element2 >: element: ClassTag](suffix: IArray[element2]): IArray[element2] =
        (array.asInstanceOf[ScalaArray[element]] ++ suffix.scala).asInstanceOf[IArray[element2]]
      inline def `:+`[element2 >: element: ClassTag](value: element2): IArray[element2] =
        (array.asInstanceOf[ScalaArray[element]] :+ value).asInstanceOf[IArray[element2]]
      inline def `+:`[element2 >: element: ClassTag](value: element2): IArray[element2] =
        array.asInstanceOf[ScalaArray[element]].prepended(value).asInstanceOf[IArray[element2]]

  // `Expandable` witnesses that some collection `functor[value]` can be expanded into a sequence of
  // its `value`s, WITHOUT `functor` having to subtype our `IterableOnce`. `flatMap` routes its
  // lambda's result through this typeclass rather than relying on subtyping, which is what lets the
  // result be our own `List`, any standard-library collection, or an `Option` interchangeably — and
  // is a step towards `List` no longer needing to extend `Iterable` at all.
  //
  // It is a `Self`-typed typeclass in the Soundness style, but its `Self` is a type CONSTRUCTOR
  // (`type Self[_]`): `flatMap` must dispatch on the collection constructor across every element
  // type, and one instance must cover all standard-library collections via a `<: IterableOnce`
  // bound — both of which need constructor-keyed (not applied-type) resolution. It does not extend
  // `prepositional.Typeclass` (that module sits ABOVE murmuration, and its `Self` is a proper type).
  trait Expandable:
    type Self[_]
    // Deferred (abstract) and NON-inline: a deferred `inline def` cannot be invoked through the
    // typeclass. Each instance overrides it with an `inline def`; `flatMap` summons the instance
    // `using inline`, so the call site is monomorphic and the (identity) expansion is JIT-inlined.
    def expand[value](source: Self[value]): ScalaIterableOnce[value]

  object Expandable:
    given list: List is Expandable = new Expandable:
      type Self[value] = List[value]
      inline def expand[value](source: List[value]): ScalaIterableOnce[value] = source

    given iterableOnce: IterableOnce is Expandable = new Expandable:
      type Self[value] = IterableOnce[value]
      inline def expand[value](source: IterableOnce[value]): ScalaIterableOnce[value] = source

    given iterable: Iterable is Expandable = new Expandable:
      type Self[value] = Iterable[value]
      inline def expand[value](source: Iterable[value]): ScalaIterableOnce[value] = source

    given set: Set is Expandable = new Expandable:
      type Self[value] = Set[value]
      inline def expand[value](source: Set[value]): ScalaIterableOnce[value] = source

    given series: Series is Expandable = new Expandable:
      type Self[value] = Series[value]
      inline def expand[value](source: Series[value]): ScalaIterableOnce[value] = source

    // Our `IArray` is not `<: ScalaIterableOnce` (it erases to a bare `Array`), so it needs its own
    // instance rather than being covered by the `scalaCollection` given below.
    given iarray: IArray is Expandable = new Expandable:
      type Self[value] = IArray[value]
      inline def expand[value](source: IArray[value]): ScalaIterableOnce[value] =
        ScalaArraySeq.unsafeWrapArray(source.asInstanceOf[ScalaArray[value]])

    // Any standard-library collection (List, Seq, Set, Vector, Range, LazyList, Iterator, …).
    given scalaCollection: [collection[item] <: ScalaIterableOnce[item]] => collection is Expandable =
      new Expandable:
        type Self[value] = collection[value]
        inline def expand[value](source: collection[value]): ScalaIterableOnce[value] = source

    // `Option` is not a `scala.IterableOnce`, but is trivially expandable.
    given option: Option is Expandable = new Expandable:
      type Self[value] = Option[value]
      inline def expand[value](source: Option[value]): ScalaIterableOnce[value] = source.iterator

  // `Fabricable` is the target-side dual of `Expandable`: it witnesses that a collection constructor
  // `coll[_]` can be BUILT from a sequence of elements. Together they let `Transformable` (above this
  // module, in rudiments) convert any expandable source into any fabricable target generically, with
  // `scala.collection.Factory` appearing nowhere in the API — each instance bottoms out on the
  // collection's own `from`. Keyed on the constructor via an explicit refinement (no `Of` alias);
  // `fabricate` is a deferred non-inline `def` that instances override with an `inline def`.
  trait Fabricable:
    type Self[_]
    def fabricate[element](source: ScalaIterableOnce[element]): Self[element]

  object Fabricable:
    given list: List is Fabricable = new Fabricable:
      type Self[element] = List[element]
      inline def fabricate[element](source: ScalaIterableOnce[element]): List[element] =
        List.from(source)

    given set: Set is Fabricable = new Fabricable:
      type Self[element] = Set[element]
      inline def fabricate[element](source: ScalaIterableOnce[element]): Set[element] =
        Set.from(source)

    given series: Series is Fabricable = new Fabricable:
      type Self[element] = Series[element]
      inline def fabricate[element](source: ScalaIterableOnce[element]): Series[element] =
        Series.from(source)

    // `Series` aliases `scala.Vector`, so inside `object internal` these two instances have the same
    // underlying `Self`; at every (external) summon site `Series` and `Vector` are distinct types, so
    // `.to[Series]` and `.to[Vector]` resolve unambiguously to their respective instance.
    given vector: Vector is Fabricable = new Fabricable:
      type Self[element] = Vector[element]
      inline def fabricate[element](source: ScalaIterableOnce[element]): Vector[element] =
        Vector.from(source)

    given indexedSeq: ScalaIndexedSeq is Fabricable = new Fabricable:
      type Self[element] = ScalaIndexedSeq[element]
      inline def fabricate[element](source: ScalaIterableOnce[element]): ScalaIndexedSeq[element] =
        ScalaIndexedSeq.from(source)

    given seq: ScalaSeq is Fabricable = new Fabricable:
      type Self[element] = ScalaSeq[element]
      inline def fabricate[element](source: ScalaIterableOnce[element]): ScalaSeq[element] =
        ScalaSeq.from(source)

    given lazyList: ScalaLazyList is Fabricable = new Fabricable:
      type Self[element] = ScalaLazyList[element]
      inline def fabricate[element](source: ScalaIterableOnce[element]): ScalaLazyList[element] =
        ScalaLazyList.from(source)
