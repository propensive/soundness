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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import java.lang as jl
import java.util as ju

import scala.annotation.*
import scala.compiletime.summonFrom
import scala.reflect.ClassTag

import denominative.*
import murmuration.{Iterator, Reshapable, SortAlgorithm, Traversable}
import prepositional.*
import symbolism.*
import vacuous.*

// The sorting operations live here rather than in murmuration, beside the other operations over
// `Traversable`, because sizing the scratch array needs `Countable`, which is denominative's and
// so above murmuration — the same reason `each`, `seek`, `least` and `most` are here.
//
// They are in one file because `sort` is two overloads: an immutable collection answers with a
// new collection, while a mutable array rearranges itself. Overloads must be declared together.
//
// The element type is a type parameter rather than the path-dependent `traversable.Operand`: the
// implicit scope of a dependent type does not include the element's companion, so an instance
// placed there (as most are) would not be found for the `Comparable` these demand.
//
// `Stable` receivers only: sorting an unordered shape (`Set`, `Map`) is honestly unavailable
// rather than silently order-dropping. Which algorithm sorts is the `SortAlgorithm` in scope,
// chosen by importing one from `sortingAlgorithms`.
extension [self, element](self: self)(using traversable: self is Traversable by element)
  // Sorting by a projection of each element. A comparator overload (the stdlib's `sortWith`)
  // cannot join this name: a two-parameter lambda would resolve to it by arity, breaking
  // parameter untupling on pair-element receivers (`map.order { (key, value) => key }`).
  inline def order[key, result](lambda: element => key)
    ( using comparable: key is Comparable,
            algorithm:  SortAlgorithm,
            reshapable: self is Reshapable.Stable by element to result )
  :   result =

    reshapable.reshape:
      sortingInternal.ordered(algorithm, traversable.traverse(self), lambda, comparable, sizeHint)

  // Sorting by the elements' own order, the no-key sibling of `order(lambda)`. Elements are
  // their own keys here, so this pays for none of the decoration `order` needs.
  inline def sort[result]
    ( using comparable: element is Comparable,
            algorithm:  SortAlgorithm,
            reshapable: self is Reshapable.Stable by element to result )
  :   result =

    reshapable.reshape
      ( sortingInternal.sorted(algorithm, traversable.traverse(self), comparable, sizeHint) )

  // The element whose key is greatest (`maximize`) or least (`minimize`), or `Unset` on an
  // empty receiver. Beside `order` because they ask the same question of the same
  // `Comparable`: `xs.maximize(f)` is the last element of `xs.order(f)`, found in one pass
  // with no sort. The first of several equal-keyed elements wins, as the stable sort would
  // place it. Over `Comparable`, not hypotenuse's numeric `Commensurable`, so any ordered key
  // -- a `Text`, an `Ordinal`, a date -- selects.
  def maximize[key](lambda: element => key)(using comparable: key is Comparable)
  :   Optional[element] =
    sortingInternal.extremum(traversable.traverse(self), lambda, comparable, true)

  def minimize[key](lambda: element => key)(using comparable: key is Comparable)
  :   Optional[element] =
    sortingInternal.extremum(traversable.traverse(self), lambda, comparable, false)

  // How many elements are coming, when that is known and the caller has accepted the cost of
  // asking. `Countable` is complexity-gated — a `List` answers only where `linearSize` has been
  // imported, a `Chain` only under `unboundedSize` — so this takes the size exactly when
  // counting is a cost the call site has already agreed to, and otherwise sorts from an
  // iterator of unknown length, as it always did.
  private inline def sizeHint: Int = summonFrom:
    // `self` is backticked: an unquoted lowercase name in a type pattern binds a *fresh* type
    // variable, so the branch would match a `Countable` of anything at all, and the instance
    // would arrive too weakly typed to accept this receiver.
    case countable: (`self` is Countable) => countable.size(self)
    case _                                => -1

// Sorting a mutable array in place: the elements are rearranged where they are, and nothing is
// returned. This is the operation the array shape deserves a specialization for — it is the only
// receiver whose elements are already in the arrangement the algorithms work on, so it needs
// neither the copy into a scratch array that a collection needs, nor the rebuild afterwards. The
// empty parameter list marks it as the mutating operation.
extension [element](array: Array[element]^)
  def sort()(using algorithm: SortAlgorithm, comparable: element is Comparable): Unit =
    val raw = array.raw

    // A reference array is what the algorithms already sort, so it is sorted where it lies.
    // A primitive array is not, and its elements are boxed into scratch and written back —
    // still without the decoration or the rebuild that sorting a collection would cost.
    if raw.isInstanceOf[scala.Array[AnyRef]] then
      algorithm.sort(raw.asInstanceOf[scala.Array[AnyRef]^], sortingInternal.comparator(comparable))
    else
      val size = raw.length
      val boxed = new scala.Array[AnyRef](size)
      var index = 0

      while index < size do
        boxed(index) = raw(index).asInstanceOf[AnyRef]
        index += 1

      algorithm.sort(boxed, sortingInternal.comparator(comparable))
      index = 0

      while index < size do
        raw(index) = boxed(index).asInstanceOf[element]
        index += 1

// The plumbing between a collection and the array an algorithm sorts. Public only because the
// extensions above are `inline`, and an inline body may not reach into a private. Its name is
// not `sorting`: beside the class `Sorting`, the two would compile to class files whose names
// differ only by case, which not every filesystem can hold together.
object sortingInternal:
  // The decorated element: its sort key, computed once, beside the element it came from. A
  // class rather than a pair, so that neither the key nor the element needs unpicking from a
  // tuple after the cast back from the untyped scratch array. `sort` needs none of this — there,
  // an element is its own key — so only `order` pays for it.
  class Entry(val key: Any, val element: Any)

  // One pass over the elements, keeping the one whose key compares `More` (or `Less`) than
  // every key seen so far; a tie keeps the earlier element.
  def extremum[element, key]
    ( iterator:   Iterator[element]^,
      lambda:     element => key,
      comparable: key is Comparable,
      greatest:   Boolean )
  :   Optional[element] =

    var started = false
    var current: element = null.asInstanceOf[element]
    var best: key = null.asInstanceOf[key]

    iterator.each: element =>
      if !started then
        current = element
        best = lambda(element)
        started = true
      else
        val next = lambda(element)
        val comparison = comparable.compare(next, best)

        if (if greatest then comparison.more else comparison.less) then
          current = element
          best = next

    if started then current else Unset

  // The comparator the algorithms take, from a `Comparable` over the elements themselves.
  def comparator[element](comparable: element is Comparable): ju.Comparator[AnyRef] =
    (left, right) =>
      comparable.compare(left.asInstanceOf[element], right.asInstanceOf[element]).sign

  // The elements of a traversal, in order. The array is a bare `scala.Array[AnyRef]`: it is
  // interior scratch, never escaping, and holding it untyped avoids demanding a `ClassTag` for
  // an element type the caller has no reason to have one for.
  def sorted[element]
    ( algorithm:          SortAlgorithm,
      consume elements:   Iterator[element]^,
      comparable:         element is Comparable,
      size:               Int )
  :   Iterator[element]^ =

    val array = drain(elements.map(_.asInstanceOf[AnyRef]), size)
    algorithm.sort(array, comparator(comparable))
    Iterator.array(array).map(_.asInstanceOf[element])

  // The elements of a traversal, in the order of a projection of them. Each element is paired
  // with its key before sorting, so the projection is applied once per element rather than twice
  // per comparison. Applying it in the comparison instead was tried and measured: it doubles the
  // time (14.29 ms against 7.23 ms for a hundred thousand), the projection being a `Function1`
  // whose every application is an interface call returning a boxed key.
  //
  // The object per element that this costs can be avoided by interleaving each key with its
  // element in one array — measured at a third faster — but every algorithm would then have to
  // move two cells at a time, which `java.util.Arrays.sort`, and so Timsort, cannot.
  def ordered[element, key]
    ( algorithm:          SortAlgorithm,
      consume elements:   Iterator[element]^,
      project:            element => key,
      comparable:         key is Comparable,
      size:               Int )
  :   Iterator[element]^ =

    val entries =
      elements.map: element => Entry(project(element), element).asInstanceOf[AnyRef]

    val array = drain(entries, size)

    val keyComparator: ju.Comparator[AnyRef] = (left, right) =>
      val leftKey = left.asInstanceOf[Entry].key.asInstanceOf[key]
      val rightKey = right.asInstanceOf[Entry].key.asInstanceOf[key]

      comparable.compare(leftKey, rightKey).sign

    algorithm.sort(array, keyComparator)
    Iterator.array(array).map(_.asInstanceOf[Entry].element.asInstanceOf[element])

  // The slow drain: into a growing buffer, when the count is unknown or was wrong.
  private def gather(elements: Iterator[AnyRef]^): scala.Array[AnyRef]^ =
    val buffer = scala.collection.mutable.ArrayBuffer[AnyRef]()
    elements.each(buffer += _)
    buffer.toArray(using ClassTag.AnyRef)

  // An iterator's elements as an array. Given the count, the array is allocated once and filled;
  // without it, a builder grows one, which reallocates and copies as it goes. The count is
  // treated as a hint rather than a promise: an instance that answers with the wrong number
  // yields a shorter or longer array, not a broken one.
  private def drain(elements: Iterator[AnyRef]^, size: Int): scala.Array[AnyRef]^ =
    if size < 0 then gather(elements) else
      val array = new scala.Array[AnyRef](size)
      var index = 0
      var exhausted = false

      while index < size && !exhausted do
        elements.next() match
          case Iterator.Exhausted => exhausted = true
          case element: AnyRef    =>
            array(index) = element
            index += 1

      if exhausted then (if index == size then array else truncated(array, index))
      else
        // More elements than the count promised: the rest are drained the slow way and joined.
        val extra = gather(elements)

        if extra.length == 0 then array else
          val whole = new scala.Array[AnyRef](index + extra.length)
          jl.System.arraycopy(array, 0, whole, 0, index)
          jl.System.arraycopy(extra, 0, whole, index, extra.length)
          whole

  private def truncated(array: scala.Array[AnyRef]^, size: Int): scala.Array[AnyRef]^ =
    val shorter = new scala.Array[AnyRef](size)
    jl.System.arraycopy(array, 0, shorter, 0, size)
    shorter

// A search in logarithmic time needs its receiver to be sorted, and sorted by the very order the
// search compares with: a collection sorted by one key and searched by another answers wrongly,
// and no brand on a collection's type can say which lambda sorted it. A `Sorting` therefore holds
// the three things together — the elements, in order; the projection which gives an element's
// key; and the `Comparable` which orders the keys — so that it is sorted by construction, and
// every search uses the order it was sorted by.
//
//     val cast = Sorting(players)(_.name)
//     cast.locate(t"Viola")          // the `Ordinal` of the player of that name, or `Unset`
//     cast.lowerBound(t"M")          // where the names from `M` onwards begin
//
// The projection is a pure function, so that a `Sorting` is a value like any other collection.
object Sorting:
  // The elements of `elements`, sorted by the key `lambda` gives each. Elements with equal keys
  // keep the order they came in, under a stable `SortAlgorithm`.
  def apply[self, element, key](elements: self)(lambda: element -> key)
    ( using traversable: self is Traversable by element,
            comparable:  key is Comparable,
            algorithm:   SortAlgorithm )
  :   Sorting[element, key] =

    val sorted: Iterator[element]^ =
      sortingInternal.ordered(algorithm, traversable.traverse(elements), lambda, comparable, -1)

    new Sorting(sorted.to[Sequence], lambda, comparable)

  // The elements of `elements` in their own order: each is its own key.
  def apply[self, element](elements: self)
    ( using traversable: self is Traversable by element,
            comparable:  element is Comparable,
            algorithm:   SortAlgorithm )
  :   Sorting[element, element] =

    val sorted: Iterator[element]^ =
      sortingInternal.sorted(algorithm, traversable.traverse(elements), comparable, -1)

    new Sorting(sorted.to[Sequence], identity(_), comparable)

  given traversable: [element, key] => Sorting[element, key] is Traversable by element =
    sorting => Iterator.from(Sequence.iterator(sorting.elements))

  given countable: [element, key] => Sorting[element, key] is Countable =
    sorting => Sequence.size(sorting.elements)

final class Sorting[element, key] private
  ( val elements:   Sequence[element],
    project:        element -> key,
    comparable:     key is Comparable ):

  // The first position whose key is not less than `key`: where `key` is, if it is here at all,
  // and otherwise where it would be inserted to keep the order. It is the position after the
  // last element if every key is less.
  def lowerBound(key: key): Ordinal = bound(key, false, 0, Sequence.size(elements)).z

  // The first position whose key is greater than `key`: just past the last element with that
  // key. The elements with exactly that key are those from `lowerBound` up to here.
  def upperBound(key: key): Ordinal = bound(key, true, 0, Sequence.size(elements)).z

  // The position of the first element whose key is `key`, or `Unset` if there is none.
  def locate(key: key): Optional[Ordinal] =
    val index: Int = bound(key, false, 0, Sequence.size(elements))

    if index < Sequence.size(elements)
       && comparable.compare(project(Sequence.at(elements, index)), key).same
    then index.z
    else Unset

  // The first element whose key is `key`, or `Unset` if there is none.
  def find(key: key): Optional[element] = locate(key).let: ordinal =>
    Sequence.at(elements, ordinal.n0)

  // Whether an element has the key `key`.
  def defines(key: key): Boolean = locate(key).present

  // The first index in `low` until `high` whose key is not less than `key` — or, if `after`,
  // is greater than it.
  @tailrec
  private def bound(key: key, after: Boolean, low: Int, high: Int): Int =
    if low >= high then low else
      val middle: Int = (low + high) >>> 1
      val comparison: Comparison = comparable.compare(project(Sequence.at(elements, middle)), key)

      if comparison.less || (after && comparison.same) then bound(key, after, middle + 1, high)
      else bound(key, after, low, middle)
