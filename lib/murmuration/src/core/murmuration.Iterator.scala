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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.caps, caps.{Stateful, unsafe}
import scala.collection.immutable as sci
import scala.collection.mutable as scm
import scala.annotation.unchecked.uncheckedVariance
import scala.compiletime.uninitialized
import scala.language.experimental.pureFunctions

import prepositional.*

// A one-shot, stateful source of elements: the collection layer's internal currency
// (`Traversable` produces one, the transforming operations consume it) and the honest type of
// anything read from the outside world once. Unlike `scala.Iterator`, it is capture- and
// separation-checked, and it has no way to ask for an element that is not there.
//
// The fast core is `advance` — an `update` method which moves to the next element and
// materialises it — and `current`, a pure, unboxed read of the materialised element. Both are
// gated to this package (and to implementors, who override them): a caller outside cannot read
// `current` without having advanced, because it cannot reach either. Everything outside goes
// through one of three safe surfaces:
//
// - `pull`, which consumes the iterator and yields either `Exhausted` or a linear `Cursor` that
//   owns it: `cursor.value` reads the element, and `cursor.pull` consumes the cursor to advance.
//   Separation checking rejects a cursor used after it was pulled, an iterator used after it
//   was pulled, and a `value` with no cursor. Nothing allocates: the cursor is the iterator
//   reference itself, the sentinel is a `null`. This is the shape for a hand-written loop.
// - `next()`, one `update` call that advances and yields the element, or the sentinel once the
//   source is exhausted (and thereafter): the shape for a stateful consumer that holds the
//   iterator in a field, where a linear token cannot live. There is no `next` without a
//   `hasNext` because they are the same call.
// - the terminal operations (`each`, `fold`, `exists`, …), `update` methods that drain a
//   borrowed iterator.
//
// The lazy combinators (`map`, `filter`, …) consume the iterator they transform and *share* the
// lambda they are given: the result's capture set names the lambda and a fresh capability
// (`^{lambda, caps.any}`), so a lambda stays usable afterwards and a method may forward its own
// lambda parameter without consuming it. `flatMap` is the exception (see below). Extends
// `Stateful`, not `Mutable`: `Mutable` is `Unscoped`, which could not capture a scoped lambda,
// while `Stateful` permits the `var` fields and still enforces `consume` linearity.
trait Iterator[+element] extends Stateful:
  protected[murmuration] update def advance(): Boolean
  protected[murmuration] def current: element

  // The linear surface: consume this iterator for a cursor over its first element, or the
  // sentinel if it has none.
  consume def pull: Iterator.Pull[element] =
    if advance() then Iterator.Cursor(this) else Iterator.Exhausted

  // The stateful-consumer surface: the next element, or the sentinel once exhausted.
  update def next(): element | Iterator.Exhausted = if advance() then current else Iterator.Exhausted

  update def each(lambda: element => Unit): Unit = while advance() do lambda(current)

  update def fold[state](initial: state)(lambda: (state, element) => state): state =
    var state = initial
    while advance() do state = lambda(state, current)
    state

  update def exists(predicate: element => Boolean): Boolean =
    var found = false
    while !found && advance() do found = predicate(current)
    found

  update def all(predicate: element => Boolean): Boolean =
    var holds = true
    while holds && advance() do holds = predicate(current)
    holds

  update def count(predicate: element => Boolean): Int =
    var total = 0
    while advance() do if predicate(current) then total += 1
    total

  // The first element satisfying `predicate`, handed to `present`, or `absent` if none does; the
  // rest is left unconsumed. Two continuations rather than a union result, so that the element
  // reaches the caller at its own type with no test and no cast (this module cannot name
  // `Optional`; rudiments' `seek` is `seek(predicate)(Unset)(identity(_))`).
  update def seek[result](predicate: element => Boolean)(absent: => result)
    ( present: element => result )
  :   result =

    var searching = true
    var found = false

    while searching && advance() do
      if predicate(current) then
        found = true
        searching = false

    if found then present(current) else absent

  // The next element as a stdlib `Option`, for the lazy stdlib shapes (`LazyList.unfold`) that
  // pull through a closure and cannot hold a cursor.
  update def option: Option[element] = if advance() then Some(current) else None

  // The remaining elements as a lazy `Chain`, pulled one cell at a time as the chain is forced:
  // the chain memoizes what it has pulled, and nothing records the iterator it pulls from (a
  // `Chain` is pure), so this is the one shape that lets an iterator outlive its owner.
  update def chain: Chain[element] = Chain.from(sci.LazyList.unfold(())(_ => option.map((_, ()))))

  // Drain into a collection of the given shape: `iterator.to[List]`, `iterator.to[Set]`. An
  // iterator is not `Traversable` -- a traversal borrows, and borrowing an exclusive iterator
  // into a result that outlives the call is what separation checking forbids -- so the
  // conversion is a member rather than the generic `to` (a member, not a companion extension,
  // because rudiments' lexically-scoped generic `to` would outrank an extension). The element
  // is covariant, and the reshaping evidence names it invariantly; it is sound here because
  // reshaping only reads the elements.
  update def to[result[_]]
    ( using reshapable: result[element @uncheckedVariance] is Reshapable
                          by (element @uncheckedVariance) to result[element @uncheckedVariance] )
  :   result[element @uncheckedVariance] =
    reshapable.reshape(this)

  // The number of elements remaining, by draining them.
  update def size: Int =
    var total = 0
    while advance() do total += 1
    total

  consume def map[element2](lambda: element => element2): Iterator[element2]^{lambda, caps.any} =
    val source = this

    new Iterator[element2]:
      private var head: element2 = uninitialized

      protected[murmuration] update def advance(): Boolean =
        source.advance() && { head = lambda(source.current); true }

      protected[murmuration] def current: element2 = head

  consume def filter(predicate: element => Boolean): Iterator[element]^{predicate, caps.any} =
    val source = this

    new Iterator[element]:
      private var head: element = uninitialized

      protected[murmuration] update def advance(): Boolean =
        var found = false

        while !found && source.advance() do
          if predicate(source.current) then
            head = source.current
            found = true

        found

      protected[murmuration] def current: element = head

  // Filter and map in one pass, keeping the elements the partial function is defined at.
  consume def sweep[element2](lambda: PartialFunction[element, element2]^)
  :   Iterator[element2]^{lambda, caps.any} =

    val source = this

    new Iterator[element2]:
      private var head: element2 = uninitialized

      protected[murmuration] update def advance(): Boolean =
        var found = false

        while !found && source.advance() do
          val value = source.current

          if lambda.isDefinedAt(value) then
            head = lambda(value)
            found = true

        found

      protected[murmuration] def current: element2 = head

  // The monadic bind. The lambda yields a *collection* (anything `Traversable`), not an
  // iterator: a lambda cannot hand back a freshly minted capability through a function type,
  // so the inner traversal is opened here, one at a time, as the result advances. The inner
  // iterator is held in an untyped field and read back exclusive where it is advanced --
  // separation checking will not let a field hold a fresh capability -- which is the one place
  // in the iterator where the checker is told rather than shown.
  consume def flatMap[inner, element2](lambda: element => inner)
    ( using innerTraversable: inner is Traversable by element2 )
  :   Iterator[element2]^{lambda, caps.any} =

    val source = this

    new Iterator[element2]:
      // The inner iterator currently being drained, or `null`: held untyped (see above).
      private var current0: AnyRef | Null = null
      private var head: element2 = uninitialized

      protected[murmuration] update def advance(): Boolean =
        var result = false
        var searching = true

        while searching do
          val pending = current0

          if pending == null then
            if source.advance()
            then current0 = innerTraversable.traverse(lambda(source.current)).asInstanceOf[AnyRef]
            else searching = false
          else
            // [iterator-inner] the untyped field read back as the exclusive iterator it holds
            val exclusive = pending.asInstanceOf[Iterator[element2]^]

            if exclusive.advance() then
              head = exclusive.current
              result = true
              searching = false
            else current0 = null

        result

      protected[murmuration] def current: element2 = head

  consume def zip[element2](consume that: Iterator[element2]^): Iterator[(element, element2)]^ =
    val source = this

    new Iterator[(element, element2)]:
      private var head: (element, element2) = uninitialized

      protected[murmuration] update def advance(): Boolean =
        source.advance() && that.advance() && { head = (source.current, that.current); true }

      protected[murmuration] def current: (element, element2) = head

  // Each element paired with its position, counted from zero. An `Int`, not an `Ordinal`:
  // denominative sits above this module; `rudiments.indexed` converts.
  consume def indexed: Iterator[(element, Int)]^ =
    val source = this

    new Iterator[(element, Int)]:
      private var index: Int = -1
      private var head: (element, Int) = uninitialized

      protected[murmuration] update def advance(): Boolean =
        source.advance() && { index += 1; head = (source.current, index); true }

      protected[murmuration] def current: (element, Int) = head

  // The first `count` elements, and everything after them.
  consume def keep(limit: Int): Iterator[element]^ =
    val source = this

    new Iterator[element]:
      private var remaining: Int = limit
      protected[murmuration] update def advance(): Boolean =
        remaining > 0 && { remaining -= 1; source.advance() }
      protected[murmuration] def current: element = source.current

  consume def skip(limit: Int): Iterator[element]^ =
    val source = this

    new Iterator[element]:
      private var pending: Int = limit

      protected[murmuration] update def advance(): Boolean =
        while pending > 0 && source.advance() do pending -= 1
        pending = 0
        source.advance()

      protected[murmuration] def current: element = source.current

  // The longest leading run satisfying `predicate`, and everything from the first element that
  // does not.
  consume def keep(predicate: element => Boolean): Iterator[element]^{predicate, caps.any} =
    val source = this

    new Iterator[element]:
      private var finished: Boolean = false

      protected[murmuration] update def advance(): Boolean =
        !finished && source.advance() && (if predicate(source.current) then true else { finished = true; false })

      protected[murmuration] def current: element = source.current

  consume def skip(predicate: element => Boolean): Iterator[element]^{predicate, caps.any} =
    val source = this

    new Iterator[element]:
      private var skipping: Boolean = true
      private var head: element = uninitialized

      protected[murmuration] update def advance(): Boolean =
        if skipping then
          var found = false

          while !found && source.advance() do
            if !predicate(source.current) then
              head = source.current
              found = true

          skipping = false
          found
        else
          source.advance() && { head = source.current; true }

      protected[murmuration] def current: element = head

  // The running accumulation (the stdlib's `scanLeft`): every intermediate state, initial first.
  consume def trace[state](initial: state)(lambda: (state, element) => state)
  :   Iterator[state]^{lambda, caps.any} =

    val source = this

    new Iterator[state]:
      private var started: Boolean = false
      private var head: state = initial

      protected[murmuration] update def advance(): Boolean =
        if !started then
          started = true
          true
        else source.advance() && { head = lambda(head, source.current); true }

      protected[murmuration] def current: state = head

  // The elements from position `from` (inclusive) to `until` (exclusive).
  consume def excerpt(from: Int, until: Int): Iterator[element]^ =
    skip(from).keep((until - from).max(0))

  // The first occurrence of each key, in first-occurrence order.
  consume def deduplicate[key](lambda: element => key): Iterator[element]^{lambda, caps.any} =
    val seen = scm.HashSet[key]()
    filter { element => seen.add(lambda(element)) }

  consume def distinct: Iterator[element]^ = deduplicate(identity(_))

  // Consecutive batches of at most `size` elements; the last may be shorter.
  consume def batched(length: Int): Iterator[List[element]]^ =
    val source = this

    new Iterator[List[element]]:
      private var head: List[element] = Nil

      protected[murmuration] update def advance(): Boolean =
        val builder = sci.List.newBuilder[element]
        var count = 0

        while count < length && source.advance() do
          builder += source.current
          count += 1

        head = List.from(builder.result())
        count > 0

      protected[murmuration] def current: List[element] = head

  // This iterator's elements, then `that`'s.
  consume def concat[element2 >: element](consume that: Iterator[element2]^): Iterator[element2]^ =
    val source = this

    new Iterator[element2]:
      private var first: Boolean = true
      private var head: element2 = uninitialized

      protected[murmuration] update def advance(): Boolean =
        if first && source.advance() then
          head = source.current
          true
        else
          first = false
          that.advance() && { head = that.current; true }

      protected[murmuration] def current: element2 = head

  // The boundary to the standard library and Java: a `scala.Iterator` that drains this one, for
  // the `IterableOnce` factories (`List.from`, …) and the APIs that take it. Consumes this
  // iterator: the stdlib view owns it from here on, and nothing else may advance it. The view
  // is typed pure, as every stdlib iterator is: the standard library tracks nothing, and a
  // capability-carrying stdlib type would only be laundered at the next stdlib call anyway.
  consume def stdlib: scala.collection.Iterator[element] =
    val source = this

    // [stdlib-boundary] the stdlib view owns the consumed iterator; the standard library does
    // not track it
    unsafe.unsafeAssumePure:
      new scala.collection.AbstractIterator[element]:
        // [stdlib-boundary] the lookahead flag of the stdlib view over this iterator
        @unsafe.untrackedCaptures private var advanced: Boolean = source.advance()
        def hasNext: Boolean = advanced

        def next(): element =
          val head = source.current
          advanced = source.advance()
          head

object Iterator:
  // The sentinel for "no more elements". Spelt as vacuous spells `Unset` (which this module,
  // below vacuous, cannot name): `<: caps.Pure` rather than `<: Null`, so a union with it is
  // pure whenever the other side is, and `.nn` does not strip it; backed by `null`, so it costs
  // nothing.
  opaque type Exhausted <: Matchable & caps.Pure = Null & caps.Pure
  val Exhausted: Exhausted = null.asInstanceOf[Exhausted]

  given exhaustedEquality: [value] => CanEqual[value, Exhausted] = CanEqual.derived
  given equalityExhausted: [value] => CanEqual[Exhausted, value] = CanEqual.derived

  // The linear token over a present element: the iterator reference itself, so a pull allocates
  // nothing, and opaque so it cannot be forged. Bounded by `Stateful` so that a `match` can test
  // for it against the sentinel.
  opaque type Cursor[+element] <: Stateful = Iterator[element]^
  type Pull[+element] = Exhausted | Cursor[element]^

  object Cursor:
    def apply[element](consume iterator: Iterator[element]^): Cursor[element]^ = iterator

    extension [element](cursor: Cursor[element]^)
      def value: element = (cursor: Iterator[element]^).current

    extension [element](consume cursor: Cursor[element]^)
      def pull: Pull[element] =
        val iterator: Iterator[element]^ = cursor
        if iterator.advance() then Cursor(iterator) else Exhausted

  // The elements of a JVM array that no writer reaches while the iterator runs: the
  // `Traversable` instances of the frozen array come here. `apply` boxes a primitive element
  // through `ScalaRunTime`, as the stdlib's array iterator does.
  def array[element](array: scala.Array[element]^{caps.any.rd}): Iterator[element]^{array, caps.any} = new Iterator[element]:
    private var index: Int = -1
    protected[murmuration] update def advance(): Boolean = { index += 1; index < array.length }
    protected[murmuration] def current: element = array(index)

  def empty[element]: Iterator[element]^ = new Iterator[element]:
    protected[murmuration] update def advance(): Boolean = false
    protected[murmuration] def current: element = throw new NoSuchElementException()

  def apply[element](elements: element*): Iterator[element]^ = from(elements.iterator)

  def range(start: Int, end: Int): Iterator[Int]^ = new Iterator[Int]:
    private var upcoming: Int = start
    private var head: Int = 0

    protected[murmuration] update def advance(): Boolean =
      upcoming < end && { head = upcoming; upcoming += 1; true }

    protected[murmuration] def current: Int = head

  def range(start: Int, end: Int, step: Int): Iterator[Int]^ = new Iterator[Int]:
    private var upcoming: Int = start
    private var head: Int = 0

    protected[murmuration] update def advance(): Boolean =
      (if step > 0 then upcoming < end else upcoming > end) && { head = upcoming; upcoming += step; true }

    protected[murmuration] def current: Int = head

  def fill[element](limit: Int)(element: => element): Iterator[element]^{element, caps.any} = new Iterator[element]:
    private var remaining: Int = limit
    private var head: element = uninitialized

    protected[murmuration] update def advance(): Boolean =
      remaining > 0 && { remaining -= 1; head = element; true }

    protected[murmuration] def current: element = head

  def tabulate[element](count: Int)(lambda: Int => element): Iterator[element]^{lambda, caps.any} =
    range(0, count).map(lambda)

  def continually[element](element: => element): Iterator[element]^{element, caps.any} = new Iterator[element]:
    private var head: element = uninitialized
    protected[murmuration] update def advance(): Boolean = { head = element; true }
    protected[murmuration] def current: element = head

  def iterate[element](initial: element)(lambda: element => element): Iterator[element]^{lambda, caps.any} =
    new Iterator[element]:
      private var started: Boolean = false
      private var head: element = initial

      protected[murmuration] update def advance(): Boolean =
        if started then head = lambda(head) else started = true
        true

      protected[murmuration] def current: element = head

  // The boundaries from the standard library and Java: a one-shot source whose remaining
  // elements this iterator takes over.
  def from[element](consume iterator: scala.collection.Iterator[element]^): Iterator[element]^ =
    new Iterator[element]:
      private var head: element = uninitialized

      protected[murmuration] update def advance(): Boolean =
        iterator.hasNext && { head = iterator.next(); true }

      protected[murmuration] def current: element = head

  def from[element](iterator: java.util.Iterator[element]): Iterator[element]^ =
    new Iterator[element]:
      private var head: element = uninitialized

      protected[murmuration] update def advance(): Boolean =
        iterator.hasNext && { head = iterator.next().nn; true }

      protected[murmuration] def current: element = head

  def from[element](enumeration: java.util.Enumeration[element]): Iterator[element]^ =
    new Iterator[element]:
      private var head: element = uninitialized

      protected[murmuration] update def advance(): Boolean =
        enumeration.hasMoreElements && { head = enumeration.nextElement().nn; true }

      protected[murmuration] def current: element = head
