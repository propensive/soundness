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
package vacuous

import scala.language.experimental.pureFunctions

import scala.compiletime.uninitialized

import scala.caps.*, unsafe.untrackedCaptures

// A mutable, one-shot source of elements, and the first mutable collection. Its fast core —
// `advance` (an `update` method: move to the next element and materialise it, reporting presence)
// and `current` (a pure read of the materialised element, UNBOXED) — is gated to the `vacuous`
// package: partial access can't leak, because outside code pulls only through `pull`/`Cursor`, the
// separation-checked safe surface. `pull` consumes the iterator and hands back a linear `Cursor`;
// `Cursor.value` reads the element unboxed, and `Cursor.pull` consumes the cursor to advance — so
// `value` cannot be read without a present, unconsumed cursor, and a cursor cannot be reused.
// Elements are never boxed; `Optional` only ever wraps the cursor reference. `Iterator` is the
// internal currency of the collections layer: `Traversable` produces one, the transforming
// operations consume it.
trait Iterator[element] extends Stateful:
  protected[vacuous] update def advance(): Boolean
  protected[vacuous] def current: element

  // Safe, separation-checked entry point: consume the iterator, yielding a linear cursor or `Unset`.
  consume def pull: Optional[Iterator.Cursor[element]^] =
    if advance() then Iterator.Cursor(this) else Unset

  update def each(lambda: element => Unit): Unit = while advance() do lambda(current)

  update def fold[state](initial: state)(lambda: (state, element) => state): state =
    var state = initial
    while advance() do state = lambda(state, current)
    state

  update def exists(predicate: element => Boolean): Boolean =
    var found = false
    while !found && advance() do found = predicate(current)
    found

  consume def map[element2](consume lambda: element => element2): Iterator[element2]^ =
    val source = this

    new Iterator[element2]:
      private var head: element2 = uninitialized
      protected[vacuous] update def advance(): Boolean =
        source.advance() && { head = lambda(source.current); true }

      protected[vacuous] def current: element2 = head

  consume def filter(consume predicate: element => Boolean): Iterator[element]^ =
    val source = this

    new Iterator[element]:
      private var head: element = uninitialized
      protected[vacuous] update def advance(): Boolean =
        var found = false
        while !found && source.advance() do if predicate(source.current) then
          head = source.current
          found = true

        found

      protected[vacuous] def current: element = head

  // `flatMap`'s function is *pure* (`->`), unlike the other combinators' capturing lambdas. It has
  // to be: the result iterator stores the function and calls it once per source element, so it must
  // be both captured (which, for an exclusive capability, would demand `consume`) and repeatable —
  // a contradiction separation checking rejects. A pure function is freely repeatable and its
  // results don't alias it, so the native, lazy implementation type-checks.
  consume def flatMap[element2](lambda: element -> Iterator[element2]^): Iterator[element2]^ =
    val source = this

    new Iterator[element2]:
      @untrackedCaptures private var inner: Optional[Iterator[element2]^] = Unset
      private var head: element2 = uninitialized

      protected[vacuous] update def advance(): Boolean =
        var result = false
        var searching = true
        while searching do
          if inner.absent then
            if source.advance() then inner = lambda(source.current) else searching = false
          else if inner.vouch.advance() then
            head = inner.vouch.current
            result = true
            searching = false
          else inner = Unset

        result

      protected[vacuous] def current: element2 = head

  consume def zip[element2](consume that: Iterator[element2]^): Iterator[(element, element2)]^ =
    val source = this

    new Iterator[(element, element2)]:
      private var head: (element, element2) = uninitialized
      protected[vacuous] update def advance(): Boolean =
        source.advance() && that.advance() && { head = (source.current, that.current); true }

      protected[vacuous] def current: (element, element2) = head

  // Bridge to the standard-library iterator, for boundaries that build stdlib collections.
  consume def stdlib: scala.collection.Iterator[element]^ =
    val source = this

    new scala.collection.AbstractIterator[element]:
      @untrackedCaptures private var advanced: Boolean = source.advance()
      def hasNext: Boolean = advanced

      def next(): element =
        val head = source.current
        advanced = source.advance()
        head

object Iterator:
  opaque type Cursor[element] = Iterator[element]^

  object Cursor:
    def apply[element](consume iterator: Iterator[element]^): Cursor[element]^ = iterator

    extension [element](cursor: Cursor[element]^)
      def value: element = (cursor: Iterator[element]^).current

    extension [element](consume cursor: Cursor[element]^)
      def pull: Optional[Cursor[element]^] =
        val iterator: Iterator[element]^ = cursor
        if iterator.advance() then Cursor(iterator) else Unset

  def from[element](consume iterator: scala.collection.Iterator[element]^): Iterator[element]^ =
    new Iterator[element]:
      private var head: element = uninitialized
      protected[vacuous] update def advance(): Boolean =
        iterator.hasNext && { head = iterator.next(); true }

      protected[vacuous] def current: element = head
