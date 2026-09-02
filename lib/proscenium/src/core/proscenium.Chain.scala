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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
                                                                                                  */
package proscenium

import scala.collection.immutable as sci

// The lazy, potentially-infinite opaque collection alias, backed by `sci.LazyList`. Same design
// as `List`/`Sequence`/`Set`/`Map`: members invisible, API via typeclasses, construction and the
// greppable `stdlib` bridge in the companion, casts at the boundary, no `Conversion` to a stdlib
// supertype. The distinguishing constraint is LAZINESS: the `#::` cons keeps its tail operand
// by-name, and the `#::` extractor is name-based and non-forcing (matching a head forces only the
// first element, never the tail's contents).
object Chain:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing lambdas
  // crashes the capture checker's boxer (boxDeeply assertion), and streaming code is cc-heavy.
  private[proscenium] def of[element](chain: sci.LazyList[element]): Chain[element] =
    chain.asInstanceOf[Chain[element]]

  // Deliberately NOT the branded literal constructor `List` and `Sequence` have: `Chain` is the
  // streaming collection, and the `transparent inline` expansion at capture-checked call sites
  // (e.g. `Chain(data)` in turbulence) lets `any.rd` into the element's inference — the same
  // hazard that keeps `of` non-`inline`. A `Chain` literal is therefore unbranded; prove
  // non-emptiness with `occupied` or a `#::` match where it matters.
  def apply[element](elements: element*): Chain[element] = of(sci.LazyList(elements*))

  def empty[element]: Chain[element] = of(sci.LazyList.empty[element])

  def from[element](elements: IterableOnce[element]^): Chain[element] =
    of(sci.LazyList.from(elements))

  // `continually`/`iterate`/`unfold` build (potentially infinite) lazy streams; the generators
  // stay by-name so nothing is evaluated until forced.
  def continually[element](element: => element): Chain[element] =
    of(sci.LazyList.continually(element))

  def iterate[element](start: => element)(next: element => element): Chain[element] =
    of(sci.LazyList.iterate(start)(next))

  def unfold[state, element](init: state)(lambda: state => Option[(element, state)])
  :   Chain[element] =
    of(sci.LazyList.unfold(init)(lambda))

  def range[element: Integral](start: element, end: element): Chain[element] =
    of(sci.LazyList.range(start, end))

  // The primitive lazy cons: the tail is by-name and stays unforced until demanded.
  def cons[element](head: element, tail: => Chain[element]): Chain[element] =
    of(sci.LazyList.cons(head, tail.stdlib))

  // Defers evaluation of `chain` until the result is forced. `empty.lazyAppendedAll(=> chain)`
  // keeps the by-name suffix unforced — equivalent to (and cheaper than) the old
  // `(dummy #:: chain).tail`, and it sidesteps the captured-by-name cons under cc.
  def defer[element](chain: => Chain[element]): Chain[element] =
    Chain.empty[element].lazyAppendedAll(chain)

  def unapplySeq[element](chain: Chain[element]): Option[Seq[element]] = Some(chain.stdlib)

  // `.to[Chain]` support (see `List`): the conversion is on `Chain.type` only, so it cannot
  // expose members of `Chain` values.
  given factory: [element]
        => Conversion[Chain.type, scala.collection.Factory[element, Chain[element]]] =
    _ =>
      new scala.collection.Factory[element, Chain[element]]:
        def fromSpecific(elements: IterableOnce[element]^): Chain[element] =
          Chain.from(elements)

        def newBuilder: scala.collection.mutable.Builder[element, Chain[element]] =
          sci.LazyList.newBuilder[element].mapResult(of(_))

  extension [element](chain: Chain[element])
    inline def stdlib: sci.LazyList[element] = chain.asInstanceOf[sci.LazyList[element]]

// The lazy cons constructor. As with `List`'s `::`, right-associative extensions read in usage
// order, so the receiver is the HEAD; it rides on a given (a top-level name would clash with the
// extractor object). The head is by-value (call sites hoist it), but the TAIL is by-name so the
// stream stays lazy — `head #:: recur()` must not evaluate `recur()`.
  given chainIsSpreadable: [element] => (Spreadable[Chain[element]] { type Out = sci.LazyList[element] }) =
    new Spreadable[Chain[element]]:
      type Out = sci.LazyList[element]
      def spread(value: Chain[element]): sci.LazyList[element] = value

given lazyCons: Object with
  extension [element](head: element)
    infix def #:: (tail: => Chain[element]): Chain[element] =
      Chain.of(sci.LazyList.cons(head, tail.asInstanceOf[sci.LazyList[element]]))

// The lazy concatenation operator. Like `#::`, the receiver is the left operand (usage order)
// and the suffix is by-name, so `prefix #::: suffix` does not force `suffix`.
extension [element](prefix: Chain[element])
  infix def #::: [element2 >: element](suffix: => Chain[element2]): Chain[element2] =
    Chain.of:
      prefix.asInstanceOf[sci.LazyList[element]]
      . lazyAppendedAll(suffix.asInstanceOf[sci.LazyList[element2]])

// The non-forcing lazy-cons deconstructor: a name-based extractor for `case head #:: tail =>`.
// `isEmpty`/`_1` force only the first node; `_2` returns the tail via `sci.LazyList#tail`, which
// does not force the tail's elements.
object `#::`:
  final class ConsView[element](chain: sci.LazyList[element]):
    def isEmpty: Boolean = chain.isEmpty
    def get: this.type = this
    def _1: element = chain.head
    def _2: Chain[element] = Chain.of(chain.tail)

  def unapply[element](chain: Chain[element]): ConsView[element] = ConsView(chain.stdlib)

opaque type Chain[+element] = sci.LazyList[element]
