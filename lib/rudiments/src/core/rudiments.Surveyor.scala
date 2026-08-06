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

import denominative.*
import prepositional.*
import vacuous.*

// The confined parser cursor (issue #1666, category 5): a mutable position over a stable,
// indexable collection, lent by `value.survey { surveyor => ... }`. Every product is branded to
// the *collection* — runs come back as `Interval in value.type` and positions as
// `Ordinal in value.type` — so they feed the confined machinery (`at`, `iterate`,
// `Segmentable`) with no bounds checks, and run lengths come from interval sizes rather
// than raw index arithmetic. This is the shape ypsiloid, stratiform, facsimile and escapade
// converged on informally with `i`/`j` walker loops.
//
// Position may equal the size (exhaustion); no read is offered at the bare position, so
// there is nothing partial to call: reads happen only inside `skipWhile`-family combinators,
// which check `more` and the predicate together, or through the branded products.
//
// The brand is sound for immutable receivers on stable paths, like `within` and `extent`.
final class Surveyor[collection, brand, operand] @scala.annotation.publicInBinary private[rudiments]
  ( value: collection, read: (collection, Int) => operand, size: Int ):

  // Untracked: the position is reached only through the surveyor, which the lender confines to
  // one lambda; `Stateful` would force capability typing onto a transient walker.
  @scala.caps.unsafe.untrackedCaptures
  private var mark0: Int = 0

  // The number of elements already passed over.
  inline def passed: Int = mark0

  // Whether at least one element remains.
  inline def more: Boolean = mark0 < size

  // Step over one element; false at exhaustion.
  inline def advance(): Boolean = if mark0 < size then { mark0 += 1; true } else false

  // Whether the current element satisfies the predicate, without advancing: the dispatch
  // primitive (`surveyor.peek(_ == '-')`), false at exhaustion — the same non-consuming
  // meaning as zephyrine's `Cursor.peek`, fused with the exhaustion check so there is no
  // boxed `Optional` on the hot path.
  inline def peek(inline predicate: operand => Boolean): Boolean =
    mark0 < size && predicate(read(value, mark0))

  // Consume the current element: apply the lambda to it and advance, or yield `otherwise`
  // at exhaustion — the `read-then-advance` shape of byte-at-a-time decoders, fused so
  // nothing partial exists to call and nothing boxes.
  inline def next[result](inline otherwise: => result)(inline lambda: operand => result): result =
    if mark0 < size then
      val element = read(value, mark0)
      mark0 += 1
      lambda(element)
    else otherwise

  // Consume up to `count` elements, returning the branded run actually traversed (clamped
  // at exhaustion): the counted form of `skipWhile`.
  inline def take(count: Int): Interval in brand =
    val start = mark0
    mark0 = (mark0 + count.max(0)).min(size)
    Interval.zerary(start, mark0).asInstanceOf[Interval in brand]

  // Whether the elements at the current position match `pattern` under `equal`, without
  // advancing: the marker-match primitive of scanning parsers. False when fewer elements
  // remain than the pattern's size.
  inline def matches[pattern](pattern: pattern)
    ( using countable: pattern is Countable, indexable: (pattern is Indexable by Ordinal) )
    ( inline equal: (operand, indexable.Result) => Boolean )
  :   Boolean =

    val count = countable.size(pattern)

    if size - mark0 < count then false else
      var index = 0

      while index < count
        && equal(read(value, mark0 + index), indexable.access(pattern, Ordinal.zerary(index)))
      do index += 1

      index == count

  // The branded window of the next `count` elements, without advancing, or `Unset` when
  // fewer remain: fixed-lookahead reads then go through checked or iterated access.
  inline def glimpse(count: Int): Optional[Interval in brand] =
    if size - mark0 < count then Unset
    else Interval.zerary(mark0, mark0 + count).asInstanceOf[Interval in brand]

  // The branded position, when not exhausted.
  inline def point: Optional[Ordinal in brand] =
    if mark0 < size then Ordinal.zerary(mark0).asInstanceOf[Ordinal in brand] else Unset

  // Everything not yet passed over, as a branded interval (possibly empty).
  inline def remainder: Interval in brand =
    Interval.zerary(mark0, size).asInstanceOf[Interval in brand]

  // Advance while the predicate holds of the current element, returning the branded run
  // traversed (possibly empty): `surveyor.skipWhile(_ == ' ')` is the whitespace skip, and
  // `surveyor.skipWhile(_ == style)` is run detection, with the run's length available as the
  // interval's size.
  inline def skipWhile(inline predicate: operand => Boolean): Interval in brand =
    val start = mark0

    while mark0 < size && predicate(read(value, mark0)) do mark0 += 1

    Interval.zerary(start, mark0).asInstanceOf[Interval in brand]

  // Advance until the predicate holds (or exhaustion), returning the branded run skipped.
  inline def skipUntil(inline predicate: operand => Boolean): Interval in brand =
    skipWhile(!predicate(_))

extension [collection](value: collection)
  // Lend a surveyor over this collection: reads resolve through the `Indexable` instance, and
  // the products carry the collection's brand.
  inline def survey[result]
    ( using indexable: (collection is Indexable by Ordinal), countable: collection is Countable )
    ( inline lambda: Surveyor[collection, value.type, indexable.Result] => result )
  :   result =

    lambda:
      new Surveyor
        ( value,
          (collection, index) => indexable.access(collection, Ordinal.zerary(index)),
          countable.size(value) )
