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

import scala.reflect.ClassTag

import denominative.*
import prepositional.*

// The write-side counterpart of confined reading (issue #1666), as a builder-lender: an
// opaque handle over a freshly-allocated array that grants writes (and read-back) only
// through `Ordinal`s branded to the handle's identity, so no index it accepts can be out of
// range. `Array.build` allocates, lends the scribe with its branded extent, and freezes the
// result — the only writer is statically retired when the lender returns, so the freeze is
// sound by construction, exactly as `Array.freeze`'s `consume` form.
//
// A `Scribe` is `Countable`, so the whole confined-scan family (`iterate`, `spot`, `lead`,
// `pare`, `retrace`) applies to it directly: `scribe.iterate { i => scribe(i) = ... }`.
//
// The accessors live in the companion (found through implicit scope) rather than at the top
// level, so same-named extensions imported by wildcard cannot shadow them.
// An untyped carrier, as `Region`/`Slate`: an opaque alias of `scala.Array` directly would be
// mutable-classified by the mutalias patch, annotating every in-module signature with
// `^{any.rd}` and breaking capture-free summons across the boundary. The casts are sound:
// a scribe is minted only over the array `Array.scribe` just allocated.
opaque type Scribe[element] = AnyRef

object Scribe:
  // Written out (not a SAM lambda): the lambda form infers a capture-annotated `Self`
  // (`Scribe[element]^{any.rd}`), which then fails to match capture-free summons.
  given countable: [element] => Scribe[element] is Countable:
    def size(self: Scribe[element]): Int = self.asInstanceOf[scala.Array[element]].length

  private[rudiments] inline def over[element, result](buffer: scala.Array[element])
    ( inline lambda: (scribe: Scribe[element]) => (Interval in scribe.type) => result )
  :   result =

    val scribe: Scribe[element] = buffer.asInstanceOf[AnyRef]
    lambda(scribe)(Interval.initial(buffer.length).asInstanceOf[Interval in scribe.type])

  extension [element](scribe: Scribe[element])
    inline def update(index: Ordinal in scribe.type, value: element): Unit =
      val ordinal: Ordinal = index
      // The cast re-asserts the exclusivity the carrier forgot: the array is reached only
      // through this scribe, which the lender confines to one lambda.
      scribe.asInstanceOf[scala.Array[element]^](ordinal.n0) = value

    inline def apply(index: Ordinal in scribe.type): element =
      val ordinal: Ordinal = index
      scribe.asInstanceOf[scala.Array[element]](ordinal.n0)

    // Bulk copy of a whole frozen array into the scribe at `at`, clamped to the space that
    // remains: the confined form of `place`. Returns the count copied.
    inline def place(source: Array[element]^{}, at: Ordinal in scribe.type): Int =
      val ordinal: Ordinal = at
      val target: scala.Array[element]^ = scribe.asInstanceOf[scala.Array[element]^]
      val count = source.readable.length.min(target.length - ordinal.n0)

      System.arraycopy
        (source.asInstanceOf[scala.Array[element]], 0, target, ordinal.n0, count)

      count

extension (companion: Array.type)
  // Allocate, lend, freeze: `lambda` receives the scribe and its branded extent, and the
  // frozen result is returned once the only writer has been retired.
  inline def scribe[element: ClassTag](size: Int)
    ( inline lambda: (scribe: Scribe[element]) => (Interval in scribe.type) => Unit )
  :   Array[element]^{} =

    val buffer = new scala.Array[element](size.max(0))
    Scribe.over[element, Unit](buffer)(lambda)
    buffer.asInstanceOf[Array[element]^{}]