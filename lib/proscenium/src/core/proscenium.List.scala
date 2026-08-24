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

// The workhorse opaque collection alias: an immutable linked list backed by `sci.List`.
// Same design as `Sequence`/`Set`/`Map`: members invisible, API via typeclasses, construction
// and the greppable `stdlib` bridge in the companion, casts at the boundary, and NO
// `Conversion` to a stdlib supertype (member-selection would re-expose the partial surface).
// Vararg splices (`f(list*)`) work directly via proscala's `spliceopaque` feature.
//
// Pattern support: `case head :: tail =>` matches via the name-based extractor below;
// `case Nil =>` matches by equality against the `Nil` value (not an extractor object), so
// existing syntax compiles unchanged; `case List(a, b)` matches via `unapplySeq`.
object List:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing
  // lambdas crashes the capture checker's boxer (boxDeeply assertion).
  private[proscenium] def of[element](list: sci.List[element]): List[element] =
    list.asInstanceOf[List[element]]

  def apply[element](elements: element*): List[element] = of(sci.List(elements*))

  // The branded literal constructors: one fixed-arity overload per arity up to twelve, each
  // returning `& Populated` — the arity of the call *is* the non-emptiness proof. Fixed
  // arities rather than a `(head, tail*)` overload (ambiguous with `(elements*)` at every
  // arity) or a `transparent inline` macro (each expansion is an implicit search under the
  // caller's live type variables, the exact trigger of the mainline `wildApprox` crash,
  // scala/scala3#24824). A thirteen-element literal falls back to the unbranded varargs form;
  // `occupied` recovers the proof there.
  def apply[element](e1: element): List[element] & Populated =
    populated(sci.List(e1))

  def apply[element](e1: element, e2: element): List[element] & Populated =
    populated(sci.List(e1, e2))

  def apply[element](e1: element, e2: element, e3: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3))

  def apply[element](e1: element, e2: element, e3: element, e4: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element, e6: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5, e6))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element, e6: element, e7: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5, e6, e7))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element, e6: element, e7: element, e8: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5, e6, e7, e8))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element, e6: element, e7: element, e8: element, e9: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5, e6, e7, e8, e9))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element, e6: element, e7: element, e8: element, e9: element, e10: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element, e6: element, e7: element, e8: element, e9: element, e10: element, e11: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11))

  def apply[element](e1: element, e2: element, e3: element, e4: element, e5: element, e6: element, e7: element, e8: element, e9: element, e10: element, e11: element, e12: element): List[element] & Populated =
    populated(sci.List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12))

  // The branded cast behind the fixed-arity constructors: a plain method (not an inlined
  // cast) for the same boxer reason as `of`; its callers' arity is the non-emptiness proof.
  private def populated[element](list: sci.List[element]): List[element] & Populated =
    list.asInstanceOf[List[element] & Populated]

  def empty[element]: List[element] = of(sci.List.empty[element])

  def from[element](elements: IterableOnce[element]^): List[element] =
    of(sci.List.from(elements))

  def unapplySeq[element](list: List[element]): Option[Seq[element]] = Some(list.stdlib)

  def fill[element](count: Int)(element: => element): List[element] =
    of(sci.List.fill(count)(element))

  def tabulate[element](count: Int)(lambda: Int => element): List[element] =
    of(sci.List.tabulate(count)(lambda))

  def range(start: Int, end: Int): List[Int] = of(sci.List.range(start, end))

  // `.to[List]` support: the stdlib's `Iterable.to` takes a `Factory`, and passing the
  // companion object adapts through this conversion — on `List.type` only, so it cannot
  // expose members of `List` values.
  given factory: [element] => Conversion[List.type, scala.collection.Factory[element, List[element]]] =
    _ =>
      new scala.collection.Factory[element, List[element]]:
        def fromSpecific(elements: IterableOnce[element]^): List[element] =
          List.from(elements)

        def newBuilder: scala.collection.mutable.Builder[element, List[element]] =
          sci.List.newBuilder[element].mapResult(of(_))

  extension [element](list: List[element])
    inline def stdlib: sci.List[element] = list.asInstanceOf[sci.List[element]]

val Nil: List[Nothing] = List.of(sci.Nil)

// The cons constructor. Right-associative extensions read in usage order, so the receiver
// is the HEAD (the left operand). It cannot be a top-level extension (the name would clash
// with the extractor object), so it rides on a given, whose extensions are candidates
// wherever the given is visible — everywhere, via `-Yimports`.
  // Subtype-parametric so branded lists (`List[T] & Populated`) splat too.
  given listIsSpreadable: [element, list <: List[element]]
  =>  (Spreadable[list] { type Out = sci.List[element] }) =
    new Spreadable[list]:
      type Out = sci.List[element]
      def spread(value: list): sci.List[element] = value

given consConstructor: Object with
  extension [element](head: element)
    infix def :: (tail: List[element]): List[element] =
      List.of(tail.asInstanceOf[sci.List[element]].::(head))

// The cons deconstructor: a name-based extractor (no `Option` allocation) for
// `case head :: tail =>` patterns on the opaque alias.
object `::`:
  final class ConsView[element](list: sci.List[element]):
    def isEmpty: Boolean = list.isEmpty
    def get: this.type = this
    def _1: element = list.head
    def _2: List[element] = List.of(list.tail)

  def unapply[element](list: List[element]): ConsView[element] = ConsView(list.stdlib)

opaque type List[+element] = sci.List[element]
