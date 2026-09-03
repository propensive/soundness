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
package proscenium

import scala.collection.immutable as sci

// The second opaque collection alias: an immutable set backed by `sci.Set`. Invariant, like the
// stdlib's. Same design as `Sequence`: members invisible, API via typeclasses, construction and
// the greppable `stdlib` bridge in the companion, casts at the boundary (capture checking
// rejects identity ascriptions for capturing element types), and deliberately NO `Conversion`
// to any stdlib supertype, which would re-expose the partial surface at member selection.
object Set:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing
  // lambdas crashes the capture checker's boxer (boxDeeply assertion).
  private[proscenium] def of[element](set: sci.Set[element]): Set[element] =
    set.asInstanceOf[Set[element]]

  def apply[element](elements: element*): Set[element] = of(sci.Set(elements*))
  def empty[element]: Set[element] = of(sci.Set.empty[element])

  def from[element](elements: IterableOnce[element]^): Set[element] =
    of(sci.Set.from(elements))

  // `.to[Set]` support (see `List`): the conversion is on `Set.type` only, so it cannot
  // expose members of `Set` values.
  given factory: [element] => Conversion[Set.type, scala.collection.Factory[element, Set[element]]] =
    _ =>
      new scala.collection.Factory[element, Set[element]]:
        def fromSpecific(elements: IterableOnce[element]^): Set[element] =
          Set.from(elements)

        def newBuilder: scala.collection.mutable.Builder[element, Set[element]] =
          sci.Set.newBuilder[element].mapResult(of(_))

  extension [element](set: Set[element])
    inline def stdlib: sci.Set[element] = set.asInstanceOf[sci.Set[element]]

  // Lifting and unlifting for macros; see `List`'s companion for the full rationale
  // (public `from` splice, backticked pattern types, unlift limited to produced shapes).
  given toExpr: [element: {scala.quoted.Type, scala.quoted.ToExpr}]
  =>  scala.quoted.ToExpr[Set[element]]:
    def apply(set: Set[element])(using scala.quoted.Quotes): scala.quoted.Expr[Set[element]] =
      '{Set.from(${scala.quoted.Expr.ofList(set.toList.map(scala.quoted.Expr(_)))})}

  given fromExpr: [element: {scala.quoted.Type, scala.quoted.FromExpr}]
  =>  scala.quoted.FromExpr[Set[element]]:
    def unapply(expr: scala.quoted.Expr[Set[element]])(using scala.quoted.Quotes)
    :   Option[Set[element]] =
      expr match
        case '{Set.from($elements: sci.List[`element`])} =>
          scala.quoted.FromExpr.ListFromExpr[element].unapply(elements).map(Set.from(_))

        case _ => None

  // The primitive operations, for the typeclass instances defined in the libraries above;
  // see `List` for the rationale. Within this file the opaque alias is transparent.
  def size[element](set: Set[element]): Int = set.size
  def nil[element](set: Set[element]): Boolean = set.isEmpty
  def has[element](set: Set[element], value: element): Boolean = set.contains(value)
  def insert[element](set: Set[element], value: element): Set[element] = set.incl(value)
  def concat[element](left: Set[element], right: Set[element]): Set[element] = left ++ right

  def intersect[element](left: Set[element], right: Set[element]): Set[element] =
    left.intersect(right)

  def except[element](left: Set[element], right: Set[element]): Set[element] = left.diff(right)

  def map[element, element2](set: Set[element], lambda: element => element2): Set[element2] =
    set.map(lambda)

  def iterator[element](set: Set[element]): Iterator[element] = set.iterator

opaque type Set[element] = sci.Set[element]
