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

// The third opaque collection alias: an immutable map backed by `sci.Map`. Same design as
// `Sequence` and `Set`: members invisible, API via typeclasses (`Applicable` provides `at`/`defines`/
// `confine`; `Traversable by (key, value)` the transforming surface), construction and the
// greppable `stdlib` bridge in the companion, casts at the boundary, and deliberately NO
// `Conversion` to a stdlib supertype.
object Map:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing
  // lambdas crashes the capture checker's boxer (boxDeeply assertion).
  private[proscenium] def of[key, value](map: sci.Map[key, value]): Map[key, value] =
    map.asInstanceOf[Map[key, value]]

  def apply[key, value](pairs: (key, value)*): Map[key, value] = of(sci.Map(pairs*))
  def empty[key, value]: Map[key, value] = of(sci.Map.empty[key, value])

  def from[key, value](pairs: IterableOnce[(key, value)]^): Map[key, value] =
    of(sci.Map.from(pairs))

  // `.to[Map]` support (see `List`): the conversion is on `Map.type` only, so it cannot
  // expose members of `Map` values.
  given factory: [key, value] => Conversion[Map.type, scala.collection.Factory[(key, value), Map[key, value]]] =
    _ =>
      new scala.collection.Factory[(key, value), Map[key, value]]:
        def fromSpecific(elements: IterableOnce[(key, value)]^): Map[key, value] =
          Map.from(elements)

        def newBuilder: scala.collection.mutable.Builder[(key, value), Map[key, value]] =
          sci.Map.newBuilder[key, value].mapResult(of(_))

  extension [key, value](map: Map[key, value])
    inline def stdlib: sci.Map[key, value] = map.asInstanceOf[sci.Map[key, value]]

  // Lifting and unlifting for macros; see `List`'s companion for the full rationale
  // (public `from` splice, backticked pattern types, unlift limited to produced shapes).
  given toExpr: [key: {scala.quoted.Type, scala.quoted.ToExpr},
                 value: {scala.quoted.Type, scala.quoted.ToExpr}]
  =>  scala.quoted.ToExpr[Map[key, value]]:
    def apply(map: Map[key, value])(using scala.quoted.Quotes)
    :   scala.quoted.Expr[Map[key, value]] =
      '{Map.from(${scala.quoted.Expr.ofList(map.toList.map(scala.quoted.Expr(_)))})}

  given fromExpr: [key: {scala.quoted.Type, scala.quoted.FromExpr},
                   value: {scala.quoted.Type, scala.quoted.FromExpr}]
  =>  scala.quoted.FromExpr[Map[key, value]]:
    def unapply(expr: scala.quoted.Expr[Map[key, value]])(using scala.quoted.Quotes)
    :   Option[Map[key, value]] =
      expr match
        case '{Map.from($pairs: sci.List[(`key`, `value`)])} =>
          scala.quoted.FromExpr.ListFromExpr[(key, value)].unapply(pairs).map(Map.from(_))

        case _ => None

  // The primitive operations, for the typeclass instances defined in the libraries above;
  // see `List` for the rationale. Within this file the opaque alias is transparent. `read`
  // mirrors the stdlib's `Option`-returning `get`: the ergonomic `Optional` form is layered
  // above, in `vacuous`-aware libraries.
  def size[key, value](map: Map[key, value]): Int = map.size
  def nil[key, value](map: Map[key, value]): Boolean = map.isEmpty
  def defines[key, value](map: Map[key, value], index: key): Boolean = map.contains(index)
  def at[key, value](map: Map[key, value], index: key): value = map(index)
  def read[key, value](map: Map[key, value], index: key): Option[value] = map.get(index)

  def define[key, value](map: Map[key, value], index: key, value0: value): Map[key, value] =
    map.updated(index, value0)

  def omit[key, value](map: Map[key, value], index: key): Map[key, value] = map.removed(index)
  def keys[key, value](map: Map[key, value]): Set[key] = Set.of(map.keySet)
  def values[key, value](map: Map[key, value]): List[value] = List.of(map.values.toList)

  def concat[key, value](left: Map[key, value], right: Map[key, value]): Map[key, value] =
    left ++ right

  // `map` transforms the *values*, keys structural — the shape `Mappable` gives a `Map`.
  def map[key, value, value2](map: Map[key, value], lambda: value => value2): Map[key, value2] =
    map.view.mapValues(lambda).toMap

  def iterator[key, value](map: Map[key, value]): Iterator[(key, value)] = map.iterator

opaque type Map[key, +value] = sci.Map[key, value]
