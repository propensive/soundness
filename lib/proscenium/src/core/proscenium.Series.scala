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

// The first opaque collection alias: an immutable indexed sequence backed by `Vector`, whose
// members are deliberately invisible — its API is the typeclass-driven extension surface
// (`Indexable`, `Countable`, `Traversable`, `Reshapable`, `Convertible`, …), which is total.
// The companion holds only construction, the `stdlib` bridge, pattern support and the `Seq`
// conversion (for vararg splices and stdlib-boundary interop); operations live with their
// typeclasses. The boundary functions cast (`asInstanceOf`): under capture checking an identity
// ascription fails box adaptation when the element type captures.
object Series:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing
  // lambdas crashes the capture checker's boxer (boxDeeply assertion).
  def of[element](vector: sci.Vector[element]): Series[element] =
    vector.asInstanceOf[Series[element]]

  def apply[element](elements: element*): Series[element] = of(sci.Vector(elements*))
  def empty[element]: Series[element] = of(sci.Vector.empty[element])

  // The parameter is capturing (`^`): iterators produced by the transforming operations capture
  // their lambdas, and a pure parameter type would reject them. It is consumed eagerly here.
  def from[element](elements: IterableOnce[element]^): Series[element] =
    of(sci.Vector.from(elements))

  def unapplySeq[element](series: Series[element]): Option[Seq[element]] = Some(series.stdlib)

  // Deliberately NO `Conversion[Series[e], Seq[e]]`: with `implicitConversions` enabled
  // globally, such a conversion applies at *member selection* too, silently re-exposing the
  // entire partial `Seq` surface (`series.head` would compile again). Vararg splices work
  // directly (`f(series*)`) via proscala's `spliceopaque` feature (3.9.0-RC1-p5+), which
  // pierces an opaque alias over a Seq/Array at splice positions only; other `Seq`-boundary
  // crossings use the explicit, greppable bridge (`series.stdlib`).

  extension [element](series: Series[element])
    inline def stdlib: sci.Vector[element] = series.asInstanceOf[sci.Vector[element]]

opaque type Series[+element] = sci.Vector[element]
