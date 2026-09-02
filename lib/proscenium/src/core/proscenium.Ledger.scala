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

// A `Ledger` is an insertion-ordered immutable map: iteration visits entries in the order
// they were first added, which `Map` deliberately does not promise. Backed by `sci.VectorMap`
// rather than `sci.ListMap`: both iterate in insertion order, but `VectorMap` looks up and
// appends in effectively constant time where `ListMap` needs a full traversal for each, and
// the registries kept in ledgers (dialect member tables, attribute lists, test reports) grow
// into the hundreds. Same design as `Map`: members invisible, API via typeclasses and the
// compat surface, construction and the greppable `stdlib` bridge in the companion, casts at
// the boundary, and deliberately NO `Conversion` to a stdlib supertype.
object Ledger:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing
  // lambdas crashes the capture checker's boxer (boxDeeply assertion).
  private[proscenium] def of[key, value](map: sci.VectorMap[key, value]): Ledger[key, value] =
    map.asInstanceOf[Ledger[key, value]]

  def apply[key, value](pairs: (key, value)*): Ledger[key, value] = of(sci.VectorMap(pairs*))
  def empty[key, value]: Ledger[key, value] = of(sci.VectorMap.empty[key, value])

  def from[key, value](pairs: IterableOnce[(key, value)]^): Ledger[key, value] =
    of(sci.VectorMap.from(pairs))

  // `.to(Ledger)` support (see `List`): the conversion is on `Ledger.type` only, so it cannot
  // expose members of `Ledger` values.
  given factory: [key, value] => Conversion[Ledger.type, scala.collection.Factory[(key, value), Ledger[key, value]]] =
    _ =>
      new scala.collection.Factory[(key, value), Ledger[key, value]]:
        def fromSpecific(elements: IterableOnce[(key, value)]^): Ledger[key, value] =
          Ledger.from(elements)

        def newBuilder: scala.collection.mutable.Builder[(key, value), Ledger[key, value]] =
          sci.VectorMap.newBuilder[key, value].mapResult(of(_))

  extension [key, value](ledger: Ledger[key, value])
    inline def stdlib: sci.VectorMap[key, value] = ledger.asInstanceOf[sci.VectorMap[key, value]]

  // The primitive operations, for the typeclass instances defined in the libraries above;
  // see `List` and `Map` for the rationale.
  def size[key, value](ledger: Ledger[key, value]): Int = ledger.size
  def nil[key, value](ledger: Ledger[key, value]): Boolean = ledger.isEmpty
  def defines[key, value](ledger: Ledger[key, value], index: key): Boolean = ledger.contains(index)
  def at[key, value](ledger: Ledger[key, value], index: key): value = ledger(index)
  def read[key, value](ledger: Ledger[key, value], index: key): Option[value] = ledger.get(index)

  def define[key, value](ledger: Ledger[key, value], index: key, value0: value)
  :   Ledger[key, value] =
    ledger.updated(index, value0)

  def omit[key, value](ledger: Ledger[key, value], index: key): Ledger[key, value] =
    ledger.removed(index)

  def keys[key, value](ledger: Ledger[key, value]): List[key] = List.of(ledger.keys.toList)
  def values[key, value](ledger: Ledger[key, value]): List[value] = List.of(ledger.values.toList)

  def concat[key, value](left: Ledger[key, value], right: Ledger[key, value])
  :   Ledger[key, value] =
    left ++ right

  // `map` transforms the *values*, preserving entry order, so it builds through
  // `VectorMap.from` rather than a view's unordered `toMap`.
  def map[key, value, value2](ledger: Ledger[key, value], lambda: value => value2)
  :   Ledger[key, value2] =
    sci.VectorMap.from(ledger.iterator.map { (key, value0) => key -> lambda(value0) })

  def iterator[key, value](ledger: Ledger[key, value]): Iterator[(key, value)] = ledger.iterator

opaque type Ledger[key, +value] = sci.VectorMap[key, value]
