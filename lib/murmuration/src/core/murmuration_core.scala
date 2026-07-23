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

import scala.collection.immutable as sci
import scala.math.Ordering

import prepositional.*

// `collection.has(value)` (value membership) for any `collection` that is `Inclusive`; the queried
// value type is fixed by the instance's `Operand`. Whether a *key/index* is present is `Indexable`'s
// `defines` instead (that lives in `rudiments`, being `Ordinal`-adjacent).
extension [self](self: self)(using inclusive: self is Inclusive)
  def has(value: inclusive.Operand): Boolean = inclusive.has(self, value)

// The shape-preserving `map`, driven by `Mappable` rather than `Traversable`+`Reshapable`, so a
// `Map` maps its *values* (keys preserved) instead of iterating `(key, value)` pairs; `remap` (below)
// covers the pairwise/entry case. `mappable` is summoned at the *extension* level so the lambda's
// parameter type is the concrete `mappable.Operand` (so `xs.map(_.field)` infers the element type);
// the mapped-container constructor is bound as the higher-kinded type parameter `result[_]` so the
// return type `result[element2]` is a plain application, never a path-dependent projection (#1411).
extension [self, result[_]](self: self)
  (using mappable: self is Mappable { type Result[element2] = result[element2] })
  def map[element2](lambda: mappable.Operand => element2): result[element2] =
    mappable.map(self, lambda)

// The transforming operations over any `Traversable`, rebuilt through `Reshapable`. `remap` is the
// pairwise/entry map (a `Map`'s entries as `(key, value)` pairs, reshaped into whatever the lambda's
// result implies). The `Ordinal`-indexed `each` and `Optional`-returning `seek`/`where` stay in
// rudiments (they depend on denominative/vacuous, which sit above this module).
extension [self](self: self)(using traversable: self is Traversable)
  def remap[element2, result](lambda: traversable.Operand => element2)
    ( using reshapable: self is Reshapable by element2 to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).map(lambda))

  // `subsumes` tests whether `subsequence` occurs as a contiguous run of elements within `self` — a
  // substring, for `Text`. The empty subsequence is always present.
  def subsumes(subsequence: self): Boolean =
    val whole = sci.Vector.from(traversable.traverse(self))
    val part  = sci.Vector.from(traversable.traverse(subsequence))
    val last  = whole.length - part.length

    part.isEmpty || whole.indices.exists: start =>
      start <= last && part.indices.forall: offset =>
        whole(start + offset) == part(offset)

  // The preferred name for monadic binding at explicit call sites; `flatMap` (below) is the same
  // operation, retained solely because `for`-comprehensions desugar to that name.
  def bind[inner, element2, result](lambda: traversable.Operand => inner)
    ( using innerTraversable: inner is Traversable by element2,
            reshapable:       self is Reshapable by element2 to result )
  :   result =

    reshapable.reshape:
      traversable.traverse(self).flatMap { element => innerTraversable.traverse(lambda(element)) }

  def flatMap[inner, element2, result](lambda: traversable.Operand => inner)
    ( using innerTraversable: inner is Traversable by element2,
            reshapable:       self is Reshapable by element2 to result )
  :   result =

    reshapable.reshape:
      traversable.traverse(self).flatMap { element => innerTraversable.traverse(lambda(element)) }

  def filter[result](predicate: traversable.Operand => Boolean)
    ( using reshapable: self is Reshapable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).filter(predicate))

  // For-comprehension desugaring requires the literal name `withFilter`; this is *strict*, a
  // deliberate divergence from the stdlib's lazy `WithFilter` carrier, whose captured predicate
  // would have to thread through every desugared stage under capture checking.
  def withFilter[result](predicate: traversable.Operand => Boolean)
    ( using reshapable: self is Reshapable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).filter(predicate))

  def foreach(lambda: traversable.Operand => Unit): Unit =
    traversable.traverse(self).foreach(lambda)

  inline def exists(predicate: traversable.Operand => Boolean): Boolean =
    traversable.traverse(self).exists(predicate)

  def fold[state](initial: state)(lambda: (state, traversable.Operand) => state): state =
    traversable.traverse(self).foldLeft(initial)(lambda)

  def zip[that, result](that: that)
    ( using thatTraversable: that is Traversable,
            reshapable:      self is Reshapable.Stable
                             by (traversable.Operand, thatTraversable.Operand) to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).zip(thatTraversable.traverse(that)))

  // Each group is rebuilt in the source's own shape.
  def group[key, result](lambda: traversable.Operand => key)
    ( using reshapable: self is Reshapable by traversable.Operand to result )
  :   Map[key, result] =

    Map.of:
      traversable.traverse(self).toList.groupBy(lambda).map: (key, elements) =>
        (key, reshapable.reshape(elements.iterator))

  // `Stable` receivers only: sorting an unordered shape (`Set`, `Map`) is honestly unavailable
  // rather than silently order-dropping.
  def sort[key, result](lambda: traversable.Operand => key)
    ( using ordering:   Ordering[key],
            reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).toList.sortBy(lambda).iterator)

  def distinct[result]
    ( using reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).distinct)

  // Split into consecutive batches of at most `size` elements, each rebuilt in the source's own
  // shape; `Stable` because the batch boundaries depend on order. The final batch may be shorter.
  def batched[result](size: Int)
    ( using reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   List[result] =

    List.of:
      traversable.traverse(self).grouped(size).map { chunk => reshapable.reshape(chunk.iterator) }
      . toList
