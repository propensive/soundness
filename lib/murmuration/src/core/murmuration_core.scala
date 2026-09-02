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
import scala.collection.mutable as scm
import scala.math.Ordering

import prepositional.*

// `collection.has(value)` (value membership) for any `collection` that is `Inclusive`; the queried
// value type is fixed by the instance's `Operand`. Whether a *key/index* is present is `Applicable`'s
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

  // A value-taking overload is deliberately omitted, like `where`'s: a path-dependent
  // `traversable.Operand` makes it ambiguous with the predicate form for any lambda argument,
  // so `count(_ == value)` is the idiom for counting a specific element.
  inline def count(predicate: traversable.Operand => Boolean): Int =
    traversable.traverse(self).count(predicate)

  // Flattens one level of nesting, rebuilding in the outer source's shape: the inner
  // values may be any `Traversable`, so a `List[Set[element]]` flattens to `List[element]`.
  def flat[element2, result]
    ( using innerTraversable: traversable.Operand is Traversable by element2,
            reshapable:       self is Reshapable by element2 to result )
  :   result =

    reshapable.reshape:
      traversable.traverse(self).flatMap { element => innerTraversable.traverse(element) }

  // The running accumulation (the stdlib's `scanLeft`): every intermediate state, initial
  // state first, in the source's own (stable) shape.
  def trace[state, result](initial: state)(lambda: (state, traversable.Operand) => state)
    ( using reshapable: self is Reshapable.Stable by state to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).scanLeft(initial)(lambda))

  // The contiguous run from `from` (inclusive) to `until` (exclusive), by position: the
  // total counterpart of `slice`, empty when the bounds fall outside the source.
  def excerpt[result](from: Int, until: Int)
    ( using reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).slice(from, until))

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


      traversable.traverse(self).toList.groupBy(lambda).map: (key, elements) =>
        (key, reshapable.reshape(elements.iterator))
      . to(Map)

  // `Stable` receivers only: sorting an unordered shape (`Set`, `Map`) is honestly unavailable
  // rather than silently order-dropping. `order` sorts by a projection of each element; `sort`
  // (below) sorts by the elements' own order. A comparator overload (the stdlib's `sortWith`)
  // cannot join this name: a two-parameter lambda would resolve to it by arity, breaking
  // parameter untupling on pair-`Operand` receivers (`map.order { (key, value) => key }`).
  def order[key, result](lambda: traversable.Operand => key)
    ( using ordering:   Ordering[key],
            reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).toList.sortBy(lambda).iterator)

  def distinct[result]
    ( using reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).distinct)

  // The first element for each value of `lambda`, in first-occurrence order (the stdlib's
  // `distinctBy`). Which occurrence survives is positional, so — like `distinct` — this is
  // honestly unavailable on unordered shapes. Not `Iterator#distinctBy`, whose parameter
  // capture checking requires to be pure; a seen-set filter accepts any lambda `filter` would.
  def deduplicate[key, result](lambda: traversable.Operand => key)
    ( using reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   result =

    val seen = scm.HashSet[key]()
    reshapable.reshape(traversable.traverse(self).filter { element => seen.add(lambda(element)) })

  // The longest leading run satisfying `predicate`, and the remainder; each side is traversed
  // independently, since a single `Iterator` cannot be consumed twice.
  def span[result](predicate: traversable.Operand => Boolean)
    ( using reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   (result, result) =

    ( reshapable.reshape(traversable.traverse(self).takeWhile(predicate)),
      reshapable.reshape(traversable.traverse(self).dropWhile(predicate)) )

  // Every element satisfying `predicate`, and every element that does not; each side is rebuilt
  // in the source's own shape. Unlike `span`, membership ignores position, so unordered shapes
  // (`Set`, `Map`) partition meaningfully; hence not `Stable`.
  def partition[result](predicate: traversable.Operand => Boolean)
    ( using reshapable: self is Reshapable by traversable.Operand to result )
  :   (result, result) =

    ( reshapable.reshape(traversable.traverse(self).filter(predicate)),
      reshapable.reshape(traversable.traverse(self).filterNot(predicate)) )

  // Sorting by the elements' own order, the no-key sibling of `order(lambda)` above.
  def sort[result]
    ( using ordering:   Ordering[traversable.Operand],
            reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).toList.sorted.iterator)

  // Filter and map in one pass, keeping only the elements the partial function is defined at
  // (the stdlib's `collect`). Not `Stable`: gathering from a `Set` or `Map` is meaningful. The
  // partial function is capability-annotated so that lambdas capturing a `Tactic` (or any other
  // capability) are accepted; `sweep` applies it eagerly and retains nothing.
  def sweep[element2, result](lambda: PartialFunction[traversable.Operand, element2]^)
    ( using reshapable: self is Reshapable by element2 to result )
  :   result =

    reshapable.reshape(traversable.traverse(self).collect(lambda))

  // Split into consecutive batches of at most `size` elements, each rebuilt in the source's own
  // shape; `Stable` because the batch boundaries depend on order. The final batch may be shorter.
  def batched[result](size: Int)
    ( using reshapable: self is Reshapable.Stable by traversable.Operand to result )
  :   List[result] =


      traversable.traverse(self).grouped(size).map { chunk => reshapable.reshape(chunk.iterator) }
      . toList
      . to(List)

