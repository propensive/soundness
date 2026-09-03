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
package symbolism

import java.lang as jl

import scala.math.Ordering

import prepositional.*

// The low-priority half of `Comparable`'s companion: any stdlib `Ordering` confers a
// `Comparable`, so third-party types (and any type whose own comparison is still expressed as
// an `Ordering`) sort without ceremony. It sits in a parent trait rather than the companion
// body so that a native instance always outranks it; same-scope specificity is not reliable
// enough to depend on (see `murmuration.Traversable2`).
transparent trait Comparable2:
  given ordering: [value] => (ordering: Ordering[value]) => value is Comparable =
    (left, right) => Comparison(ordering.compare(left, right))

object Comparable extends Comparable2:
  def apply[value](lambda: (value, value) -> Comparison): value is Comparable =
    (left, right) => lambda(left, right)

  // The counterpart of `Ordering.fromLessThan`, for the comparisons that are naturally written
  // as a strict less-than. The predicate is applied twice for values that are not less-than in
  // the first order, which is the price of recovering a three-way result from a Boolean one.
  def less[value](lambda: (value, value) -> Boolean): value is Comparable = (left, right) =>
    if lambda(left, right) then Comparison.Less
    else if lambda(right, left) then Comparison.More
    else Comparison.Same

  // The primitives compare through their `java.lang` box methods, so `Double` and `Float` get
  // the JDK's *total* order (`NaN` sorts last, `-0.0` before `0.0`) rather than the partial
  // order of the `<` operator — the same choice as the stdlib's `Ordering.DoubleOrdering`.
  given int: Int is Comparable = (left, right) => Comparison(jl.Integer.compare(left, right))
  given long: Long is Comparable = (left, right) => Comparison(jl.Long.compare(left, right))
  given double: Double is Comparable = (left, right) => Comparison(jl.Double.compare(left, right))
  given float: Float is Comparable = (left, right) => Comparison(jl.Float.compare(left, right))
  given char: Char is Comparable = (left, right) => Comparison(jl.Character.compare(left, right))
  given byte: Byte is Comparable = (left, right) => Comparison(jl.Byte.compare(left, right))
  given short: Short is Comparable = (left, right) => Comparison(jl.Short.compare(left, right))

  given boolean: Boolean is Comparable =
    (left, right) => Comparison(jl.Boolean.compare(left, right))

  // Lexicographic order on lists: the first position at which the elements differ decides, and
  // a proper prefix precedes what extends it.
  given list: [element] => (element is Comparable) => List[element] is Comparable =
    (left, right) =>
      val comparable = summon[element is Comparable]
      val lefts = left.stdlib.iterator
      val rights = right.stdlib.iterator
      var result = Comparison.Same

      while result.same && lefts.hasNext && rights.hasNext
      do result = comparable.compare(lefts.next(), rights.next())

      result.also(Comparison(jl.Boolean.compare(lefts.hasNext, rights.hasNext)))

  given pair: [left, right] => (left is Comparable, right is Comparable)
  =>  (left, right) is Comparable = (first, second) =>
    summon[left is Comparable].compare(first(0), second(0))
    . also(summon[right is Comparable].compare(first(1), second(1)))

// A total three-way comparison of two values of the same type. Unlike hypotenuse's
// `Commensurable`/`Orderable`, whose `compare` is an abstract `inline def` resolved statically
// at each comparison operator, this one dispatches at runtime, so it serves the operations
// whose element type is abstract: sorting, extrema, and recurrence bounds.
//
// It lives in symbolism, below every collection, because instances are needed as far down as
// `denominative.Ordinal` and `anticipation.Level`.
trait Comparable extends Typeclass.Pure:
  comparable =>

  def compare(left: Self, right: Self): Comparison

  def less(left: Self, right: Self): Boolean = compare(left, right).less
  def atMost(left: Self, right: Self): Boolean = !compare(left, right).more
  def greater(left: Self, right: Self): Boolean = compare(left, right).more
  def atLeast(left: Self, right: Self): Boolean = !compare(left, right).less
  def same(left: Self, right: Self): Boolean = compare(left, right).same
  def min(left: Self, right: Self): Self = if compare(left, right).more then right else left
  def max(left: Self, right: Self): Self = if compare(left, right).less then right else left

  // Deliberately a method rather than a `given`: as a given it would form a cycle with
  // `Comparable2.ordering`. It exists only for the seams where a stdlib API insists on an
  // `Ordering` of its own — the ordering of `TreeMap`/`SortedMap` keys, say. `fromLessThan`
  // rather than a direct implementation because `Ordering` cannot be implemented under
  // `-Yexplicit-nulls`: it inherits `java.util.Comparator`, whose loaded signature takes
  // nullable parameters, which a `compare(Self, Self)` does not override.
  def ordering: Ordering[Self] = Ordering.fromLessThan(comparable.less(_, _))

  // Comparison by a projection, the counterpart of `Ordering.by`.
  def on[other](lambda: other -> Self): other is Comparable =
    (left, right) => comparable.compare(lambda(left), lambda(right))
