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
package murmuration

import prepositional.*

// Set algebra over abstract receivers: `intersect` keeps the elements present in both, and
// `except` removes the other's elements (the stdlib's `diff`). An ordered shape is treated as
// a multiset, and keeps its order. Union is not here: it is plain
// concatenation for an unordered unique shape, so `left + right` (symbolism's `Concatenable`)
// already provides it.
object Intersectable:
  // The declared `Result` is the PLAIN type: intersection can empty a non-empty receiver, so
  // a branded `Self` must not survive (the same soundness rule as `Truncable`'s `Result`).
  given set: [element] => Set[element] is Intersectable by Set[element] to Set[element] =
    new Intersectable:
      type Self = Set[element]
      type Operand = Set[element]
      type Result = Set[element]

      def intersect(left: Self, right: Operand): Result = Set.intersect(left, right)
      def except(left: Self, right: Operand): Result = Set.except(left, right)

  // The ordered shapes are multisets here: each element of the right operand accounts for at
  // most one equal element of the left, whose order is kept. `except` therefore removes one
  // occurrence for each occurrence in the operand (the stdlib's `Seq#diff`), and `intersect`
  // keeps an element as many times as it occurs in both. `Self` and `Operand` are
  // subtype-parametric, as `Concatenable`'s are, so that a branded value (a non-empty literal)
  // matches on either side.
  given list: [element, list <: List[element], operand <: List[element]]
  =>  list is Intersectable by operand to List[element] =

    new Intersectable:
      type Self = list
      type Operand = operand
      type Result = List[element]

      def intersect(left: Self, right: Operand): Result = List.intersect(left, right)
      def except(left: Self, right: Operand): Result = List.except(left, right)

  given sequence: [element, sequence <: Sequence[element], operand <: Sequence[element]]
  =>  sequence is Intersectable by operand to Sequence[element] =

    new Intersectable:
      type Self = sequence
      type Operand = operand
      type Result = Sequence[element]

      def intersect(left: Self, right: Operand): Result = Sequence.intersect(left, right)
      def except(left: Self, right: Operand): Result = Sequence.except(left, right)

  // The operand of a `Chain` is read in full, so it must be finite.
  given chain: [element] => Chain[element] is Intersectable by Chain[element] to Chain[element] =
    new Intersectable:
      type Self = Chain[element]
      type Operand = Chain[element]
      type Result = Chain[element]

      def intersect(left: Self, right: Operand): Result = Chain.intersect(left, right)
      def except(left: Self, right: Operand): Result = Chain.except(left, right)

trait Intersectable extends Typeclass.Pure, Operable, Resultant:
  def intersect(left: Self, right: Operand): Result
  def except(left: Self, right: Operand): Result

extension [self, operand](left: self)(using intersectable: self is Intersectable by operand)
  def intersect(right: operand): intersectable.Result = intersectable.intersect(left, right)
  def except(right: operand): intersectable.Result = intersectable.except(left, right)
