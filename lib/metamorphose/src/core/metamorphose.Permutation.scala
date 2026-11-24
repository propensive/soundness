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
package metamorphose

import scala.annotation.*
import scala.collection.mutable.BitSet

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*

object Permutation:
  // The big-endian two's-complement bytes of the permutation's factoradic number.
  given encodable: Permutation is Encodable in Data = permutation =>
    // [java-boundary] fresh array returned by the JDK, retained by nobody else
    Array.unsafeFrozen(permutation.factoradic.number.toByteArray)

  def bySize(n: Int): Chain[Permutation] = Chain.range[BigInt](0, Factorial(n)).map: i =>
    Permutation(Factoradic(i))

  def apply(sequence: Sequence[Int]): Permutation raises Permutation.Error =
    val length = sequence.size
    val seen: BitSet = BitSet()
    var index = 0

    sequence.each: element =>
      if element >= length || element < 0
      then raise(Permutation.Error(Permutation.Error.Reason.InvalidIndex(element, length - 1)))

      if seen.has(element)
      then raise(Permutation.Error(Permutation.Error.Reason.DuplicateIndex(element, index)))

      seen(element) = true
      index += 1

    unchecked(sequence)

  // The number of a sequence already known to be a permutation of its own indices: what `apply`
  // checks, and what `inverse` constructs. The digit for each position is its entry in the
  // inversion table: how many of the indices before it are greater than it. Position `i` has
  // at most `i` of those, so it is a digit in base `i + 1`, with weight `i!`; and an index
  // which stays in place after every moved one has none, so adds nothing. The number is
  // therefore the same however many fixed indices follow, which is what lets a permutation
  // act on the front of any longer sequence.
  private def unchecked(sequence: Sequence[Int]): Permutation =
    val seen: BitSet = BitSet()
    var index = 0
    var digits: List[Int] = Nil

    sequence.each: element =>
      digits = (index - seen.count(_ < element)) :: digits
      seen(element) = true
      index += 1

    Permutation(Factoradic.unchecked(digits))

  // PermutationError → Permutation.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case BaseRange(value: Int, base: Int)        extends Reason(1)
      case DuplicateIndex(index: Int, element: Int) extends Reason(2)
      case InvalidIndex(last: Int, max: Int)       extends Reason(3)
      case TooShort(length: Int, min: Int)         extends Reason(4)

    import Reason.*

    given communicable: Reason is Communicable =
      case BaseRange(value, base) =>
        m"the value $value is too large for its positional base $base"

      case DuplicateIndex(element, index) =>
        m"the index $element was duplicated at $index"

      case InvalidIndex(index, max) =>
        m"the index $index appears, but every index should be in the range 0-$max"

      case TooShort(size, min) =>
        m"the input, of size $size, is too short for the permutation of size $min"

  case class Error(reason: Permutation.Error.Reason)(using Diagnostics)
  extends fulminate.Error(28, 1, reason.number)(m"could not construct permutation because $reason")

// A permutation of the natural numbers which moves only finitely many of them: it rearranges
// the first `extent` positions, and fixes every position from `extent` on. So it is not tied to
// one length: `Permutation(Sequence(0, 2, 1))` and `Permutation(Sequence(0, 2, 1, 3, 4))` are the
// same permutation, which acts on any sequence of three elements or more.
case class Permutation(factoradic: Factoradic):
  // The inversion table, most significant digit first: the entry for position `extent - 1`,
  // which is never zero, down to the entry for position 0, which always is.
  private lazy val digits: List[Int] = factoradic.expand

  // The number of leading positions the permutation rearranges: one more than the last
  // position it moves, or zero for the identity.
  lazy val extent: Int = digits.size

  // For each of the first `extent` positions in order, how many earlier positions hold a
  // greater index.
  lazy val inversions: List[Int] = digits.reverse

  // The index at each of the first `extent` positions.
  lazy val expansion: List[Int] =
    // Decoded from the last position back: among the indices not yet placed, which are those
    // of positions `0` to `position`, the one with `digit` greater indices beside it.
    def recur(digits: List[Int], position: Int, remaining: List[Int], result: List[Int])
    :   List[Int] =

      digits match
        case digit :: more =>
          remaining.skip(position - digit) match
            case index :: after =>
              recur(more, position - 1, remaining.keep(position - digit) + after, index :: result)

            case _ =>
              result

        case _ =>
          result

    recur(digits, extent - 1, List.range(0, extent), Nil)

  def apply(n: Int): Int =
    // A permutation fixes every point outside its domain; `List` positional access is
    // O(n), accepted here explicitly through the asymptotic gate.
    import denominative.dysasymptotics.linearAccess
    expansion(Ordinal.zerary(n)).or(n)

  def apply[element](sequence: List[element]): List[element] raises Permutation.Error =
    if sequence.size < extent then
      raise(Permutation.Error(Permutation.Error.Reason.TooShort(sequence.size, extent)))

    permute(sequence)

  // Applies the permutation to a sequence of at least `extent` elements: what `apply` checks.
  // The first `extent` elements are rearranged, each position taking the element at the index
  // `expansion` gives it, and the rest follow unmoved.
  private def permute[element](sequence: List[element]): List[element] =
    // `List` positional access is O(n), accepted here explicitly through the asymptotic gate.
    import denominative.dysasymptotics.linearAccess
    val moved = sequence.keep(extent)
    expansion.flatMap { index => moved(Ordinal.zerary(index)).lay(Nil)(List(_)) } +
      sequence.skip(extent)

  def inverse: Permutation = if extent == 0 then this else
    val array: scala.Array[Int]^ = new scala.Array(extent)
    var index = 0
    var sequence: List[Int] = expansion

    while sequence.absolve match
      case head :: tail => array(head) = index
        index += 1
        sequence = tail
        true

      case Nil          => false
    do ()

    Permutation.unchecked(Sequence.from(array.iterator))
