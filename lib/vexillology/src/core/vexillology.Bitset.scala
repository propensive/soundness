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
package vexillology

import java.util as ju

import scala.caps

import denominative.*
import murmuration.*
import prepositional.*
import symbolism.*

// An immutable set of non-negative integers, each given as an `Ordinal`, held as bits: a member
// costs one bit, so a set of small integers (the rows of a table which have been visited, the
// positions in a text which matched) is dense, and its union, intersection and difference with
// another are a word at a time. `Flags` is the same idea for the cases of an enumeration, and
// `B64` for exactly sixty-four bits; a `Bitset` grows to its largest member.
//
// It is a collection like any other: `has` tests a member, `+` and `-` add and remove one,
// `+` with another `Bitset` is their union, `intersect` and `except` are as for a `Set`, `size`
// counts the members, and traversal gives them in ascending order.
object Bitset:
  val empty: Bitset = new Bitset(ju.BitSet())

  def apply(ordinals: Ordinal*): Bitset =
    build: builder =>
      List.iterator(ordinals).foreach(builder.add(_))

  // Makes a `Bitset` by filling it in a block:
  //
  //     Bitset.build: bits =>
  //       rows.each: row =>
  //         if row.visited then bits.add(row.index)
  //
  // The builder is a capability the block cannot let escape, so nothing can write to the bits
  // once they are a `Bitset`.
  def build(lambda: Builder^ => Unit): Bitset =
    val builder: Builder^ = new Builder()
    lambda(builder)
    builder.result()

  // The set while it is being filled. Only an exclusive reference can write to it, and the
  // only one there is belongs to the block `build` runs.
  final class Builder private[vexillology] () extends caps.Mutable:
    private val bits: ju.BitSet = ju.BitSet()

    def has(ordinal: Ordinal): Boolean = ordinal.n0 >= 0 && bits.get(ordinal.n0)
    update def add(ordinal: Ordinal): Unit = bits.set(ordinal.n0)
    update def remove(ordinal: Ordinal): Unit = bits.clear(ordinal.n0)

    private[vexillology] def result(): Bitset = new Bitset(bits.clone.asInstanceOf[ju.BitSet])

  given inclusive: Bitset is Inclusive by Ordinal = (bitset, ordinal) => bitset.contains(ordinal)

  given addable: Bitset is Addable by Ordinal to Bitset = Addable(_.including(_))

  given subtractable: Bitset is Subtractable by Ordinal to Bitset =
    Subtractable(_.excluding(_))

  // Union, as for a `Set`: `left + right`.
  given concatenable: Bitset is Concatenable by Bitset to Bitset = (left, right) => left.union(right)

  given intersectable: Bitset is Intersectable by Bitset to Bitset =
    new Intersectable:
      type Self = Bitset
      type Operand = Bitset
      type Result = Bitset

      def intersect(left: Bitset, right: Bitset): Bitset = left.intersection(right)
      def except(left: Bitset, right: Bitset): Bitset = left.difference(right)

  // The members are counted a word at a time, not one by one, so `size` is ungated.
  given countable: Bitset is Countable:
    def size(self: Bitset): Int = self.cardinality
    override def nil(self: Bitset): Boolean = self.vacant

  // In ascending order, from each member to the next set bit.
  given traversable: Bitset is Traversable by Ordinal = _.ordinals

// The bits are never written once the `Bitset` exists: every operation which changes the set
// works on a copy. Equality and the hash are the bits', so two sets with the same members are
// equal however they were made.
final class Bitset private[vexillology] (private val bits: ju.BitSet):
  private def derive(lambda: ju.BitSet => Unit): Bitset =
    val copy: ju.BitSet = bits.clone.asInstanceOf[ju.BitSet]
    lambda(copy)
    new Bitset(copy)

  private[vexillology] def contains(ordinal: Ordinal): Boolean =
    ordinal.n0 >= 0 && bits.get(ordinal.n0)

  private[vexillology] def including(ordinal: Ordinal): Bitset =
    if contains(ordinal) then this else derive(_.set(ordinal.n0))

  private[vexillology] def excluding(ordinal: Ordinal): Bitset =
    if contains(ordinal) then derive(_.clear(ordinal.n0)) else this

  private[vexillology] def union(that: Bitset): Bitset = derive(_.or(that.bits))
  private[vexillology] def intersection(that: Bitset): Bitset = derive(_.and(that.bits))
  private[vexillology] def difference(that: Bitset): Bitset = derive(_.andNot(that.bits))
  private[vexillology] def cardinality: Int = bits.cardinality
  private[vexillology] def vacant: Boolean = bits.isEmpty

  private[vexillology] def ordinals: Iterator[Ordinal]^ =
    Iterator.iterate(bits.nextSetBit(0)) { index => bits.nextSetBit(index + 1) }
    . keep(_ >= 0)
    . map(_.z)

  // The greatest member, if there is one.
  def last: vacuous.Optional[Ordinal] =
    if bits.isEmpty then vacuous.Unset else (bits.length - 1).z

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Bitset => bits.equals(that.bits)
    case _            => false

  override def hashCode: Int = bits.hashCode
  override def toString: String = bits.toString
