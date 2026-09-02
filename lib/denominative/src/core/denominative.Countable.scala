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
package denominative

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{ArrayBuffer, HashMap}

import anticipation.*
import prepositional.*

object Countable:
  // NOT subtype-parametric, unlike `Traversable`'s instances: several parametric instances in
  // one companion let a Self-directed search select the wrong collection's candidate and fail
  // hard inside an explicit evidence parameter (seen via `Vacuiscible.countable`), and a
  // generic `self & Populated` bridge mis-unifies (`self` absorbs the brand) and diverges.
  // Consequence: `Countable`-gated operations (`size`, `gamut`, `keep`…) see a branded value
  // through an ascription to its unbranded type — a pre-existing edge, since branded
  // receivers never had these operations.

  // The frozen array, `Array[element]^{}`, likewise: `length` is O(1) and reads are sound
  // through any reference, so the instance is ungated.
  given frozenArray: [element] => (Array[element]^{}) is Countable =
    (array: Array[element]^{}) => array.length
  given int: Int is Countable = identity(_)

  given arrayBuffer: [element] => ArrayBuffer[element] is Countable:
    def size(self: ArrayBuffer[element]): Int = self.length
    override def nil(self: ArrayBuffer[element]): Boolean = self.isEmpty

  given option: [element] => Option[element] is Countable:
    def size(self: Option[element]): Int = if self == None then 0 else 1
    override def nil(self: Option[element]): Boolean = self.isEmpty

  // `List#size` is O(n), so the `Countable` instance is gated behind `Dysasymptotic.LinearSize`; the O(1)
  // `nil`/`occupied` come from the ungated `Vacuiscible.list` instead.
  given list: [element, list <: List[element]] => (complexity: Dysasymptotic.LinearSize)
  =>  list is Countable:
    def size(self: list): Int = self.stdlib.length
    override def nil(self: list): Boolean = self.stdlib.isEmpty

  given iterable: [element] => Iterable[element] is Countable:
    def size(self: Iterable[element]): Int = self.size
    override def nil(self: Iterable[element]): Boolean = self.isEmpty

  // Opaque `Map` is no longer an `Iterable` subtype, so its instance bridges via `stdlib`.
  given map: [key, element] => Map[key, element] is Countable:
    def size(self: Map[key, element]): Int = self.stdlib.size
    override def nil(self: Map[key, element]): Boolean = self.stdlib.isEmpty

  given ledger: [key, element] => Ledger[key, element] is Countable:
    def size(self: Ledger[key, element]): Int = self.stdlib.size
    override def nil(self: Ledger[key, element]): Boolean = self.stdlib.isEmpty

  given trieMap: [key, element] => TrieMap[key, element] is Countable:
    def size(self: TrieMap[key, element]): Int = self.size
    override def nil(self: TrieMap[key, element]): Boolean = self.isEmpty

  given hashMap: [key, element] => HashMap[key, element] is Countable:
    def size(self: HashMap[key, element]): Int = self.size
    override def nil(self: HashMap[key, element]): Boolean = self.isEmpty

  // `Chain#length` forces the whole stream (and diverges on infinite ones), so the
  // `Countable` instance is gated behind `Dysasymptotic.UnboundedSize`; the O(1) `nil` comes from the
  // ungated `Vacuiscible.chain` instead.
  given chain: [element] => (complexity: Dysasymptotic.UnboundedSize) => Chain[element] is Countable:
    def size(self: Chain[element]): Int = self.stdlib.length
    override def nil(self: Chain[element]): Boolean = self.stdlib.isEmpty

  given stringBuilder: StringBuilder is Countable:
    def size(self: StringBuilder): Int = self.length
    override def nil(self: StringBuilder): Boolean = self.isEmpty

  // Opaque `Set` is no longer an `Iterable` subtype, so its instance bridges via `stdlib`.
  given set: [element] => Set[element] is Countable:
    def size(self: Set[element]): Int = self.stdlib.size
    override def nil(self: Set[element]): Boolean = self.stdlib.isEmpty

  given indexedSeq: [element] => IndexedSeq[element] is Countable:
    def size(self: IndexedSeq[element]): Int = self.length
    override def nil(self: IndexedSeq[element]): Boolean = self.isEmpty

  // Opaque `Sequence` is no longer an `IndexedSeq` subtype, so it needs its own instance.
  given sequence: [element, sequence <: Sequence[element]] => sequence is Countable:
    def size(self: sequence): Int = self.stdlib.length
    override def nil(self: sequence): Boolean = self.stdlib.isEmpty

  given text: Text is Countable:
    def size(self: Text): Int = self.s.length
    override def nil(self: Text): Boolean = self.s.isEmpty

trait Countable extends Vacuiscible:
  def size(self: Self): Int
  def nil(self: Self): Boolean = size(self) == 0
