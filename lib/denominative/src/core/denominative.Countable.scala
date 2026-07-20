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
package denominative

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{ArrayBuffer, HashMap}

import anticipation.*
import prepositional.*

object Countable:
  given iarray: [element] => IArray[element] is Countable = (iarray: IArray[element]) => iarray.length
  given int: Int is Countable = identity(_)

  given arrayBuffer: [element] => ArrayBuffer[element] is Countable:
    def size(self: ArrayBuffer[element]): Int = self.length
    override def nil(self: ArrayBuffer[element]): Boolean = self.isEmpty

  given option: [element] => Option[element] is Countable:
    def size(self: Option[element]): Int = if self == None then 0 else 1
    override def nil(self: Option[element]): Boolean = self.isEmpty

  // `List#size` is O(n), so the `Countable` instance is gated behind `LinearSizeComplexity`; the O(1)
  // `nil`/`occupied` come from the ungated `Populable.list` instead.
  given list: [element] => (complexity: LinearSizeComplexity) => List[element] is Countable:
    def size(self: List[element]): Int = self.stdlib.length
    override def nil(self: List[element]): Boolean = self.stdlib.isEmpty

  given iterable: [element] => Iterable[element] is Countable:
    def size(self: Iterable[element]): Int = self.size
    override def nil(self: Iterable[element]): Boolean = self.isEmpty

  // Opaque `Map` is no longer an `Iterable` subtype, so its instance bridges via `stdlib`.
  given map: [key, element] => Map[key, element] is Countable:
    def size(self: Map[key, element]): Int = self.stdlib.size
    override def nil(self: Map[key, element]): Boolean = self.stdlib.isEmpty

  given trieMap: [key, element] => TrieMap[key, element] is Countable:
    def size(self: TrieMap[key, element]): Int = self.size
    override def nil(self: TrieMap[key, element]): Boolean = self.isEmpty

  given hashMap: [key, element] => HashMap[key, element] is Countable:
    def size(self: HashMap[key, element]): Int = self.size
    override def nil(self: HashMap[key, element]): Boolean = self.isEmpty

  // `Progression#length` forces the whole stream (and diverges on infinite ones), so the
  // `Countable` instance is gated behind `UnboundedSizeComplexity`; the O(1) `nil` comes from the
  // ungated `Populable.lazyList` instead.
  given lazyList: [element] => (complexity: UnboundedSizeComplexity) => Progression[element] is Countable:
    def size(self: Progression[element]): Int = self.stdlib.length
    override def nil(self: Progression[element]): Boolean = self.stdlib.isEmpty

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

  // Opaque `Series` is no longer an `IndexedSeq` subtype, so it needs its own instance.
  given series: [element] => Series[element] is Countable:
    def size(self: Series[element]): Int = self.stdlib.length
    override def nil(self: Series[element]): Boolean = self.stdlib.isEmpty

  given text: Text is Countable:
    def size(self: Text): Int = self.s.length
    override def nil(self: Text): Boolean = self.s.isEmpty

trait Countable extends Populable:
  def size(self: Self): Int
  def nil(self: Self): Boolean = size(self) == 0
