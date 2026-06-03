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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import scala.collection.mutable.{ArrayBuffer, HashMap}

import anticipation.*
import prepositional.*

object Countable:
  given iarray: [element] => IArray[element] is Countable = _.length
  given int: Int is Countable = identity(_)

  given sequence: [element] => Seq[element] is Countable:
    def size(self: Seq[element]): Int = self.length
    override def nil(self: Seq[element]): Boolean = self.isEmpty

  given arrayBuffer: [element] => ArrayBuffer[element] is Countable:
    def size(self: ArrayBuffer[element]): Int = self.length
    override def nil(self: ArrayBuffer[element]): Boolean = self.isEmpty

  given option: [element] => Option[element] is Countable:
    def size(self: Option[element]): Int = if self == None then 0 else 1
    override def nil(self: Option[element]): Boolean = self.isEmpty

  given list: [element] => List[element] is Countable:
    def size(self: List[element]): Int = self.length
    override def nil(self: List[element]): Boolean = self.scala.isEmpty

  // The project `List` (above) is opaque; the standard library's `List` needs its own instance.
  given scalaList: [element] => scala.collection.immutable.List[element] is Countable:
    def size(self: scala.collection.immutable.List[element]): Int = self.length
    override def nil(self: scala.collection.immutable.List[element]): Boolean = self.isEmpty

  given map: [key, element] => Map[key, element] is Countable:
    def size(self: Map[key, element]): Int = self.size
    override def nil(self: Map[key, element]): Boolean = self.isEmpty

  given trieMap: [key, element] => TrieMap[key, element] is Countable:
    def size(self: TrieMap[key, element]): Int = self.size
    override def nil(self: TrieMap[key, element]): Boolean = self.isEmpty

  given hashMap: [key, element] => HashMap[key, element] is Countable:
    def size(self: HashMap[key, element]): Int = self.size
    override def nil(self: HashMap[key, element]): Boolean = self.isEmpty

  given lazyList: [element] => LazyList[element] is Countable:
    def size(self: LazyList[element]): Int = self.length
    override def nil(self: LazyList[element]): Boolean = self.isEmpty

  given stringBuilder: StringBuilder is Countable:
    def size(self: StringBuilder): Int = self.length
    override def nil(self: StringBuilder): Boolean = self.isEmpty

  given set: [element] => Set[element] is Countable:
    def size(self: Set[element]): Int = self.scala.size
    override def nil(self: Set[element]): Boolean = self.scala.isEmpty

  given series: [element] => Series[element] is Countable:
    def size(self: Series[element]): Int = self.length
    override def nil(self: Series[element]): Boolean = self.scala.isEmpty

  given indexedSeq: [element] => IndexedSeq[element] is Countable:
    def size(self: IndexedSeq[element]): Int = self.length
    override def nil(self: IndexedSeq[element]): Boolean = self.isEmpty

  given text: Text is Countable:
    def size(self: Text): Int = self.s.length
    override def nil(self: Text): Boolean = self.s.isEmpty

object Sizable:
  // Every `Countable` (collections, `Text`, …) is `Sizable`; this bridge makes those instances
  // resolvable when searching for `X is Sizable` (the instances live in `Countable`'s companion).
  given countable: [value] => (countable: value is Countable) => value is Sizable =
    countable.size(_)

trait Sizable extends Typeclass:
  def size(self: Self): Int

trait Countable extends Sizable:
  def nil(self: Self): Boolean = size(self) == 0
