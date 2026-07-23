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
package rudiments

import scala.collection.immutable.Seq

import scala.collection as sc
import scala.collection.immutable as sci

import denominative.*
import prepositional.*

object Bijection:
  def apply[key, value](map: sci.Map[key, value]): Bijection[key, value] =
    Bijection(map, map.map(_.swap).to(sci.Map))

  // `Indexable` (in `denominative`) can't host this — `Bijection` lives above it — but `Bijection`'s
  // own companion is in implicit scope for `Bijection is Indexable`, keeping `bijection.at(key)` working.
  given indexable: [key, value] => Bijection[key, value] is Indexable:
    type Self = Bijection[key, value]
    type Operand = key
    type Result = value

    def contains(value: Self, index: key): Boolean = value.map.contains(index)
    def access(value: Self, index: key): value = value.map(index)

case class Bijection[key, value](map: sci.Map[key, value], transposition: sci.Map[value, key])
extends Iterable[(key, value)], sc.Map[key, value]:
  private inline def bijection: this.type = this
  def iterator: Iterator[(key, value)] = map.iterator

  infix def - (key: key): Bijection[key, value] =
    Bijection(map - key, transposition - map(key))

  infix def - (key1: key, key2: key, keys: Seq[key]): Bijection[key, value] =
    Bijection(map - key1 - key2 -- keys, transposition - map(key1) - map(key2) -- keys.map(map(_)))

  def get(key: key): Option[value] = map.get(key)

  def flip: Bijection[value, key] = new Bijection(transposition, map):
    override def flip: Bijection[key, value] = bijection
