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
package gossamer

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import vacuous.*

object Lexicon:
  def apply[element]()(using Proximity by Int): Lexicon[element] = new Lexicon[element]:
    private var lexicon: Optional[Lexicon[element]] = Unset

    def update(key: Text, value: element): Unit =
      lexicon.let(_.update(key, value)).or:
        lexicon = Node(key, value)

    def search(query: Text, radius: Int): Set[element] = lexicon.lay(Set())(_.search(query, radius))

  def apply(terms: List[Text])(using Proximity { type Triangulable = true } by Int): Lexicon[Text] =
    apply(terms.bi.to(Map))


  def apply[element](terms: Map[Text, element])(using Proximity { type Triangulable = true } by Int)
  :   Lexicon[element] =

    if terms.nil then apply() else Node(terms.head(0), terms.head(1)).tap: tree =>
      terms.drop(1).each(tree(_) = _)


  class Node[element](term: Text, value: element)(using Proximity by Int) extends Lexicon[element]:
    val children: scm.HashMap[Int, Lexicon[element]] = scm.HashMap()

    def update(key: Text, value: element): Unit =
      val distance = term.proximity(key)
      children.at(distance).let(_(key) = value).or(children(distance) = new Node(key, value))

    def search(query: Text, radius: Int): Set[element] =
      val distance = query.proximity(term)
      children.collect:
        case (key, tree) if (distance - radius) <= key <= (distance + radius) =>
          tree.search(query, radius)

      . to(Set)
      . flatten
      ++ (if distance <= radius then Set(value) else Set())

trait Lexicon[element]:
  def update(key: Text, value: element): Unit
  def search(query: Text, radius: Int): Set[element]
