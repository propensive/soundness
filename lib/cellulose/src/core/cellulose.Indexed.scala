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
package cellulose

import language.dynamics

import anticipation.*
import contingency.*
import denominative.*
import rudiments.*
import spectacular.*
import vacuous.*

object Codllike:
  def apply(children0: IArray[CodlNode]): Codllike = new Codllike:
    def children = children0

trait Codllike:
  def children: IArray[CodlNode]

trait Indexed extends Codllike, Dynamic:
  def children: IArray[CodlNode]
  def schema: CodlSchema
  def layout: Formation
  def paramIndex: Map[Text, Int]

  lazy val index: Map[Text, List[Int]] =
    children.map(_.data).zipWithIndex.foldLeft(Map[Text, List[Int]]()):
      case (acc, (data: Atom, index)) =>
        if index < layout.params then schema.param(index).lay(acc): entry =>
          acc.upsert(entry.key, _.lay(List(index))(index :: _))
        else acc.upsert(data.key, _.lay(List(index))(index :: _))

      case (acc, _) =>
        acc

    . view.mapValues(_.reverse).to(Map)

  protected lazy val idIndex: Map[Text, Int] =
    def recur(index: Int, map: Map[Text, Int] = Map()): Map[Text, Int] =
      if index < 0 then map
      else recur(index - 1, children(index).id.lay(map)(map.updated(_, index)))

    recur(children.length - 1)

  def ids: Set[Text] = idIndex.keySet

  def apply(index: Int = 0): CodlNode raises CodlError =
    children.at(index.z).lest(CodlError(CodlError.Reason.MissingIndexValue(index)))

  def apply(key: Text): List[CodlNode] = index.at(key).or(Nil).map(children(_))

  def get(key: Text): List[Indexed] =
    paramIndex.lift(key) match
      case None =>
        index.lift(key) match
          case None          => Nil
          case Some(indexes) => indexes.map(children(_).data.vouch)

      case Some(index) =>
        List.range(index, layout.params).map: index =>
          Atom(key, IArray(unsafely(children(index))), Formation.empty, CodlSchema.Free)

  def selectDynamic(key: String)(using erased DynamicCodlEnabler): List[Atom] raises CodlError =
    index(key.show).map(children(_).data).collect:
      case data: Atom => data


  def applyDynamic(key: String)(index: Int = 0)(using erased DynamicCodlEnabler)
  :   Atom raises CodlError =

    selectDynamic(key)(index)
