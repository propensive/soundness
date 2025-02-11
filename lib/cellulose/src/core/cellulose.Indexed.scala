/*
    Cellulose, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import anticipation.*
import contingency.*
import denominative.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics

trait Indexed extends Dynamic:
  def children: IArray[CodlNode]
  def schema: CodlSchema
  def layout: Layout
  def paramIndex: Map[Text, Int]

  lazy val index: Map[Text, List[Int]] =
    children.map(_.data).zipWithIndex.foldLeft(Map[Text, List[Int]]()):
      case (acc, (data: Data, idx)) =>
        if idx < layout.params then schema.param(idx).lay(acc): entry =>
          acc.upsert(entry.key, _.lay(List(idx))(idx :: _))
        else acc.upsert(data.key, _.lay(List(idx))(idx :: _))
      case (acc, _) => acc

    . view.mapValues(_.reverse).to(Map)

  protected lazy val idIndex: Map[Text, Int] =
    def recur(idx: Int, map: Map[Text, Int] = Map()): Map[Text, Int] =
      if idx < 0 then map else recur(idx - 1, children(idx).id.lay(map)(map.updated(_, idx)))

    recur(children.length - 1)

  def ids: Set[Text] = idIndex.keySet

  def apply(idx: Int = 0)(using Tactic[MissingIndexValueError]): CodlNode =
    children.at(idx.z).lest(MissingIndexValueError(idx))

  def apply(key: Text): List[CodlNode] = index.at(key).or(Nil).map(children(_))

  def get(key: Text): List[Indexed] =
    paramIndex.lift(key) match
      case None => index.lift(key) match
        case None       => Nil
        case Some(idxs) => idxs.map(children(_).data.vouch)

      case Some(idx) =>
        List.range(idx, layout.params).map: idx =>
          Data(key, IArray(unsafely(children(idx))), Layout.empty, CodlSchema.Free)

  def selectDynamic(key: String)(using erased DynamicCodlEnabler)(using Tactic[MissingValueError])
  :     List[Data] =
    index(key.show).map(children(_).data).collect:
      case data: Data => data

  def applyDynamic(key: String)(idx: Int = 0)(using erased DynamicCodlEnabler)
     (using Tactic[MissingValueError])
  :     Data =

    selectDynamic(key)(idx)
