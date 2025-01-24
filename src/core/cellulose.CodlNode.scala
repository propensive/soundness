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
import chiaroscuro.*
import contextual.*
import contingency.*
import dissonance.*
import gossamer.{where as _, *}
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics

object CodlNode:
  given CodlNode is Inspectable = _.data.option.fold(t"!"): data =>
    t"${data.key}[${data.children.map(_.inspect).join(t",")}]"

  val empty: CodlNode = CodlNode()
  def apply(key: Text)(child: CodlNode*): CodlNode = CodlNode(Data(key, IArray.from(child)))

  given contrastable: (CodlNode is Inspectable) => CodlNode is Contrastable = (left, right) =>
    if left == right then Juxtaposition.Same(left.inspect) else
      val comparison = IArray.from:
        diff(left.children, right.children).rdiff(_.id == _.id).changes.map:
          case Par(_, _, v) => v.let(_.key).or(t"—") -> Juxtaposition.Same(v.let(_.inspect).toString.tt)
          case Ins(_, v)    => Optional(v).let(_.key).or(t"—") -> Juxtaposition.Different(t"—", v.inspect)
          case Del(_, v)    => v.let(_.key).or(t"—") -> Juxtaposition.Different(v.let(_.inspect).toString.tt, t"—")

          case Sub(_, v, lv, rv) =>
            if lv.let(_.key) == rv.let(_.key) then lv.let(_.key).or(t"—") -> lv.juxtapose(rv)
            else t"[key]" -> Juxtaposition.Different(lv.let(_.key).or(t"—"), rv.let(_.key).or(t"—"))

      Juxtaposition.Collation(comparison, left.key.or(t"—"), right.key.or(t"—"))

case class CodlNode(data: Optional[Data] = Unset, meta: Optional[Meta] = Unset) extends Dynamic:
  def key: Optional[Text] = data.let(_.key)
  def empty: Boolean = unsafely(data.absent || data.assume.children.isEmpty)
  def blank: Boolean = data.absent && meta.absent
  def schema: Optional[CodlSchema] = data.let(_.schema)
  def layout: Optional[Layout] = data.let(_.layout)
  def id: Optional[Text] = data.let(_.id)
  def uniqueId: Optional[Text] = data.let(_.uniqueId)
  def children: IArray[CodlNode] = data.let(_.children).or(IArray[CodlNode]())
  def paramValue: Optional[Text] = if children.isEmpty then key else Unset
  def structValue: Optional[Text] = if children.size == 1 then children.head.paramValue else Unset
  def fieldValue: Optional[Text] = paramValue.or(structValue)
  def promote(n: Int) = copy(data = data.let(_.promote(n)))

  def apply(key: Text): List[Data] = data.lay(List[CodlNode]())(_(key)).map(_.data).collect:
    case data: Data => data

  def selectDynamic(key: String)(using erased DynamicCodlEnabler)(using Tactic[MissingValueError]): List[Data] =
    data.lest(MissingValueError(key.show)).selectDynamic(key)

  def applyDynamic(key: String)(idx: Int = 0)(using erased DynamicCodlEnabler)(using Tactic[MissingValueError]): Data = selectDynamic(key)(idx)

  def untyped: CodlNode =
    val data2 = data.let { data => Data(data.key, children = data.children.map(_.untyped)) }
    CodlNode(data2, meta)

  def uncommented: CodlNode =
    val data2 = data.let { data => Data(data.key, children = data.children.map(_.uncommented), Layout.empty, data.schema) }
    CodlNode(data2, Unset)

  def wiped: CodlNode = untyped.uncommented
