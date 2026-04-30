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
import chiaroscuro.*
import contingency.*
import denominative.*
import dissonance.*
import gossamer.*
import spectacular.*
import vacuous.*

object CodlNode:
  given inspectable: CodlNode is Inspectable = _.data.option.fold(t"!"): data =>
    t"${data.key}[${data.children.map(_.inspect).join(t",")}]"

  val empty: CodlNode = CodlNode()

  def apply(key: Text)(child: CodlNode*): CodlNode = CodlNode(Atom(key, IArray.from(child)))

  given contrastable: (CodlNode is Inspectable) => CodlNode is Contrastable = (left, right) =>
    if left == right then Juxtaposition.Same(left.inspect) else
      val comparison =
        diff(left.children, right.children).rdiff(_.id == _.id).changes.map:
          case Par(_, _, v) =>
            v.let(_.key).or(t"—") -> Juxtaposition.Same(v.let(_.inspect).toString.tt)

          case Ins(_, v) =>
            Optional(v).let(_.key).or(t"—") -> Juxtaposition.Different(t"—", v.inspect)

          case Del(_, v) =>
            v.let(_.key).or(t"—") -> Juxtaposition.Different(v.let(_.inspect).toString.tt, t"—")

          case Sub(_, v, lv, rv) =>
            if lv.let(_.key) == rv.let(_.key) then lv.let(_.key).or(t"—") -> lv.contrast(rv)
            else t"[key]" -> Juxtaposition.Different(lv.let(_.key).or(t"—"), rv.let(_.key).or(t"—"))

        . to(List)

      Juxtaposition.Collation(t"Node", comparison, left.key.or(t"—"), right.key.or(t"—"))

case class CodlNode(data: Optional[Atom] = Unset, extra: Optional[Extra] = Unset) extends Dynamic:
  def key: Optional[Text] = data.let(_.key)
  def nil: Boolean = unsafely(data.absent || data.assume.children.nil)
  def blank: Boolean = data.absent && extra.absent
  def schema: Optional[CodlSchema] = data.let(_.schema)
  def layout: Optional[Formation] = data.let(_.layout)
  def id: Optional[Text] = data.let(_.id)
  def uniqueId: Optional[Text] = data.let(_.uniqueId)
  def children: IArray[CodlNode] = data.let(_.children).or(IArray[CodlNode]())
  def paramValue: Optional[Text] = if children.nil then key else Unset
  def structValue: Optional[Text] = if children.size == 1 then children.head.paramValue else Unset
  def fieldValue: Optional[Text] = paramValue.or(structValue)
  def promote(n: Int) = copy(data = data.let(_.promote(n)))
  override def toString: String = data.toString

  def apply(key: Text): List[Atom] = data.lay(List[CodlNode]())(_(key)).map(_.data).collect:
    case data: Atom => data

  def selectDynamic(key: String)(using erased DynamicCodlEnabler): List[Atom] raises CodlError =

    data.lest(CodlError(CodlError.Reason.MissingValue(key.show))).selectDynamic(key)


  def applyDynamic(key: String)(index: Int = 0)(using erased DynamicCodlEnabler)
  :   Atom raises CodlError =

    selectDynamic(key)(index)


  def untyped: CodlNode =
    val data2 = data.let { data => Atom(data.key, children = data.children.map(_.untyped)) }
    CodlNode(data2, extra)

  def uncommented: CodlNode =
    val data2 = data.let: data =>
      Atom(data.key, children = data.children.map(_.uncommented), Formation.empty, data.schema)

    CodlNode(data2, Unset)

  def wiped: CodlNode = untyped.uncommented
