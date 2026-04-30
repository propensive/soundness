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

import java.io as ji

import anticipation.*
import chiaroscuro.*
import contingency.*
import dissonance.*
import distillate.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

object CodlDoc:
  def apply(nodes: CodlNode*): CodlDoc = CodlDoc(IArray.from(nodes), CodlSchema.Free, 0)

  given inspectable: CodlDoc is Inspectable = _.write
  given showable: (printer: CodlPrinter) => CodlDoc is Showable = printer.serialize(_)
  given similarity: Similarity[CodlDoc] = _.schema == _.schema


  given aggregable: [subject: CodlSchematic] => Tactic[ParseError]
  =>  (CodlDoc of subject) is Aggregable by Text =

    subject.schema().parse(_).asInstanceOf[CodlDoc of subject]


  given aggregable2: Tactic[ParseError] => CodlDoc is Aggregable by Text = Codl.parse(_)

case class CodlDoc
  ( children: IArray[CodlNode], schema: CodlSchema, margin: Int, body: Stream[Char] = Stream() )
extends Indexed:
  type Topic

  override def toString: String = s"^[${children.mkString(", ")}]"

  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: CodlDoc =>
      schema == that.schema && margin == that.margin && children.sameElements(that.children)

    case _ =>
      false

  override def hashCode: Int = children.toSeq.hashCode ^ schema.hashCode ^ margin.hashCode
  def layout: Formation = Formation.empty
  def paramIndex: Map[Text, Int] = Map()
  def materialize(using Topic is Decodable in Codl): Topic raises CodlError = as[Topic]

  def merge(input: CodlDoc): CodlDoc =
    def cmp(x: CodlNode, y: CodlNode): Boolean =
      if x.uniqueId.absent || y.uniqueId.absent then
        if x.data.absent || y.data.absent then x.extra == y.extra
        else x.data == y.data
      else
        x.id == y.id

    def recur(original: IArray[CodlNode], updates: IArray[CodlNode]): IArray[CodlNode] =
      val changes = diff[CodlNode](children, updates, cmp).edits

      val nodes2 = changes.foldLeft(List[CodlNode]()):
        case (nodes, Del(left, value))  => nodes
        case (nodes, Ins(right, value)) => value :: nodes

        case (nodes, Par(left, right, value)) =>
          val orig: CodlNode = original(left)
          val origAtom: Atom = orig.data.or(???)

          if orig.id.absent || updates(right).id.absent then orig :: nodes
          else
            val children2 = recur(origAtom.children, updates(right).data.or(???).children)
            // FIXME: Check layout remains safe
            orig.copy(data = origAtom.copy(children = children2)) :: nodes

      IArray.from(nodes2.reverse)

    copy(children = recur(children, input.children))

  def as[value: Decodable in Codl]: value raises CodlError = value.decoded(Codl(List(this)))
  def uncommented: CodlDoc = CodlDoc(children.map(_.uncommented), schema, margin, body)
  def untyped: CodlDoc = CodlDoc(children.map(_.untyped), CodlSchema.Free, margin, body)
  def wiped = uncommented.untyped

  def bcodl: Text =
    val writer: ji.Writer = ji.StringWriter()
    Bcodl.write(writer, this)
    writer.toString().tt

  def write: Text =
    val writer: ji.Writer = ji.StringWriter()
    Printer.print(writer, this)
    writer.toString().tt
