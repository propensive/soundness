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
import contingency.*
import dissonance.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import java.io as ji

import language.dynamics

object CodlDoc:
  def apply(nodes: CodlNode*): CodlDoc = CodlDoc(IArray.from(nodes), CodlSchema.Free, 0)

  given CodlDoc is Inspectable = _.write
  given (printer: CodlPrinter) => CodlDoc is Showable = printer.serialize(_)

  given similarity: Similarity[CodlDoc] = _.schema == _.schema

  // given contrastable: CodlDoc is Contrastable:
  //     type Self = CodlDoc
  //     def apply(left: CodlDoc, right: CodlDoc) =
  //       if left == right then Juxtaposition.Same(left.inspect) else
  //         val comparison = IArray.from:
  //           (t"[schema]", left.schema.juxtapose(right.schema)) +:
  //           (t"[margin]", left.margin.juxtapose(right.margin)) +:
  //           diff(left.children, right.children).rdiff(_.id == _.id).changes.map:
  //             case Par(_, _, v)      =>
  //               v.let(_.key).or(t"—") -> Juxtaposition.Same(v.let(_.inspect).toString.tt)

  //             case Ins(_, v)         =>
  //               v.let(_.key).or(t"—") -> Juxtaposition.Different(t"—", v.inspect)

  //             case Del(_, v)         =>
  //               v.let(_.key).or(t"—") -> Juxtaposition.Different(v.let(_.inspect).toString.tt, t"—")

  //             case Sub(_, v, lv, rv) =>
  //               val key =
  //                 if lv.let(_.key) == rv.let(_.key) then lv.let(_.key).or(t"—")
  //                 else t"${lv.let(_.key).or(t"—")}/${rv.let(_.key).or(t"—")}"

  //               key -> lv.juxtapose(rv)

  //         Juxtaposition.Collation(comparison, t"", t"")

case class CodlDoc
   (children: IArray[CodlNode], schema: CodlSchema, margin: Int, body: Stream[Char] = Stream())
extends Indexed:
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: CodlDoc =>
      schema == that.schema && margin == that.margin && children.sameElements(that.children)

    case _ =>
      false

  override def hashCode: Int = children.toSeq.hashCode ^ schema.hashCode ^ margin.hashCode

  def layout: Layout = Layout.empty
  def paramIndex: Map[Text, Int] = Map()

  def merge(input: CodlDoc): CodlDoc =

    def cmp(x: CodlNode, y: CodlNode): Boolean =
      if x.uniqueId.absent || y.uniqueId.absent then
        if x.data.absent || y.data.absent then x.meta == y.meta
        else x.data == y.data
      else x.id == y.id

    def recur(original: IArray[CodlNode], updates: IArray[CodlNode]): IArray[CodlNode] =
      val changes = diff[CodlNode](children, updates, cmp).edits

      val nodes2 = changes.foldLeft(List[CodlNode]()):
        case (nodes, Del(left, value))         => nodes
        case (nodes, Ins(right, value))        => value :: nodes
        case (nodes, Par(left, right, value)) =>
          val orig: CodlNode = original(left)
          val origData: Data = orig.data.or(???)

          if orig.id.absent || updates(right).id.absent then orig :: nodes
          else
            val children2 = recur(origData.children, updates(right).data.or(???).children)
            // FIXME: Check layout remains safe
            orig.copy(data = origData.copy(children = children2)) :: nodes

      IArray.from(nodes2.reverse)

    copy(children = recur(children, input.children))

  def as[ValueType: CodlDecoder]: ValueType raises CodlReadError = ValueType.decode(List(this))
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
