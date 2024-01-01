/*
    Cellulose, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import perforate.*
import gossamer.*
import anticipation.*
import contextual.*
import spectacular.*
import dissonance.*
import chiaroscuro.*

import java.io as ji

import language.dynamics
//import language.experimental.captureChecking

object CodlNode:
  given Debug[CodlNode] = _.data.option.fold(t"!"): data =>
    t"${data.key}[${data.children.map(_.debug).join(t",")}]"

  val empty: CodlNode = CodlNode()
  def apply(key: Text)(child: CodlNode*): CodlNode = CodlNode(Data(key, IArray.from(child)))
  
  given Contrast[CodlNode] = (left, right) =>
    if left == right then Semblance.Identical(left.debug) else
      val comparison = IArray.from:
        diff(left.children, right.children).rdiff(_.id == _.id).changes.map:
          case Par(_, _, v) => v.let(_.key).or(t"—") -> Semblance.Identical(v.debug)
          case Ins(_, v)    => v.let(_.key).or(t"—") -> Semblance.Different(t"—", v.debug)
          case Del(_, v)    => v.let(_.key).or(t"—") -> Semblance.Different(v.debug, t"—")
          
          case Sub(_, v, lv, rv) =>
            if lv.let(_.key) == rv.let(_.key) then lv.let(_.key).or(t"—") -> lv.contrastWith(rv)
            else t"[key]" -> Semblance.Different(lv.let(_.key).or(t"—"), rv.let(_.key).or(t"—"))

      Semblance.Breakdown(comparison, left.key.or(t"—"), right.key.or(t"—"))
  
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

  def selectDynamic(key: String)(using erased DynamicCodlEnabler)(using Raises[MissingValueError]): List[Data] =
    data.option.getOrElse(abort(MissingValueError(key.show))).selectDynamic(key)
  
  def applyDynamic(key: String)(idx: Int = 0)(using erased DynamicCodlEnabler)(using Raises[MissingValueError]): Data = selectDynamic(key)(idx)

  def untyped: CodlNode =
    val data2 = data.let { data => Data(data.key, children = data.children.map(_.untyped)) }
    CodlNode(data2, meta)
  
  def uncommented: CodlNode =
    val data2 = data.let { data => Data(data.key, children = data.children.map(_.uncommented), Layout.empty, data.schema) }
    CodlNode(data2, Unset)

  def wiped: CodlNode = untyped.uncommented
  
object CodlDoc:
  def apply(nodes: CodlNode*): CodlDoc = CodlDoc(IArray.from(nodes), CodlSchema.Free, 0)

  given debug: Debug[CodlDoc] = _.write
  given show(using printer: CodlPrinter): Show[CodlDoc] = printer.serialize(_)
  
  given similarity: Similarity[CodlDoc] = _.schema == _.schema

  inline given contrast: Contrast[CodlDoc] = new Contrast[CodlDoc]:
    def apply(left: CodlDoc, right: CodlDoc) =
      inline if left == right then Semblance.Identical(left.debug) else
        val comparison = IArray.from:
          (t"[schema]", left.schema.contrastWith(right.schema)) +:
          (t"[margin]", left.margin.contrastWith(right.margin)) +:
          diff(left.children, right.children).rdiff(_.id == _.id).changes.map:
            case Par(_, _, v)      => v.let(_.key).or(t"—") -> Semblance.Identical(v.debug)
            case Ins(_, v)         => v.let(_.key).or(t"—") -> Semblance.Different(t"—", v.debug)
            case Del(_, v)         => v.let(_.key).or(t"—") -> Semblance.Different(v.debug, t"—")
            case Sub(_, v, lv, rv) =>
              val key = if lv.let(_.key) == rv.let(_.key) then lv.let(_.key).or(t"—") else t"${lv.let(_.key).or(t"—")}/${rv.let(_.key).or(t"—")}"
              key -> lv.contrastWith(rv)
        
        Semblance.Breakdown(comparison, t"", t"")

case class CodlDoc(children: IArray[CodlNode], schema: CodlSchema, margin: Int, body: LazyList[Char] = LazyList())
extends Indexed:
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: CodlDoc => schema == that.schema && margin == that.margin && children.sameElements(that.children)
    case _         => false

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


  def as[T](using reader: CodlReader[T])(using Raises[CodlReadError]): T = reader.read(List(this))
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

object Data:
  given [T: CodlWriter]: Insertion[List[Data], T] =
    value => summon[CodlWriter[T]].write(value).head.to(List).map(_.data).collect { case data: Data => data }

  given debug: Debug[Data] = data => t"Data(${data.key}, ${data.children.length})"

case class Data(key: Text, children: IArray[CodlNode] = IArray(), layout: Layout = Layout.empty,
                    schema: CodlSchema = CodlSchema.Free)
extends Indexed:

  lazy val paramIndex: Map[Text, Int] =
    (0 until layout.params.min(schema.paramCount)).map: idx =>
      schema.subschemas(idx).key -> idx
    .to(Map)

  def uniqueId: Optional[Text] = schema.subschemas.find(_.schema.arity == Arity.Unique) match
    case Some(CodlSchema.Entry(name: Text, schema)) =>
      paramIndex.get(name).map(children(_).fieldValue).getOrElse(Unset)
    case _ => Unset

  def id: Optional[Text] = schema.subschemas.find(_.schema.arity == Arity.Unique) match
    case Some(CodlSchema.Entry(name: Text, schema)) =>
      index(name).let(_.headOption.optional).let(children(_).fieldValue)
    case _ => key

  def promote(n: Int): Data = copy(layout = layout.copy(params = n))

  def has(key: Text): Boolean = index.contains(key) || paramIndex.contains(key)
  
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: Data => key == that.key && children.sameElements(that.children) && layout == that.layout &&
                           schema == that.schema
    case _          => false

  override def hashCode: Int = key.hashCode ^ children.toSeq.hashCode ^ layout.hashCode ^ schema.hashCode


case class Meta(blank: Int = 0, comments: List[Text] = Nil, remark: Optional[Text] = Unset)
object Layout:
  final val empty = Layout(0, false, 0)

case class Layout(params: Int, multiline: Boolean, col: Int)

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
    .view.mapValues(_.reverse).to(Map)
    
  protected lazy val idIndex: Map[Text, Int] =
    def recur(idx: Int, map: Map[Text, Int] = Map()): Map[Text, Int] =
      if idx < 0 then map else recur(idx - 1, children(idx).id.lay(map)(map.updated(_, idx)))
    
    recur(children.length - 1)

  def ids: Set[Text] = idIndex.keySet

  def apply(idx: Int = 0)(using Raises[MissingIndexValueError]): CodlNode =
    children.lift(idx).getOrElse(abort(MissingIndexValueError(idx)))
  
  def apply(key: Text): List[CodlNode] =
    index.get(key).getOrElse(Nil).map(children(_))

  def get(key: Text): List[Indexed] =
    paramIndex.lift(key) match
      case None => index.lift(key) match
        case None       => Nil
        case Some(idxs) => idxs.map(children(_).data.vouch(using Unsafe))
      
      case Some(idx) =>
        List.range(idx, layout.params).map: idx =>
          Data(key, IArray(unsafely(children(idx))), Layout.empty, CodlSchema.Free)

  def selectDynamic(key: String)(using erased DynamicCodlEnabler)(using Raises[MissingValueError]): List[Data] =
    index(key.show).map(children(_).data).collect:
      case data: Data => data
  
  def applyDynamic(key: String)(idx: Int = 0)(using erased DynamicCodlEnabler)(using Raises[MissingValueError]): Data = selectDynamic(key)(idx)

erased trait DynamicCodlEnabler

object dynamicCodlAccess:
  erased given enabled: DynamicCodlEnabler = ###
