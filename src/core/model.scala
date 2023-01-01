/*
    Cellulose, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import contextual.*
import eucalyptus.*
import quagmire.*

import java.io as ji

import language.dynamics

object Nodule:
  given Debug[Nodule] = _.data.option.fold(t"!"): data =>
    t"${data.key}[${data.children.map(_.debug).join(t",")}]"

  val empty: Nodule = Nodule()
  def apply(key: Text)(child: Nodule*): Nodule = Nodule(Data(key, IArray.from(child)))

case class Nodule(data: Maybe[Data] = Unset, meta: Maybe[Meta] = Unset) extends Dynamic:
  def key: Maybe[Text] = data.mm(_.key)
  def empty: Boolean = unsafely(data.unset || data.assume.children.isEmpty)
  def blank: Boolean = data.unset && meta.unset
  def schema: Maybe[CodlSchema] = data.mm(_.schema)
  def layout: Maybe[Layout] = data.mm(_.layout)
  def id: Maybe[Text] = data.mm(_.id)
  def uniqueId: Maybe[Text] = data.mm(_.uniqueId)
  def children: IArray[Nodule] = data.mm(_.children).or(IArray[Nodule]())
  def paramValue: Maybe[Text] = if children.isEmpty then key else Unset
  def structValue: Maybe[Text] = if children.size == 1 then children.head.paramValue else Unset
  def fieldValue: Maybe[Text] = paramValue.or(structValue)
  def promote(n: Int) = copy(data = data.mm(_.promote(n)))

  def apply(key: Text): List[Data] = data.fm(List[Nodule]())(_(key)).map(_.data).sift[Data]

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    data.option.getOrElse(throw MissingValueError(key.show)).selectDynamic(key)
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)

  def untyped: Nodule =
    val data2 = data.mm { data => Data(data.key, children = data.children.map(_.untyped)) }
    Nodule(data2, meta)
  
  def uncommented: Nodule =
    val data2 = data.mm { data => Data(data.key, children = data.children.map(_.uncommented), Layout.empty, data.schema) }
    Nodule(data2, Unset)

  def wiped: Nodule = untyped.uncommented
  
  override def toString(): String =
    if !children.isEmpty then s"$key[${children.mkString(" ")}]" else key.mm(_.s).or:
      meta.toString

object CodlDoc:
  def apply(nodes: Nodule*): CodlDoc = CodlDoc(IArray.from(nodes), CodlSchema.Free, 0)

case class CodlDoc(children: IArray[Nodule], schema: CodlSchema, margin: Int, body: LazyList[Text] = LazyList())
extends Indexed:
  override def toString(): String = s"[[${children.mkString(" ")}]]"
  
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: CodlDoc => schema == that.schema && margin == that.margin && children.sameElements(that.children)
    case _         => false

  override def hashCode: Int = children.toSeq.hashCode ^ schema.hashCode ^ margin.hashCode

  def layout: Layout = Layout.empty
  def paramIndex: Map[Text, Int] = Map()

  def merge(input: CodlDoc): CodlDoc =
    
    def cmp(x: Nodule, y: Nodule): Boolean =
      if x.uniqueId.unset || y.uniqueId.unset then
        if x.data.unset || y.data.unset then x.meta == y.meta
        else x.data == y.data
      else x.id == y.id

    def recur(original: IArray[Nodule], updates: IArray[Nodule]): IArray[Nodule] =
      val diff = Diff.diff[Nodule](children, updates, cmp)
      
      val nodes2 = diff.changes.foldLeft(List[Nodule]()):
        case (nodes, Change.Del(left, value))         => nodes
        case (nodes, Change.Ins(right, value))        => value :: nodes
        case (nodes, Change.Keep(left, right, value)) =>
          val orig: Nodule = original(left)
          val origData: Data = orig.data.or(???)
          
          if orig.id.unset || updates(right).id.unset then orig :: nodes
          else
            val children2 = recur(origData.children, updates(right).data.or(???).children)
            // FIXME: Check layout remains safe
            orig.copy(data = origData.copy(children = children2)) :: nodes
      
      IArray.from(nodes2.reverse)
    
    copy(children = recur(children, input.children))


  def as[T](using codec: Codec[T]): T throws IncompatibleTypeError = codec.deserialize(List(this))
  def uncommented: CodlDoc = CodlDoc(children.map(_.uncommented), schema, margin, body)
  def untyped: CodlDoc = CodlDoc(children.map(_.untyped), CodlSchema.Free, margin, body)
  def wiped = uncommented.untyped

  def binary(using Log): Text =
    val writer: ji.Writer = ji.StringWriter()
    Bin.write(writer, this)
    writer.toString().show

  def serialize: Text =
    val writer: ji.Writer = ji.StringWriter()
    Printer.print(writer, this)
    writer.toString().show

object Data:
  given [T: Codec]: Insertion[List[Data], T] =
    value => summon[Codec[T]].serialize(value).head.to(List).map(_.data).sift[Data]

case class Data(key: Text, children: IArray[Nodule] = IArray(), layout: Layout = Layout.empty,
                    schema: CodlSchema = CodlSchema.Free)
extends Indexed:

  lazy val paramIndex: Map[Text, Int] =
    (0 until layout.params.min(schema.paramCount)).map: idx =>
      schema.subschemas(idx).key -> idx
    .to(Map)

  def uniqueId: Maybe[Text] = schema.subschemas.find(_.schema.arity == Arity.Unique) match
    case Some(CodlSchema.Entry(name: Text, schema)) =>
      paramIndex.get(name).map(children(_).fieldValue).getOrElse(Unset)
    case None => Unset

  def id: Maybe[Text] = schema.subschemas.find(_.schema.arity == Arity.Unique) match
    case Some(CodlSchema.Entry(name: Text, schema)) =>
      index(name).mm(_.headOption.maybe).mm(children(_).fieldValue)
    case _ => key

  def promote(n: Int): Data = copy(layout = layout.copy(params = n))

  def has(key: Text): Boolean = index.contains(key) || paramIndex.contains(key)
  override def toString(): String = s"[${children.mkString(" ")}]"
  
  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: Data => key == that.key && children.sameElements(that.children) && layout == that.layout &&
                           schema == that.schema
    case _          => false

  override def hashCode: Int = key.hashCode ^ children.toSeq.hashCode ^ layout.hashCode ^ schema.hashCode


case class Meta(blank: Int = 0, comments: List[Text] = Nil, remark: Maybe[Text] = Unset, tabs: Tabs = Tabs())
object Layout:
  final val empty = Layout(0, false)

case class Layout(params: Int, multiline: Boolean)
case class Tabs(stops: TreeSet[Int] = TreeSet())

trait Indexed extends Dynamic:
  def children: IArray[Nodule]
  def schema: CodlSchema
  def layout: Layout
  def paramIndex: Map[Text, Int]

  lazy val index: Map[Text, List[Int]] =
    children.map(_.data).zipWithIndex.foldLeft(Map[Text, List[Int]]()):
      case (acc, (data: Data, idx)) =>
        if idx < layout.params then schema.param(idx).fm(acc): entry =>
          acc.upsert(entry.key, _.fm(List(idx))(idx :: _))
        else acc.upsert(data.key, _.fm(List(idx))(idx :: _))
      case (acc, _) => acc
    .view.mapValues(_.reverse).to(Map)

  protected lazy val idIndex: Map[Text, Int] =
    def recur(idx: Int, map: Map[Text, Int] = Map()): Map[Text, Int] =
      if idx < 0 then map else recur(idx - 1, children(idx).id.fm(map)(map.updated(_, idx)))
    
    recur(children.length - 1)

  def ids: Set[Text] = idIndex.keySet

  def apply(idx: Int = 0): Nodule throws MissingIndexValueError =
    children.lift(idx).getOrElse(throw MissingIndexValueError(idx))
  
  def apply(key: Text): List[Nodule] = index.get(key).getOrElse(Nil).map(children(_))

  def get(key: Text): List[Indexed] = paramIndex.lift(key) match
    case None      => index.lift(key) match
      case None       => Nil
      case Some(idxs) => unsafely(idxs.map(children(_).data.assume))
    case Some(idx) => List.range(idx, layout.params).map: idx =>
                        Data(key, IArray(unsafely(children(idx))), Layout.empty, CodlSchema.Free)

  def selectDynamic(key: String): List[Data] throws MissingValueError =
    index(key.show).map(children(_).data).sift[Data]
  
  def applyDynamic(key: String)(idx: Int = 0): Data throws MissingValueError = selectDynamic(key)(idx)
