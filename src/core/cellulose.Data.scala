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
import contextual.*
import gossamer.{where as _, *}
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics

object Data:
  given insertion[T: CodlEncoder]: Insertion[List[Data], T] =
    value => summon[CodlEncoder[T]].encode(value).head.to(List).map(_.data).collect { case data: Data => data }

  given Data is Inspectable = data => t"Data(${data.key}, ${data.children.length})"

case class Data(key: Text, children: IArray[CodlNode] = IArray(), layout: Layout = Layout.empty,
                    schema: CodlSchema = CodlSchema.Free)
extends Indexed:

  lazy val paramIndex: Map[Text, Int] =
    (0 until layout.params.min(schema.paramCount)).map: idx =>
      schema.subschemas(idx).key -> idx

    . to(Map)

  def uniqueId: Optional[Text] = schema.subschemas.where(_.schema.arity == Arity.Unique).let:
    case CodlSchema.Entry(name: Text, schema) => paramIndex.at(name).let(children(_).fieldValue)
    case _                                    => Unset

  def id: Optional[Text] = schema.subschemas.where(_.schema.arity == Arity.Unique) match
    case CodlSchema.Entry(name: Text, schema) =>
      index(name).prim.let(children(_).fieldValue)
    case _ => key

  def promote(n: Int): Data = copy(layout = layout.copy(params = n))

  def has(key: Text): Boolean = index.contains(key) || paramIndex.contains(key)

  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: Data => key == that.key && children.sameElements(that.children) && layout == that.layout &&
                           schema == that.schema
    case _          => false

  override def hashCode: Int = key.hashCode ^ children.toSeq.hashCode ^ layout.hashCode ^ schema.hashCode
