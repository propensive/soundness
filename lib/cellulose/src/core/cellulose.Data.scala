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

import anticipation.*
import contextual.*
import gossamer.{where as _, *}
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics

object Atom:
  given insertion: [entity: Encodable in Codl] => Insertion[List[Atom], entity] =
    value =>
      entity.encoded(value).list.head.children.to(List).map(_.data).collect:
        case data: Atom => data

  given inspectable: Atom is Inspectable = data => t"Atom(${data.key}, ${data.children.length})"

case class Atom(key: Text, children: IArray[CodlNode] = IArray(), layout: Layout = Layout.empty,
                    schema: CodlSchema = CodlSchema.Free)
extends Indexed:

  lazy val paramIndex: Map[Text, Int] =
    (0 until layout.params.min(schema.paramCount)).map: index =>
      schema.subschemas(index).key -> index

    . to(Map)

  def uniqueId: Optional[Text] = schema.subschemas.where(_.schema.arity == Arity.Unique).let:
    case CodlSchema.Entry(name: Text, schema) => paramIndex.at(name).let(children(_).fieldValue)
    case _                                    => Unset

  def id: Optional[Text] = schema.subschemas.where(_.schema.arity == Arity.Unique) match
    case CodlSchema.Entry(name: Text, schema) => index(name).prim.let(children(_).fieldValue)
    case _                                    => key

  def promote(n: Int): Atom = copy(layout = layout.copy(params = n))

  def has(key: Text): Boolean = index.contains(key) || paramIndex.contains(key)

  override def equals(that: Any) = that.matchable(using Unsafe) match
    case that: Atom =>
      key == that.key && children.sameElements(that.children) && layout == that.layout
      && schema == that.schema

    case _ =>
      false

  override def hashCode: Int =
    key.hashCode ^ children.toSeq.hashCode ^ layout.hashCode ^ schema.hashCode

  override def toString: String = s"$key[${children.mkString(", ")}]:${schema}"
