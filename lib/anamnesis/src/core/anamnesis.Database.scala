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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package anamnesis

import proscenium.compat.*

import beneficence.*
import contingency.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Database:
  sealed trait Relation[left, right]

  transparent inline def apply[relations <: Tuple](): Database of relations =
    val size = valueOf[Tuple.Size[relations]]
    new Database(size).asInstanceOf[Database of relations]

  // DataError → Database.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case UnknownReference extends Reason(1)

    given communicable: Reason is Communicable =
      case Reason.UnknownReference => m"the value has not been stored in the database"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(229, reason.number)(m"the database operation failed because $reason")

class Database(size: Int) extends Findable:
  import Database.Relation

  @scala.caps.unsafe.untrackedCaptures
  private var nextId: Int = 1

  def allocate[ref](): Ref of ref in this.type =
    nextId.asInstanceOf[Ref of ref in this.type].also:
      nextId += 1

  type Topic <: Tuple
  type AllRelations = Tuple.Union[Topic]
  type Has[relation <: Relation[?, ?]] = relation <:< AllRelations

  private val mutex: Mutex = Mutex()
  @scala.caps.unsafe.untrackedCaptures
  private var references: Map[Any, Ref] = Map()
  @scala.caps.unsafe.untrackedCaptures
  private var dereferences: Map[Ref, Any] = Map()

  @scala.caps.unsafe.untrackedCaptures
  private val relations: scala.Array[Map[Ref, Set[Ref]]] = scala.Array.fill(size)(Map())
  @scala.caps.unsafe.untrackedCaptures
  private val corelations: scala.Array[Map[Ref, Ref]] = scala.Array.fill(size)(Map())

  def dereference[ref](ref: Ref of ref): ref = dereferences(ref).asInstanceOf[ref]

  protected inline def relate[left, right]: Map[Ref, Set[Ref]] =
    relations(!![Topic].indexOf[left -< right])

  protected inline def corelate[left, right]: Map[Ref, Ref] =
    corelations(!![Topic].indexOf[left -< right])

  inline def store[left](left: left): Ref of left in this.type =
    references(left).or:
      allocate[left]().tap: ref =>
        mutex:
          references = references.updated(left, ref)
          dereferences = dereferences.updated(ref, left)

    . asInstanceOf[Ref of left in this.type]

  inline def ref[left](left: left): Ref of left in this.type raises Database.Error =
    references(left).lest(Database.Error(Database.Error.Reason.UnknownReference))
    . asInstanceOf[Ref of left in this.type]


  inline def assign[left, right]
    ( left: Ref of left in this.type, right: Ref of right in this.type )
    ( using (left -< right) <:< Tuple.Union[Topic] )
  :   Unit raises Database.Error =

    val relationIndex = !![Topic].indexOf[left -< right]
    val relation = relate[left, right]
    val corelation = corelate[left, right]
    val relation2 = relation.updated(left, relation(left).or(Set()) + right)
    val corelation2 = corelation.updated(right, left)
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2


  inline def lookup[left, right](left: Ref of left in this.type)
  :   Set[Ref of right in this.type] raises Database.Error =

    relate[left, right](left).or(Set()).asInstanceOf[Set[Ref of right in this.type]]


  inline def unassign[left, right]
    ( left: Ref of left in this.type, right: Ref of right in this.type )
    ( using (left -< right) <:< Tuple.Union[Topic] )
  :   Unit raises Database.Error =

    val relationIndex = !![Topic].indexOf[left -< right]
    val relation = relate[left, right]
    val corelation = corelate[left, right]

    val relation2: Map[Ref, Set[Ref]] =
      relation.updated(left, relation(left).let(_ - right).or(Set()))

    val corelation2 = corelation.removed(right)
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2
