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
┃    Soundness, version 0.32.0.                                                                    ┃
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

import contingency.*
import prepositional.*
import rudiments.*
import vacuous.*

object Database:
  erased trait Relation[left, right]

  inline def apply[relations <: Tuple](): Database of relations =
    val size = valueOf[Tuple.Size[relations]]
    new Database(size).asInstanceOf[Database of relations]

class Database(size: Int):
  import Database.Relation

  private var nextId: Int = 1

  def allocate[ref](): Ref of ref in this.type =
    nextId.asInstanceOf[Ref of ref in this.type].also:
      nextId += 1

  type Subject <: Tuple
  type AllRelations = Tuple.Union[Subject]

  type Has[relation <: Relation[?, ?]] = relation <:< AllRelations

  private var references: Map[Any, Ref] = Map()
  private var dereferences: Map[Ref, Any] = Map()

  private val relations: Array[Map[Ref, Set[Ref]]] = Array.fill(size)(Map())
  private val corelations: Array[Map[Ref, Ref]] = Array.fill(size)(Map())

  def dereference[ref](ref: Ref of ref): ref = dereferences(ref).asInstanceOf[ref]

  protected inline def relate[left, right]: Map[Ref, Set[Ref]] =
    relations(!![Subject].indexOf[left -< right])

  protected inline def corelate[left, right]: Map[Ref, Ref] =
    corelations(!![Subject].indexOf[left -< right])

  inline def store[left](left: left): Ref of left in this.type =
    references.at(left).or:
      allocate[left]().tap: ref =>
        this.synchronized:
          references = references.updated(left, ref)
          dereferences = dereferences.updated(ref, left)

    . asInstanceOf[Ref of left in this.type]

  inline def ref[left](left: left): Ref of left in this.type raises DataError =
    references.at(left).or(abort(DataError())).asInstanceOf[Ref of left in this.type]

  inline def assign[left, right]
              (left: Ref of left in this.type, right: Ref of right in this.type)
              (using (left -< right) <:< Tuple.Union[Subject])
  :     Unit raises DataError =

    val relationIndex = !![Subject].indexOf[left -< right]
    val relation = relate[left, right]
    val corelation = corelate[left, right]

    val relation2 = relation.updated(left, relation.at(left).or(Set()) + right)
    val corelation2 = corelation.updated(right, left)
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2

  inline def lookup[left, right](left: Ref of left in this.type)
  :     Set[Ref of right in this.type] raises DataError =
    relate[left, right].at(left).or(Set()).asInstanceOf[Set[Ref of right in this.type]]

  inline def unassign[left, right]
              (left: Ref of left in this.type, right: Ref of right in this.type)
              (using (left -< right) <:< Tuple.Union[Subject])
  :     Unit raises DataError =

    val relationIndex = !![Subject].indexOf[left -< right]
    val relation = relate[left, right]
    val corelation = corelate[left, right]

    val relation2: Map[Ref, Set[Ref]] =
      relation.updated(left, relation.at(left).let(_ - right).or(Set()))

    val corelation2 = corelation - right
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2
