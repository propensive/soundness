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
package adversaria

import scala.quoted.*

import anticipation.*
import denominative.*
import panopticon.*
import rudiments.*
import vacuous.*
import prepositional.*

object Annotated:
  transparent inline given annotations: [topic <: StaticAnnotation, self, plane, limit]
  =>  self is Annotated by topic on plane under limit =

    ${adversaria.internal.general[topic, self, plane, limit]}

  trait Fields extends Annotated:
    def fields: Map[Text, Set[Operand]]

  class AnnotatedFields[operand <: StaticAnnotation, self, plane, limit]
    ( annotations0: Set[operand], fields0: Map[Text, Set[operand]] )
  extends Fields:
    type Operand = operand
    type Self = self
    type Plane = plane
    type Limit = limit

    def fields: Map[Text, Set[operand]] = fields0
    def annotations: Set[operand] = annotations0
    def apply(): Set[operand] = annotations0

  trait Field extends Fields, Targetable, Topical:
    def field: Text

    // The lens onto the annotated field, which is usually what a caller wants once it has found
    // it (#490). `Target` is the field's name as a `Label`, and panopticon's `deref` resolves that
    // against `Self` — so the lens is summoned at the use site, where `Self` is known to be a
    // product, rather than built here where it is not.
    transparent inline def lens(using lens: Target is Lens from Self): Target is Lens from Self =
      lens

  class AnnotatedField[operand <: StaticAnnotation, self, plane, limit, topic, target <: Label]
    ( annotations0: Set[operand], fields0: Map[Text, Set[operand]] )
  extends AnnotatedFields[operand, self, plane, limit](annotations0, fields0), Field:
    type Unique = true
    type Topic = topic
    type Target = target
    // Exactly one key, by construction; the primitive keeps the impossible empty case loud.
    def field: Text = Set.iterator(fields.keys).next()
    override def apply(): Set[operand] = fields(field).or(Set())

  trait Subtypes extends Annotated:
    def subtypes: Map[Text, Set[Operand]]
    def apply(): Map[Text, Set[Operand]]

  class AnnotatedSubtypes[operand <: StaticAnnotation, self, plane, limit]
    ( subtypes0: Map[Text, Set[operand]] )
  extends Subtypes:
    type Operand = operand
    type Self = self
    type Plane = plane
    type Limit = limit

    def subtypes: Map[Text, Set[operand]] = subtypes0
    def apply(): Map[Text, Set[operand]] = subtypes0

sealed trait Annotated extends Planar, Typeclass, Limited, Topical, Targetable:
  type Operand <: StaticAnnotation
  type Unique <: Boolean
