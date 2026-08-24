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
package xylophone

import scala.language.dynamics

import anticipation.*
import prepositional.*
import typonym.*

@unexported
object Tag:
  def root(children: Set[Text]): Tag =
    new Tag("#root", Attributes.empty, children):
      this: Tag =>
      type Result = this.type

      def node(attributes: Attributes): Result = this

  def freeform(label: Text): Container = Container(label, Attributes.empty, Set())


  def container
    [ label    <: Label: ValueOf,
      children <: Label: Reifiable to List[String],
      schema   <: XmlSchema ]
    ( presets: Attributes = Attributes.empty )
  :   Container of label over children in schema =

    val admissible: Set[Text] = children.reification().stdlib.map(_.tt).to(Set)

    Container(valueOf[label].tt, presets, admissible)
    . of[label]
    . over[children]
    . in[schema]


  class Container
    ( label:      Text,
      presets:    Attributes = Attributes.empty,
      admissible: Set[Text]  = Set() )
  extends Tag(label, presets, admissible):

    type Result = Element & Xml.Vacuiscible of Topic over Transport in Form

    def node(attributes: Attributes): Result =
      new Element(label, presets ++ attributes, Array()) with Xml.Vacuiscible()
      . of[Topic]
      . over[Transport]
      . in[Form]

sealed abstract class Tag
  ( label: Text, val presets: Attributes = Attributes.empty, val admissible:  Set[Text] = Set() )
extends Element(label, presets, Array()), Formal, Dynamic:
  type Result <: Element

  inline def applyDynamicNamed(method: "apply")(inline attributes: (String, Any)*): Result =
    ${xylophone.internal.attributes[Result, this.type]('this, 'attributes)}

  def node(attributes: Attributes): Result
