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
┃    Soundness, version 0.46.0.                                                                    ┃
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

import language.dynamics

import java.lang as jl

import scala.collection.mutable as scm

import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import turbulence.*
import typonym.*
import vacuous.*
import zephyrine.*

import classloaders.threadContext
import charDecoders.utf8
import textSanitizers.skip

object Tag:
  def root(children: Set[Text]): Tag =
    new Tag("#root", false, Xml.Mode.Normal, Map(), children, false, false, false):
      type Result = this.type

      def node(attributes: Map[Text, Optional[Text]]): Result = this


  def container[label <: Label: ValueOf, children <: Label: Reifiable to List[String], schema <: XmlSchema]
       (autoclose:  Boolean                   = false,
        mode:       Xml.Mode                 = Xml.Mode.Normal,
        presets:    Map[Text, Optional[Text]] = Map(),
        insertable: Boolean                   = false)
  : Container of label over children in schema =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)

      Container(valueOf[label].tt, autoclose, mode, presets, admissible, insertable)
      . of[label]
      . over[children]
      . in[schema]


  def transparent[label <: Label: ValueOf, children <: Label: Reifiable to List[String], schema <: XmlSchema]
       (presets: Map[Text, Optional[Text]] = Map())
  : Transparent of label over children in schema =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)
      transparent(valueOf[label].tt, admissible, presets).of[label].over[children].in[schema]


  def transparent[schema <: XmlSchema](label: Text, children: Set[Text], presets: Map[Text, Optional[Text]])
  : Transparent in schema =

      Transparent(label, children, presets).in[schema]

  class Container
         (label:      Text,
          autoclose:  Boolean                   = false,
          mode:       Xml.Mode                 = Xml.Mode.Normal,
          presets:    Map[Text, Optional[Text]] = Map(),
          admissible: Set[Text]                 = Set(),
          insertable: Boolean                   = false)
  extends Tag(label, autoclose, mode, presets, admissible, insertable):
    type Result = Element & Xml.Populable of Topic over Transport in Form

    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray()) with Xml.Populable()
      . of[Topic]
      . over[Transport]
      . in[Form]

  class Transparent
         (label:      Text,
          admissible: Set[Text],
          presets:    Map[Text, Optional[Text]] = Map())
  extends Tag
           (label       = label,
            autoclose   = false,
            mode        = Xml.Mode.Normal,
            presets     = presets,
            admissible  = admissible,
            insertable  = false,
            transparent = true):

    type Result = Element & Xml.Transparent of Topic over Transport in Form


    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray()) with Xml.Transparent()
      . of[Topic]
      . over[Transport]
      . in[Form]


abstract class Tag
       (    label:       Text,
        val autoclose:   Boolean                   = false,
        val mode:        Xml.Mode                 = Xml.Mode.Normal,
        val presets:     Map[Text, Optional[Text]] = Map(),
        val admissible:  Set[Text]                 = Set(),
        val insertable:  Boolean                   = false,
        val void:        Boolean                   = false,
        val transparent: Boolean                   = false)
extends Element(label, presets, IArray()), Formal, Dynamic:

  type Result <: Element


  inline def applyDynamicNamed(method: "apply")(inline attributes: (String, Any)*): Result =
    ${Xylophone.attributes[Result, this.type]('this, 'attributes)}

  def node(attributes: Map[Text, Optional[Text]]): Result
