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
package honeycomb

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
    new Tag("#root", false, Html.Mode.Normal, Map(), children, false, false, false):
      type Result = this.type

      def node(attributes: Map[Text, Optional[Text]]): Result = this


  def void[label <: Label: ValueOf](presets: Map[Text, Optional[Text]] = Map()): Tag.Void of label =
    new Void(valueOf[label].tt, presets).of[label]


  def foreign(label: Text, attributes0: Map[Text, Optional[Text]])
  : Tag of "#foreign" over "#foreign" =

      new Tag.Container(label, false, Html.Mode.Normal, attributes0, Set(), false, true)
      . of["#foreign"]
      . over["#foreign"]


  def container[label <: Label: ValueOf, children <: Label: Reifiable to List[String]]
       (autoclose:  Boolean                   = false,
        mode:       Html.Mode                 = Html.Mode.Normal,
        presets:    Map[Text, Optional[Text]] = Map(),
        insertable: Boolean                   = false)
  : Container of label over children =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)

      Container(valueOf[label].tt, autoclose, mode, presets, admissible, insertable)
      . of[label]
      . over[children]


  def transparent[label <: Label: ValueOf, children <: Label: Reifiable to List[String]]
       (presets: Map[Text, Optional[Text]] = Map())
  : Transparent of label over children =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)
      transparent(valueOf[label].tt, admissible, presets).of[label].over[children]


  def transparent(label: Text, children: Set[Text], presets: Map[Text, Optional[Text]])
  : Transparent =

      Transparent(label, children, presets)

  def foreign[label <: Label: ValueOf](): Container of label over "#foreign" =
    Container(valueOf[label], foreign = true).of[label].over["#foreign"]


  class Container
         (label:      Text,
          autoclose:  Boolean                   = false,
          mode:       Html.Mode                 = Html.Mode.Normal,
          presets:    Map[Text, Optional[Text]] = Map(),
          admissible: Set[Text]                 = Set(),
          insertable: Boolean                   = false,
          foreign:    Boolean                   = false)
  extends Tag(label, autoclose, mode, presets, admissible, insertable, foreign = foreign):
    type Result = Element & Html.Populable of Topic over Transport in Form


    def applyDynamic[className <: Label](method: className)(children: Html of Transport*)
          (using css: CssClass of className)
    : Element of Topic =

        val cls = css.name.lay(Map()) { name => Map(t"class" -> name) }
        Element(label, cls, children.nodes, foreign).of[Topic]


    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray(), foreign) with Html.Populable()
      . of[Topic]
      . over[Transport]
      . in[Form]


  class Transparent
         (label:      Text,
          admissible: Set[Text],
          presets:    Map[Text, Optional[Text]] = Map(),
          foreign:    Boolean                   = false)
  extends Tag
           (label       = label,
            autoclose   = false,
            mode        = Html.Mode.Normal,
            presets     = presets,
            admissible  = admissible,
            insertable  = false,
            foreign     = foreign,
            transparent = true):

    type Result = Element & Html.Transparent of Topic over Transport in Form


    def applyDynamic[className <: Label](method: className)(children: Html of Transport*)
          (using css: CssClass of className)
    : Element of Topic =

        val cls = css.name.lay(Map()) { name => Map(t"class" -> name) }
        Element(label, cls, children.nodes, foreign).of[Topic]


    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray(), foreign) with Html.Transparent()
      . of[Topic]
      . over[Transport]
      . in[Form]


  class Void(label: Text, presets: Map[Text, Optional[Text]])
  extends Tag(label, presets = presets, void = true):
    type Result = Element of Topic in Form

    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray(), this.foreign)
      . of[Topic]
      . in[Form]

abstract class Tag
       (    label:       Text,
        val autoclose:   Boolean                   = false,
        val mode:        Html.Mode                 = Html.Mode.Normal,
        val presets:     Map[Text, Optional[Text]] = Map(),
        val admissible:  Set[Text]                 = Set(),
        val insertable:  Boolean                   = false,
            foreign:     Boolean                   = false,
        val void:        Boolean                   = false,
        val transparent: Boolean                   = false)
extends Element(label, presets, IArray(), foreign), Formal, Dynamic:

  type Result <: Element


  inline def applyDynamicNamed(method: "apply")(inline attributes: (String, Any)*): Result =
    ${Honeycomb.attributes[Result, this.type]('this, 'attributes)}

  def node(attributes: Map[Text, Optional[Text]]): Result
