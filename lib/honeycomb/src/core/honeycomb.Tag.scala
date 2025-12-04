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
    new Tag("#root", false, Html.TextContent.Normal, Nil, children, false, foreign = false, void = false):
      type Result = this.type

      def node(attributes: List[Optional[Attribute]]): Result = this


  def void[label <: Label: ValueOf](presets: List[Attribute] = Nil): Tag.Void of label =
    new Void(valueOf[label].tt, presets):
      type Topic = label

  def foreign(label: Text, attributes0: List[Attribute]): Tag of "#foreign" =
    new Tag.Container(label, false, Html.TextContent.Normal, attributes0, Set(), false, foreign = true):
      type Topic = "#foreign"

  def container[label <: Label: ValueOf, children <: Label: Reifiable to List[String]]
       (autoclose:  Boolean          = false,
        content:    Html.TextContent = Html.TextContent.Normal,
        presets:    List[Attribute]  = Nil,
        insertable: Boolean          = false)
  : Container of label over children =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)

      Container(valueOf[label].tt, autoclose, content, presets, admissible, insertable)
      . of[label]
      . over[children]


  def transparent[label <: Label: ValueOf](presets: List[Attribute] = Nil): Transparent of label =
    transparent(valueOf[label].tt, presets).of[label]


  def transparent(label: Text, presets: List[Attribute]): Transparent =
    Transparent(label, presets)

  def foreign[label <: Label: ValueOf](): Container of label over "#foreign" =
    Container(valueOf[label], foreign = true).of[label].over["#foreign"]


  class Container
         (label:      Text,
          autoclose:  Boolean          = false,
          content:    Html.TextContent = Html.TextContent.Normal,
          presets:    List[Attribute]  = Nil,
          admissible: Set[Text]        = Set(),
          insertable: Boolean          = false,
          foreign:    Boolean          = false)
  extends Tag(label, autoclose, content, presets, admissible, insertable, foreign = foreign):
    tag =>
      type Result = Html.Node & Html.Populable of Topic over Transport

      def applyDynamic(method: "apply")(children: Html of Transport*): Html.Node of Topic =
        Html.Node(label, Nil, IArray.from(children), foreign).of[Topic]

      def node(attributes: List[Optional[Attribute]]): Result =
        new Html.Node(label, attributes.compact, IArray(), foreign) with Html.Populable:
          type Topic = tag.Topic
          type Transport = tag.Transport


  class Transparent(label: Text, presets: List[Attribute] = Nil, foreign: Boolean = false)
  extends Tag
           (label       = label,
            autoclose   = false,
            content     = Html.TextContent.Normal,
            presets     = presets,
            admissible  = Set("#transparent"),
            insertable  = false,
            foreign     = foreign,
            transparent = true):
    tag =>
      type Result = Html.Node & Html.Transparent of Topic over Transport

      def applyDynamic(method: "apply")(children: Html of Transport*): Html.Node of Topic =
        Html.Node(label, Nil, IArray.from(children), foreign).of[Topic]

      def node(attributes: List[Optional[Attribute]]): Result =
        new Html.Node(label, attributes.compact, IArray(), foreign) with Html.Transparent:
          type Topic = tag.Topic
          type Transport = tag.Transport


  class Void(label: Text, presets: List[Attribute])
  extends Tag(label, presets = presets, void = true):
    tag =>
      type Result = Html.Node of Topic


      def node(attributes: List[Optional[Attribute]]): Result =
        new Html.Node(label, attributes.compact, IArray(), tag.foreign):
          type Topic = tag.Topic

abstract class Tag
       (    label:       Text,
        val autoclose:   Boolean          = false,
        val content:     Html.TextContent = Html.TextContent.Normal,
        val presets:     List[Attribute]  = Nil,
        val admissible:  Set[Text]        = Set(),
        val insertable:  Boolean          = false,
            foreign:     Boolean          = false,
        val void:        Boolean          = false,
        val transparent: Boolean          = false)
extends Html.Node(label, Nil, IArray(), foreign), Dynamic:

  type Result <: Html.Node


  transparent inline def applyDynamicNamed(method: "apply")(inline attributes: (String, Any)*): Any =
    ${Honeycomb.attributes[this.type]('this, 'attributes)}


  def node(attributes: List[Optional[Attribute]]): Result
