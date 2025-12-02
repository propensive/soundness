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
    new Tag("#root", false, Html.TextContent.Normal, Nil, children, false, false)

  def void
       [label      <: Label: ValueOf]
       (autoclose:  Boolean          = false,
        content:    Html.TextContent = Html.TextContent.Normal,
        presets:    List[Attribute]  = Nil)
  : Tag of label over "" =

      new Tag(valueOf[label].tt, autoclose, content, presets, Set(), false, false):
        type Topic = label
        type Transport = ""

  def foreign(name: Text, attributes0: List[Attribute]): Tag =

    new Tag(name, false, Html.TextContent.Normal, Nil, Set(), false, true):
      override def void = false
      override val attributes: List[Attribute] = attributes0

  def foreign[label <: Label: ValueOf](presets: List[Attribute] = Nil): Tag of label over "" =
    new Tag(valueOf[label].tt, false, Html.TextContent.Normal, presets, Set(), false, true):
      type Topic = label
      type Transport = ""

      override def void = false

  def container
       [label      <: Label: ValueOf,
        children   <: Label: Reifiable to List[String]]
       (autoclose:  Boolean          = false,
        content:    Html.TextContent = Html.TextContent.Normal,
        presets:    List[Attribute]  = Nil,
        insertable: Boolean          = false)
  : Container of label over children =

      val admissible: Set[Text] = children.reification().map(_.tt).to(Set)

      new Container(valueOf[label].tt, autoclose, content, presets, admissible, insertable):
        type Topic = label
        type Transport = children

  class Container
         (name:      Text,
          autoclose:  Boolean          = false,
          content:    Html.TextContent = Html.TextContent.Normal,
          presets:    List[Attribute]  = Nil,
          admissible: Set[Text]        = Set(),
          insertable: Boolean          = false)
  extends Tag(name, autoclose, content, presets, admissible, insertable, false):
    override def void = false

    def applyDynamic(method: "apply")(children: Html of Transport*)
    : Html.Node of Topic =

        Html.Node(name, Nil, IArray.from(children)).asInstanceOf[Html.Node of Topic]

class Tag
       (    tagname:    Text,
        val autoclose:  Boolean          = false,
        val content:    Html.TextContent = Html.TextContent.Normal,
        val presets:    List[Attribute]  = Nil,
        val admissible: Set[Text]        = Set(),
        val insertable: Boolean          = false,
        val foreign:    Boolean          = false)
extends Html.Node(tagname, Nil, IArray()), Dynamic:

  def void: Boolean = true

  def applyDynamicNamed(method: "apply")(attributes: Optional[Attribute of Topic]*)
  : Html.Node & Html.Vacant of Topic over Transport =

      Html.Node(tagname, attributes.to(List).compact, IArray())
      . asInstanceOf[Html.Node & Html.Vacant of Topic over Transport]
