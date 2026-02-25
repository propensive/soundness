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
package honeycomb

import language.dynamics

import anticipation.*
import denominative.*
import fulminate.*
import gossamer.*
import panopticon.*
import prepositional.*
import proscenium.*
import rudiments.*
import typonym.*
import vacuous.*

object Tag:
  given optical: [html <: Html] => Tag is Optical from html onto Node = tag =>
    Optic: (origin, lambda) =>
      origin match
        case Element(label, attributes, children, boundary) =>
          val children2 = children.map:
            case element@Element(tag.label, _, _, _) => lambda(element)
            case other                               => other

          Element(label, attributes, children2, boundary).asInstanceOf[html]

        case other =>
          other

  def root(children: Set[Text]): Tag =
    new Tag("#root", false, Html.Mode.Normal, Map(), children, false, false, false):
      type Result = this.type

      def node(attributes: Map[Text, Optional[Text]]): Result = this


  def void[label <: Label: ValueOf, dom <: Dom]
    ( presets: Map[Text, Optional[Text]] = Map(), boundary: Boolean = false )
  :   Tag.Void of label in dom =

    new Void(valueOf[label].tt, presets, boundary).of[label].in[dom]


  def foreign[dom <: Dom](label: Text, attributes0: Map[Text, Optional[Text]])
  :   Tag of "#foreign" over "#foreign" in dom =

    new Tag.Container(label, false, Html.Mode.Normal, attributes0, Set(), false, true)
    . of["#foreign"]
    . over["#foreign"]
    . in[dom]


  def container[label <: Label: ValueOf, children <: Label: Reifiable to List[String], dom <: Dom]
    ( autoclose:  Boolean                   = false,
      mode:       Html.Mode                 = Html.Mode.Normal,
      presets:    Map[Text, Optional[Text]] = Map(),
      insertable: Boolean                   = false,
      boundary:   Boolean                   = false )
  :   Container of label over children in dom =

    val admissible: Set[Text] = children.reify.map(_.tt).to(Set)

    Container
      ( valueOf[label].tt, autoclose, mode, presets, admissible, insertable, false, boundary )

    . of[label]
    . over[children]
    . in[dom]


  def transparent[label <: Label: ValueOf, children <: Label: Reifiable to List[String], dom <: Dom]
    ( presets: Map[Text, Optional[Text]] = Map(), boundary: Boolean = false )
  :   Transparent of label over children in dom =

    val admissible: Set[Text] = children.reify.map(_.tt).to(Set)

    transparent(valueOf[label].tt, admissible, presets, boundary = boundary)
    . of[label]
    . over[children]


  def transparent[dom <: Dom]
    ( label:    Text,
      children: Set[Text],
      presets:  Map[Text, Optional[Text]],
      boundary: Boolean )
  :   Transparent in dom =

    Transparent(label, children, presets, boundary = boundary).in[dom]


  def foreign[label <: Label: ValueOf, dom <: Dom](): Container of label over "#foreign" =
    Container(valueOf[label], foreign = true).of[label].over["#foreign"].in[dom]

  class Container
    ( label:      Text,
      autoclose:  Boolean                   = false,
      mode:       Html.Mode                 = Html.Mode.Normal,
      presets:    Map[Text, Optional[Text]] = Map(),
      admissible: Set[Text]                 = Set(),
      insertable: Boolean                   = false,
      foreign:    Boolean                   = false,
      boundary:   Boolean                   = false )
  extends Tag
    ( label, autoclose, mode, presets, admissible, insertable, foreign, false, false, boundary ):
    type Result = Element & Html.Populable of Topic over Transport in Form

    def applyDynamic[className <: Label: ValueOf](method: className)
      ( children: Optional[Html of (? <: Transport)]* )
      ( using css: Stylesheet of (? >: className) )
    :   Element of Topic over Transport in Form =

      val nodes = children.compact.nodes

      val presets2 = if css.classes.nil then presets else
        val cls: Text = valueOf[className]
        val value = presets.at("class").lay(cls) { preset => t"$preset $cls" }
        presets.updated("class", value)

      Element(label, presets2, nodes, foreign).of[Topic].over[Transport].in[Form]

    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray(), foreign) with Html.Populable()
      . of[Topic]
      . over[Transport]
      . in[Form]

  class Transparent
    ( label:      Text,
      admissible: Set[Text],
      presets:    Map[Text, Optional[Text]] = Map(),
      foreign:    Boolean                   = false,
      boundary:   Boolean                   = false )
  extends Tag
    ( label       = label,
      autoclose   = false,
      mode        = Html.Mode.Normal,
      presets     = presets,
      admissible  = admissible,
      insertable  = false,
      foreign     = foreign,
      transparent = true,
      boundary    = boundary ):

    type Result = Element & Html.Transparent of Topic over Transport in Form


    def applyDynamic[className <: Label](method: className)
      ( children: Optional[Html of (? <: Transport)]* )
      ( using css: Stylesheet of (? >: className) )
    :   Element of Topic in Form =

      val presets2 = if css.classes.nil then presets else
        val cls = css.classes.join(t" ")
        val value = presets.at("class").lay(cls) { preset => t"$preset $cls" }
        presets.updated("class", value)

      val nodes: IArray[Node] = children.compact.nodes
      Element(label, presets2, nodes, foreign).of[Topic].in[Form]


    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray(), foreign) with Html.Transparent()
      . of[Topic]
      . over[Transport]
      . in[Form]


  class Void(label: Text, presets: Map[Text, Optional[Text]], boundary: Boolean)
  extends Tag(label, presets = presets, void = true, boundary = boundary):
    type Result = Element of Topic in Form

    def node(attributes: Map[Text, Optional[Text]]): Result =
      new Element(label, presets ++ attributes, IArray(), this.foreign)
      . of[Topic]
      . in[Form]

abstract class Tag
  (     label:       Text,
    val autoclose:   Boolean                   = false,
    val mode:        Html.Mode                 = Html.Mode.Normal,
    val presets:     Map[Text, Optional[Text]] = Map(),
    val admissible:  Set[Text]                 = Set(),
    val insertable:  Boolean                   = false,
        foreign:     Boolean                   = false,
    val void:        Boolean                   = false,
    val transparent: Boolean                   = false,
    val boundary:    Boolean                   = false )
extends Element(label, presets, IArray(), foreign), Formal, Dynamic:
  type Result <: Element


  inline def applyDynamicNamed[label <: Label: Precise](inline method: label)
    ( inline attributes: (String, Any)* )
  :   Result =

    inline if method == "apply" then make(Map(), attributes*) else
      val stylesheet = infer[Stylesheet of (? >: label)]
      make(Map(t"class" -> stylesheet.classes.to(List).join(t" ")), attributes*)


  inline def make(presets: Map[Text, Text], inline attributes: (String, Any)*): Result =
    ${honeycomb.internal.attributes[Result, this.type]('this, 'presets, 'attributes)}

  def node(attributes: Map[Text, Optional[Text]]): Result
