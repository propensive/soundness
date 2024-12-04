/*
    Honeycomb, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

import rudiments.*
import vacuous.*
import anticipation.*
import spectacular.*

import language.dynamics

object Element:
  @targetName("make")
  def apply[NodeType <: Label, ChildType <: Label]
     (labelString: String,
      unclosed:    Boolean,
      block:       Boolean,
      verbatim:    Boolean,
      attributes:  Attributes,
      children:    Seq[Optional[Html[ChildType]] | Seq[Html[ChildType]]] = Nil)
          : Element[NodeType] =

    new Element(labelString, unclosed, block, verbatim, attributes, flatten(children))

  private def flatten[ChildType <: Label](nodes: Seq[Optional[Html[ChildType]] | Seq[Html[ChildType]]])
          : Seq[Html[ChildType]] =

    nodes.flatMap:
      case Unset                                => Seq()
      case seq: Seq[Html[ChildType] @unchecked] => seq
      case node: Html[ChildType] @unchecked     => Seq(node)

case class Element[+NameType <: Label]
   (labelString: String,
    unclosed:    Boolean,
    tagBlock:    Boolean,
    verbatim:    Boolean,
    attributes:  Map[String, Optional[Text]],
    children:    Seq[Html[?]])
extends Node[NameType]:

  def label: Text = labelString.show

  val block: Boolean = tagBlock || children.exists: child =>
    (child: @unchecked) match
      case node: Node[?] => node.block
      case _: Text       => false
      case _: Int        => false
