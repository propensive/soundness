/*
    Honeycomb, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import gossamer.*
import proscenium.*

import language.dynamics

type Html[+ChildType <: Label] = Node[ChildType] | Text | Int | HtmlXml

object Html extends Node["html"]:
  def label: Text = t"html"
  def attributes: Attributes = Map()
  def children: Seq[Html[?]] = Nil

  def apply(head: Node["head"], body: Node["body"]): Element["html"] =
    Element(label.s, Map(), Seq(head, body))

  private[honeycomb] val unclosedElements: Set[Text] =
    Set
     (t"area", t"base", t"br", t"col", t"embed", t"hr", t"img", t"input", t"link", t"meta",
      t"param", t"source", t"track", t"wbr")

  private[honeycomb] val verbatimElements: Set[Text] = Set(t"pre", t"script", t"textarea")

  private[honeycomb] val inlineElements: Set[Text] =
    Set
     (t"a", t"abbr", t"area", t"audio", t"b", t"base", t"bdi", t"bdo", t"br", t"button", t"canvas",
      t"cite", t"code", t"col", t"data", t"datalist", t"del", t"dfn", t"em", t"embed", t"map",
      t"hr", t"i", t"iframe", t"img", t"input", t"ins", t"kbd", t"label", t"link", t"mark", t"meta",
      t"meter", t"noscript", t"object", t"output", t"picture", t"pre", t"progress", t"q", t"ruby",
      t"s", t"samp", t"script", t"select", t"slot", t"small", t"span", t"strong", t"sub", t"sup",
      t"template", t"textarea", t"time", t"u", t"var", t"video", t"wbr")
