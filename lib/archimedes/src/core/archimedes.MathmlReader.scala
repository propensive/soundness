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
package archimedes

import anticipation.*
import contingency.*
import gossamer.*
import vacuous.*
import xylophone.*
import proscenium.compat.*

import honeycomb.Html

// Extracts MathML embedded in HTML. Honeycomb parses `<math>` as a foreign
// element with its own (non-xylophone) `Element`/`Node` types, so the reader
// walks the honeycomb tree, finds the first `<math>` subtree, transcribes it
// into a xylophone `Xml` element, and hands that to `MathmlParser` to reuse the
// same label-dispatch decoding used for standalone XML.

object MathmlReader:
  def read(html: Html)(using Tactic[MathmlError]): Math =
    findMath(html).lay(abort(MathmlError(MathmlError.Reason.NotMathml(t"<missing>")))): element =>
      MathmlParser.decodeMath(toXmlElement(element))

  def findMath(html: Html): Optional[honeycomb.Element] = html match
    case element: honeycomb.Element =>
      if element.label == t"math" then element else searchNodes(element.children)

    case fragment: honeycomb.Fragment => searchNodes(Array.from(fragment.nodes))
    case _                            => Unset

  private def searchNodes(nodes: Array[honeycomb.Node]^{}): Optional[honeycomb.Element] =
    var result: Optional[honeycomb.Element] = Unset
    var index = 0

    while index < nodes.length && result.absent do
      result = findMath(nodes(index))
      index += 1

    result

  private def toXmlElement(element: honeycomb.Element): Element =
    val pairs: List[(Text, Text)] =
      element.attributes.keys.map { key => (key, element.attributes(key).or(t"")) }.to(List)

    val nodes: Array[Node]^{} = element.children.map(toXmlNode)
    Element(element.label, Attributes(pairs*), nodes)

  private def toXmlNode(node: honeycomb.Node): Node = node match
    case element: honeycomb.Element   => toXmlElement(element)
    case textNode: honeycomb.TextNode => TextNode(textNode.text)
    case comment: honeycomb.Comment   => Comment(comment.text)
    case _                            => TextNode(t"")
