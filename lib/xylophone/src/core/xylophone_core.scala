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
package xylophone

import language.dynamics

import scala.annotation.*
import scala.collection.mutable as scm

import anticipation.*
import contextual.*
import denominative.*
import gossamer.*
import panopticon.*
import prepositional.*
import rudiments.*

export Xml.attribute

extension [encodable: Encodable in Xml](value: encodable)
  def xml: Xml = encodable.encoded(value)

extension (inline context: StringContext)
  transparent inline def x: Interpolation = interpolation[Xml](context)
  transparent inline def xp: Interpolation = interpolation[XPath](context)

// Panopticon optics over an XML element's children. `lens` navigates to the first
// child element with the given name — replacing it on update, or appending if
// absent — so `xml.lens(_.book.title = …)` works. `ordinalOptical` and `eachOptical`
// address the n-th, or every, child element of a node. All rebuild the element
// immutably; non-element nodes (text, comments) are preserved in place.
private def xmlNodes(xml: Xml): IArray[Node] = xml match
  case Fragment(nodes*) => IArray.from(nodes)
  case node: Node       => IArray(node)
  case _                => IArray[Node]()

private def firstNode(xml: Xml, fallback: Node): Node =
  val nodes = xmlNodes(xml)
  if nodes.isEmpty then fallback else nodes(0)

private def replaceNamedChild(xml: Xml, name: String, value: Xml): Xml = xml match
  case Element(label, attributes, children) =>
    val replacement = xmlNodes(value)
    val buffer = scm.ArrayBuffer[Node]()
    var done = false

    var i = 0

    while i < children.length do
      children(i) match
        case element: Element if !done && element.label == name.tt =>
          buffer ++= replacement
          done = true
        case other =>
          buffer += other

      i += 1

    if !done then buffer ++= replacement
    Element(label, attributes, IArray.from(buffer))

  case Fragment(node: Element) =>
    Fragment(replaceNamedChild(node, name, value).asInstanceOf[Node])

  case other =>
    other

private def updateChildElements(xml: Xml, select: Int => Boolean, lambda: Xml => Xml): Xml =
  xml match
    case Element(label, attributes, children) =>
      var index = 0

      val out = children.map:
        case element: Element =>
          val here = index
          index += 1
          if select(here) then firstNode(lambda(element), element) else element
        case other =>
          other

      Element(label, attributes, out)

    case Fragment(node: Element) =>
      Fragment(updateChildElements(node, select, lambda).asInstanceOf[Node])

    case other =>
      other

given lens: [name <: Label: ValueOf] => (erased DynamicXmlEnabler)
=>  name is Lens from Xml onto Xml =
  Lens(_.applyDynamic(valueOf[name])(Prim), replaceNamedChild(_, valueOf[name], _))

given ordinalOptical: [element] => Ordinal is Optical from Xml onto Xml = ordinal =>
  Optic: (origin, lambda) => updateChildElements(origin, _ == ordinal.n0, lambda)

given eachOptical: Each.type is Optical from Xml onto Xml = _ =>
  Optic: (origin, lambda) => updateChildElements(origin, _ => true, lambda)

