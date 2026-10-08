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

import scala.annotation.*
import scala.collection.mutable as scm

import anticipation.*
import contextual.*
import denominative.*
import gossamer.*
import panopticon.*
import prepositional.*
import rudiments.*
import vacuous.{Unset, or}

export xylophone.internal.{Attributes, Scope}

export Xml.{attribute, xmlns, unqualified}

extension (inline context: StringContext)
  transparent inline def x: Interpolation = interpolation[Xml](context)
  transparent inline def xp: Interpolation = interpolation[XPath](context)

// Panopticon optics over an XML element's children. `lens` navigates to the first
// child element with the given name — replacing it on update, or appending if
// absent — so `xml.lens(_.book.title = …)` works. `ordinalOptical` and `eachOptical`
// address the n-th, or every, child element of a node. All rebuild the element
// immutably; non-element nodes (text, comments) are preserved in place.
private def xmlNodes(xml: Xml): Array[Xml.Node]^{} = xml match
  case Xml.Fragment(nodes*) => Array.from(nodes)
  case node: Xml.Node       => Array(node)

private def firstNode(xml: Xml, fallback: Xml.Node): Xml.Node =
  val nodes = xmlNodes(xml)
  nodes.prim.or(fallback)

private def replaceNamedChild(xml: Xml, name: String, value: Xml)(using Xml.Scope): Xml =
  xml match
  case parent @ Xml.Element(label, attributes, children) =>
    val replacement = xmlNodes(value)
    val buffer = scm.ArrayBuffer[Xml.Node]()
    var done = false

    children.iterate: index =>
      children.at(index) match
        case element: Xml.Element if !done && parent.selects(element, name.tt) =>
          buffer ++= replacement.readable.toSeq
          done = true

        case other =>
          buffer += other

    if !done then buffer ++= replacement.readable.toSeq
    Xml.Element(label, attributes, Array.from(buffer))

  case Xml.Fragment(node: Xml.Element) =>
    Xml.Fragment(replaceNamedChild(node, name, value).asInstanceOf[Xml.Node])

  case other =>
    other

private def updateChildElements(xml: Xml, select: Int => Boolean, lambda: Xml => Xml): Xml =
  xml match
    case Xml.Element(label, attributes, children) =>
      var index = 0

      val out = children.remap:
        case element: Xml.Element =>
          val here = index
          index += 1
          if select(here) then firstNode(lambda(element), element) else element

        case other =>
          other

      Xml.Element(label, attributes, out)

    case Xml.Fragment(node: Xml.Element) =>
      Xml.Fragment(updateChildElements(node, select, lambda).asInstanceOf[Xml.Node])

    case other =>
      other

package optics:
  given xmlLens: [name <: Label: ValueOf]
  =>  ( erased dynamical: (? >: Xml) is Dynamical, scope: Xml.Scope )
  =>  name is Lens from Xml onto Xml =
    Lens(_.applyDynamic(valueOf[name])(Prim), replaceNamedChild(_, valueOf[name], _))

  given xmlOrdinalOptical: [element] => Ordinal is Optical from Xml onto Xml = ordinal =>
    Optic: (origin, lambda) => updateChildElements(origin, _ == ordinal.n0, lambda)

  given xmlEachOptical: Each.type is Optical from Xml onto Xml = _ =>
    Optic: (origin, lambda) => updateChildElements(origin, _ => true, lambda)

// How an `Optional` field reads a missing element or attribute, or a value its inner decoder
// rejects: lenient yields `Unset`, strict raises `Xml.Error`. Absence is lenient by default;
// faults are strict. XML has no null, so there is no nullity option.
package optionalityOptions:
  given strictXmlAbsence:  distillate.Decodable.Absence in Xml = distillate.Decodable.Absence(true)
  given lenientXmlAbsence: distillate.Decodable.Absence in Xml = distillate.Decodable.Absence(false)
  given strictXmlFaults:   distillate.Decodable.Fault in Xml   = distillate.Decodable.Fault(true)
  given lenientXmlFaults:  distillate.Decodable.Fault in Xml   = distillate.Decodable.Fault(false)

// Whether a parse treats a prefix with no binding as an error (the default) or as no namespace
package namespaceOptions:
  inline given strictNamespaces: Xml.Namespacing = Xml.Namespacing.Strict
  inline given lenientNamespaces: Xml.Namespacing = Xml.Namespacing.Lenient

// The schema a parse validates against. `Freeform` admits any element, attribute and the five
// predefined entities, and is the usual choice when no XSD is in play.
package xmlSchemas:
  given freeformXmlSchema: XmlSchema = XmlSchema.Freeform

package formatting:
  given compactXmlFormatting: Xml.Formatting = Xml.Formatting(Unset, trailingNewline = false)

  given indentedXmlFormatting: Xml.Formatting =
    Xml.Formatting(t"  ", trailingNewline = true)
