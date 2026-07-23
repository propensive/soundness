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
import rudiments.*
import vacuous.*
import xylophone.*

import Mathml.*

// Decodes a xylophone `Xml` tree into the Archimedes model by dispatching on
// each element's label. Every element's attributes are preserved verbatim in
// the node's `attributes` bag (except `xmlns`/`display` on the root, which are
// represented structurally), so a parse/serialise round-trip is lossless.

object MathmlParser:
  def labelOf(xml: Xml): Text = xml match
    case element: Element => element.label
    case _                => t"<unknown>"

  def findMath(nodes: List[Node])(using Tactic[MathmlError]): Element =
    nodes.stdlib.collectFirst { case element: Element if element.label == t"math" => element }
    . getOrElse:
      abort(MathmlError(MathmlError.Reason.NotMathml(t"<missing>")))

  def rootElement(xml: Xml)(using Tactic[MathmlError]): Element = xml match
    case element: Element if element.label == t"math" => element
    case Fragment(nodes*)                             => findMath(nodes.transmute[List])

    case other =>
      abort(MathmlError(MathmlError.Reason.NotMathml(labelOf(other))))

  private def childElements(elem: Element): List[Element] =
    List.of(elem.children.toList.collect { case element: Element => element })

  private def attributesOf(elem: Element): List[(Text, Text)] =
    List.of(elem.attributes.keys.map { key => (key, elem.attributes.at(key).or(t"")) }.toList)

  private def textOf(elem: Element): Text =
    List.of(elem.children.toList.collect { case TextNode(text) => text }).join

  private def children(elem: Element)(using Tactic[MathmlError]): List[Mathml] =
    childElements(elem).map(decodeNode)

  private def at(nodes: List[Mathml], index: Int): Mathml =
    nodes.stdlib.lift(index).getOrElse(Mrow(Nil))

  def decodeMath(elem: Element)(using Tactic[MathmlError]): Math =
    val kept = attributesOf(elem).filter { case (key, _) => key != t"xmlns" && key != t"display" }

    val display: Optional[Display] = elem.attributes.at(t"display").let: text =>
      Display.unapply(text).getOrElse(Display.Inline)

    Math(children(elem), display, kept)

  def decodeNode(elem: Element)(using Tactic[MathmlError]): Mathml =
    val attrs = attributesOf(elem)
    val cs = children(elem)

    elem.label match
      case t"mi"             => Mi(textOf(elem), attrs)
      case t"mn"             => Mn(textOf(elem), attrs)
      case t"mo"             => Mo(textOf(elem), attrs)
      case t"mtext"          => Mtext(textOf(elem), attrs)
      case t"ms"             => Ms(textOf(elem), attrs)
      case t"mspace"         => Mspace(attrs)
      case t"mglyph"         => Mglyph(attrs)

      case t"mrow"           => Mrow(cs, attrs)
      case t"msqrt"          => Msqrt(cs, attrs)
      case t"mstyle"         => Mstyle(cs, attrs)
      case t"merror"         => Merror(cs, attrs)
      case t"mpadded"        => Mpadded(cs, attrs)
      case t"mphantom"       => Mphantom(cs, attrs)
      case t"menclose"       => Menclose(cs, attrs)
      case t"mfenced"        => Mfenced(cs, attrs)
      case t"mfrac"          => Mfrac(at(cs, 0), at(cs, 1), attrs)
      case t"mroot"          => Mroot(at(cs, 0), at(cs, 1), attrs)

      case t"msub"           => Msub(at(cs, 0), at(cs, 1), attrs)
      case t"msup"           => Msup(at(cs, 0), at(cs, 1), attrs)
      case t"msubsup"        => Msubsup(at(cs, 0), at(cs, 1), at(cs, 2), attrs)
      case t"munder"         => Munder(at(cs, 0), at(cs, 1), attrs)
      case t"mover"          => Mover(at(cs, 0), at(cs, 1), attrs)
      case t"munderover"     => Munderover(at(cs, 0), at(cs, 1), at(cs, 2), attrs)
      case t"mmultiscripts"  => Mmultiscripts(cs, attrs)
      case t"mprescripts"    => Mprescripts(attrs)
      case t"mnone"          => Mnone(attrs)

      case t"mtable"         => Mtable(cs, attrs)
      case t"mtr"            => Mtr(cs, attrs)
      case t"mlabeledtr"     => Mlabeledtr(cs, attrs)
      case t"mtd"            => Mtd(cs, attrs)
      case t"maligngroup"    => Maligngroup(attrs)
      case t"malignmark"     => Malignmark(attrs)

      case t"mstack"         => Mstack(cs, attrs)
      case t"mlongdiv"       => Mlongdiv(cs, attrs)
      case t"msgroup"        => Msgroup(cs, attrs)
      case t"msrow"          => Msrow(cs, attrs)
      case t"mscarries"      => Mscarries(cs, attrs)
      case t"mscarry"        => Mscarry(cs, attrs)
      case t"msline"         => Msline(attrs)

      case t"maction"        => Maction(cs, attrs)
      case t"semantics"      => Semantics(cs, attrs)
      case t"annotation"     => Annotation(textOf(elem), attrs)
      case t"annotation-xml" => AnnotationXml(cs, attrs)

      case other             => abort(MathmlError(MathmlError.Reason.UnknownElement(other)))
