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
package archimedes

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import honeycomb.Html
import honeycomb.Renderable
import prepositional.*
import spectacular.*
import turbulence.*
import vacuous.*
import xylophone.*
import zephyrine.ParseError

// The root `<math>` element and Archimedes' integration points.
//
//   - `Aggregable`/`Loadable` parse MathML text into `Math` (delegating the raw
//     XML parse to xylophone, then mapping the tree with `MathmlParser`);
//   - `Showable` serialises a `Document[Math]` to MathML text with an XML header;
//   - `Renderable in "math"` lets a `Math` value drop straight into honeycomb
//     HTML wherever `<math>` is admissible (embedded/phrasing/flow content),
//     reusing honeycomb's own serializer.

object Math:
  given aggregable: (XmlSchema)
  =>  Tactic[ParseError]
  =>  Tactic[XmlError]
  =>  Tactic[MathmlError]
  =>  Math is Aggregable by Text =

    source =>
      val xml: Xml = summon[Xml is Aggregable by Text].aggregate(source)
      MathmlParser.decodeMath(MathmlParser.rootElement(xml))


  given loadable: (XmlSchema)
  =>  Tactic[ParseError]
  =>  Tactic[XmlError]
  =>  Tactic[MathmlError]
  =>  Math is Loadable by Text =

    source =>
      val xmlDoc: Document[Xml] = summon[Xml is Loadable by Text].load(source)
      val mathElement = MathmlParser.rootElement(xmlDoc.root)
      val parsedMath: Math = MathmlParser.decodeMath(mathElement)

      val encoding: Encoding =
        xmlDoc.metadata.encoding.let: name => Encoding.unapply(name).getOrElse(enc"UTF-8")
        . or(enc"UTF-8")

      Document[Math](parsedMath, encoding)


  given showable: [doc <: Document[Math]] => doc is Showable =
    document =>
      val header = Header(t"1.0", document.metadata.name, Unset)

      val full: Xml = document.root.xml.absolve match
        case node: Node       => Fragment(header, node)
        case Fragment(nodes*) => Fragment((header +: nodes)*)

      full.show


  given renderable: (Math is Renderable { type Form = "math" }) = math =>
    val pairs = math.attributePairs.map { case (key, value) => (key, value: Optional[Text]) }
    val children = math.contents.map(_.html)
    honeycomb.doms.html.whatwg.Math.node(honeycomb.Attributes(pairs*))(children*)

  def apply(children: Mathml*): Math = Math(children.to(List))


case class Math
  ( contents:   List[Mathml],
    display:    Optional[Display]  = Unset,
    attributes: List[(Text, Text)] = Nil )
extends Documentary:

  type Self = Math
  type Metadata = Encoding

  def attributePairs: List[(Text, Text)] =
    val displayPairs: List[(Text, Text)] = display.lay(Nil): value =>
      List(t"display" -> value.encode)

    (t"xmlns" -> mathmlNamespace) :: displayPairs ::: attributes

  def xml: Xml =
    val children: IArray[Node] = contents.map(_.xml).nodes
    Element(t"math", Attributes(attributePairs*), children)

  def html: Html of "math" = Math.renderable.render(this)
