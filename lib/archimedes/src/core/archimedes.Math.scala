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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.math

import proscenium.compat.*

import anticipation.*
import baroque.*
import contingency.*
import gossamer.*
import hieroglyph.*
import honeycomb.Html
import honeycomb.Renderable
import mosquito.*
import prepositional.*
import quantitative.*
import spectacular.*
import turbulence.*
import vacuous.*
import xylophone.*
import zephyrine.ParseError

import Mathml.*

// The root `<math>` element and Archimedes' integration points.
//
//   - `Aggregable`/`Loadable` parse MathML text into `Math` (delegating the raw
//     XML parse to xylophone, then mapping the tree with `MathmlParser`);
//   - `Showable` serialises a `Document[Math]` to MathML text with an XML header;
//   - `Renderable in "math"` lets a `Math` value drop straight into honeycomb
//     HTML wherever `<math>` is admissible (embedded/phrasing/flow content),
//     reusing honeycomb's own serializer.

object Math:
  given aggregable: (schema: XmlSchema)
  =>  (parseTactic: Tactic[ParseError], xmlTactic: Tactic[XmlError], mathmlTactic: Tactic[MathmlError])
  =>  ((Math is Aggregable by Text)^{parseTactic, xmlTactic, mathmlTactic}) =

    source =>
      val xml: Xml = summon[Xml is Aggregable by Text].aggregate(source)
      MathmlParser.decodeMath(MathmlParser.rootElement(xml))


  given loadable: (XmlSchema)
  =>  (parseTactic: Tactic[ParseError])
  =>  (xmlTactic: Tactic[XmlError])
  =>  (mathmlTactic: Tactic[MathmlError])
  =>  ((Math is Loadable by Text)^{parseTactic, xmlTactic, mathmlTactic}) =

    source =>
      val xmlDoc: Document[Xml] = summon[(Xml is Loadable by Text)^].load(source)
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
    val pairs = math.attributePairs.stdlib.map { case (key, value) => (key, value: Optional[Text]) }
    val children = math.contents.stdlib.map(_.html)
    honeycomb.doms.html.whatwg.Math.node(honeycomb.Attributes(pairs*))(children*)

  def apply(children: Mathml*): Math = Math(children.to(List))

  // `Encodable in Math` instances: any value encodes to a `<math>` document, just
  // as `Encodable in Xml` yields an `Xml`. `Mathml.atom` collapses a root back to a
  // single node where one is needed (the `.mathml` extension, the `ergo""` macro).
  given int:        Int is Encodable in Math        = value => Math(Mn(value.toString.tt))
  given long:       Long is Encodable in Math       = value => Math(Mn(value.toString.tt))
  given short:      Short is Encodable in Math      = value => Math(Mn(value.toString.tt))
  given byte:       Byte is Encodable in Math       = value => Math(Mn(value.toString.tt))
  given double:     Double is Encodable in Math     = value => Math(Mn(value.toString.tt))
  given float:      Float is Encodable in Math      = value => Math(Mn(value.toString.tt))
  given bigInt:     BigInt is Encodable in Math     = value => Math(Mn(value.toString.tt))
  given bigDecimal: BigDecimal is Encodable in Math = value => Math(Mn(value.toString.tt))
  given text:       Text is Encodable in Math       = value => Math(Mtext(value))
  given identical:  Mathml is Encodable in Math     = node => Math(List(node))
  given self:       Math is Encodable in Math       = identity(_)

  // Wraps the encoder lambda in a class (rather than an inline function value) to
  // avoid inline-given code bloat, mirroring quantitative's `ShowableQuantity`. It
  // must be public because the inline given inlines a reference to it.
  // A pure function (`->`): the instance retains it, and a capturing conversion would make
  // the typeclass instance itself a capability, which its pure self type (rightly) forbids.
  class QuantityEncodable[units <: Measure](lambda: Quantity[units] -> Math)
  extends Encodable:
    type Self = Quantity[units]
    type Form = Math
    def encoded(quantity: Quantity[units]): Math = lambda(quantity)

  // A quantity renders its magnitude as `<mn>` followed by each unit as `<mi>`
  // (or `<msup>` when the exponent is not 1), joined by invisible multiplication.
  inline given quantity: [units <: Measure] => Quantity[units] is Encodable in Math =
    QuantityEncodable[units]: quantity =>
      Math(quantityMathml(quantity.value, quantity.units))

  given complex: [component: Encodable in Math] => Complex[component] is Encodable in Math =
    value => Math(Mrow(List(value.real.mathml, Mo(t"+"), value.imaginary.mathml, Mi(t"i"))))

  given vector: [element: Encodable in Math, size <: Int]
  =>  Vector[element, size] is Encodable in Math =
    vector =>
      Math(fenced(Mtable(vector.list.stdlib.map { element => Mtr(Mtd(element.mathml)) }*), t"(", t")"))

  given matrix: [element: Encodable in Math, height <: Int, width <: Int]
  =>  Matrix[element, height, width] is Encodable in Math =
    matrix =>
      val rows = (0 until matrix.rows).toList.map: row =>
        Mtr((0 until matrix.columns).toList.map { column => Mtd(matrix(row, column).mathml) }*)

      Math(fenced(Mtable(rows*), t"[", t"]"))

  private def quantityMathml(value: Double, units: Map[Text, Int]): Mathml =
    val unitNodes: scala.collection.immutable.List[Mathml] =
      units.stdlib.toList.sortBy(_._1.s).map: (symbol, power) =>
        if power == 1 then Mi(symbol) else Msup(Mi(symbol), Mn(power.toString.tt))

    product(List.of(Mn(value.toString.tt) :: unitNodes))

  private def product(nodes: List[Mathml]): Mathml = nodes match
    case one :: Nil   => one
    case head :: tail =>
      Mrow(List.of(head :: tail.stdlib.flatMap { node => scala.collection.immutable.List(Mo(t"⁢"), node) }))
    case Nil          => Mrow(Nil)

  private def fenced(inner: Mathml, open: Text, close: Text): Mathml =
    val stretchy = List(t"stretchy" -> t"true")
    Mrow(List(Mo(open, stretchy), inner, Mo(close, stretchy)))


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
    val children: IArray[Node] = contents.stdlib.map(_.xml).nodes
    Element(t"math", Attributes(attributePairs*), children)

  def html: Html of "math" = Math.renderable.render(this)
