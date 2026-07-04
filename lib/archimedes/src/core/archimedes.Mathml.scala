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
import baroque.*
import gossamer.*
import honeycomb.Html
import mosquito.*
import prepositional.*
import quantitative.*
import vacuous.*
import xylophone.*

// The base type for every MathML (Presentation MathML) node. Each element is an
// immutable case class that knows its own tag `label`, its ordered `attributes`
// bag (so that any presentation/global attribute round-trips losslessly, even
// when Archimedes exposes no typed accessor for it), and either a `text` payload
// (token elements like `<mi>`) or a list of child `contents` (layout elements).
//
// From that common shape a node renders to two different document models:
//
//   - `xml`  builds a `xylophone.Xml` tree (used for reading/writing MathML as
//     standalone XML, and as the payload embedded in an XML document);
//   - `html` builds a `honeycomb.Html` foreign-element tree (used for embedding
//     MathML inside HTML, where honeycomb reserves `<math>` as a foreign tag).

// `Encodable in Mathml` instances live here (the `Form` companion), so `.mathml`
// and `.math` (defined in `archimedes_core.scala`) resolve without an import.
object Mathml:
  given int:        Int is Encodable in Mathml        = value => Mn(value.toString.tt)
  given long:       Long is Encodable in Mathml       = value => Mn(value.toString.tt)
  given short:      Short is Encodable in Mathml      = value => Mn(value.toString.tt)
  given byte:       Byte is Encodable in Mathml       = value => Mn(value.toString.tt)
  given double:     Double is Encodable in Mathml     = value => Mn(value.toString.tt)
  given float:      Float is Encodable in Mathml      = value => Mn(value.toString.tt)
  given bigInt:     BigInt is Encodable in Mathml     = value => Mn(value.toString.tt)
  given bigDecimal: BigDecimal is Encodable in Mathml = value => Mn(value.toString.tt)
  given text:       Text is Encodable in Mathml       = Mtext(_)
  given identical:  Mathml is Encodable in Mathml     = identity(_)

  // Wraps the encoder lambda in a class (rather than an inline function value) to
  // avoid inline-given code bloat, mirroring quantitative's `ShowableQuantity`. It
  // must be public because the inline given inlines a reference to it.
  class QuantityEncodable[units <: Measure](lambda: Quantity[units] => Mathml)
  extends Encodable:
    type Self = Quantity[units]
    type Form = Mathml
    def encoded(quantity: Quantity[units]): Mathml = lambda(quantity)

  // A quantity renders its magnitude as `<mn>` followed by each unit as `<mi>`
  // (or `<msup>` when the exponent is not 1), joined by invisible multiplication.
  inline given quantity: [units <: Measure] => Quantity[units] is Encodable in Mathml =
    QuantityEncodable[units]: quantity =>
      quantityMathml(quantity.value, quantity.units)

  given complex: [component: Encodable in Mathml] => Complex[component] is Encodable in Mathml =
    value => Mrow(List(value.real.mathml, Mo(t"+"), value.imaginary.mathml, Mi(t"i")))

  given vector: [element: Encodable in Mathml, size <: Int]
  =>  Vector[element, size] is Encodable in Mathml =
    vector => fenced(Mtable(vector.list.map { element => Mtr(Mtd(element.mathml)) }*), t"(", t")")

  given matrix: [element: Encodable in Mathml, height <: Int, width <: Int]
  =>  Matrix[element, height, width] is Encodable in Mathml =
    matrix =>
      val rows = (0 until matrix.rows).to(List).map: row =>
        Mtr((0 until matrix.columns).to(List).map { column => Mtd(matrix(row, column).mathml) }*)

      fenced(Mtable(rows*), t"[", t"]")

  private def quantityMathml(value: Double, units: Map[Text, Int]): Mathml =
    val unitNodes: List[Mathml] = units.to(List).sortBy(_._1.s).map: (symbol, power) =>
      if power == 1 then Mi(symbol) else Msup(Mi(symbol), Mn(power.toString.tt))

    product(Mn(value.toString.tt) :: unitNodes)

  private def product(nodes: List[Mathml]): Mathml = nodes match
    case one :: Nil   => one
    case head :: tail => Mrow(head :: tail.flatMap { node => List(Mo(t"⁢"), node) })
    case Nil          => Mrow(Nil)

  private def fenced(inner: Mathml, open: Text, close: Text): Mathml =
    val stretchy = List(t"stretchy" -> t"true")
    Mrow(List(Mo(open, stretchy), inner, Mo(close, stretchy)))

trait Mathml:
  def label: Text
  def attributes: List[(Text, Text)]
  def contents: List[Mathml]
  def text: Optional[Text]

  def htmlAttributes: Seq[(Text, Optional[Text])] =
    attributes.map { case (key, value) => (key, value: Optional[Text]) }

  def xml: Xml =
    val children: Seq[Xml] = text.lay(contents.map(_.xml)): value =>
      List(TextNode(value))

    Element(label, Attributes(attributes*), children.nodes)

  def html: Html of "#foreign" =
    val children: Seq[Html of "#foreign"] = text.lay(contents.map(_.html)): value =>
      List(honeycomb.Html.string2(value.s))

    honeycomb.Element.foreign(label, honeycomb.Attributes(htmlAttributes*), children*)
