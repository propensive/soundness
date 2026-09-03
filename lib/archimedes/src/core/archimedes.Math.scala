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

import scala.math

import anticipation.*
import baroque.*
import contingency.*
import gossamer.*
import hieroglyph.*
import honeycomb.Html
import hypotenuse.*
import honeycomb.Renderable
import mosquito.*
import prepositional.*
import quantitative.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import contextual.*
import xylophone.*
import symbolism.*
import zephyrine.Parse

import Mathml.*

// The root `<math>` element and Archimedes' integration points.
//
//   - `Aggregable`/`Loadable` parse MathML text into `Math` (delegating the raw
//     XML parse to xylophone, then mapping the tree with `Mathml.Parser`);
//   - `Showable` serialises a `Document[Math]` to MathML text with an XML header;
//   - `Renderable in "math"` lets a `Math` value drop straight into honeycomb
//     HTML wherever `<math>` is admissible (embedded/phrasing/flow content),
//     reusing honeycomb's own serializer.

object Math:
  given aggregable: (schema: XmlSchema)
  =>  (parseTactic: Tactic[Parse.Error], xmlTactic: Tactic[Xml.Error], mathmlTactic: Tactic[Mathml.Error])
  =>  ((Math is Aggregable by Text)^{parseTactic, xmlTactic, mathmlTactic}) =

    source =>
      val xml: Xml = summon[Xml is Aggregable by Text].aggregate(source)
      Mathml.Parser.decodeMath(Mathml.Parser.rootElement(xml))

  given loadable: (XmlSchema)
  =>  (parseTactic: Tactic[Parse.Error])
  =>  (xmlTactic: Tactic[Xml.Error])
  =>  (mathmlTactic: Tactic[Mathml.Error])
  =>  ((Math is Loadable by Text)^{parseTactic, xmlTactic, mathmlTactic}) =

    source =>
      val xmlDoc: Document[Xml] = summon[(Xml is Loadable by Text)^].load(source)
      val mathElement = Mathml.Parser.rootElement(xmlDoc.root)
      val parsedMath: Math = Mathml.Parser.decodeMath(mathElement)

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

  // Only a `Document[Math]` has a `Showable`, and it produces multi-line serialized MathML with an
  // XML header. Inspection instead writes the node tree in a bracketed prefix form — each node as
  // its MathML element name, its attributes, then its character data or its children — which is
  // the structure a parse or layout bug is read from, on one line.
  private def node(mathml: Mathml): Text =
    val attributes = mathml.attributes.map { pair => t" ${pair(0)}=${pair(1).inspect}" }.join
    val children = mathml.contents.map { child => t" ${node(child)}" }.join
    val body = mathml.text.let { text => t" ${text.inspect}" }.or(children)

    t"⟨${mathml.label}$attributes$body⟩"

  // Renders an `Optional` exactly as `Inspectable.derived`'s `Mandatable` case would, spelt out
  // rather than summoned: that instance cannot be constructed under `-scalajs`, where the SAM is
  // expanded to an anonymous class whose parameter acquires a capture variable that `Self` does
  // not carry (soundness#1892, proscala#46). Inline this again once the fork accepts it.
  private def optional[value: Inspectable](value: Optional[value]): Text =
    value.let { present => t"｢${present.inspect}｣" }.or(t"○")

  given inspectable: [math <: Math] => math is Inspectable = math =>
    val attributes = math.attributes.map { pair => t" ${pair(0)}=${pair(1).inspect}" }.join
    val children = math.contents.map { child => t" ${node(child)}" }.join

    t"Math(${optional(math.display)}$attributes$children)"

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

  // Rationals render as `<mfrac>`, collapsing to a plain `<mn>` for whole values, with
  // the sign hoisted out as an `<mo>`.
  given q64: Q64 is Encodable in Math = value => rationalMathml(value.numerator, value.denominator)
  given q32: Q32 is Encodable in Math = value => rationalMathml(value.numerator, value.denominator)

  private def rationalMathml(numerator: Long, denominator: Long): Math =
    if denominator == 0L then Math(Mtext(t"NaR")) else
      val body =
        if denominator == 1L then Mn(math.abs(numerator).toString.tt)
        else Mfrac(Mn(math.abs(numerator).toString.tt), Mn(denominator.toString.tt))

      if numerator < 0L then Math(Mrow(List(Mo(t"−"), body))) else Math(body)

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
    QuantityEncodable[units]: quantity => Math(quantityMathml(quantity.value, quantity.units))

  given complex: [component: Encodable in Math] => Complex[component] is Encodable in Math =
    value => Math(Mrow(List(value.real.mathml, Mo(t"+"), value.imaginary.mathml, Mi(t"i"))))

  given vector: [element: Encodable in Math, size <: Int]
  =>  Vector[element, size] is Encodable in Math =
    vector =>
      Math(fenced(Mtable(vector.list.map { element => Mtr(Mtd(element.mathml)) }*), t"(", t")"))

  given matrix: [element: Encodable in Math, height <: Int, width <: Int]
  =>  Matrix[element, height, width] is Encodable in Math =
    matrix =>
      val rows = (0 until matrix.rows).toList.map: row =>
        Mtr((0 until matrix.columns).toList.map { column => Mtd(matrix(row, column).mathml) }*)

      Math(fenced(Mtable(rows*), t"[", t"]"))

  private def quantityMathml(value: Double, units: Map[Text, Int]): Mathml =
    val unitNodes: List[Mathml] =
      units.order(_(0).s).remap: (symbol, power) =>
        if power == 1 then Mi(symbol) else Msup(Mi(symbol), Mn(power.toString.tt))

    product(Mn(value.toString.tt) :: unitNodes)

  private def product(nodes: List[Mathml]): Mathml = nodes match
    case one :: Nil   => one
    case head :: tail =>
      Mrow(head :: tail.flatMap { node => List(Mo(t"⁢"), node) })
    case Nil          => Mrow(Nil)

  private def fenced(inner: Mathml, open: Text, close: Text): Mathml =
    val stretchy = List(t"stretchy" -> t"true")
    Mrow(List(Mo(open, stretchy), inner, Mo(close, stretchy)))

  // The `ergo""` interpolator: `ergo"(x↗y)"` parses an ergo shorthand literal into
  // a `Math` value, checking it at compile time (see `internal.ergoInterpolator`). In the
  // companion, so it is in `Math`'s implicit scope with no import.
  inline given interpolable: Math is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Math =

      ${archimedes.internal.ergoInterpolator[parts, origins]('insertions)}

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

    (t"xmlns" -> mathmlNamespace) :: displayPairs + attributes

  def xml: Xml =
    val children: Array[Node]^{} = contents.map(_.xml).nodes
    Element(t"math", Attributes(attributePairs*), children)

  def html: Html of "math" = Math.renderable.render(this)
