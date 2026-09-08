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
import contingency.*
import fulminate.*
import gossamer.*
import honeycomb.Html
import prepositional.*
import rudiments.*
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
//
// Every element type lives inside the `Mathml` companion (as `Mathml.Mrow`,
// `Mathml.Token`, and so on) rather than at the top level, so that generic names
// like `Token`, `Layout` and `Ms` do not leak into the `soundness` namespace and
// collide with unrelated modules. The category traits (`Token`, `Layout`, …) group
// the elements by their MathML spec class; nothing dispatches on them — the
// codebase programs against `Mathml` and the concrete elements.

// `Encodable in Math` instances live in the `Math` companion (the `Form`, mirroring
// `Encodable in Xml`); `atom` collapses an encoded `<math>` root back to a single
// node for callers — the `.mathml` extension and the `ergo""` macro — that want one.
object Mathml:
  def atom(math: Math): Mathml = math.contents match
    case List(node) => node
    case nodes      => Mrow(nodes)

  // Token (leaf) elements: the elements whose content is character data rather
  // than child elements. `Mspace` and `Mglyph` carry no text at all (they are
  // controlled entirely by their attributes), so their `text` is `Unset`.

  sealed trait Token extends Mathml:
    def contents: List[Mathml] = Nil

  case class Mi(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = "mi"
    def text: Optional[Text] = value

  case class Mn(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = "mn"
    def text: Optional[Text] = value

  case class Mo(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = "mo"
    def text: Optional[Text] = value

  case class Mtext(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = "mtext"
    def text: Optional[Text] = value

  case class Ms(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = "ms"
    def text: Optional[Text] = value

  case class Mspace(attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = "mspace"
    def text: Optional[Text] = Unset

  case class Mglyph(attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = "mglyph"
    def text: Optional[Text] = Unset

  // General layout schemata. The uniform containers (`Mrow`, `Msqrt`, `Mstyle`,
  // `Merror`, `Mpadded`, `Mphantom`, `Menclose`, `Mfenced`) hold an ordered list
  // of children; each provides a varargs `apply` for ergonomic construction. The
  // positional schemata (`Mfrac`, `Mroot`) name their children instead.

  sealed trait Layout extends Mathml:
    def text: Optional[Text] = Unset

  object Mrow:
    def apply(children: Mathml*): Mrow = Mrow(children.to(List))

  case class Mrow(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "mrow"

  case class Mfrac(numerator: Mathml, denominator: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Layout:
    def label: Text = "mfrac"
    def contents: List[Mathml] = List(numerator, denominator)

  object Msqrt:
    def apply(children: Mathml*): Msqrt = Msqrt(children.to(List))

  case class Msqrt(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "msqrt"

  case class Mroot(base: Mathml, index: Mathml, attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "mroot"
    def contents: List[Mathml] = List(base, index)

  object Mstyle:
    def apply(children: Mathml*): Mstyle = Mstyle(children.to(List))

  case class Mstyle(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "mstyle"

  object Merror:
    def apply(children: Mathml*): Merror = Merror(children.to(List))

  case class Merror(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "merror"

  object Mpadded:
    def apply(children: Mathml*): Mpadded = Mpadded(children.to(List))

  case class Mpadded(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "mpadded"

  object Mphantom:
    def apply(children: Mathml*): Mphantom = Mphantom(children.to(List))

  case class Mphantom(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "mphantom"

  object Menclose:
    def apply(children: Mathml*): Menclose = Menclose(children.to(List))

  case class Menclose(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "menclose"

  object Mfenced:
    def apply(children: Mathml*): Mfenced = Mfenced(children.to(List))

  case class Mfenced(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = "mfenced"

  // Script and limit schemata. `Msub`/`Msup`/`Msubsup` and `Munder`/`Mover`/
  // `Munderover` are positional; `Mmultiscripts` is a container whose children
  // interleave base, postscripts, an `Mprescripts` marker and prescripts, using
  // `Mnone` as an empty-script placeholder.

  sealed trait Script extends Mathml:
    def text: Optional[Text] = Unset

  case class Msub(base: Mathml, subscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = "msub"
    def contents: List[Mathml] = List(base, subscript)

  case class Msup(base: Mathml, superscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = "msup"
    def contents: List[Mathml] = List(base, superscript)

  case class Msubsup
    ( base:        Mathml,
      subscript:   Mathml,
      superscript: Mathml,
      attributes:  List[(Text, Text)] = Nil )
  extends Script:
    def label: Text = "msubsup"
    def contents: List[Mathml] = List(base, subscript, superscript)

  case class Munder(base: Mathml, underscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = "munder"
    def contents: List[Mathml] = List(base, underscript)

  case class Mover(base: Mathml, overscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = "mover"
    def contents: List[Mathml] = List(base, overscript)

  case class Munderover
    ( base:        Mathml,
      underscript: Mathml,
      overscript:  Mathml,
      attributes:  List[(Text, Text)] = Nil )
  extends Script:
    def label: Text = "munderover"
    def contents: List[Mathml] = List(base, underscript, overscript)

  object Mmultiscripts:
    def apply(children: Mathml*): Mmultiscripts = Mmultiscripts(children.to(List))

  case class Mmultiscripts(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = "mmultiscripts"

  case class Mprescripts(attributes: List[(Text, Text)] = Nil) extends Script:
    def label: Text = "mprescripts"
    def contents: List[Mathml] = Nil

  case class Mnone(attributes: List[(Text, Text)] = Nil) extends Script:
    def label: Text = "mnone"
    def contents: List[Mathml] = Nil

  // Table schemata: `<mtable>` and its rows (`<mtr>`, `<mlabeledtr>`), cells
  // (`<mtd>`), and the alignment markers `<maligngroup>` and `<malignmark>`.

  sealed trait Tabular extends Mathml:
    def text: Optional[Text] = Unset

  object Mtable:
    def apply(rows: Mathml*): Mtable = Mtable(rows.to(List))

  case class Mtable(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = "mtable"

  object Mtr:
    def apply(cells: Mathml*): Mtr = Mtr(cells.to(List))

  case class Mtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = "mtr"

  object Mlabeledtr:
    def apply(cells: Mathml*): Mlabeledtr = Mlabeledtr(cells.to(List))

  case class Mlabeledtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Tabular:
    def label: Text = "mlabeledtr"

  object Mtd:
    def apply(children: Mathml*): Mtd = Mtd(children.to(List))

  case class Mtd(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = "mtd"

  case class Maligngroup(attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = "maligngroup"
    def contents: List[Mathml] = Nil

  case class Malignmark(attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = "malignmark"
    def contents: List[Mathml] = Nil

  // Elementary-math schemata, used for column arithmetic and long division:
  // `<mstack>`, `<mlongdiv>`, `<msgroup>`, `<msrow>`, `<mscarries>`, `<mscarry>`
  // and `<msline>`.

  sealed trait Elementary extends Mathml:
    def text: Optional[Text] = Unset

  object Mstack:
    def apply(children: Mathml*): Mstack = Mstack(children.to(List))

  case class Mstack(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = "mstack"

  object Mlongdiv:
    def apply(children: Mathml*): Mlongdiv = Mlongdiv(children.to(List))

  case class Mlongdiv(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = "mlongdiv"

  object Msgroup:
    def apply(children: Mathml*): Msgroup = Msgroup(children.to(List))

  case class Msgroup(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = "msgroup"

  object Msrow:
    def apply(children: Mathml*): Msrow = Msrow(children.to(List))

  case class Msrow(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = "msrow"

  object Mscarries:
    def apply(children: Mathml*): Mscarries = Mscarries(children.to(List))

  case class Mscarries(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = "mscarries"

  object Mscarry:
    def apply(children: Mathml*): Mscarry = Mscarry(children.to(List))

  case class Mscarry(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = "mscarry"

  case class Msline(attributes: List[(Text, Text)] = Nil) extends Elementary:
    def label: Text = "msline"
    def contents: List[Mathml] = Nil

  // `<maction>` (bound actions such as toggle/highlight) plus the semantics
  // bridge: `<semantics>` pairs a presentation subtree with one or more
  // annotations. `<annotation>` carries character data (e.g. a TeX string) while
  // `<annotation-xml>` carries a nested markup subtree.

  sealed trait Semantic extends Mathml

  object Maction:
    def apply(children: Mathml*): Maction = Maction(children.to(List))

  case class Maction(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = "maction"
    def text: Optional[Text] = Unset

  object Semantics:
    def apply(children: Mathml*): Semantics = Semantics(children.to(List))

  case class Semantics(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = "semantics"
    def text: Optional[Text] = Unset

  case class Annotation(value: Text, attributes: List[(Text, Text)] = Nil) extends Semantic:
    def label: Text = "annotation"
    def contents: List[Mathml] = Nil
    def text: Optional[Text] = value

  object AnnotationXml:
    def apply(children: Mathml*): AnnotationXml = AnnotationXml(children.to(List))

  case class AnnotationXml(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = "annotation-xml"
    def text: Optional[Text] = Unset

  // MathmlError → Mathml.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NotMathml(label: Text)      extends Reason(1)
      case UnknownElement(label: Text) extends Reason(2)

    given communicable: Reason is Communicable =
      case Reason.NotMathml(label)      => m"the root element was <$label> instead of <math>"
      case Reason.UnknownElement(label) => m"the element <$label> is not a known MathML element"

  case class Error(reason: Mathml.Error.Reason)(using Diagnostics)
  extends fulminate.Error(430, reason.number)(m"the MathML could not be parsed because $reason")

  // MathmlParser → Mathml.Parser
  // Decodes a xylophone `Xml` tree into the Archimedes model by dispatching on
  // each element's label. Every element's attributes are preserved verbatim in
  // the node's `attributes` bag (except `xmlns`/`display` on the root, which are
  // represented structurally), so a parse/serialise round-trip is lossless.

  object Parser:
    def labelOf(xml: Xml): Text = xml match
      case element: Element => element.label
      case _                => "<unknown>"

    def findMath(nodes: List[Node])(using Tactic[Mathml.Error]): Element =
      nodes.reap { case element: Element if element.label == "math" => element }
      . or:
        abort(Mathml.Error(Mathml.Error.Reason.NotMathml("<missing>")))

    def rootElement(xml: Xml)(using Tactic[Mathml.Error]): Element = xml match
      case element: Element if element.label == "math" => element
      case Fragment(nodes*)                             => findMath(nodes.to(List))

      case other =>
        abort(Mathml.Error(Mathml.Error.Reason.NotMathml(labelOf(other))))

    private def childElements(elem: Element): List[Element] =
      (elem.children.readable.toList.collect { case element: Element => element }).to(List)

    private def attributesOf(elem: Element): List[(Text, Text)] =
      (elem.attributes.keys.map { key => (key, elem.attributes(key).or(t"")) }.toList).to(List)

    private def textOf(elem: Element): Text =
      (elem.children.readable.toList.collect { case TextNode(text) => text }).to(List).join

    private def children(elem: Element)(using Tactic[Mathml.Error]): List[Mathml] =
      childElements(elem).map(decodeNode)

    private def at(nodes: List[Mathml], index: Int): Mathml =
      nodes.stdlib.lift(index).getOrElse(Mrow(Nil))

    def decodeMath(elem: Element)(using Tactic[Mathml.Error]): Math =
      val kept = attributesOf(elem).filter { case (key, _) => key != "xmlns" && key != "display" }

      val display: Optional[Display] = elem.attributes("display").let: text =>
        Display.unapply(text).getOrElse(Display.Inline)

      Math(children(elem), display, kept)

    def decodeNode(elem: Element)(using Tactic[Mathml.Error]): Mathml =
      val attrs = attributesOf(elem)
      val cs = children(elem)

      elem.label match
        case "mi"             => Mi(textOf(elem), attrs)
        case "mn"             => Mn(textOf(elem), attrs)
        case "mo"             => Mo(textOf(elem), attrs)
        case "mtext"          => Mtext(textOf(elem), attrs)
        case "ms"             => Ms(textOf(elem), attrs)
        case "mspace"         => Mspace(attrs)
        case "mglyph"         => Mglyph(attrs)

        case "mrow"           => Mrow(cs, attrs)
        case "msqrt"          => Msqrt(cs, attrs)
        case "mstyle"         => Mstyle(cs, attrs)
        case "merror"         => Merror(cs, attrs)
        case "mpadded"        => Mpadded(cs, attrs)
        case "mphantom"       => Mphantom(cs, attrs)
        case "menclose"       => Menclose(cs, attrs)
        case "mfenced"        => Mfenced(cs, attrs)
        case "mfrac"          => Mfrac(at(cs, 0), at(cs, 1), attrs)
        case "mroot"          => Mroot(at(cs, 0), at(cs, 1), attrs)

        case "msub"           => Msub(at(cs, 0), at(cs, 1), attrs)
        case "msup"           => Msup(at(cs, 0), at(cs, 1), attrs)
        case "msubsup"        => Msubsup(at(cs, 0), at(cs, 1), at(cs, 2), attrs)
        case "munder"         => Munder(at(cs, 0), at(cs, 1), attrs)
        case "mover"          => Mover(at(cs, 0), at(cs, 1), attrs)
        case "munderover"     => Munderover(at(cs, 0), at(cs, 1), at(cs, 2), attrs)
        case "mmultiscripts"  => Mmultiscripts(cs, attrs)
        case "mprescripts"    => Mprescripts(attrs)
        case "mnone"          => Mnone(attrs)

        case "mtable"         => Mtable(cs, attrs)
        case "mtr"            => Mtr(cs, attrs)
        case "mlabeledtr"     => Mlabeledtr(cs, attrs)
        case "mtd"            => Mtd(cs, attrs)
        case "maligngroup"    => Maligngroup(attrs)
        case "malignmark"     => Malignmark(attrs)

        case "mstack"         => Mstack(cs, attrs)
        case "mlongdiv"       => Mlongdiv(cs, attrs)
        case "msgroup"        => Msgroup(cs, attrs)
        case "msrow"          => Msrow(cs, attrs)
        case "mscarries"      => Mscarries(cs, attrs)
        case "mscarry"        => Mscarry(cs, attrs)
        case "msline"         => Msline(attrs)

        case "maction"        => Maction(cs, attrs)
        case "semantics"      => Semantics(cs, attrs)
        case "annotation"     => Annotation(textOf(elem), attrs)
        case "annotation-xml" => AnnotationXml(cs, attrs)

        case other             => abort(Mathml.Error(Mathml.Error.Reason.UnknownElement(other)))

  // MathmlReader → Mathml.Reader
  // Extracts MathML embedded in HTML. Honeycomb parses `<math>` as a foreign
  // element with its own (non-xylophone) `Element`/`Node` types, so the reader
  // walks the honeycomb tree, finds the first `<math>` subtree, transcribes it
  // into a xylophone `Xml` element, and hands that to `Mathml.Parser` to reuse the
  // same label-dispatch decoding used for standalone XML.

  object Reader:
    def read(html: Html)(using Tactic[Mathml.Error]): Math =
      findMath(html).lay(abort(Mathml.Error(Mathml.Error.Reason.NotMathml("<missing>")))): element =>
        Mathml.Parser.decodeMath(toXmlElement(element))

    def findMath(html: Html): Optional[honeycomb.Element] = html match
      case element: honeycomb.Element =>
        if element.label == "math" then element else searchNodes(element.children)

      case fragment: honeycomb.Fragment => searchNodes(Array.from(fragment.nodes))
      case _                            => Unset

    private def searchNodes(nodes: Array[honeycomb.Node]^{}): Optional[honeycomb.Element] =
      var result: Optional[honeycomb.Element] = Unset
      var index = 0

      while index < nodes.length && result.absent do
        result = findMath(nodes.readUnchecked(index))
        index += 1

      result

    private def toXmlElement(element: honeycomb.Element): Element =
      val pairs: List[(Text, Text)] =
        element.attributes.keys.map { key => (key, element.attributes(key).or(t"")) }.to(List)

      val nodes: Array[Node]^{} = element.children.remap(toXmlNode)
      Element(element.label, Attributes(pairs*), nodes)

    private def toXmlNode(node: honeycomb.Node): Node = node match
      case element: honeycomb.Element   => toXmlElement(element)
      case textNode: honeycomb.TextNode => TextNode(textNode.text)
      case comment: honeycomb.Comment   => Comment(comment.text)
      case _                            => TextNode("")

trait Mathml:
  def label: Text
  def attributes: List[(Text, Text)]
  def contents: List[Mathml]
  def text: Optional[Text]

  def htmlAttributes: List[(Text, Optional[Text])] =
    attributes.map { case (key, value) => (key, value: Optional[Text]) }

  def xml: Xml =
    val children: List[Xml] = text.lay(contents.map(_.xml)): value =>
      List(TextNode(value))

    Element(label, Attributes(attributes*), children.nodes)

  def html: Html of "#foreign" =
    val children: List[Html of "#foreign"] =
      text.lay(contents.map(_.html)): value =>
        List(honeycomb.Html.string2(value.s))

    honeycomb.Element.foreign(label, honeycomb.Attributes(htmlAttributes*), children*)
