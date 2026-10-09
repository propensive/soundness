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
import denominative.*
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
  // An element's MathML as XML, as `element.in[Xml]`, and as foreign HTML content for
  // embedding in a page, as `element.html`. Both range over every `node <: Mathml`, since `is`
  // fixes `Self` exactly and an element is always a concrete subtype.
  given encodable: [node <: Mathml] => node is Encodable in Xml = node =>
    val children: List[Xml] = node.text.lay(node.contents.map(_.in[Xml])): value =>
      List(Xml.Text(value))

    Xml.Element(node.label, Attributes(node.attributes*), children.nodes)

  given renderable: [node <: Mathml] => (node is honeycomb.Renderable { type Form = "#foreign" }) =
    node =>
      val children: List[Html of "#foreign"] =
        node.text.lay(node.contents.map(renderable.render(_))): value =>
          List(honeycomb.Html.Text.foreign(value))

      honeycomb.Html.Element.foreign(node.label, honeycomb.Attributes(node.attributes*), children*)

  def atom(math: Math): Mathml = math.contents match
    case List(node) => node
    case nodes      => Mrow(nodes)

  // The varargs constructor shared by every element whose children are a plain list.
  trait Container[node](make: List[Mathml] -> node):
    def apply(children: Mathml*): node = make(children.to(List))

  // Token (leaf) elements: the elements whose content is character data rather
  // than child elements. `Mspace` and `Mglyph` carry no text at all (they are
  // controlled entirely by their attributes), so their `text` is `Unset`.

  sealed trait Token extends Mathml:
    def contents: List[Mathml] = Nil

  case class Mi(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = t"mi"
    def text: Optional[Text] = value

  case class Mn(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = t"mn"
    def text: Optional[Text] = value

  case class Mo(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = t"mo"
    def text: Optional[Text] = value

  case class Mtext(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = t"mtext"
    def text: Optional[Text] = value

  case class Ms(value: Text, attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = t"ms"
    def text: Optional[Text] = value

  case class Mspace(attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = t"mspace"
    def text: Optional[Text] = Unset

  case class Mglyph(attributes: List[(Text, Text)] = Nil) extends Token:
    def label: Text = t"mglyph"
    def text: Optional[Text] = Unset

  // General layout schemata. The uniform containers (`Mrow`, `Msqrt`, `Mstyle`,
  // `Merror`, `Mpadded`, `Mphantom`, `Menclose`, `Mfenced`) hold an ordered list
  // of children; each provides a varargs `apply` for ergonomic construction. The
  // positional schemata (`Mfrac`, `Mroot`) name their children instead.

  sealed trait Layout extends Mathml:
    def text: Optional[Text] = Unset

  object Mrow extends Container(new Mrow(_))

  case class Mrow(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mrow"

  case class Mfrac(numerator: Mathml, denominator: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Layout:
    def label: Text = t"mfrac"
    def contents: List[Mathml] = List(numerator, denominator)

  object Msqrt extends Container(new Msqrt(_))

  case class Msqrt(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"msqrt"

  case class Mroot(base: Mathml, index: Mathml, attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mroot"
    def contents: List[Mathml] = List(base, index)

  object Mstyle extends Container(new Mstyle(_))

  case class Mstyle(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mstyle"

  object Merror extends Container(new Merror(_))

  case class Merror(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"merror"

  object Mpadded extends Container(new Mpadded(_))

  case class Mpadded(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mpadded"

  object Mphantom extends Container(new Mphantom(_))

  case class Mphantom(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mphantom"

  object Menclose extends Container(new Menclose(_))

  case class Menclose(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"menclose"

  object Mfenced extends Container(new Mfenced(_))

  case class Mfenced(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mfenced"

  // Script and limit schemata. `Msub`/`Msup`/`Msubsup` and `Munder`/`Mover`/
  // `Munderover` are positional; `Mmultiscripts` is a container whose children
  // interleave base, postscripts, an `Mprescripts` marker and prescripts, using
  // `Mnone` as an empty-script placeholder.

  sealed trait Script extends Mathml:
    def text: Optional[Text] = Unset

  case class Msub(base: Mathml, subscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = t"msub"
    def contents: List[Mathml] = List(base, subscript)

  case class Msup(base: Mathml, superscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = t"msup"
    def contents: List[Mathml] = List(base, superscript)

  case class Msubsup
    ( base:        Mathml,
      subscript:   Mathml,
      superscript: Mathml,
      attributes:  List[(Text, Text)] = Nil )
  extends Script:
    def label: Text = t"msubsup"
    def contents: List[Mathml] = List(base, subscript, superscript)

  case class Munder(base: Mathml, underscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = t"munder"
    def contents: List[Mathml] = List(base, underscript)

  case class Mover(base: Mathml, overscript: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = t"mover"
    def contents: List[Mathml] = List(base, overscript)

  case class Munderover
    ( base:        Mathml,
      underscript: Mathml,
      overscript:  Mathml,
      attributes:  List[(Text, Text)] = Nil )
  extends Script:
    def label: Text = t"munderover"
    def contents: List[Mathml] = List(base, underscript, overscript)

  object Mmultiscripts extends Container(new Mmultiscripts(_))

  case class Mmultiscripts(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Script:
    def label: Text = t"mmultiscripts"

  case class Mprescripts(attributes: List[(Text, Text)] = Nil) extends Script:
    def label: Text = t"mprescripts"
    def contents: List[Mathml] = Nil

  case class Mnone(attributes: List[(Text, Text)] = Nil) extends Script:
    def label: Text = t"mnone"
    def contents: List[Mathml] = Nil

  // Table schemata: `<mtable>` and its rows (`<mtr>`, `<mlabeledtr>`), cells
  // (`<mtd>`), and the alignment markers `<maligngroup>` and `<malignmark>`.

  sealed trait Tabular extends Mathml:
    def text: Optional[Text] = Unset

  object Mtable extends Container(new Mtable(_))

  case class Mtable(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = t"mtable"

  object Mtr extends Container(new Mtr(_))

  case class Mtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = t"mtr"

  object Mlabeledtr extends Container(new Mlabeledtr(_))

  case class Mlabeledtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Tabular:
    def label: Text = t"mlabeledtr"

  object Mtd extends Container(new Mtd(_))

  case class Mtd(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = t"mtd"

  case class Maligngroup(attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = t"maligngroup"
    def contents: List[Mathml] = Nil

  case class Malignmark(attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = t"malignmark"
    def contents: List[Mathml] = Nil

  // Elementary-math schemata, used for column arithmetic and long division:
  // `<mstack>`, `<mlongdiv>`, `<msgroup>`, `<msrow>`, `<mscarries>`, `<mscarry>`
  // and `<msline>`.

  sealed trait Elementary extends Mathml:
    def text: Optional[Text] = Unset

  object Mstack extends Container(new Mstack(_))

  case class Mstack(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"mstack"

  object Mlongdiv extends Container(new Mlongdiv(_))

  case class Mlongdiv(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"mlongdiv"

  object Msgroup extends Container(new Msgroup(_))

  case class Msgroup(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"msgroup"

  object Msrow extends Container(new Msrow(_))

  case class Msrow(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"msrow"

  object Mscarries extends Container(new Mscarries(_))

  case class Mscarries(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"mscarries"

  object Mscarry extends Container(new Mscarry(_))

  case class Mscarry(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"mscarry"

  case class Msline(attributes: List[(Text, Text)] = Nil) extends Elementary:
    def label: Text = t"msline"
    def contents: List[Mathml] = Nil

  // `<maction>` (bound actions such as toggle/highlight) plus the semantics
  // bridge: `<semantics>` pairs a presentation subtree with one or more
  // annotations. `<annotation>` carries character data (e.g. a TeX string) while
  // `<annotation-xml>` carries a nested markup subtree.

  sealed trait Semantic extends Mathml

  object Maction extends Container(new Maction(_))

  case class Maction(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = t"maction"
    def text: Optional[Text] = Unset

  object Semantics extends Container(new Semantics(_))

  case class Semantics(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = t"semantics"
    def text: Optional[Text] = Unset

  case class Annotation(value: Text, attributes: List[(Text, Text)] = Nil) extends Semantic:
    def label: Text = t"annotation"
    def contents: List[Mathml] = Nil
    def text: Optional[Text] = value

  object AnnotationXml extends Container(new AnnotationXml(_))

  case class AnnotationXml(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = t"annotation-xml"
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
      case element: Xml.Element => element.label
      case _                => t"<unknown>"

    def findMath(nodes: List[Xml.Node])(using Tactic[Mathml.Error]): Xml.Element =
      nodes.reap { case element: Xml.Element if element.label == t"math" => element }
      . or:
        abort(Mathml.Error(Mathml.Error.Reason.NotMathml(t"<missing>")))

    def rootElement(xml: Xml)(using Tactic[Mathml.Error]): Xml.Element = xml match
      case element: Xml.Element if element.label == t"math" => element
      case Xml.Fragment(nodes*)                             => findMath(nodes.to(List))

      case other =>
        abort(Mathml.Error(Mathml.Error.Reason.NotMathml(labelOf(other))))

    private def childElements(elem: Xml.Element): List[Xml.Element] =
      elem.children.readable.collect { case element: Xml.Element => element }.to(List)

    private def textOf(elem: Xml.Element): Text =
      elem.children.readable.collect { case Xml.Text(text) => text }.to(List).join

    private def children(elem: Xml.Element)(using Tactic[Mathml.Error]): List[Mathml] =
      childElements(elem).map(decodeNode)

    // A MathML element has few children, so reading one by position is cheap.
    private def at(nodes: List[Mathml], index: Int): Mathml =
      import denominative.dysasymptotics.linearAccess
      nodes.at(index.z).or(Mrow(Nil))

    def decodeMath(elem: Xml.Element)(using Tactic[Mathml.Error]): Math =
      val kept = elem.attributes.to[List].filter: (key, _) =>
        key != t"xmlns" && key != t"display"

      val display: Optional[Display] = elem.attributes(t"display").let:
        case Display(display) => display
        case _                => Display.Inline

      Math(children(elem), display, kept)

    def decodeNode(elem: Xml.Element)(using Tactic[Mathml.Error]): Mathml =
      val attrs = elem.attributes.to[List]
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

        case other             => abort(Mathml.Error(Mathml.Error.Reason.UnknownElement(other)))

  // MathmlReader → Mathml.Reader
  // Extracts MathML embedded in HTML. Honeycomb parses `<math>` as a foreign
  // element with its own (non-xylophone) `Element`/`Node` types, so the reader
  // walks the honeycomb tree, finds the first `<math>` subtree, transcribes it
  // into a xylophone `Xml` element, and hands that to `Mathml.Parser` to reuse the
  // same label-dispatch decoding used for standalone XML.

  object Reader:
    def read(html: Html)(using Tactic[Mathml.Error]): Math =
      findMath(html).lay(abort(Mathml.Error(Mathml.Error.Reason.NotMathml(t"<missing>")))): element =>
        Mathml.Parser.decodeMath(toXmlElement(element))

    def findMath(html: Html): Optional[honeycomb.Html.Element] = html match
      case element: honeycomb.Html.Element =>
        if element.label == t"math" then element else searchNodes(element.children)

      case fragment: honeycomb.Html.Fragment => searchNodes(Array.from(fragment.nodes))
      case _                                 => Unset

    private def searchNodes(nodes: Array[honeycomb.Html.Node]^{})
    :   Optional[honeycomb.Html.Element] =

      var result: Optional[honeycomb.Html.Element] = Unset
      var index = 0

      while index < nodes.length && result.absent do
        result = findMath(nodes.readUnchecked(index))
        index += 1

      result

    private def toXmlElement(element: honeycomb.Html.Element): Xml.Element =
      val pairs: List[(Text, Text)] =
        element.attributes.keys.map { key => (key, element.attributes(key).or(t"")) }.to(List)

      val nodes: Array[Xml.Node]^{} = element.children.remap(toXmlNode)
      Xml.Element(element.label, Attributes(pairs*), nodes)

    private def toXmlNode(node: honeycomb.Html.Node): Xml.Node = node match
      case element: honeycomb.Html.Element => toXmlElement(element)
      case textNode: honeycomb.Html.Text   => Xml.Text(textNode.text)
      case comment: honeycomb.Html.Comment => Xml.Comment(comment.text)
      case _                               => Xml.Text(t"")

trait Mathml:
  def label: Text
  def attributes: List[(Text, Text)]
  def contents: List[Mathml]
  def text: Optional[Text]
