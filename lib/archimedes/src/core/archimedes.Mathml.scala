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
    def apply(children: Mathml*): node = make(children)

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

  case class Mroot(base: Mathml, index: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Layout:
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
      case _                    => t"<unknown>"

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
      elem.children.sweep { case element: Xml.Element => element }.to[List]

    private def textOf(elem: Xml.Element): Text =
      elem.children.sweep { case Xml.Text(text) => text }.to[List].join

    private def children(elem: Xml.Element)(using Tactic[Mathml.Error]): List[Mathml] =
      childElements(elem).map(decodeNode)

    // A MathML element has few children, so reading one by position is cheap.
    private def at(nodes: List[Mathml], index: Int): Mathml =
      import denominative.dysasymptotics.linearAccess
      nodes.at(index.z).or(Mrow(Nil))

    def decodeMath(elem: Xml.Element)(using Tactic[Mathml.Error]): Math =
      val kept = elem.attributes.to[List].filter: (key, _) => key != t"xmlns" && key != t"display"

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
      findMath(html)
      . lay(abort(Mathml.Error(Mathml.Error.Reason.NotMathml(t"<missing>")))): element =>
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
        element.attributes.keys.map { key => (key, element.attributes(key).or(t"")) }.to[List]

      val nodes: Array[Xml.Node]^{} = element.children.remap(toXmlNode)
      Xml.Element(element.label, Attributes(pairs*), nodes)

    private def toXmlNode(node: honeycomb.Html.Node): Xml.Node = node match
      case element: honeycomb.Html.Element => toXmlElement(element)
      case textNode: honeycomb.Html.Text   => Xml.Text(textNode.text)
      case comment: honeycomb.Html.Comment => Xml.Comment(comment.text)
      case _                               => Xml.Text(t"")

  object Display:
    import anticipation.*
    import gossamer.*
    import prepositional.*

    given encodable: Display is Encodable in Text =
      case Block  => t"block"
      case Inline => t"inline"

    def unapply(text: Text): Option[Display] = text match
      case t"block"  => Some(Block)
      case t"inline" => Some(Inline)
      case _         => None

  enum Display:
    import anticipation.*
    import gossamer.*
    import prepositional.*

    case Block, Inline

  object Cell:
    import scala.collection.immutable.{Map, Set}
    import anticipation.*
    import gossamer.*
    import hieroglyph.Measurable
    import hieroglyph.textMetrics.wideCharacterWidthMetric
    import denominative.*
    import rudiments.*
    import spectacular.*
    import symbolism.*
    import vacuous.*
    import Mathml.*

    private val Bar: Char = '─'
    private val Radical: Char = '√'
    private val Corner: Char = '┌'
    private val Stem: Char = '│'
    private val Tick: Char = '╲'

    private val brackets: Set[Char] = Set('(', ')', '[', ']', '{', '}', '|', '‖')
    private val integrals: Set[Char] = Set('∫', '∬', '∭', '∮', '∯', '∰')
    private val bigOperators: Set[Char] = Set('∑', '∏')

    private val binaryOperators: Set[Text] =
      Set(t"+", t"−", t"-", t"=", t"±", t"∓", t"×", t"÷", t"·", t"∙", t"<", t">", t"≤", t"≥", t"≠",
          t"≈", t"≡", t"→", t"⇒", t"↦", t"∈", t"∉", t"⊂", t"⊆", t"∪", t"∩", t"∧", t"∨")

    private def max(left: Int, right: Int): Int = if left > right then left else right
    private def min(left: Int, right: Int): Int = if left < right then left else right

    private def spaces(count: Int): Writing = Writing(Text.fill(max(count, 0))(_ => ' '))
    private def repeat(char: Char, count: Int): Writing =
      Writing(Text.fill(max(count, 0))(_ => char))

    // The text of `cell`'s row, or a full-width blank when the row is out of bounds.
    private def slice(cell: Cell, row: Int): Text =
      cell.lines(row.z).let(_.text).or(spaces(cell.width).text)

    // `render` lays the block out as it will appear — several lines of monospaced text — which an
    // inspection cannot be. The dimensions and the baseline row (the state which composition
    // depends on, and which `render` shows only implicitly) are written first, then each line as a
    // text literal, so that trailing padding spaces are visible.
    given inspectable: [cell <: Cell] => cell is Inspectable = cell =>
      val lines = cell.lines.map(_.text.inspect).join(t" ")
      t"Cell(${cell.width}×${cell.height}@${cell.baseline} $lines)"

    val empty: Cell = Cell(Sequence(Writing.empty), 0, 0)

    def line(text: Text): Cell =
      val writing = Writing(text)
      Cell(Sequence(writing), summon[Writing is Measurable].width(writing), 0)

    def blank(width: Int, height: Int, baseline: Int): Cell =
      Cell(Iterator.fill(max(height, 1))(spaces(width)).to[Sequence], max(width, 0), baseline)

    // Horizontal, baseline-aligned concatenation — the workhorse for `<mrow>`. Each
    // cell is framed with blank rows so every baseline lines up, then rows are joined.
    def beside(cells: List[Cell]): Cell = cells match
      case Nil => empty

      case _ =>
        val ascent = cells.map(_.baseline).maximize(identity(_)).or(0)

        val descent =
          cells.map { cell => cell.height - 1 - cell.baseline }.maximize(identity(_)).or(0)

        val framed = cells.stdlib.map(_.framed(ascent, descent))

        // Every framed cell has exactly `ascent + descent + 1` lines, so joining row-wise is an
        // n-ary zip of the line sequences — no indexing, hence no bounds obligation to discharge.
        val rows =
          framed
          . map: cell => cell.lines.stdlib.map(_.text)
          . reduceLeft: (left, right) => left.zip(right).map { (l, r) => t"$l$r" }

        Cell(Sequence.from(rows.map(Writing(_))), cells.map(_.width).total, ascent)

    def fraction(numerator: Cell, denominator: Cell): Cell =
      val width = max(numerator.width, denominator.width) + 2
      val top = numerator.hcenter(width)
      val bottom = denominator.hcenter(width)
      Cell((top.lines :+ repeat(Bar, width)) + bottom.lines, width, top.height)

    // Unicode already provides ready-made superscript and subscript glyphs for the
    // digits, the sign and bracket operators, and much (but not all) of the Latin
    // alphabet. A script written entirely in characters these tables cover needs no
    // extra rows: it can be set on the same line as its base, so `x^2` draws as `x²`
    // instead of stacking `2` diagonally above the `x`. Anything the tables cannot
    // spell — a fraction, a nested script, a Greek letter, a `q` — falls back to the
    // diagonal layout below. The lowercase superscript alphabet has no `q`, and the
    // subscript alphabet is sparser still, so both tables are enumerated explicitly
    // rather than derived by arithmetic on codepoints.

    private val superscripts: Map[Char, Char] =
      Map('0' -> '⁰', '1' -> '¹', '2' -> '²', '3' -> '³', '4' -> '⁴',
          '5' -> '⁵', '6' -> '⁶', '7' -> '⁷', '8' -> '⁸', '9' -> '⁹',
          '+' -> '⁺', '-' -> '⁻', '−' -> '⁻', '=' -> '⁼',
          '(' -> '⁽', ')' -> '⁾',
          'a' -> 'ᵃ', 'b' -> 'ᵇ', 'c' -> 'ᶜ', 'd' -> 'ᵈ', 'e' -> 'ᵉ',
          'f' -> 'ᶠ', 'g' -> 'ᵍ', 'h' -> 'ʰ', 'i' -> 'ⁱ', 'j' -> 'ʲ',
          'k' -> 'ᵏ', 'l' -> 'ˡ', 'm' -> 'ᵐ', 'n' -> 'ⁿ', 'o' -> 'ᵒ',
          'p' -> 'ᵖ', 'r' -> 'ʳ', 's' -> 'ˢ', 't' -> 'ᵗ', 'u' -> 'ᵘ',
          'v' -> 'ᵛ', 'w' -> 'ʷ', 'x' -> 'ˣ', 'y' -> 'ʸ', 'z' -> 'ᶻ')

    private val subscripts: Map[Char, Char] =
      Map('0' -> '₀', '1' -> '₁', '2' -> '₂', '3' -> '₃', '4' -> '₄',
          '5' -> '₅', '6' -> '₆', '7' -> '₇', '8' -> '₈', '9' -> '₉',
          '+' -> '₊', '-' -> '₋', '−' -> '₋', '=' -> '₌',
          '(' -> '₍', ')' -> '₎',
          'a' -> 'ₐ', 'e' -> 'ₑ', 'h' -> 'ₕ', 'i' -> 'ᵢ', 'j' -> 'ⱼ',
          'k' -> 'ₖ', 'l' -> 'ₗ', 'm' -> 'ₘ', 'n' -> 'ₙ', 'o' -> 'ₒ',
          'p' -> 'ₚ', 'r' -> 'ᵣ', 's' -> 'ₛ', 't' -> 'ₜ', 'u' -> 'ᵤ',
          'v' -> 'ᵥ', 'x' -> 'ₓ')

    // The character data a script node stands for, provided it is nothing but
    // character data: a token element's own text, or the concatenation of a purely
    // presentational wrapper's children. Note that operators contribute their raw
    // text, without the spacing `operator` would add — `−x` is set as `⁻ˣ`, not
    // as ` − x`. Any node with real structure (a fraction, a radical, a table)
    // yields `Unset`, which is what rules out same-line rendering.
    private def scriptText(node: Mathml): Optional[Text] = node match
      case Mrow(contents, _)    => scriptTexts(contents)
      case Mstyle(contents, _)  => scriptTexts(contents)
      case Mpadded(contents, _) => scriptTexts(contents)
      case node                 => node.text

    private def scriptTexts(nodes: List[Mathml]): Optional[Text] =
      // `Optional` is a union, so a `let` inside a `let` flattens: one absent child
      // makes the whole concatenation absent.
      nodes.fold(t"": Optional[Text]): (text, node) =>
        text.let: prefix => scriptText(node).let { part => t"$prefix$part" }

    // The script rewritten in `glyphs`, or `Unset` if any character has no glyph.
    private def transcribe(text: Text, glyphs: Map[Char, Char]): Optional[Text] =
      val length = text.length

      def recur(index: Int, done: Text): Optional[Text] =
        if index == length then done else
          val char = text.s.charAt(index)
          if glyphs.contains(char) then recur(index + 1, t"$done${glyphs(char)}") else Unset

      if length == 0 then Unset else recur(0, t"")

    // A one-line rendering of `base` with `script` set beside it, when the base
    // occupies a single line and the script has glyphs for every character.
    private def sameLine(base: Cell, script: Mathml, glyphs: Map[Char, Char]): Optional[Cell] =
      if base.height > 1 then Unset else
        scriptText(script).let(transcribe(_, glyphs)).let: text => line(t"${slice(base, 0)}$text")

    def superscript(base: Cell, script: Cell): Cell =
      val height = base.height + script.height

      val lines =
        Sequence.from:
          (0 until height).map: row =>
            Writing(t"${slice(base, row - script.height)}${slice(script, row)}")

      Cell(lines, base.width + script.width, base.baseline + script.height)

    def subscript(base: Cell, script: Cell): Cell =
      val height = base.height + script.height

      val lines =
        Sequence.from:
          (0 until height).map: row =>
            Writing(t"${slice(base, row)}${slice(script, row - base.height)}")

      Cell(lines, base.width + script.width, base.baseline)

    def subsup(base: Cell, subscript: Cell, superscript: Cell): Cell =
      val right = max(subscript.width, superscript.width)
      val height = superscript.height + base.height + subscript.height
      val middle = superscript.height + base.height

      val lines =
        Sequence.from:
          (0 until height).map: row =>
            val left = slice(base, row - superscript.height)

            // `pick` makes the branch guard and the bounds proof the same act: a confined
            // ordinal indexes its own sequence bare, and an out-of-range row is simply `Unset`.
            val fromSuperscript = superscript.lines.pick(row.z): ord =>
              val pad = spaces(right - superscript.width).text
              t"${superscript.lines(ord).text}$pad"

            val fromSubscript = subscript.lines.pick((row - middle).z): ord =>
              val pad = spaces(right - subscript.width).text
              t"${subscript.lines(ord).text}$pad"

            val scripts = fromSuperscript.or(fromSubscript.or(spaces(right).text))

            Writing(t"$left$scripts")

      Cell(lines, base.width + right, superscript.height + base.baseline)

    // Each of the three script schemata prefers the same-line Unicode form, and falls
    // back to stacking when the script is not spellable in script glyphs.

    private def superscripted(base: Mathml, script: Mathml): Cell =
      val cell = of(base)
      sameLine(cell, script, superscripts).or(superscript(cell, of(script)))

    private def subscripted(base: Mathml, script: Mathml): Cell =
      val cell = of(base)
      sameLine(cell, script, subscripts).or(subscript(cell, of(script)))

    // Both scripts must go on the line, or neither: an inline subscript beneath a
    // stacked superscript would set the two in different columns.
    private def subsupscripted(base: Mathml, sub: Mathml, sup: Mathml): Cell =
      val cell = of(base)
      val inlined = sameLine(cell, sub, subscripts).let(sameLine(_, sup, superscripts))
      inlined.or(subsup(cell, of(sub), of(sup)))

    def overscript(base: Cell, over: Cell): Cell =
      val width = max(base.width, over.width)
      Cell
        ( over.hcenter(width).lines + base.hcenter(width).lines,
          width,
          over.height + base.baseline )

    def underscript(base: Cell, under: Cell): Cell =
      val width = max(base.width, under.width)
      Cell(base.hcenter(width).lines + under.hcenter(width).lines, width, base.baseline)

    def underover(base: Cell, under: Cell, over: Cell): Cell =
      val width = max(max(base.width, under.width), over.width)

      val lines = over.hcenter(width).lines + base.hcenter(width).lines + under.hcenter(width).lines
      Cell(lines, width, over.height + base.baseline)

    // A square root. Over a single line the compact `√` glyph suffices; over taller
    // radicands the sign is drawn as a `╲` foot and a `│` stem rising to the `┌`
    // corner that turns into the `─` vinculum across the top.
    def radical(inner: Cell): Cell =
      if inner.height > 1 then tallRadical(inner) else
        val overline = Writing(t"$Corner${repeat(Bar, inner.width).text}")
        val body = Writing(t"$Radical${slice(inner, 0)}")
        Cell(Sequence(overline, body), inner.width + 1, 1)

    private def tallRadical(inner: Cell): Cell =
      val overline = Writing(t" $Corner${repeat(Bar, inner.width).text}")

      // Traversing `inner`'s own lines directly, the row content needs no index at all; the
      // index survives only to mark the last row with the radical's foot.
      val body = inner.lines.indexed.map: (line, ordinal) =>
        val foot = if ordinal.n0 == inner.height - 1 then Tick else ' '
        Writing(t"$foot$Stem${line.text}")

      Cell(Sequence(overline) + body, inner.width + 2, inner.baseline + 1)

    // An nth root always uses the tall sign so the `index` can sit one line above,
    // to the left of the corner.
    def root(base: Cell, index: Cell): Cell =
      val radicand = tallRadical(base)

      // The radicand's rows come from traversing its own lines, so they need no proof; the
      // (shorter) index cell is a checked lookup with a blank fallback — exactly `slice`.
      val lines = radicand.lines.indexed.map: (line, ordinal) =>
        Writing(t"${slice(index, ordinal.n0)}${line.text}")

      Cell(lines, index.width + radicand.width, radicand.baseline)

    def fence(open: Char, close: Char, inner: Cell): Cell =
      val left = bracket(open, inner.height, inner.baseline, opening = true)
      val right = bracket(close, inner.height, inner.baseline, opening = false)
      beside(List(left, inner, right))

    // A stretchy bracket sized to `height`, built from box-drawing pieces; for a
    // single-line cell it degrades to the literal bracket character.
    def bracket(char: Char, height: Int, baseline: Int, opening: Boolean): Cell =
      if height <= 1 then line(char.show) else
        val glyphs =
          Sequence.from:
            (0 until height).map: row =>
              Writing(bracketGlyph(char, row, height, baseline, opening).show)

        Cell(glyphs, 1, baseline)

    private def bracketGlyph(char: Char, row: Int, height: Int, axis: Int, opening: Boolean): Char =
      val first = row == 0
      val last = row == height - 1

      char match
        case '(' | ')' =>
          if opening then (if first then '⎛' else if last then '⎝' else '⎜')
          else (if first then '⎞' else if last then '⎠' else '⎟')

        case '[' | ']' =>
          if opening then (if first then '⎡' else if last then '⎣' else '⎢')
          else (if first then '⎤' else if last then '⎦' else '⎥')

        case '{' | '}' =>
          if opening then
            if first then '⎧' else if last then '⎩' else if row == axis then '⎨' else '⎪'
          else
            if first then '⎫' else if last then '⎭' else if row == axis then '⎬' else '⎪'

        case _ =>
          '│'

    private def opening(char: Char): Boolean =
      char == '(' || char == '[' || char == '{' || char == '|' || char == '‖'

    // A stretchy integral sign: each stroke is a rounded `╭` hook at the top and `╯`
    // at the foot, joined by a `│` stem; double and triple integrals repeat the
    // stroke, and contour integrals overlay a `○` on the axis row. A blank column of
    // padding follows, separating the sign from its operand.
    def integral(char: Char, height: Int, axis: Int): Cell =
      if height <= 1 then line(char.show) else
        val (strokes, circle) = integralShape(char)

        val lines =
          Sequence.from:
            (0 until height).map: row =>
              val glyphs = (0 until strokes).map: column =>
                if row == 0 then t"╭"
                else if row == height - 1 then t"╯"
                else if circle && column == 0 && row == axis then t"○"
                else Stem.show

              Writing(t"${glyphs.join} ")

        Cell(lines, strokes + 1, axis)

    private def integralShape(char: Char): (Int, Boolean) = char match
      case '∮' => (1, true)
      case '∬' => (2, false)
      case '∭' => (3, false)
      case '∯' => (2, true)
      case '∰' => (3, true)
      case _   => (1, false)

    // A big operator (n-ary sum `∑` or product `∏`), rendered as multi-line line-art
    // and sized to its operand (with a minimum so the shape is always legible). The
    // baseline sits at the vertical middle, where the operand aligns.
    def bigOperator(char: Char, height: Int): Cell =
      if char == '∏' then product(height) else summation(height)

    // Pi adapts to any height; sigma only to even heights (at least 4), because its
    // `╲`/`╱` diagonals must pair up symmetrically, so an odd operand rounds up.
    private def bigOperatorHeight(char: Char, content: Int): Int =
      if char == '∏' then content else max(4, content + content%2)

    // `∑`: inward-pointing `▁`/`▔` bars top and bottom, joined by `╲` then `╱`
    // diagonals that step in towards a central vertex.
    private def summation(height: Int): Cell =
      val depth = max((height - 3)/2, 0)
      val width = depth + 3

      val lines =
        Sequence.from:
          (0 until height).map: row =>
            if row == 0 then repeat('▁', width)
            else if row == height - 1 then repeat('▔', width) else
              val fromTop = row - 1
              val fromBottom = height - 2 - row
              val column = min(fromTop, fromBottom)
              val glyph = if fromTop <= fromBottom then Tick else '╱'
              Writing(t"${spaces(column).text}$glyph${spaces(width - column - 1).text}")

      Cell(lines, width, height/2)

    // `∏`: a `┬──┬` lintel over two `│` legs.
    private def product(height: Int): Cell =
      val width = 4

      val lines =
        Sequence.from:
          (0 until height).map: row =>
            if row == 0 then Writing(t"┬${repeat(Bar, width - 2).text}┬")
            else Writing(t"$Stem${spaces(width - 2).text}$Stem")

      Cell(lines, width, height/2)

    private def stretchyChar(node: Mathml): Optional[Char] = node match
      case Mo(value, _) if value.length == 1 && stretchable(value.s.charAt(0)) => value.s.charAt(0)
      case _                                                                   => Unset

    private def stretchable(char: Char): Boolean =
      brackets.contains(char) || integrals.contains(char) || bigOperators.contains(char)

    private def operator(value: Text): Text =
      if value.length == 1 && value.s.charAt(0) >= '⁡' && value.s.charAt(0) <= '⁤' then t""
      else if binaryOperators.contains(value) then t" $value "
      else value

    // Lay out a sequence of nodes as a row, stretching any bracket or integral
    // operators to the height of the surrounding (non-stretchy) content.
    def row(nodes: List[Mathml]): Cell =
      val content = nodes.filter { node => stretchyChar(node).absent }.map(of)
      val ascent = content.map(_.baseline).maximize(identity(_)).or(0)

      val descent =
        content.map { cell => cell.height - 1 - cell.baseline }.maximize(identity(_)).or(0)

      val height = ascent + descent + 1

      val cells: List[Cell] = nodes.map: node =>
        stretchyChar(node).lay(of(node)): char =>
          // A one-line subject keeps the plain glyph; taller subjects grow the art.
          if bigOperators.contains(char) then
            if height <= 1 then line(char.show)
            else bigOperator(char, bigOperatorHeight(char, height))
          else if integrals.contains(char) then
            integral(char, height, ascent)
          else
            bracket(char, height, ascent, opening(char))

      beside(cells)

    def of(node: Mathml): Cell = node match
      case Mo(value, _)              => line(operator(value))
      case Mfrac(numer, denom, _)    => fraction(of(numer), of(denom))
      case Msqrt(contents, _)        => radical(row(contents))
      case Mroot(base, index, _)     => root(of(base), of(index))
      case Msup(base, script, _)     => superscripted(base, script)
      case Msub(base, script, _)     => subscripted(base, script)
      case Msubsup(base, sb, sp, _)  => subsupscripted(base, sb, sp)
      case Mover(base, over, _)      => overscript(of(base), of(over))
      case Munder(base, under, _)    => underscript(of(base), of(under))
      case Munderover(base, u, o, _) => underover(of(base), of(u), of(o))
      case Mrow(contents, _)         => row(contents)
      case Mfenced(contents, _)      => fence('(', ')', row(contents))
      case Mspace(_)                 => blank(1, 1, 0)

      case Mphantom(contents, _) =>
        val inner = row(contents)
        blank(inner.width, inner.height, inner.baseline)

      case node =>
        node.text.lay(row(node.contents))(line)

  case class Cell(lines: Sequence[Writing], width: Int, baseline: Int):
    import anticipation.*
    import gossamer.*
    import hieroglyph.Measurable
    import hieroglyph.textMetrics.wideCharacterWidthMetric
    import denominative.*
    import rudiments.*
    import spectacular.*
    import symbolism.*
    import vacuous.*
    import Mathml.*

    def height: Int = lines.size
    def render: Text = lines.map(_.text).join(t"\n")

    // Pad each line on the left and right with spaces, widening the block.
    def hpad(left: Int, right: Int): Cell =
      val leftText = Cell.spaces(left).text
      val rightText = Cell.spaces(right).text

      def pad(line: Writing): Writing = Writing(t"$leftText${line.text}$rightText")
      val padded = lines.map(pad)

      Cell(padded, width + Cell.max(left, 0) + Cell.max(right, 0), baseline)

    // Centre the block within `target` columns (never narrows).
    def hcenter(target: Int): Cell =
      if target <= width then this else
        val total = target - width
        hpad(total/2, total - total/2)

    // Add blank rows above/below so the block has the given ascent and descent.
    def framed(ascent: Int, descent: Int): Cell =
      val above = Cell.max(ascent - baseline, 0)
      val below = Cell.max(descent - (height - 1 - baseline), 0)
      val blank = Cell.spaces(width)

      Cell
        ( Iterator.fill(above)(blank).to[Sequence] + lines
          + Iterator.fill(below)(blank).to[Sequence],
          width,
          ascent )

trait Mathml:
  def label: Text
  def attributes: List[(Text, Text)]
  def contents: List[Mathml]
  def text: Optional[Text]
