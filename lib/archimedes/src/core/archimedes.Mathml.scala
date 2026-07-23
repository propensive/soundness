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

import anticipation.*
import gossamer.*
import honeycomb.Html
import prepositional.*
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

  object Mrow:
    def apply(children: Mathml*): Mrow = Mrow(children.to(List))

  case class Mrow(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mrow"

  case class Mfrac(numerator: Mathml, denominator: Mathml, attributes: List[(Text, Text)] = Nil)
  extends Layout:
    def label: Text = t"mfrac"
    def contents: List[Mathml] = List(numerator, denominator)

  object Msqrt:
    def apply(children: Mathml*): Msqrt = Msqrt(children.to(List))

  case class Msqrt(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"msqrt"

  case class Mroot(base: Mathml, index: Mathml, attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mroot"
    def contents: List[Mathml] = List(base, index)

  object Mstyle:
    def apply(children: Mathml*): Mstyle = Mstyle(children.to(List))

  case class Mstyle(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mstyle"

  object Merror:
    def apply(children: Mathml*): Merror = Merror(children.to(List))

  case class Merror(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"merror"

  object Mpadded:
    def apply(children: Mathml*): Mpadded = Mpadded(children.to(List))

  case class Mpadded(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mpadded"

  object Mphantom:
    def apply(children: Mathml*): Mphantom = Mphantom(children.to(List))

  case class Mphantom(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"mphantom"

  object Menclose:
    def apply(children: Mathml*): Menclose = Menclose(children.to(List))

  case class Menclose(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
    def label: Text = t"menclose"

  object Mfenced:
    def apply(children: Mathml*): Mfenced = Mfenced(children.to(List))

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

  object Mmultiscripts:
    def apply(children: Mathml*): Mmultiscripts = Mmultiscripts(children.to(List))

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

  object Mtable:
    def apply(rows: Mathml*): Mtable = Mtable(rows.to(List))

  case class Mtable(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = t"mtable"

  object Mtr:
    def apply(cells: Mathml*): Mtr = Mtr(cells.to(List))

  case class Mtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
    def label: Text = t"mtr"

  object Mlabeledtr:
    def apply(cells: Mathml*): Mlabeledtr = Mlabeledtr(cells.to(List))

  case class Mlabeledtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Tabular:
    def label: Text = t"mlabeledtr"

  object Mtd:
    def apply(children: Mathml*): Mtd = Mtd(children.to(List))

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

  object Mstack:
    def apply(children: Mathml*): Mstack = Mstack(children.to(List))

  case class Mstack(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"mstack"

  object Mlongdiv:
    def apply(children: Mathml*): Mlongdiv = Mlongdiv(children.to(List))

  case class Mlongdiv(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"mlongdiv"

  object Msgroup:
    def apply(children: Mathml*): Msgroup = Msgroup(children.to(List))

  case class Msgroup(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"msgroup"

  object Msrow:
    def apply(children: Mathml*): Msrow = Msrow(children.to(List))

  case class Msrow(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"msrow"

  object Mscarries:
    def apply(children: Mathml*): Mscarries = Mscarries(children.to(List))

  case class Mscarries(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Elementary:
    def label: Text = t"mscarries"

  object Mscarry:
    def apply(children: Mathml*): Mscarry = Mscarry(children.to(List))

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

  object Maction:
    def apply(children: Mathml*): Maction = Maction(children.to(List))

  case class Maction(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = t"maction"
    def text: Optional[Text] = Unset

  object Semantics:
    def apply(children: Mathml*): Semantics = Semantics(children.to(List))

  case class Semantics(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = t"semantics"
    def text: Optional[Text] = Unset

  case class Annotation(value: Text, attributes: List[(Text, Text)] = Nil) extends Semantic:
    def label: Text = t"annotation"
    def contents: List[Mathml] = Nil
    def text: Optional[Text] = value

  object AnnotationXml:
    def apply(children: Mathml*): AnnotationXml = AnnotationXml(children.to(List))

  case class AnnotationXml(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
  extends Semantic:
    def label: Text = t"annotation-xml"
    def text: Optional[Text] = Unset

trait Mathml:
  def label: Text
  def attributes: List[(Text, Text)]
  def contents: List[Mathml]
  def text: Optional[Text]

  def htmlAttributes: List[(Text, Optional[Text])] =
    List.of[(Text, Optional[Text])]
     (attributes.stdlib.map { case (key, value) => (key, value: Optional[Text]) })

  def xml: Xml =
    val children: List[Xml] = text.lay(List.of(contents.stdlib.map(_.xml))): value =>
      List(TextNode(value))

    Element(label, Attributes(attributes*), children.stdlib.nodes)

  def html: Html of "#foreign" =
    val children: List[Html of "#foreign"] =
      text.lay(List.of(contents.stdlib.map(_.html))): value =>
        List(honeycomb.Html.string2(value.s))

    honeycomb.Element.foreign(label, honeycomb.Attributes(htmlAttributes*), children*)
