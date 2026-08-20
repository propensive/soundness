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
package facsimile

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import quantitative.*
import rudiments.*
import vacuous.*

object Page:
  enum Rotation:
    case None, Quarter, Half, ThreeQuarters

  // The inheritable page-tree attributes (ISO 32000-2 §7.7.3.4): a child's own entry always
  // wins over anything accumulated from its ancestors.
  private[facsimile] case class Inherited
    ( resources: Optional[Cos] = Unset,
      mediaBox:  Optional[Cos] = Unset,
      cropBox:   Optional[Cos] = Unset,
      rotate:    Optional[Cos] = Unset ):

    def update(node: Map[Text, Cos]): Inherited =
      Inherited
        ( node(t"Resources").or(resources),
          node(t"MediaBox").or(mediaBox),
          node(t"CropBox").or(cropBox),
          node(t"Rotate").or(rotate) )

// A leaf of the page tree, with its inherited attributes applied. A `Page` resolves lazily
// through the document, so it captures the `Pdf` and cannot outlive the `open` scope;
// everything extracted from it — boxes, rotation, text — is pure and portable.
class Page private[facsimile]
  ( private[facsimile] val pdf: Pdf,
    val index: Ordinal,
    private[facsimile] val number: Optional[Int],
    private[facsimile] val entries: Map[Text, Cos],
    private[facsimile] val inherited: Page.Inherited ):

  def dictionary: Map[Text, Cos] = entries

  // 1 default user-space unit is `userUnit`/72 inch; `/UserUnit` is not inheritable.
  def userUnit(using Tactic[Pdf.Error]): Double =
    entries(t"UserUnit").let(pdf.resolved(_).double).or(1.0)

  def mediaBox(using Tactic[Pdf.Error]): Pdf.Rect =
    box(entries(t"MediaBox").or(inherited.mediaBox))
    . or(abort(Pdf.Error(Pdf.Error.Reason.MissingEntry(t"MediaBox"))))

  def cropBox(using Tactic[Pdf.Error]): Pdf.Rect =
    box(entries(t"CropBox").or(inherited.cropBox)).or(mediaBox)

  // The bleed, trim and art boxes are not inheritable and default to the crop box.
  def bleedBox(using Tactic[Pdf.Error]): Pdf.Rect = box(entries(t"BleedBox")).or(cropBox)
  def trimBox(using Tactic[Pdf.Error]): Pdf.Rect = box(entries(t"TrimBox")).or(cropBox)
  def artBox(using Tactic[Pdf.Error]): Pdf.Rect = box(entries(t"ArtBox")).or(cropBox)

  def rotation(using Tactic[Pdf.Error]): Page.Rotation =
    val degrees = entries(t"Rotate").or(inherited.rotate).let(pdf.resolved(_).long).or(0L)

    ((degrees%360 + 360)%360) match
      case 90L  => Page.Rotation.Quarter
      case 180L => Page.Rotation.Half
      case 270L => Page.Rotation.ThreeQuarters
      case _    => Page.Rotation.None

  // The page's displayed size: the crop box, with its axes exchanged when the page is
  // rotated a quarter-turn either way.
  def width(using Tactic[Pdf.Error]): Quantity[Points[1]] = rotation match
    case Page.Rotation.Quarter | Page.Rotation.ThreeQuarters => cropBox.height
    case _                                                   => cropBox.width

  def height(using Tactic[Pdf.Error]): Quantity[Points[1]] = rotation match
    case Page.Rotation.Quarter | Page.Rotation.ThreeQuarters => cropBox.width
    case _                                                   => cropBox.height

  // The page's fonts, keyed by resource name — the names `Tf` refers to.
  def fonts(using Tactic[Pdf.Error]): Map[Text, Pdf.Font] =
    val resources = pdf.resolved(entries(t"Resources").or(inherited.resources).or(Cos.Nil))
      . dictionary.or(Map[Text, Cos]())

    pdf.resolved(resources(t"Font").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())
    . to[List].bind: (name, value) =>
        Pdf.Font.read(pdf.resolved(value))(using pdf).lay(List[(Text, Pdf.Font)]()): font =>
          List(name -> font)

    . to[Map]

  // The page's content: its `/Contents` streams decoded and concatenated, which the
  // specification requires to be treated as a single stream, with whitespace between.
  def content(using Tactic[Pdf.Error]): Data =
    val streams = pdf.resolved(entries(t"Contents").or(Cos.Nil)) match
      case body: Cos.Body =>
        List(body)

      case Cos.Sequence(elements) =>
        elements.flatMap: element =>
          pdf.resolved(element) match
            case body: Cos.Body => List(body)
            case _              => List()

      case _ =>
        List()

    streams.map(pdf.payload(_)) match
      case List()       => Array.empty[Byte]
      case List(single) => single
      case many         => Array.unsafeFrozen(many.stdlib.map(_.readable).reduce(_ ++ scala.IArray(0x0a.toByte) ++ _).toArray)

  def operators(using Tactic[Pdf.Error]): List[Pdf.Operator] =
    ContentTokens.read(content).map(Pdf.Operator.read(_))

  // Every show-text operation, decoded and positioned in points.
  def runs(using Tactic[Pdf.Error]): List[TextRun] =
    TextExtractor.extract(operators, fonts, userUnit)(0)

  // The page's plain text, in content order: spaces bridge gaps on a baseline, newlines mark
  // baseline movement — the fidelity of `pdftotext -raw`; layout-aware ordering can come
  // later as an additive option.
  def text(using Tactic[Pdf.Error]): Text =
    TextExtractor.extract(operators, fonts, userUnit)(1)

  def annotations(using Tactic[Pdf.Error]): List[Annotation] =
    val pages = pdf.pageNumbers
    val named = pdf.rawDestinations
    val scale = userUnit

    pdf.resolved(entries(t"Annots").or(Cos.Nil)).elements.lay(List()): items =>
      items.flatMap: item =>
        Annotation.read(item, pages, named(_), scale)(using pdf).lay(List())(List(_))

  private def box(value: Optional[Cos])(using Tactic[Pdf.Error]): Optional[Pdf.Rect] =
    value.let(Pdf.Rect.read(_, userUnit)(using pdf))
