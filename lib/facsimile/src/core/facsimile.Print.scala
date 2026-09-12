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
import gossamer.*
import phoenicia.*
import prepositional.*
import symbolism.*
import vacuous.*

object Print:
  // The instance the standard fonts share; a named class gives it a stable type.
  class Provision[family <: Label](name: Text, val coverage: Coverage) extends Typesettable:
    type Self = Typeface of family
    type Form = Print
    val sources: List[Typesettable.Source] = List(Typesettable.Source.Local(name))

  val Helvetica: Typeface of "Helvetica" = Typeface["Helvetica"]
  val Times: Typeface of "Times" = Typeface["Times"]
  val Courier: Typeface of "Courier" = Typeface["Courier"]
  val Symbol: Typeface of "Symbol" = Typeface["Symbol"]
  val ZapfDingbats: Typeface of "ZapfDingbats" = Typeface["ZapfDingbats"]

  // Regular and bold, each upright and slanted: the four faces of a standard text font. The
  // features are unknown: these are Type 1 fonts, and a PDF applies no layout features anyway.
  private val textFaces: Coverage =
    Coverage.Entries
      ( List
          ( Coverage.Entry(Weight.Regular.range, true, true, Stretch.Normal.range, Nil, Unset),
            Coverage.Entry(Weight.Bold.range, true, true, Stretch.Normal.range, Nil, Unset) ) )

  private val singleFace: Coverage =
    Coverage.Entries
      ( List(Coverage.Entry(Weight.Regular.range, true, false, Stretch.Normal.range, Nil, Unset)) )

  // The standard fourteen fonts (ISO 32000-2 §9.6.2.2), which every PDF reader has, are provided
  // in print without anything being embedded. They live here, in the medium's companion, which
  // is in the implicit scope of `(Typeface of "Helvetica") is Typesettable in Print`.
  given helvetica: (Typeface of "Helvetica") is Typesettable in Print =
    Provision(t"Helvetica", textFaces)

  given times: (Typeface of "Times") is Typesettable in Print = Provision(t"Times", textFaces)
  given courier: (Typeface of "Courier") is Typesettable in Print = Provision(t"Courier", textFaces)
  given symbol: (Typeface of "Symbol") is Typesettable in Print = Provision(t"Symbol", singleFace)

  given zapfDingbats: (Typeface of "ZapfDingbats") is Typesettable in Print =
    Provision(t"ZapfDingbats", singleFace)

  // The standard font a face selects — bold from weight 600, slanted for any slant — or nothing
  // for a typeface that is not one of the fourteen.
  def standard(font: Font): Optional[Pdf.Font.Standard] =
    import Pdf.Font.Standard
    val bold = font.face.weight.value >= 600
    val slanted = !font.face.slant.upright

    font.typeface.name.s match
      case "Helvetica" =>
        if bold then (if slanted then Standard.HelveticaBoldOblique else Standard.HelveticaBold)
        else if slanted then Standard.HelveticaOblique else Standard.Helvetica

      case "Times" =>
        if bold then (if slanted then Standard.TimesBoldItalic else Standard.TimesBold)
        else if slanted then Standard.TimesItalic else Standard.TimesRoman

      case "Courier" =>
        if bold then (if slanted then Standard.CourierBoldOblique else Standard.CourierBold)
        else if slanted then Standard.CourierOblique else Standard.Courier

      case "Symbol"       => Standard.Symbol
      case "ZapfDingbats" => Standard.ZapfDingbats
      case _              => Unset

  // A standard font's PostScript name, as a `/BaseFont` entry gives it.
  def baseFont(standard: Pdf.Font.Standard): Text = standard match
    case Pdf.Font.Standard.Helvetica             => t"Helvetica"
    case Pdf.Font.Standard.HelveticaBold         => t"Helvetica-Bold"
    case Pdf.Font.Standard.HelveticaOblique      => t"Helvetica-Oblique"
    case Pdf.Font.Standard.HelveticaBoldOblique  => t"Helvetica-BoldOblique"
    case Pdf.Font.Standard.TimesRoman            => t"Times-Roman"
    case Pdf.Font.Standard.TimesBold             => t"Times-Bold"
    case Pdf.Font.Standard.TimesItalic           => t"Times-Italic"
    case Pdf.Font.Standard.TimesBoldItalic       => t"Times-BoldItalic"
    case Pdf.Font.Standard.Courier               => t"Courier"
    case Pdf.Font.Standard.CourierBold           => t"Courier-Bold"
    case Pdf.Font.Standard.CourierOblique        => t"Courier-Oblique"
    case Pdf.Font.Standard.CourierBoldOblique    => t"Courier-BoldOblique"
    case Pdf.Font.Standard.Symbol                => t"Symbol"
    case Pdf.Font.Standard.ZapfDingbats          => t"ZapfDingbats"

  // The file of an embedded provision to embed for a font: the first whose coverage admits the
  // face, else the first file, else nothing for a provision without files.
  def embedded(font: Font): Optional[Sfnt] =
    def files(sources: List[Typesettable.Source]): List[Sfnt] = sources match
      case Typesettable.Source.Embedded(sfnt) :: tail => List(sfnt) + files(tail)
      case _ :: tail                                  => files(tail)
      case _                                          => Nil

    def admitting(sfnts: List[Sfnt]): Optional[Sfnt] = sfnts match
      case head :: tail =>
        if Coverage.of(head).complaint(font.face).absent then head else admitting(tail)

      case _ =>
        Unset

    val all = files(font.provision.sources)

    admitting(all).or:
      all match
        case head :: _ => head
        case _         => Unset

  // A font for print, where the expected type does not name the medium itself. Inline, so that
  // the face's request reaches the compile-time check.
  inline def font[family <: Label](inline face: Face of family)
    ( using provision: Typeface of family is Typesettable in (? >: Print),
            tactic:    Tactic[Font.Error] )
  :   Font in Print =

    Font.of[Print](face)

// Print as a medium: text set by a PDF reader, where a typeface is available if its file is
// embedded in the document or it is one of the standard fourteen fonts every reader has.
trait Print extends Medium
