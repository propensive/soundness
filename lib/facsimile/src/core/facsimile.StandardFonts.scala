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
package facsimile

import proscenium.compat.*

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// The standard fourteen fonts (ISO 32000-2 §9.6.2.2), which a PDF may use without embedding
// or even declaring widths. The AFM metrics for the printable ASCII range — which covers the
// overwhelming majority of standard-font text — are tabulated here for extraction; other
// codes fall back to a nominal width. Obliqued variants share their upright metrics.
private[facsimile] object StandardFonts:
  import PdfFont.Standard

  def recognize(baseFont: Text): Optional[Standard] =
    // Subsetted fonts carry a six-letter prefix, e.g. `ABCDEF+Helvetica`.
    val name =
      if baseFont.s.length > 7 && baseFont.s.charAt(6) == '+' then baseFont.s.substring(7).nn
      else baseFont.s

    name match
      case "Helvetica" | "Arial" | "ArialMT"        => Standard.Helvetica
      case "Helvetica-Bold" | "Arial-BoldMT"        => Standard.HelveticaBold
      case "Helvetica-Oblique" | "Arial-ItalicMT"   => Standard.HelveticaOblique
      case "Helvetica-BoldOblique"                  => Standard.HelveticaBoldOblique
      case "Arial-BoldItalicMT"                     => Standard.HelveticaBoldOblique
      case "Times-Roman" | "TimesNewRomanPSMT"      => Standard.TimesRoman
      case "Times-Bold" | "TimesNewRomanPS-BoldMT"  => Standard.TimesBold
      case "Times-Italic"                           => Standard.TimesItalic
      case "TimesNewRomanPS-ItalicMT"               => Standard.TimesItalic
      case "Times-BoldItalic"                       => Standard.TimesBoldItalic
      case "TimesNewRomanPS-BoldItalicMT"           => Standard.TimesBoldItalic
      case "Courier" | "CourierNew" | "CourierNewPSMT" => Standard.Courier
      case "Courier-Bold" | "CourierNewPS-BoldMT"   => Standard.CourierBold
      case "Courier-Oblique" | "CourierNewPS-ItalicMT" => Standard.CourierOblique
      case "Courier-BoldOblique"                    => Standard.CourierBoldOblique
      case "CourierNewPS-BoldItalicMT"              => Standard.CourierBoldOblique
      case "Symbol"                                 => Standard.Symbol
      case "ZapfDingbats"                           => Standard.ZapfDingbats
      case _                                        => Unset

  // The width of a code in thousandths of an em.
  def width(standard: Standard, code: Int): Double = standard match
    case Standard.Courier | Standard.CourierBold => 600
    case Standard.CourierOblique                 => 600
    case Standard.CourierBoldOblique             => 600
    case Standard.Symbol | Standard.ZapfDingbats => 500

    case Standard.Helvetica | Standard.HelveticaOblique =>
      lookup(helvetica, code)

    case Standard.HelveticaBold | Standard.HelveticaBoldOblique =>
      lookup(helveticaBold, code)

    case Standard.TimesRoman      => lookup(times, code)
    case Standard.TimesBold       => lookup(timesBold, code)
    case Standard.TimesItalic     => lookup(timesItalic, code)
    case Standard.TimesBoldItalic => lookup(timesBoldItalic, code)

  private def lookup(table: IArray[Short], code: Int): Double =
    if code >= 32 && code < 32 + table.length then table(code - 32).toDouble else 500

  // AFM widths for codes 32–126, in ASCII order.
  private val helvetica: IArray[Short] = IArray[Short]
    ( 278, 278, 355, 556, 556, 889, 667, 191, 333, 333, 389, 584, 278, 333, 278, 278,
      556, 556, 556, 556, 556, 556, 556, 556, 556, 556, 278, 278, 584, 584, 584, 556,
      1015, 667, 667, 722, 722, 667, 611, 778, 722, 278, 500, 667, 556, 833, 722, 778,
      667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556,
      333, 556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833, 556, 556,
      556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, 334, 260, 334, 584 )

  private val helveticaBold: IArray[Short] = IArray[Short]
    ( 278, 333, 474, 556, 556, 889, 722, 238, 333, 333, 389, 584, 278, 333, 278, 278,
      556, 556, 556, 556, 556, 556, 556, 556, 556, 556, 333, 333, 584, 584, 584, 611,
      975, 722, 722, 722, 722, 667, 611, 778, 722, 278, 556, 722, 611, 833, 722, 778,
      667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584, 556,
      333, 556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889, 611, 611,
      611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500, 389, 280, 389, 584 )

  private val times: IArray[Short] = IArray[Short]
    ( 250, 333, 408, 500, 500, 833, 778, 180, 333, 333, 500, 564, 250, 333, 250, 278,
      500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 278, 278, 564, 564, 564, 444,
      921, 722, 667, 667, 722, 611, 556, 722, 722, 333, 389, 722, 611, 889, 722, 722,
      556, 722, 667, 556, 611, 722, 722, 944, 722, 722, 611, 333, 278, 333, 469, 500,
      333, 444, 500, 444, 500, 444, 333, 500, 500, 278, 278, 500, 278, 778, 500, 500,
      500, 500, 333, 389, 278, 500, 500, 722, 500, 500, 444, 480, 200, 480, 541 )

  private val timesBold: IArray[Short] = IArray[Short]
    ( 250, 333, 555, 500, 500, 1000, 833, 278, 333, 333, 500, 570, 250, 333, 250, 278,
      500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 333, 333, 570, 570, 570, 500,
      930, 722, 667, 722, 722, 667, 611, 778, 778, 389, 500, 778, 667, 944, 722, 778,
      611, 778, 722, 556, 667, 722, 722, 1000, 722, 722, 667, 333, 278, 333, 581, 500,
      333, 500, 556, 444, 556, 444, 333, 500, 556, 278, 333, 556, 278, 833, 556, 500,
      556, 556, 444, 389, 333, 556, 500, 722, 500, 500, 444, 394, 220, 394, 520 )

  private val timesItalic: IArray[Short] = IArray[Short]
    ( 250, 333, 420, 500, 500, 833, 778, 214, 333, 333, 500, 675, 250, 333, 250, 278,
      500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 333, 333, 675, 675, 675, 500,
      920, 611, 611, 667, 722, 611, 611, 722, 722, 333, 444, 667, 556, 833, 667, 722,
      611, 722, 611, 500, 556, 722, 611, 833, 611, 556, 556, 389, 278, 389, 422, 500,
      333, 500, 500, 444, 500, 444, 278, 500, 500, 278, 278, 444, 278, 722, 500, 500,
      500, 500, 389, 389, 278, 500, 444, 667, 444, 444, 389, 400, 275, 400, 541 )

  private val timesBoldItalic: IArray[Short] = IArray[Short]
    ( 250, 389, 555, 500, 500, 833, 778, 278, 333, 333, 500, 570, 250, 333, 250, 278,
      500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 333, 333, 570, 570, 570, 500,
      832, 667, 667, 667, 722, 667, 667, 722, 778, 389, 500, 667, 611, 889, 722, 722,
      611, 722, 667, 556, 611, 722, 667, 889, 667, 611, 611, 333, 278, 333, 570, 500,
      333, 500, 500, 444, 500, 444, 333, 500, 556, 278, 278, 500, 278, 778, 556, 500,
      500, 500, 389, 389, 278, 556, 444, 667, 500, 444, 389, 348, 220, 348, 570 )
