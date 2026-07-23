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
import contingency.*
import gossamer.*
import iridescence.*
import rudiments.*
import vacuous.*

object PdfOperator:
  enum LineCap:
    case Butt, Round, Square

  enum LineJoin:
    case Miter, Round, Bevel

  enum FillRule:
    case NonZero, EvenOdd

  enum TextRenderMode:
    case Fill, Stroke, FillStroke, Invisible, FillClip, StrokeClip, FillStrokeClip, Clip

  // Interprets one lexed instruction as a typed operator. Unknown operators become
  // `Unrecognized` — required inside `BX`/`EX` compatibility sections, and kind to the
  // future — while known operators with malformed operands are errors.
  private[facsimile] def read(instruction: ContentTokens.Instruction)
  :   PdfOperator raises PdfError =

    val operands = instruction.operands
    val operator = instruction.operator

    def malformed: Nothing = abort(PdfError(PdfError.Reason.MalformedOperator(operator)))

    def numbers(count: Int): List[Double] =
      val values = operands.bind(_.double.lay(List())(List(_)))
      if values.stdlib.length != count || operands.stdlib.length != count then malformed else values

    def name(index: Int): Text =
      if index < operands.stdlib.length then operands.stdlib(index).name.or(malformed) else malformed

    def chars(index: Int): Data =
      if index < operands.stdlib.length then operands.stdlib(index).chars.or(malformed) else malformed

    def matrix: PdfMatrix = numbers(6) match
      case List(a, b, c, d, e, f) => PdfMatrix(a, b, c, d, e, f)
      case _                      => malformed

    def int(limit: Int): Int =
      val value = numbers(1).stdlib(0).toInt
      if value < 0 || value >= limit then malformed else value

    def pair(index: Int): Optional[Cos] =
      if operands.stdlib.length > index then operands.stdlib(index) else Unset

    operator.s match
      // Graphics state (ISO 32000-2 §8.4.4)
      case "q"  => Save
      case "Q"  => Restore
      case "cm" => Concat(matrix)
      case "w"  => SetLineWidth(numbers(1).stdlib(0))
      case "J"  => SetLineCap(LineCap.fromOrdinal(int(LineCap.values.length)))
      case "j"  => SetLineJoin(LineJoin.fromOrdinal(int(LineJoin.values.length)))
      case "M"  => SetMiterLimit(numbers(1).stdlib(0))
      case "ri" => SetIntent(name(0))
      case "i"  => SetFlatness(numbers(1).stdlib(0))
      case "gs" => SetParameters(name(0))

      case "d" => operands match
        case List(Cos.Sequence(elements), phase) =>
          SetDashPattern(elements.map(_.double.or(malformed)), phase.double.or(malformed))

        case _ =>
          malformed

      // Path construction and painting (§8.5)
      case "m" => numbers(2) match
        case List(x, y) => Move(x, y)
        case _          => malformed

      case "l" => numbers(2) match
        case List(x, y) => Line(x, y)
        case _          => malformed

      case "c" => numbers(6) match
        case List(x1, y1, x2, y2, x3, y3) => Cubic(x1, y1, x2, y2, x3, y3)
        case _                            => malformed

      case "v" => numbers(4) match
        case List(x2, y2, x3, y3) => CubicStart(x2, y2, x3, y3)
        case _                    => malformed

      case "y" => numbers(4) match
        case List(x1, y1, x3, y3) => CubicEnd(x1, y1, x3, y3)
        case _                    => malformed

      case "re" => numbers(4) match
        case List(x, y, width, height) => Rectangle(x, y, width, height)
        case _                         => malformed

      case "h"  => Close
      case "S"  => Stroke
      case "s"  => CloseStroke
      case "f"  => Fill(FillRule.NonZero)
      case "F"  => Fill(FillRule.NonZero)
      case "f*" => Fill(FillRule.EvenOdd)
      case "B"  => FillStroke(FillRule.NonZero)
      case "B*" => FillStroke(FillRule.EvenOdd)
      case "b"  => CloseFillStroke(FillRule.NonZero)
      case "b*" => CloseFillStroke(FillRule.EvenOdd)
      case "n"  => EndPath
      case "W"  => Clip(FillRule.NonZero)
      case "W*" => Clip(FillRule.EvenOdd)

      // Text (§9.4)
      case "BT" => BeginText
      case "ET" => EndText
      case "Td" => numbers(2) match
        case List(dx, dy) => Offset(dx, dy)
        case _            => malformed

      case "TD" => numbers(2) match
        case List(dx, dy) => OffsetLeading(dx, dy)
        case _            => malformed
      case "Tm" => SetTextMatrix(matrix)
      case "T*" => NextLine
      case "Tc" => SetCharSpacing(numbers(1).stdlib(0))
      case "Tw" => SetWordSpacing(numbers(1).stdlib(0))
      case "Tz" => SetScaling(numbers(1).stdlib(0))
      case "TL" => SetLeading(numbers(1).stdlib(0))
      case "Tr" => SetRenderMode(TextRenderMode.fromOrdinal(int(TextRenderMode.values.length)))
      case "Ts" => SetRise(numbers(1).stdlib(0))
      case "Tj" => ShowText(chars(0))
      case "'"  => NextLineShow(chars(0))

      case "Tf" => operands match
        case List(Cos.Name(font), size) => SetFont(font, size.double.or(malformed))
        case _                          => malformed

      case "TJ" => operands match
        case List(Cos.Sequence(elements)) =>
          ShowTexts:
            elements.map: element =>
              element.chars.or(element.double.or(malformed))

        case _ =>
          malformed

      case "\"" => operands match
        case List(word, char, Cos.Chars(bytes)) =>
          NextLineShowSpaced(word.double.or(malformed), char.double.or(malformed), bytes)

        case _ =>
          malformed

      // Colour (§8.6.8)
      case "CS" => StrokeSpace(name(0))
      case "cs" => FillSpace(name(0))
      case "G"  => StrokeGray(numbers(1).stdlib(0))
      case "g"  => FillGray(numbers(1).stdlib(0))

      case "RG" => numbers(3) match
        case List(red, green, blue) => StrokeRgb(Srgb(red, green, blue))
        case _                      => malformed

      case "rg" => numbers(3) match
        case List(red, green, blue) => FillRgb(Srgb(red, green, blue))
        case _                      => malformed

      case "K" => numbers(4) match
        case List(cyan, magenta, yellow, key) => StrokeCmyk(Cmyk(cyan, magenta, yellow, key))
        case _                                => malformed

      case "k" => numbers(4) match
        case List(cyan, magenta, yellow, key) => FillCmyk(Cmyk(cyan, magenta, yellow, key))
        case _                                => malformed

      case "SC" | "SCN" =>
        val (values, pattern) = components(operands, malformed)
        StrokeColor(values, pattern)

      case "sc" | "scn" =>
        val (values, pattern) = components(operands, malformed)
        FillColor(values, pattern)

      // XObjects, inline images and shading (§8.8–8.10)
      case "Do" => Draw(name(0))
      case "sh" => Shade(name(0))

      case "BI" => operands match
        case List(Cos.Dictionary(entries), Cos.Chars(data)) => InlineImage(entries, data)
        case _                                               => malformed

      // Marked content (§14.6) and compatibility (§7.8.2)
      case "MP"  => MarkPoint(name(0), Unset)
      case "DP"  => MarkPoint(name(0), pair(1))
      case "BMC" => BeginMarked(name(0), Unset)
      case "BDC" => BeginMarked(name(0), pair(1))
      case "EMC" => EndMarked
      case "BX"  => BeginCompatibility
      case "EX"  => EndCompatibility

      // Type 3 glyph metrics (§9.6.4)
      case "d0" => numbers(2) match
        case List(wx, wy) => GlyphWidth(wx, wy)
        case _            => malformed

      case "d1" => numbers(6) match
        case List(wx, wy, llx, lly, urx, ury) => GlyphMetrics(wx, wy, llx, lly, urx, ury)
        case _                                => malformed

      case _ =>
        Unrecognized(operator, operands)

  private def components(operands: List[Cos], malformed: => Nothing)
  :   (List[Double], Optional[Text]) =

    operands.reverse match
      case Cos.Name(pattern) :: rest => (rest.reverse.map(_.double.or(malformed)), pattern)
      case all                       => (all.reverse.map(_.double.or(malformed)), Unset)

// A content-stream operator with typed operands (ISO 32000-2 §8–9, §14.6). Coordinates stay
// `Double`: they are user-space values, meaningful only under the current transformation
// matrix. Show-text operands stay encoded `Data`: mapping them to text needs the font. The
// AST is deliberately symmetric enough to serialize back — the seed of the future writer.
enum PdfOperator:
  // Graphics state
  case Save, Restore
  case Concat(matrix: PdfMatrix)
  case SetLineWidth(width: Double)
  case SetLineCap(cap: PdfOperator.LineCap)
  case SetLineJoin(join: PdfOperator.LineJoin)
  case SetMiterLimit(limit: Double)
  case SetDashPattern(pattern: List[Double], phase: Double)
  case SetIntent(intent: Text)
  case SetFlatness(tolerance: Double)
  case SetParameters(name: Text)

  // Path construction and painting
  case Move(x: Double, y: Double)
  case Line(x: Double, y: Double)
  case Cubic(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double)
  case CubicStart(x2: Double, y2: Double, x3: Double, y3: Double)
  case CubicEnd(x1: Double, y1: Double, x3: Double, y3: Double)
  case Close
  case Rectangle(x: Double, y: Double, width: Double, height: Double)
  case Stroke, CloseStroke
  case Fill(rule: PdfOperator.FillRule)
  case FillStroke(rule: PdfOperator.FillRule)
  case CloseFillStroke(rule: PdfOperator.FillRule)
  case EndPath
  case Clip(rule: PdfOperator.FillRule)

  // Text
  case BeginText, EndText
  case Offset(dx: Double, dy: Double)
  case OffsetLeading(dx: Double, dy: Double)
  case SetTextMatrix(matrix: PdfMatrix)
  case NextLine
  case SetCharSpacing(space: Double)
  case SetWordSpacing(space: Double)
  case SetScaling(percent: Double)
  case SetLeading(leading: Double)
  case SetFont(font: Text, size: Double)
  case SetRenderMode(mode: PdfOperator.TextRenderMode)
  case SetRise(rise: Double)
  case ShowText(string: Data)
  case ShowTexts(elements: List[Data | Double])
  case NextLineShow(string: Data)
  case NextLineShowSpaced(word: Double, char: Double, string: Data)

  // Colour
  case StrokeSpace(name: Text)
  case FillSpace(name: Text)
  case StrokeColor(components: List[Double], pattern: Optional[Text])
  case FillColor(components: List[Double], pattern: Optional[Text])
  case StrokeGray(gray: Double)
  case FillGray(gray: Double)
  case StrokeRgb(color: Srgb)
  case FillRgb(color: Srgb)
  case StrokeCmyk(color: Cmyk)
  case FillCmyk(color: Cmyk)

  // XObjects, inline images and shading
  case Draw(name: Text)
  case InlineImage(parameters: Map[Text, Cos], data: Data)
  case Shade(name: Text)

  // Marked content and compatibility
  case MarkPoint(tag: Text, properties: Optional[Cos])
  case BeginMarked(tag: Text, properties: Optional[Cos])
  case EndMarked
  case BeginCompatibility, EndCompatibility

  // Type 3 glyph metrics
  case GlyphWidth(wx: Double, wy: Double)
  case GlyphMetrics(wx: Double, wy: Double, llx: Double, lly: Double, urx: Double, ury: Double)

  case Unrecognized(operator: Text, operands: List[Cos])
