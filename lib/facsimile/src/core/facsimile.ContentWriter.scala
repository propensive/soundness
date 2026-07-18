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

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import iridescence.*
import rudiments.*
import vacuous.*

// Serialises a content-stream operator list back to its byte form, the inverse of
// `PdfOperator.read`: each instruction is its operands followed by its operator, one per
// line. Numbers are written as plain decimals; show-text operands are re-escaped literal
// strings; names re-escape through the same rules as the object writer.
private[facsimile] object ContentWriter:
  import PdfOperator.*

  def write(operators: List[PdfOperator]): Data =
    val builder = Array.newBuilder[Byte]
    operators.each { operator => line(builder, operator) }
    builder.result().immutable(using Unsafe)

  private def line(builder: scala.collection.mutable.ArrayBuilder[Byte], operator: PdfOperator)
  :   Unit =

    def out(text: Text): Unit =
      val raw = charEncoders.iso88591Encoder.encoded(text)
      var i = 0
      while i < raw.length do { builder += raw(i); i += 1 }

    def num(value: Double): Text =
      if value == value.toLong.toDouble then value.toLong.toString.tt
      else safely(Decimal(value).text).or(t"0")

    def nums(values: Double*): Text = values.map(num).join(t" ")
    def string(data: Data): Unit = builder.addAll(CosWriter.write(Cos.Chars(data)).mutable(using Unsafe))
    def name(text: Text): Unit = builder.addAll(CosWriter.write(Cos.Name(text)).mutable(using Unsafe))
    def matrix(m: PdfMatrix): Text = nums(m.a, m.b, m.c, m.d, m.e, m.f)

    operator match
      case Save                     => out(t"q\n")
      case Restore                  => out(t"Q\n")
      case Concat(m)                => out(t"${matrix(m)} cm\n")
      case SetLineWidth(w)          => out(t"${num(w)} w\n")
      case SetLineCap(cap)          => out(t"${cap.ordinal} J\n")
      case SetLineJoin(join)        => out(t"${join.ordinal} j\n")
      case SetMiterLimit(limit)     => out(t"${num(limit)} M\n")
      case SetIntent(intent)        => name(intent); out(t" ri\n")
      case SetFlatness(tolerance)   => out(t"${num(tolerance)} i\n")
      case SetParameters(gs)        => name(gs); out(t" gs\n")

      case SetDashPattern(pattern, phase) =>
        out(t"[${pattern.map(num).join(t" ")}] ${num(phase)} d\n")

      case Move(x, y)                    => out(t"${nums(x, y)} m\n")
      case Line(x, y)                    => out(t"${nums(x, y)} l\n")
      case Cubic(x1, y1, x2, y2, x3, y3) => out(t"${nums(x1, y1, x2, y2, x3, y3)} c\n")
      case CubicStart(x2, y2, x3, y3)    => out(t"${nums(x2, y2, x3, y3)} v\n")
      case CubicEnd(x1, y1, x3, y3)      => out(t"${nums(x1, y1, x3, y3)} y\n")
      case Close                         => out(t"h\n")
      case Rectangle(x, y, w, h)         => out(t"${nums(x, y, w, h)} re\n")
      case Stroke                        => out(t"S\n")
      case CloseStroke                   => out(t"s\n")
      case Fill(FillRule.NonZero)        => out(t"f\n")
      case Fill(FillRule.EvenOdd)        => out(t"f*\n")
      case FillStroke(FillRule.NonZero)  => out(t"B\n")
      case FillStroke(FillRule.EvenOdd)  => out(t"B*\n")
      case CloseFillStroke(FillRule.NonZero) => out(t"b\n")
      case CloseFillStroke(FillRule.EvenOdd) => out(t"b*\n")
      case EndPath                       => out(t"n\n")
      case Clip(FillRule.NonZero)        => out(t"W\n")
      case Clip(FillRule.EvenOdd)        => out(t"W*\n")

      case BeginText                => out(t"BT\n")
      case EndText                  => out(t"ET\n")
      case Offset(dx, dy)           => out(t"${nums(dx, dy)} Td\n")
      case OffsetLeading(dx, dy)    => out(t"${nums(dx, dy)} TD\n")
      case SetTextMatrix(m)         => out(t"${matrix(m)} Tm\n")
      case NextLine                 => out(t"T*\n")
      case SetCharSpacing(space)    => out(t"${num(space)} Tc\n")
      case SetWordSpacing(space)    => out(t"${num(space)} Tw\n")
      case SetScaling(percent)      => out(t"${num(percent)} Tz\n")
      case SetLeading(leading)      => out(t"${num(leading)} TL\n")
      case SetRenderMode(mode)      => out(t"${mode.ordinal} Tr\n")
      case SetRise(rise)            => out(t"${num(rise)} Ts\n")
      case SetFont(font, size)      => name(font); out(t" ${num(size)} Tf\n")
      case ShowText(data)           => string(data); out(t" Tj\n")
      case NextLineShow(data)       => string(data); out(t" '\n")

      case NextLineShowSpaced(word, char, data) =>
        out(t"${nums(word, char)} ")
        string(data)
        out(t" \"\n")

      case ShowTexts(elements) =>
        out(t"[")

        elements.each:
          case data: Data   => string(data)
          case gap: Double  => out(t"${num(gap)}")

        out(t"] TJ\n")

      case StrokeSpace(name0)       => name(name0); out(t" CS\n")
      case FillSpace(name0)         => name(name0); out(t" cs\n")
      case StrokeGray(gray)         => out(t"${num(gray)} G\n")
      case FillGray(gray)           => out(t"${num(gray)} g\n")
      case StrokeRgb(c)             => out(t"${nums(c.red, c.green, c.blue)} RG\n")
      case FillRgb(c)               => out(t"${nums(c.red, c.green, c.blue)} rg\n")
      case StrokeCmyk(c)            => out(t"${nums(c.cyan, c.magenta, c.yellow, c.key)} K\n")
      case FillCmyk(c)              => out(t"${nums(c.cyan, c.magenta, c.yellow, c.key)} k\n")

      case StrokeColor(components, pattern) =>
        out(t"${components.map(num).join(t" ")}")
        pattern.let { p => out(t" "); name(p) }
        out(t" SCN\n")

      case FillColor(components, pattern) =>
        out(t"${components.map(num).join(t" ")}")
        pattern.let { p => out(t" "); name(p) }
        out(t" scn\n")

      case Draw(name0)              => name(name0); out(t" Do\n")
      case Shade(name0)             => name(name0); out(t" sh\n")

      case InlineImage(parameters, data) =>
        out(t"BI ")
        builder.addAll(CosWriter.dictionaryBytes(parameters).mutable(using Unsafe))
        out(t" ID ")
        builder.addAll(data.mutable(using Unsafe))
        out(t" EI\n")

      case MarkPoint(tag, Unset)    => name(tag); out(t" MP\n")
      case BeginMarked(tag, Unset)  => name(tag); out(t" BMC\n")

      case MarkPoint(tag, property: Cos) =>
        name(tag); out(t" ")
        builder.addAll(CosWriter.write(property).mutable(using Unsafe))
        out(t" DP\n")

      case BeginMarked(tag, property: Cos) =>
        name(tag); out(t" ")
        builder.addAll(CosWriter.write(property).mutable(using Unsafe))
        out(t" BDC\n")

      case EndMarked                => out(t"EMC\n")
      case BeginCompatibility       => out(t"BX\n")
      case EndCompatibility         => out(t"EX\n")
      case GlyphWidth(wx, wy)       => out(t"${nums(wx, wy)} d0\n")

      case GlyphMetrics(wx, wy, llx, lly, urx, ury) =>
        out(t"${nums(wx, wy, llx, lly, urx, ury)} d1\n")

      case Unrecognized(operator, operands) =>
        operands.each: operand =>
          builder.addAll(CosWriter.write(operand).mutable(using Unsafe))
          out(t" ")

        out(t"$operator\n")
