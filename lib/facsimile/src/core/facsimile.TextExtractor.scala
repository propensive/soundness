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
import gossamer.*
import quantitative.*
import rudiments.*
import vacuous.*

// The text machinery of ISO 32000-2 §9.4: a fold over a page's operators tracking the
// transformation stack, the text and line matrices and the text state, producing one
// positioned `TextRun` per show operation, and — in the same pass, where the raw doubles
// are still to hand — the page's plain text: content order, a space where consecutive runs
// on a baseline leave a gap, a newline where the baseline moves.
private[facsimile] object TextExtractor:
  def extract(operators: List[PdfOperator], fonts: Map[Text, PdfFont], scale: Double)
  :   (List[TextRun], Text) =

    val runs = List.newBuilder[TextRun]
    val text = StringBuilder()

    var ctm = PdfMatrix.Identity
    var stack: List[PdfMatrix] = List()
    var tm = PdfMatrix.Identity
    var tlm = PdfMatrix.Identity
    var font: Optional[PdfFont] = Unset
    var size = 0.0
    var charSpacing = 0.0
    var wordSpacing = 0.0
    var horizontal = 1.0
    var leading = 0.0
    var rise = 0.0

    // For the plain-text derivation.
    var lastX: Optional[Double] = Unset
    var lastY: Optional[Double] = Unset

    def offset(dx: Double, dy: Double): Unit =
      tlm = PdfMatrix(1, 0, 0, 1, dx, dy)*tlm
      tm = tlm

    def show(bytes: Data): Unit = font.let: font =>
      val combined = tm*ctm
      val decoded = font.decode(bytes)

      var advance = 0.0

      font.codes(bytes).each: code =>
        val word = if font.wordBoundary(code) then wordSpacing else 0.0
        advance += (font.width(code)/1000.0*size + charSpacing + word)*horizontal

      val (x, y) = combined(0, rise)
      val effective = size*scala.math.hypot(combined.c, combined.d)
      val width = advance*scala.math.hypot(combined.a, combined.b)

      if decoded.s.nonEmpty then
        // Plain text: newline on a baseline change, space across a gap on the same line.
        lastY.let: previous =>
          if (y - previous).abs > effective*0.3 then text.append('\n')
          else lastX.let: end =>
            if x - end > effective*0.15 && !text.isEmpty && text.charAt(text.length - 1) != ' '
            then text.append(' ')

        text.append(decoded.s)
        lastX = x + width
        lastY = y

        runs += TextRun
          ( decoded, font,
            Quantity[Points[1]](effective*scale),
            Quantity[Points[1]](x*scale),
            Quantity[Points[1]](y*scale),
            Quantity[Points[1]](width*scale) )

      tm = PdfMatrix(1, 0, 0, 1, advance, 0)*tm

    def kern(adjustment: Double): Unit =
      val gap = -adjustment/1000.0*size*horizontal
      tm = PdfMatrix(1, 0, 0, 1, gap, 0)*tm

      // A large positive gap reads as a space the file never encoded.
      if gap > size*0.15 && !text.isEmpty && text.charAt(text.length - 1) != ' ' then
        text.append(' ')
        lastX.let: end =>
          lastX = end + gap

    operators.each:
      case PdfOperator.Save                  => stack = ctm :: stack
      case PdfOperator.Concat(matrix)        => ctm = matrix*ctm
      case PdfOperator.Offset(dx, dy)        => offset(dx, dy)
      case PdfOperator.NextLine              => offset(0, -leading)
      case PdfOperator.SetCharSpacing(space) => charSpacing = space
      case PdfOperator.SetWordSpacing(space) => wordSpacing = space
      case PdfOperator.SetScaling(percent)   => horizontal = percent/100.0
      case PdfOperator.SetLeading(value)     => leading = value
      case PdfOperator.SetRise(value)        => rise = value
      case PdfOperator.ShowText(bytes)       => show(bytes)

      case PdfOperator.BeginText =>
        tm = PdfMatrix.Identity
        tlm = PdfMatrix.Identity

      case PdfOperator.SetTextMatrix(matrix) =>
        tm = matrix
        tlm = matrix

      case PdfOperator.Restore => stack match
        case head :: tail =>
          ctm = head
          stack = tail

        case _ =>
          ()

      case PdfOperator.OffsetLeading(dx, dy) =>
        leading = -dy
        offset(dx, dy)

      case PdfOperator.SetFont(name, points) =>
        font = fonts.at(name)
        size = points

      case PdfOperator.ShowTexts(elements) => elements.each: element =>
        (element: @unchecked) match
          case adjustment: Double => kern(adjustment)
          case bytes: Data        => show(bytes)

      case PdfOperator.NextLineShow(bytes) =>
        offset(0, -leading)
        show(bytes)

      case PdfOperator.NextLineShowSpaced(word, char, bytes) =>
        wordSpacing = word
        charSpacing = char
        offset(0, -leading)
        show(bytes)

      case _ =>
        ()

    (runs.result(), text.toString.tt)
