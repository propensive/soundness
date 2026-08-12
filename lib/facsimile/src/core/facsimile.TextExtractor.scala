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

import proscenium.compat.*

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
  def extract(operators: List[Pdf.Operator], fonts: Map[Text, Pdf.Font], scale: Double)
  :   (List[TextRun], Text) =

    val runs = scala.collection.immutable.List.newBuilder[TextRun]
    val text = StringBuilder()

    var ctm = Pdf.Matrix.Identity
    var stack: List[Pdf.Matrix] = List()
    var tm = Pdf.Matrix.Identity
    var tlm = Pdf.Matrix.Identity
    var font: Optional[Pdf.Font] = Unset
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
      tlm = Pdf.Matrix(1, 0, 0, 1, dx, dy)*tlm
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

      tm = Pdf.Matrix(1, 0, 0, 1, advance, 0)*tm

    def kern(adjustment: Double): Unit =
      val gap = -adjustment/1000.0*size*horizontal
      tm = Pdf.Matrix(1, 0, 0, 1, gap, 0)*tm

      // A large positive gap reads as a space the file never encoded.
      if gap > size*0.15 && !text.isEmpty && text.charAt(text.length - 1) != ' ' then
        text.append(' ')
        lastX.let: end => lastX = end + gap

    operators.each:
      case Pdf.Operator.Save                  => stack = ctm :: stack
      case Pdf.Operator.Concat(matrix)        => ctm = matrix*ctm
      case Pdf.Operator.Offset(dx, dy)        => offset(dx, dy)
      case Pdf.Operator.NextLine              => offset(0, -leading)
      case Pdf.Operator.SetCharSpacing(space) => charSpacing = space
      case Pdf.Operator.SetWordSpacing(space) => wordSpacing = space
      case Pdf.Operator.SetScaling(percent)   => horizontal = percent/100.0
      case Pdf.Operator.SetLeading(value)     => leading = value
      case Pdf.Operator.SetRise(value)        => rise = value
      case Pdf.Operator.ShowText(bytes)       => show(bytes)

      case Pdf.Operator.BeginText =>
        tm = Pdf.Matrix.Identity
        tlm = Pdf.Matrix.Identity

      case Pdf.Operator.SetTextMatrix(matrix) =>
        tm = matrix
        tlm = matrix

      case Pdf.Operator.Restore => stack match
        case head :: tail =>
          ctm = head
          stack = tail

        case _ =>
          ()

      case Pdf.Operator.OffsetLeading(dx, dy) =>
        leading = -dy
        offset(dx, dy)

      case Pdf.Operator.SetFont(name, points) =>
        font = fonts(name)
        size = points

      // Via `stdlib.foreach` and a `Double`-first match: the frozen-array union member
      // takes a reach capture under pattern binding that `each`'s Traversable rejects.
      // Guard-plus-cast, not a type-test pattern: binding a `ShowTexts` refines its
      // frozen-array-union field with capture variables the checker cannot discharge.
      case operator if operator.isInstanceOf[Pdf.Operator.ShowTexts] =>
        operator.asInstanceOf[Pdf.Operator.ShowTexts].elements
        . asInstanceOf[List[scala.IArray[Byte] | Double]] // pure view; same erasure
        . stdlib.foreach: element =>
          (element.asInstanceOf[Matchable]: @unchecked) match
            case adjustment: Double => kern(adjustment)
            case bytes              => show(bytes.asInstanceOf[Data])

      case Pdf.Operator.NextLineShow(bytes) =>
        offset(0, -leading)
        show(bytes)

      case Pdf.Operator.NextLineShowSpaced(word, char, bytes) =>
        wordSpacing = word
        charSpacing = char
        offset(0, -leading)
        show(bytes)

      case _ =>
        ()

    (List.of(runs.result()), text.toString.tt)
