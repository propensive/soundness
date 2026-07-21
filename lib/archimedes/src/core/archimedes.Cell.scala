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
import gossamer.*
import hieroglyph.Measurable
import hieroglyph.textMetrics.wideCharacterWidthMetric
import vacuous.*

import Mathml.*

// A `Cell` is a rectangular block of monospaced character cells with a `baseline`
// — the row index (from the top) of the mathematical axis that composition aligns
// on. Lines are `Writing` (grapheme-aware `Text`) so that combining accents, ZWJ
// emoji and other multi-codepoint clusters count as one display cell; every line
// has grapheme width exactly `width`. A plain term is one line high with baseline
// 0. `Cell.of` recursively lays out any `Mathml` node depth-first, composing child
// blocks by aligning their baselines and padding with spaces; tall constructs
// (fractions, roots) grow the block vertically and brackets stretch to fit.

object Cell:
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

  private def graphemeWidth(writing: Writing): Int = summon[Writing is Measurable].width(writing)

  private def spaces(count: Int): Writing = Writing(Text.fill(max(count, 0))(_ => ' '))
  private def repeat(char: Char, count: Int): Writing = Writing(Text.fill(max(count, 0))(_ => char))

  // The text of `cell`'s row, or a full-width blank when the row is out of bounds.
  private def slice(cell: Cell, row: Int): Text =
    if row >= 0 && row < cell.height then cell.lines(row).text else spaces(cell.width).text

  val empty: Cell = Cell(IndexedSeq(Writing.empty), 0, 0)

  def line(text: Text): Cell =
    val writing = Writing(text)
    Cell(IndexedSeq(writing), graphemeWidth(writing), 0)

  def blank(width: Int, height: Int, baseline: Int): Cell =
    Cell(IndexedSeq.fill(max(height, 1))(spaces(width)), max(width, 0), baseline)

  // Horizontal, baseline-aligned concatenation — the workhorse for `<mrow>`. Each
  // cell is framed with blank rows so every baseline lines up, then rows are joined.
  def beside(cells: List[Cell]): Cell = cells match
    case Nil => empty

    case _ =>
      val ascent = cells.map(_.baseline).reduceLeft(max)
      val descent = cells.map { cell => cell.height - 1 - cell.baseline }.reduceLeft(max)
      val framed = cells.map(_.framed(ascent, descent))
      val height = ascent + descent + 1

      val lines = (0 until height).to(IndexedSeq).map: row =>
        Writing(framed.map { cell => cell.lines(row).text }.join)

      Cell(lines, cells.map(_.width).reduceLeft(_ + _), ascent)

  def fraction(numerator: Cell, denominator: Cell): Cell =
    val width = max(numerator.width, denominator.width) + 2
    val top = numerator.hcenter(width)
    val bottom = denominator.hcenter(width)
    Cell((top.lines :+ repeat(Bar, width)) ++ bottom.lines, width, top.height)

  def superscript(base: Cell, script: Cell): Cell =
    val height = base.height + script.height

    val lines = (0 until height).to(IndexedSeq).map: row =>
      Writing(t"${slice(base, row - script.height)}${slice(script, row)}")

    Cell(lines, base.width + script.width, base.baseline + script.height)

  def subscript(base: Cell, script: Cell): Cell =
    val height = base.height + script.height

    val lines = (0 until height).to(IndexedSeq).map: row =>
      Writing(t"${slice(base, row)}${slice(script, row - base.height)}")

    Cell(lines, base.width + script.width, base.baseline)

  def subsup(base: Cell, subscript: Cell, superscript: Cell): Cell =
    val right = max(subscript.width, superscript.width)
    val height = superscript.height + base.height + subscript.height
    val middle = superscript.height + base.height

    val lines = (0 until height).to(IndexedSeq).map: row =>
      val left = slice(base, row - superscript.height)

      val scripts =
        if row < superscript.height then
          val text = superscript.lines(row).text
          val pad = spaces(right - superscript.width).text
          t"$text$pad"
        else if row >= middle then
          val text = subscript.lines(row - middle).text
          val pad = spaces(right - subscript.width).text
          t"$text$pad"
        else
          spaces(right).text

      Writing(t"$left$scripts")

    Cell(lines, base.width + right, superscript.height + base.baseline)

  def overscript(base: Cell, over: Cell): Cell =
    val width = max(base.width, over.width)
    Cell(over.hcenter(width).lines ++ base.hcenter(width).lines, width, over.height + base.baseline)

  def underscript(base: Cell, under: Cell): Cell =
    val width = max(base.width, under.width)
    Cell(base.hcenter(width).lines ++ under.hcenter(width).lines, width, base.baseline)

  def underover(base: Cell, under: Cell, over: Cell): Cell =
    val width = max(max(base.width, under.width), over.width)

    val lines = over.hcenter(width).lines ++ base.hcenter(width).lines ++ under.hcenter(width).lines
    Cell(lines, width, over.height + base.baseline)

  // A square root. Over a single line the compact `√` glyph suffices; over taller
  // radicands the sign is drawn as a `╲` foot and a `│` stem rising to the `┌`
  // corner that turns into the `─` vinculum across the top.
  def radical(inner: Cell): Cell =
    if inner.height > 1 then tallRadical(inner) else
      val overline = Writing(t"$Corner${repeat(Bar, inner.width).text}")
      val body = Writing(t"$Radical${inner.lines(0).text}")
      Cell(IndexedSeq(overline, body), inner.width + 1, 1)

  private def tallRadical(inner: Cell): Cell =
    val overline = Writing(t" $Corner${repeat(Bar, inner.width).text}")

    val body = (0 until inner.height).to(IndexedSeq).map: row =>
      val foot = if row == inner.height - 1 then Tick else ' '
      Writing(t"$foot$Stem${inner.lines(row).text}")

    Cell(overline +: body, inner.width + 2, inner.baseline + 1)

  // An nth root always uses the tall sign so the `index` can sit one line above,
  // to the left of the corner.
  def root(base: Cell, index: Cell): Cell =
    val radicand = tallRadical(base)

    val lines = (0 until radicand.height).to(IndexedSeq).map: row =>
      val prefix = if row < index.height then index.lines(row).text else spaces(index.width).text
      Writing(t"$prefix${radicand.lines(row).text}")

    Cell(lines, index.width + radicand.width, radicand.baseline)

  def fence(open: Char, close: Char, inner: Cell): Cell =
    val left = bracket(open, inner.height, inner.baseline, opening = true)
    val right = bracket(close, inner.height, inner.baseline, opening = false)
    beside(List(left, inner, right))

  // A stretchy bracket sized to `height`, built from box-drawing pieces; for a
  // single-line cell it degrades to the literal bracket character.
  def bracket(char: Char, height: Int, baseline: Int, opening: Boolean): Cell =
    if height <= 1 then line(char.toString.tt) else
      val glyphs = (0 until height).to(IndexedSeq).map: row =>
        Writing(bracketGlyph(char, row, height, baseline, opening).toString.tt)

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
    if height <= 1 then line(char.toString.tt) else
      val (strokes, circle) = integralShape(char)

      val lines = (0 until height).to(IndexedSeq).map: row =>
        val glyphs = (0 until strokes).map: column =>
          val glyph =
            if row == 0 then '╭'
            else if row == height - 1 then '╯'
            else if circle && column == 0 && row == axis then '○'
            else Stem

          glyph.toString.tt

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

    val lines = (0 until height).to(IndexedSeq).map: row =>
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

    val lines = (0 until height).to(IndexedSeq).map: row =>
      if row == 0 then Writing(t"┬${repeat(Bar, width - 2).text}┬")
      else Writing(t"$Stem${spaces(width - 2).text}$Stem")

    Cell(lines, width, height/2)

  private def stretchyChar(node: Mathml): Optional[Char] = node match
    case Mo(value, _) if value.s.length == 1 && stretchable(value.s.charAt(0)) => value.s.charAt(0)
    case _                                                                     => Unset

  private def stretchable(char: Char): Boolean =
    brackets.contains(char) || integrals.contains(char) || bigOperators.contains(char)

  private def operator(value: Text): Text =
    if value.s.length == 1 && value.s.charAt(0) >= '⁡' && value.s.charAt(0) <= '⁤' then t""
    else if binaryOperators.contains(value) then t" $value "
    else value

  // Lay out a sequence of nodes as a row, stretching any bracket or integral
  // operators to the height of the surrounding (non-stretchy) content.
  def row(nodes: List[Mathml]): Cell =
    val content = nodes.filter { node => stretchyChar(node).absent }.map(of)
    val ascent = if content.isEmpty then 0 else content.map(_.baseline).reduceLeft(max)

    val descent =
      if content.isEmpty then 0
      else content.map { cell => cell.height - 1 - cell.baseline }.reduceLeft(max)

    val height = ascent + descent + 1

    val cells = nodes.map: node =>
      stretchyChar(node).lay(of(node)): char =>
        // A one-line subject keeps the plain glyph; taller subjects grow the art.
        if bigOperators.contains(char) then
          if height <= 1 then line(char.toString.tt)
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
    case Msup(base, script, _)     => superscript(of(base), of(script))
    case Msub(base, script, _)     => subscript(of(base), of(script))
    case Msubsup(base, sb, sp, _)  => subsup(of(base), of(sb), of(sp))
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

case class Cell(lines: IndexedSeq[Writing], width: Int, baseline: Int):
  def height: Int = lines.length
  def render: Text = lines.map(_.text).join(t"\n")

  // Pad each line on the left and right with spaces, widening the block.
  def hpad(left: Int, right: Int): Cell =
    val leftText = Cell.spaces(left).text
    val rightText = Cell.spaces(right).text

    val padded = lines.map: line =>
      Writing(t"$leftText${line.text}$rightText")

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
    Cell(IndexedSeq.fill(above)(blank) ++ lines ++ IndexedSeq.fill(below)(blank), width, ascent)
