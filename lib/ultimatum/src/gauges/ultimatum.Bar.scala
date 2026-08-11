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
package ultimatum

import anticipation.*
import denominative.*
import escapade.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Bar:
  // The characters a filled bar is drawn from. `partials` are the intermediate widths of the
  // boundary cell, in ascending order and excluding `full`; supplying them is what makes a bar
  // advance by a fraction of a cell rather than jumping a whole one at a time. An empty `partials`
  // gives a bar that steps cell by cell.
  case class Glyphs
    ( full:     Text,
       partials: Text           = t"",
       empty:    Text           = t" ",
       leftCap:  Optional[Text] = Unset,
       rightCap: Optional[Text] = Unset,
       tip:      Optional[Text] = Unset )

  // The intrinsic width every bar prefers, matching the width the repackager's bar has always had.
  val defaultColumns: Int = 40

  // The shades a bar collapses to when it has one cell: enough to tell nearly-done from
  // barely-started, which is all one cell can honestly say.
  private val shades: Text = t"░▒▓█"
  private val asciiShades: Text = t".:*#"

// How a proportion is drawn. Four shapes cover the catalogue: a bar that fills, a marker that
// travels along a track, a run of discrete pips, and a bare figure.
// Degradation is implemented here, once, rather than in each of the eighteen designs: a bar
// re-quantizes to whatever width it is given, drops its end caps when they no longer earn their
// cells, falls through to a percentage at four cells, and to a single shade glyph at one.
enum Bar:
  case Filled(glyphs: Bar.Glyphs, columns: Int, gradient: Boolean)
  case Marker(track: Text, marker: Text, columns: Int)
  case Segmented(pip: Text, hollow: Text, gap: Text, pips: Int)
  case Numeric

  def columnCount: Int = this match
    case Filled(_, columns, _)      => columns
    case Marker(_, _, columns)      => columns
    case Segmented(_, _, gap, pips) => pips + (if gap == t"" then 0 else pips - 1)
    case Numeric                    => 4

  def gaugeable(using gauging: Gauging): Fraction is Gaugeable = new Gaugeable:
    type Self = Fraction
    override def minWidth(status: Fraction): Int = 1
    override def columns(status: Fraction): Int = columnCount

    def rows(status: Fraction, tick: Tick, width: Int): List[Teletype] =
      List(Bar.this.draw(status, width, gauging))

  // Draw at exactly `width` cells.
  def draw(fraction: Fraction, width: Int, gauging: Gauging): Teletype =
    val palette = gauging.palette
    val ascii = !gauging.permits(Gaugeable.Glyphs.Unicode)

    if width <= 0 then Teletype(t"")
    else if width == 1 then
      // One cell can still carry the magnitude, as a shade.
      val shades = if ascii then Bar.asciiShades else Bar.shades
      val index = (fraction.value*shades.length).toInt.min(shades.length - 1).max(0)
      val glyph = Teletype(shades.at(index.z).let(_.show).or(t" "))

      gauging.tint(palette.lengthwise(fraction.value))(glyph)

    else if width < 4 then
      // Too narrow for a bar, wide enough for a figure: right-align the percentage into the cells
      // available, dropping its most significant digits last.
      val text = Magnitude.percentage(fraction)
      val trimmed = if text.length <= width then text else text.skip(text.length - width)
      gauging.tint(palette.caption)(Teletype(trimmed))

    else
      this match
        case Numeric =>
          val text = Magnitude.percentage(fraction)
          val padding = width - text.length
          val body = gauging.tint(palette.caption)(Teletype(text))
          if padding > 0 then e"${t" "*padding}$body" else body

        case Segmented(pip, hollow, gap, pips) =>
          // Fit as many pips as the width allows, so a segmented bar thins out rather
          // than clipping.
          val separator = if gap == t"" then 0 else 1
          val count = ((width + separator)/(1 + separator)).min(pips).max(1)
          val lit = (fraction.value*count).toInt.min(count)

          val cells = (0 until count).map: index =>
            val glyph = if index < lit then pip else hollow
            val color = if index < lit then palette.fill else palette.track
            gauging.tint(color)(Teletype(glyph))

          pad(cells.reduceLeft { (l, r) => e"$l${t" "*separator}$r" }, width, gauging)

        case Marker(track, marker, _) =>
          // A head travelling along a rail: no fill, so it reads as a position rather
          // than an amount.
          val position = (fraction.value*(width - 1)).toInt.min(width - 1).max(0)
          val before = gauging.tint(palette.track)(Teletype(track*position))
          val head = gauging.tint(palette.leadingEdge)(Teletype(marker))
          val after = gauging.tint(palette.track)(Teletype(track*(width - position - 1)))
          e"$before$head$after"

        case Filled(glyphs, _, gradient) =>
          val leftCap = glyphs.leftCap
          val caps = (if leftCap.present then 1 else 0) + (if glyphs.rightCap.present then 1 else 0)

          // The caps are the first thing to go: below eight cells they cost a fifth of the bar.
          val capped = width >= 8 && caps > 0
          val inner = if capped then width - caps else width
          val body = fill(fraction, inner, glyphs, gradient, gauging)

          if !capped then body else
            def cap(glyph: Optional[Text]): Teletype =
              glyph.lay(e""): text => gauging.tint(palette.track)(Teletype(text))

            e"${cap(leftCap)}$body${cap(glyphs.rightCap)}"

  // The filled region itself, at exactly `inner` cells. Cell accounting is exact — full cells, plus
  // at most one boundary cell, plus empty cells, always sums to `inner` — so the bar never changes
  // width as it fills.
  private def fill
    ( fraction: Fraction,
       inner:    Int,
       glyphs:   Bar.Glyphs,
       gradient: Boolean,
       gauging:  Gauging )
  :   Teletype =

    val palette = gauging.palette
    val steps = glyphs.partials.length + 1
    val total = (fraction.value*inner*steps).toInt.max(0).min(inner*steps)

    // A tip glyph replaces the last filled cell for as long as the bar is neither empty nor full —
    // the classic `[===>  ]`. It is a different idea from a partial: a partial says *how much* of
    // the boundary cell is filled, whereas a tip just marks where the fill has got to, and so must
    // be present at every intermediate value rather than only at the sub-cell steps.
    val (whole, boundary) = glyphs.tip.lay:
      val whole = total/steps
      val remainder = total%steps

      val partial: Optional[Text] =
        if remainder == 0 then Unset else glyphs.partials.at((remainder - 1).z).let(_.show)

      (whole, partial)

    . apply: tip =>
        val filledCells = (fraction.value*inner).toInt.max(0).min(inner)
        val complete = filledCells == 0 || filledCells == inner

        if complete then (filledCells, Unset) else (filledCells - 1, tip)

    val used = whole + (if boundary.present then 1 else 0)

    // A gradient bar colours each cell by where it sits along the bar; a plain one uses two runs.
    val filled =
      if !gradient then gauging.tint(palette.fill)(Teletype(glyphs.full*whole))
      else if whole == 0 then e"" else
        (0 until whole)
        . map: index =>
            val position = if inner <= 1 then 0.0 else index.toDouble/(inner - 1)
            gauging.tint(palette.lengthwise(position))(Teletype(glyphs.full))

        . reduceLeft: (left, right) => e"$left$right"

    val edge = boundary.lay(e""): glyph => gauging.tint(palette.leadingEdge)(Teletype(glyph))

    val blank = Teletype(glyphs.empty*(inner - used).max(0))

    // A track drawn with spaces is only visible as a background; one with its own glyph is drawn
    // in the track colour instead, so both kinds of design read correctly on any terminal.
    val rest =
      if glyphs.empty == t" " then gauging.wash(palette.track)(blank)
      else gauging.tint(palette.track)(blank)

    e"$filled$edge$rest"

  private def pad(content: Teletype, width: Int, gauging: Gauging): Teletype =
    val used = gauging.cells(content.plain)
    if used >= width then content else e"$content${t" "*(width - used)}"
