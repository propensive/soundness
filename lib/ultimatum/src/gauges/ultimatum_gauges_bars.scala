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
import gossamer.*

// Determinate progress bars, keyed on `Fraction`. Import one by name to choose it; with no import,
// `Fraction`'s companion supplies the smooth eighth-block bar.
// Each is a `Bar` value, so width negotiation, sub-cell quantization and the fall through to a
// percentage and then to a single shade are shared by all of them rather than reimplemented
// eighteen times.
package bars:
  // Full, partials, empty. Where `empty` is a space, the track shows as a background colour; where
  // it is a glyph, the track is drawn in the track colour instead.
  private def filled(full: Text, partials: Text, empty: Text): Bar =
    Bar.Filled(Bar.Glyphs(full, partials, empty), Bar.defaultColumns, false)

  // The eighth-width left blocks: each is one eighth wider than the last, so the bar's right edge
  // advances by a fraction of a cell rather than jumping a whole one.
  private val eighths: Text = t"▏▎▍▌▋▊▉"

  given smoothBar: Gauging => Fraction is Gaugeable = filled(t"█", eighths, t" ").gaugeable

  given blockBar: Gauging => Fraction is Gaugeable = filled(t"█", t"", t"░").gaugeable
  given shadedBar: Gauging => Fraction is Gaugeable = filled(t"█", t"░▒▓", t" ").gaugeable

  // The boundary cell rises rather than widens: a different reading of the same fraction, and the
  // one to use when the bar is short.
  given risingBar: Gauging => Fraction is Gaugeable = filled(t"█", t"▁▂▃▄▅▆▇", t" ").gaugeable

  given fineBar: Gauging => Fraction is Gaugeable = filled(t"━", t"╸", t"━").gaugeable
  given dotBar: Gauging => Fraction is Gaugeable = filled(t"●", t"", t"·").gaugeable
  given railBar: Gauging => Fraction is Gaugeable = filled(t"━", t"╸", t"─").gaugeable
  given squareBar: Gauging => Fraction is Gaugeable = filled(t"■", t"", t"□").gaugeable

  given brailleBar: Gauging => Fraction is Gaugeable =
    filled(t"⣿", t"⡀⡄⡆⡇⣇⣧⣷", t"⣀").gaugeable

  // Capped bars: the caps mark the extent, so a partly-filled bar in a wide column still reads as
  // a proportion of something. They are the first cells given up when the column narrows.
  given capsuleBar: Gauging => Fraction is Gaugeable =
    Bar.Filled(Bar.Glyphs(t"█", eighths, t"░", t"▕", t"▏"), Bar.defaultColumns, false).gaugeable

  given asciiBar: Gauging => Fraction is Gaugeable =
    Bar.Filled(Bar.Glyphs(t"#", t"", t"-", t"[", t"]"), Bar.defaultColumns, false).gaugeable

  given equalsBar: Gauging => Fraction is Gaugeable =
    Bar.Filled(Bar.Glyphs(t"=", t"", t" ", t"[", t"]"), Bar.defaultColumns, false).gaugeable

  // The classic `[===>    ]`. The arrowhead is a tip rather than a partial: it marks where the
  // fill has reached at every intermediate value, instead of appearing only on the sub-cell steps.
  given arrowheadBar: Gauging => Fraction is Gaugeable =
    val glyphs = Bar.Glyphs(t"=", t"", t" ", t"[", t"]", tip = t">")
    Bar.Filled(glyphs, Bar.defaultColumns, false).gaugeable

  // The fill colour is taken from the palette's lengthwise ramp per cell, so the bar carries a
  // gradient along its length rather than one flat colour.
  given gradientBar: Gauging => Fraction is Gaugeable =
    Bar.Filled(Bar.Glyphs(t"█", eighths, t" "), Bar.defaultColumns, true).gaugeable

  // Discrete pips rather than a continuous fill: coarser, but legible at a glance and countable,
  // which a smooth bar is not.
  given segmentedBar: Gauging => Fraction is Gaugeable =
    Bar.Segmented(t"▰", t"▱", t"", 20).gaugeable

  given pipBar: Gauging => Fraction is Gaugeable = Bar.Segmented(t"●", t"○", t" ", 10).gaugeable

  // A head travelling along a rail, with nothing filled behind it: a position rather than an
  // amount, for something that scrubs back and forth.
  given markerBar: Gauging => Fraction is Gaugeable =
    Bar.Marker(t"─", t"◆", Bar.defaultColumns).gaugeable

  // The figure alone, for a column with no room for anything else — and the design every other bar
  // degrades into.
  given percentageBar: Gauging => Fraction is Gaugeable = Bar.Numeric.gaugeable
