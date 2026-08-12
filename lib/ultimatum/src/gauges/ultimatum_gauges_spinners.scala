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

// Indeterminate activity indicators, keyed on `Busy`. Import one by name to choose it; with no
// import at all, `Busy`'s companion supplies the braille dots.
// Every design names the least adventurous repertoire that can draw it and, where it is wide or
// exotic, what to fall back to — so the catalogue degrades as a whole under `asciiGlyphs`, and in
// a narrow column, without the caller choosing a different design.
package spinners:
  import Gaugeable.Glyphs.{Ascii, Emoji, Unicode}

  // The ASCII designs, which are also the ends of most fallback chains.
  private val line: Spinner = Spinner.each(t"-\\|/", 130, Ascii)
  private val quadrant: Spinner = Spinner.each(t"◴◷◶◵", 120, Unicode, line)
  private val arc: Spinner = Spinner.each(t"◜◠◝◞◡◟", 100, Unicode, line)
  private val pulse: Spinner = Spinner.each(t"█▓▒░▒▓", 100, Unicode, line)
  private val circles: Spinner = Spinner.each(t"◌○◎◉●◉◎○", 120, Unicode, line)
  private val halves: Spinner = Spinner.each(t"◐◓◑◒", 120, Unicode, line)
  private val hourglass: Spinner = Spinner.each(t"⧖⧗", 500, Unicode, line)

  given lineSpinner: Gauging => Fraction is Gaugeable = line.gaugeable

  given crossStarSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"+x*", 120, Ascii).gaugeable

  given dqpbSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"dqpb", 100, Ascii).gaugeable

  given bounceSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t".oO°Oo.", 120, Ascii).gaugeable

  given balloonSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t" .oO@* ", 140, Ascii).gaugeable

  // Braille: the densest single-cell animations there are, and the ones most terminals show best.
  given brailleDotsSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", 80, Unicode, line).gaugeable

  given brailleSnakeSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"⣾⣽⣻⢿⡿⣟⣯⣷", 80, Unicode, line).gaugeable

  given brailleWaveSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"⠁⠂⠄⡀⢀⠠⠐⠈", 90, Unicode, line).gaugeable

  given brailleGrowSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"⠋⠙⠚⠞⠖⠦⠴⠲⠳⠓", 80, Unicode, line).gaugeable

  // Circles, arcs and quadrants.
  given arcSpinner: Gauging => Fraction is Gaugeable = arc.gaugeable
  given circleQuadrantSpinner: Gauging => Fraction is Gaugeable = quadrant.gaugeable
  given circleHalfSpinner: Gauging => Fraction is Gaugeable = halves.gaugeable
  given circlePulseSpinner: Gauging => Fraction is Gaugeable = circles.gaugeable

  given squareCornerSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"◰◳◲◱", 120, Unicode, line).gaugeable

  given triangleSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"◢◣◤◥", 120, Unicode, line).gaugeable

  given pipeSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"┤┘┴└├┌┬┐", 100, Unicode, line).gaugeable

  given boxSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"▖▘▝▗", 120, Unicode, line).gaugeable

  // Arrows and stars.
  given arrowSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"←↖↑↗→↘↓↙", 100, Unicode, line).gaugeable

  given arrowDoubleSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"⇐⇖⇑⇗⇒⇘⇓⇙", 100, Unicode, line).gaugeable

  given starSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"✶✸✹✺✹✷", 70, Unicode, line).gaugeable

  given hamburgerSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"☱☲☴", 100, Unicode, line).gaugeable

  // Shading and growth: an amount of ink rather than a shape, so they read as effort.
  given noiseSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"▓▒░", 100, Unicode, line).gaugeable

  given pulseSpinner: Gauging => Fraction is Gaugeable = pulse.gaugeable

  given growingBarSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"▏▎▍▌▋▊▉█▉▊▋▌▍▎", 90, Unicode, line).gaugeable

  given growingBlockSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"▁▃▄▅▆▇█▇▆▅▄▃", 90, Unicode, line).gaugeable

  given layerSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"-=≡", 150, Ascii).gaugeable

  // Toggles: two states, slow, for something that is alive rather than working.
  given toggleSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"⊶⊷", 250, Unicode, line).gaugeable

  given toggleSquareSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"▫▪", 250, Unicode, line).gaugeable

  given toggleRoundSpinner: Gauging => Fraction is Gaugeable =
    Spinner.each(t"⦾⦿", 250, Unicode, line).gaugeable

  // Hourglasses: the only single-cell designs that say "waiting" rather than "working".
  given hourglassThinSpinner: Gauging => Fraction is Gaugeable = hourglass.gaugeable

  // Multi-cell designs. Each falls back to a single cell, so a narrow column still animates.
  given pointsSpinner: Gauging => Fraction is Gaugeable =
    Spinner(Sequence(t"∙∙∙", t"●∙∙", t"∙●∙", t"∙∙●"), 125, 3, Unicode, line).gaugeable

  given dotsScrollSpinner: Gauging => Fraction is Gaugeable =
    Spinner(Sequence(t"   ", t".  ", t".. ", t"..."), 200, 3, Ascii, line).gaugeable

  given binarySpinner: Gauging => Fraction is Gaugeable =
    val frames =
      Sequence(t"010010", t"001100", t"100101", t"111010", t"111101", t"010111", t"101011",
          t"111000", t"110011", t"110101")

    Spinner(frames, 80, 6, Ascii, line).gaugeable

  given bouncingBarSpinner: Gauging => Fraction is Gaugeable =
    val frames =
      Sequence(t"[    ]", t"[=   ]", t"[==  ]", t"[=== ]", t"[ ===]", t"[  ==]", t"[   =]",
          t"[    ]", t"[   =]", t"[  ==]", t"[ ===]", t"[====]", t"[=== ]", t"[==  ]", t"[=   ]")

    Spinner(frames, 80, 6, Ascii, line).gaugeable

  given bouncingBallSpinner: Gauging => Fraction is Gaugeable =
    val frames =
      Sequence(t"( ●    )", t"(  ●   )", t"(   ●  )", t"(    ● )", t"(     ●)", t"(    ● )",
          t"(   ●  )", t"(  ●   )", t"( ●    )", t"(●     )")

    Spinner(frames, 80, 8, Unicode, line).gaugeable

  given aestheticSpinner: Gauging => Fraction is Gaugeable =
    val frames =
      Sequence(t"▰▱▱▱▱▱▱", t"▰▰▱▱▱▱▱", t"▰▰▰▱▱▱▱", t"▰▰▰▰▱▱▱", t"▰▰▰▰▰▱▱", t"▰▰▰▰▰▰▱",
          t"▰▰▰▰▰▰▰", t"▱▱▱▱▱▱▱")

    Spinner(frames, 120, 7, Unicode, line).gaugeable

  given shuttleSpinner: Gauging => Fraction is Gaugeable =
    val frames = Sequence(t"▸▹▹▹▹", t"▹▸▹▹▹", t"▹▹▸▹▹", t"▹▹▹▸▹", t"▹▹▹▹▸")

    Spinner(frames, 120, 5, Unicode, line).gaugeable

  // Emoji: two cells wide, and only where the terminal has said it can render them. Each falls
  // back to the BMP design that means the same thing, which is also the ASCII path.
  given clockSpinner: Gauging => Fraction is Gaugeable =
    val frames =
      Sequence(t"🕛", t"🕐", t"🕑", t"🕒", t"🕓", t"🕔", t"🕕", t"🕖", t"🕗", t"🕘", t"🕙", t"🕚")

    Spinner(frames, 100, 2, Emoji, quadrant).gaugeable

  given moonPhaseSpinner: Gauging => Fraction is Gaugeable =
    val frames = Sequence(t"🌑", t"🌒", t"🌓", t"🌔", t"🌕", t"🌖", t"🌗", t"🌘")

    Spinner(frames, 80, 2, Emoji, circles).gaugeable

  given earthSpinner: Gauging => Fraction is Gaugeable =
    Spinner(Sequence(t"🌍", t"🌎", t"🌏"), 180, 2, Emoji, halves).gaugeable

  given hourglassSpinner: Gauging => Fraction is Gaugeable =
    val frames = Sequence(t"⏳", t"⌛")

    Spinner(frames, 500, 2, Emoji, hourglass).gaugeable
