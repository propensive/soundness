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
package burdock

import anticipation.*
import denominative.*
import escapade.*
import gossamer.*
import iridescence.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

// A smooth, fixed-width progress bar for the repackager. `render` is pure (a fraction maps to a
// styled `Teletype`) so the drawing logic stays free of I/O and is directly unit-testable; the
// command-line entry point does the in-place redrawing. Smoothness comes from the eighth-width
// block glyphs `▏▎▍▌▋▊▉█`: each cell is one eighth wider than the last, so the bar's right edge
// advances sub-cell rather than jumping a whole character at a time.
object ProgressBar:
  val width: Int = 40

  // Eighth-width left blocks, `1/8` through `8/8`; index `n` is `(n + 1)` eighths filled.
  private val partials: Text = t"▏▎▍▌▋▊▉█"

  private val foreground: Fg = Fg(rgb"#ff7d26")
  private val background: Bg = Bg(rgb"#3b1700")

  // Renders `fraction` (clamped to `[0, 1]`) as a `width`-cell bar. The bar's background is
  // `#3b1700` throughout; filled cells are `#ff7d26` full blocks; the single boundary cell is a
  // partial block in the foreground colour on the background colour, so its left portion reads as
  // filled and its right portion as empty. Trailing cells are spaces (showing the background).
  // Cell accounting is exact — `full + (partial ? 1 : 0) + trailing == width` — so the bar never
  // changes width as it fills.
  def render(fraction: Double): Teletype =
    val eighths: Int = (fraction.max(0.0).min(1.0)*width*8).toInt
    val full: Int = eighths/8
    val remainder: Int = eighths%8

    val head: Text =
      if remainder == 0 then t""
      else partials.at(Ordinal.zerary(remainder - 1)).let(_.show).or(t"")

    val used: Int = full + (if remainder == 0 then 0 else 1)
    val bar: Text = t"█"*full + head + t" "*(width - used)

    e"$background(${foreground}($bar))"
