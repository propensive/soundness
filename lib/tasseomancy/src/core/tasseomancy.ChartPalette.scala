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
package tasseomancy

import denominative.*
import iridescence.*
import prepositional.*
import rudiments.*

object ChartPalette:
  // The no-import default: a light palette with a categorical ramp of eight distinguishable
  // hues. Any `import palettes.…` outranks this, because a lexically-scoped given beats a
  // companion one.
  given slate: ChartPalette = tasseomancy.palettes.slateChartPalette

  // A palette from a theme: its colours, in order, for the series; its foreground for the axes
  // and text, and a blend of the two for the grid.
  def of(theme: Theme { type Form = Srgb }): ChartPalette = new ChartPalette:
    val series: Sequence[Color in Srgb] = theme.colors.to[Sequence]
    def background: Color in Srgb = theme.background
    def foreground: Color in Srgb = theme.foreground
    def axis: Color in Srgb = mix(foreground, background, 0.3)
    def grid: Color in Srgb = mix(foreground, background, 0.85)
    def text: Color in Srgb = foreground

// The colours a chart draws with, named by the role each plays rather than by hue: the series
// ramp, cycled when there are more series than colours; the axes and their gradations; the grid;
// the lettering. A real trait rather than a structural refinement of `Palette`, as `GaugePalette`
// is, so that member selection is a virtual call rather than reflection.
trait ChartPalette extends Palette:
  type Form = Srgb
  def series: Sequence[Color in Srgb]
  def axis: Color in Srgb
  def grid: Color in Srgb
  def text: Color in Srgb

  def color(index: Int): Color in Srgb =
    if series.size == 0 then foreground else Sequence.at(series, index%series.size)
