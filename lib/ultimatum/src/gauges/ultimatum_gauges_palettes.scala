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
import iridescence.*
import prepositional.*

// Which colours a gauge draws in — an axis entirely separate from which design it uses, so that
// swapping either is one import and neither disturbs the other.
// These merge into the umbrella's existing `palettes` block (iridescence contributes only traits
// there), so every member is suffixed to keep the names distinct across the whole of `soundness`.
package palettes:
  // Hue-free: the terminal's own two colours and a half-tone between them. The only palette that
  // is honestly correct on a monochrome terminal, and the one to reach for when a gauge's output
  // may be read as text.
  given monochromeGaugePalette: GaugePalette:
    val background  = Srgb(0.0, 0.0, 0.0)
    val foreground  = Srgb(1.0, 1.0, 1.0)
    val track       = Srgb(0.25, 0.25, 0.25)
    val fill        = Srgb(0.75, 0.75, 0.75)
    val leadingEdge = Srgb(1.0, 1.0, 1.0)
    val caption     = Srgb(0.85, 0.85, 0.85)
    val muted       = Srgb(0.45, 0.45, 0.45)
    val success     = Srgb(1.0, 1.0, 1.0)
    val warning     = Srgb(0.7, 0.7, 0.7)
    val danger      = Srgb(1.0, 1.0, 1.0)

  // Only colours a sixteen-colour terminal actually has: each is that terminal's canonical value,
  // so even the 256-cube approximation lands on a shade it can display rather than between two.
  given ansiSixteenGaugePalette: GaugePalette:
    val background  = Srgb(0.0, 0.0, 0.0)
    val foreground  = Srgb(0.898, 0.898, 0.898)
    val track       = Srgb(0.333, 0.333, 0.333)
    val fill        = Srgb(0.0, 0.0, 0.804)
    val leadingEdge = Srgb(0.333, 0.333, 1.0)
    val caption     = Srgb(0.898, 0.898, 0.898)
    val muted       = Srgb(0.333, 0.333, 0.333)
    val success     = Srgb(0.0, 0.804, 0.0)
    val warning     = Srgb(0.804, 0.804, 0.0)
    val danger      = Srgb(0.804, 0.0, 0.0)

  // Solarized's own values, spelled out rather than mixed in from `iridescence.Solarized`: that
  // trait is a `Theme`, and a theme's obligations (a luminosity, a colour list, a spectrum) have
  // nothing to do with a palette's roles.
  given solarizedDarkGaugePalette: GaugePalette:
    val background  = Srgb(0.000, 0.169, 0.212)
    val foreground  = Srgb(0.992, 0.965, 0.890)
    val track       = Srgb(0.027, 0.212, 0.259)
    val fill        = Srgb(0.149, 0.545, 0.824)
    val leadingEdge = Srgb(0.165, 0.631, 0.596)
    val caption     = Srgb(0.576, 0.631, 0.631)
    val muted       = Srgb(0.345, 0.431, 0.459)
    val success     = Srgb(0.522, 0.600, 0.000)
    val warning     = Srgb(0.710, 0.537, 0.000)
    val danger      = Srgb(0.863, 0.196, 0.184)

  given solarizedLightGaugePalette: GaugePalette:
    val background  = Srgb(0.992, 0.965, 0.890)
    val foreground  = Srgb(0.000, 0.169, 0.212)
    val track       = Srgb(0.933, 0.910, 0.835)
    val fill        = Srgb(0.149, 0.545, 0.824)
    val leadingEdge = Srgb(0.424, 0.443, 0.769)
    val caption     = Srgb(0.396, 0.482, 0.514)
    val muted       = Srgb(0.576, 0.631, 0.631)
    val success     = Srgb(0.522, 0.600, 0.000)
    val warning     = Srgb(0.796, 0.294, 0.086)
    val danger      = Srgb(0.863, 0.196, 0.184)

  // The house palette: the repackager's orange on its own deep brown, which is what a Soundness
  // progress bar has always looked like.
  given emberGaugePalette: GaugePalette:
    val background  = rgb"#1a0a00".color
    val foreground  = rgb"#ffe6d0".color
    val track       = rgb"#3b1700".color
    val fill        = rgb"#ff7d26".color
    val leadingEdge = rgb"#ffc16b".color
    val caption     = rgb"#ffd9b3".color
    val muted       = rgb"#8a5230".color
    val success     = rgb"#8fd14f".color
    val warning     = rgb"#ffc857".color
    val danger      = rgb"#e5484d".color

  // Cool and low-contrast, for a gauge that will sit on screen for hours.
  given oceanicGaugePalette: GaugePalette:
    val background  = rgb"#0b1c22".color
    val foreground  = rgb"#d6eef2".color
    val track       = rgb"#123640".color
    val fill        = rgb"#2aa9a3".color
    val leadingEdge = rgb"#7fe3d8".color
    val caption     = rgb"#9fc9d1".color
    val muted       = rgb"#3f6b74".color
    val success     = rgb"#4fd18b".color
    val warning     = rgb"#e8b04b".color
    val danger      = rgb"#ef5f6b".color

  given verdantGaugePalette: GaugePalette:
    val background  = rgb"#0d1a0f".color
    val foreground  = rgb"#e3f2e4".color
    val track       = rgb"#1d3520".color
    val fill        = rgb"#5ab552".color
    val leadingEdge = rgb"#a4e05a".color
    val caption     = rgb"#c2ddc3".color
    val muted       = rgb"#456b47".color
    val success     = rgb"#7ee081".color
    val warning     = rgb"#e6c344".color
    val danger      = rgb"#e05252".color

  given plumGaugePalette: GaugePalette:
    val background  = rgb"#170f1f".color
    val foreground  = rgb"#efe4f7".color
    val track       = rgb"#2d1c3d".color
    val fill        = rgb"#9a5cd0".color
    val leadingEdge = rgb"#d79cf5".color
    val caption     = rgb"#cbb4dc".color
    val muted       = rgb"#5b4270".color
    val success     = rgb"#6fd3a0".color
    val warning     = rgb"#e2b23c".color
    val danger      = rgb"#e2506b".color

  // Cool greys with one blue accent: the fill is the only saturated colour on screen, so the eye
  // goes to the progress and to nothing else.
  given slateGaugePalette: GaugePalette:
    val background  = rgb"#14171c".color
    val foreground  = rgb"#dfe3ea".color
    val track       = rgb"#242a33".color
    val fill        = rgb"#4c8ef7".color
    val leadingEdge = rgb"#8fbcff".color
    val caption     = rgb"#a7b0be".color
    val muted       = rgb"#4a5261".color
    val success     = rgb"#3ecf8e".color
    val warning     = rgb"#e3b341".color
    val danger      = rgb"#f0616d".color

  // Maximum separation between the three report colours, for CI logs read at a glance.
  given signalGaugePalette: GaugePalette:
    val background  = rgb"#000000".color
    val foreground  = rgb"#ffffff".color
    val track       = rgb"#333333".color
    val fill        = rgb"#00b3ff".color
    val leadingEdge = rgb"#7fdcff".color
    val caption     = rgb"#ffffff".color
    val muted       = rgb"#767676".color
    val success     = rgb"#00c853".color
    val warning     = rgb"#ffab00".color
    val danger      = rgb"#ff1744".color
