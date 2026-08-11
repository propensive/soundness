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

object GaugePalette:
  // The `rgb"…"` interpolator yields a `Chroma` — a packed 24-bit integer — whereas a palette's
  // roles are `Color in Srgb`, so hex literals are converted here rather than at every one of the
  // hundred-odd places a palette names a colour.
  def hue(chroma: Chroma): Color in Srgb =
    Srgb(((chroma.red)&255)/255.0, chroma.green/255.0, chroma.blue/255.0)

  // The no-import default, chosen from what the terminal can actually render: a caller who imports
  // nothing gets colour that works where they are, and a caller who names a palette has asserted
  // they know what their terminal does. Any `import palettes.…` outranks this, because a
  // lexically-scoped given beats a companion one.
  given adaptive: (termcap: Termcap) => GaugePalette = termcap.color match
    // Qualified: `import iridescence.*` above brings `iridescence.palettes` into scope under the
    // same name. The two blocks merge in the `soundness` umbrella, so a user still writes
    // `import palettes.emberGaugePalette` and gets this one.
    case ColorDepth.NoColor | ColorDepth.Indexed8 => ultimatum.palettes.monochromeGaugePalette
    case ColorDepth.Indexed16 | ColorDepth.Cube4  => ultimatum.palettes.ansiSixteenGaugePalette
    case ColorDepth.Cube6 | ColorDepth.TrueColor  => ultimatum.palettes.slateGaugePalette

// The colours a gauge draws with, named by the role each plays rather than by hue, so that one
// design renders under any palette and one palette serves every design.
// A real trait, not a structural refinement of `Palette`: structural member selection goes through
// `iridescence.Palette.selectDynamic` — runtime reflection, which Scala Native does not support —
// whereas these are ordinary virtual calls. `chiaroscuro.JuxtapositionPalette` and
// `probably.TestPalette` are declared this way for the same reason.
// Extending `Palette` brings `background`, `foreground`, `subdue`, `accent` and the pairwise `mix`
// with it, which is how most palettes below derive their track from their fill.
trait GaugePalette extends Palette:
  type Form = Srgb

  // The unfilled part of a bar, the hollow pips, the arc a dial's needle sweeps.
  def track: Color in Srgb

  // The filled part.
  def fill: Color in Srgb

  // The boundary cell of a bar and the head of a marker: brighter than `fill`, so that the bar
  // reads as moving even in a still screenshot.
  def leadingEdge: Color in Srgb

  // Labels and captions.
  def caption: Color in Srgb

  // Present but not competing: completed steps, elapsed times, units.
  def muted: Color in Srgb

  def success: Color in Srgb
  def warning: Color in Srgb
  def danger: Color in Srgb

  // The fill interpolated along a bar's length — `position` is 0 at the left edge and 1 at the
  // right. Only a gradient design consults it; the default is the plain two-stop ramp from the
  // fill to the leading edge.
  def lengthwise: Gradient = Gradient(fill, leadingEdge)

  // The fill as a function of the *value* rather than the position: held at `success` across the
  // lower half, then climbing through `warning` to `danger`. What a battery or a load meter uses.
  def severity: Gradient = Gradient(success, success, warning, danger)

  // One background per step, for a ribbon of stages.
  def steps(count: Int): Sequence[Color in Srgb] =
    Sequence.from:
      (0 until count).map: index =>
        lengthwise(if count <= 1 then 0.0 else index.toDouble/(count - 1))

  def colorOf(standing: Standing): Color in Srgb = standing match
    case Standing.Succeeded => success
    case Standing.Failed    => danger
    case Standing.Warned    => warning
    case Standing.Skipped   => muted
    case Standing.Pending   => track
    case Standing.Running   => leadingEdge
