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
┃    Soundness, version 0.54.0.                                                                    ┃
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

// How an inline block relocates when the terminal is resized. `InlineRoot` reads
// this from context; the default (`TopAfterResize`) is provided in the companion,
// and a caller overrides it by importing one alternative from `inlineAnchoring`.
enum InlineAnchoring:
  case BottomDocked    // never switch; clear and re-dock on every resize
  case TopAnchored     // pinned to rows 1..height from the first frame
  case TopAfterResize  // bottom-docked, then top-anchored after the first resize
  case Fullscreen      // take over the alternate screen buffer, top-anchored

object InlineAnchoring:
  given default: InlineAnchoring = TopAfterResize

package inlineAnchoring:
  given bottomDocked: InlineAnchoring = InlineAnchoring.BottomDocked
  given topAnchored: InlineAnchoring = InlineAnchoring.TopAnchored
  given topAfterResize: InlineAnchoring = InlineAnchoring.TopAfterResize
  given fullscreen: InlineAnchoring = InlineAnchoring.Fullscreen

// What happens when a frame is taller than the last while bottom-docked. The
// default (`ScrollIntoScrollback`) preserves the historic behaviour.
enum InlineGrowth:
  case ScrollIntoScrollback  // scroll the screen up, pushing rows into scrollback
  case ClampToScreen         // grow upward in place, overwriting the rows above

object InlineGrowth:
  given default: InlineGrowth = ScrollIntoScrollback

package inlineGrowth:
  given scrollIntoScrollback: InlineGrowth = InlineGrowth.ScrollIntoScrollback
  given clampToScreen: InlineGrowth = InlineGrowth.ClampToScreen

// What happens when a frame is shorter than the last. The default
// (`RedockBottom`) preserves the historic behaviour.
enum InlineShrink:
  case RedockBottom  // re-dock at the bottom, clearing the rows vacated above
  case KeepTop       // hold the top row and clear below (no re-dock gap)

object InlineShrink:
  given default: InlineShrink = RedockBottom

package inlineShrink:
  given redockBottom: InlineShrink = InlineShrink.RedockBottom
  given keepTop: InlineShrink = InlineShrink.KeepTop
