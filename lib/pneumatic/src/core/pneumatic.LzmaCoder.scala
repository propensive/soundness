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
package pneumatic

import proscenium.compat.*

// Shared LZMA model constants: the standard LZMA parameters (match lengths 2..273, 12 states,
// 4 reps, 64 distance slots, a 16-entry alignment tree), documented in the LZMA specification.
// The encoder and decoder build their probability models from the same constants, so the model
// shapes are identical; only the direction of coding differs.
private[pneumatic] object Lzma:
  inline val PosStatesMax = 1 << 4

  inline val MatchLenMin = 2
  inline val LenLowSymbols = 1 << 3  // 8
  inline val LenMidSymbols = 1 << 3  // 8
  inline val LenHighSymbols = 1 << 8 // 256
  inline val MatchLenMax = MatchLenMin + LenLowSymbols + LenMidSymbols + LenHighSymbols - 1 // 273

  inline val DistStates = 4
  inline val DistSlots = 1 << 6      // 64
  inline val DistModelStart = 4
  inline val DistModelEnd = 14
  inline val FullDistances = 1 << (DistModelEnd / 2) // 128
  inline val AlignBits = 4
  inline val AlignSize = 1 << AlignBits // 16
  inline val AlignMask = AlignSize - 1

  inline val Reps = 4
  inline val States = 12

  // The `distSpecial` reverse-trees, one per distance slot in `DistModelStart..<DistModelEnd`,
  // flattened into a single probability array: tree `i` (`distSlot - DistModelStart`) has size
  // `2 << (i/2)` and starts at `DistSpecialOffsets(i)`.
  inline val DistSpecialTotal = 124
  val distSpecialOffsets: IArray[Int] =
    IArray.unsafeFromArray(scala.Array(0, 2, 4, 8, 12, 20, 28, 44, 60, 92))

  def distSpecialSize(index: Int): Int = 2 << (index/2)

  def distState(len: Int): Int =
    if len < DistStates + MatchLenMin then len - MatchLenMin else DistStates - 1

