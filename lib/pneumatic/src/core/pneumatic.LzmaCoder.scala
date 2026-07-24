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

import RangeCoder.{ProbInit, probabilities, resetProbabilities}

// Shared LZMA model constants and the coder base holding every adaptive probability array. The
// encoder and decoder derive from this so their models have identical shapes; only the direction of
// coding differs. These are the standard LZMA parameters (match lengths 2..273, 12 states, 4 reps,
// 64 distance slots, a 16-entry alignment tree), documented in the LZMA specification.
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

  def distState(len: Int): Int =
    if len < DistStates + MatchLenMin then len - MatchLenMin else DistStates - 1

import Lzma.*

// The 12-state LZMA context tracking the recent history of literal/match/rep decisions; its value
// selects which `isMatch`/`isRep*` probabilities apply.
private[pneumatic] final class State:
  private var value: Int = 0

  def get: Int = value
  def reset(): Unit = value = 0
  def set(other: State): Unit = value = other.value
  def isLiteral: Boolean = value < 7

  def updateLiteral(): Unit =
    value = if value < 4 then 0 else if value < 10 then value - 3 else value - 6

  def updateMatch(): Unit = value = if value < 7 then 7 else 10
  def updateRep(): Unit = value = if value < 7 then 8 else 11
  def updateShortRep(): Unit = value = if value < 7 then 9 else 11

// The length model: a two-bit `choice` selecting one of three symbol ranges (8 low, 8 mid, 256
// high), the low/mid trees indexed by position-state. Subclassed for decoding and encoding.
private[pneumatic] abstract class LengthCoder:
  val choice: Array[Short] = new Array[Short](2)
  val low: Array[Array[Short]] = Array.tabulate(PosStatesMax)(_ => probabilities(LenLowSymbols))
  val mid: Array[Array[Short]] = Array.tabulate(PosStatesMax)(_ => probabilities(LenMidSymbols))
  val high: Array[Short] = probabilities(LenHighSymbols)

  def reset(): Unit =
    choice(0) = ProbInit
    choice(1) = ProbInit
    var i = 0
    while i < PosStatesMax do { resetProbabilities(low(i)); resetProbabilities(mid(i)); i += 1 }
    resetProbabilities(high)

// The coder base: every probability array LZMA needs, plus the reps, state and literal-context
// arithmetic. `pb`/`lp`/`lc` fix the position and literal-context masks.
private[pneumatic] abstract class LzmaCoder(lc: Int, lp: Int, pb: Int):
  val posMask: Int = (1 << pb) - 1
  private val literalPosMask: Int = (1 << lp) - 1
  private val literalContextBits: Int = lc

  val reps: Array[Int] = new Array[Int](Reps)
  val state: State = State()

  val isMatch: Array[Array[Short]] = Array.tabulate(States)(_ => probabilities(PosStatesMax))
  val isRep: Array[Short] = probabilities(States)
  val isRep0: Array[Short] = probabilities(States)
  val isRep1: Array[Short] = probabilities(States)
  val isRep2: Array[Short] = probabilities(States)
  val isRep0Long: Array[Array[Short]] = Array.tabulate(States)(_ => probabilities(PosStatesMax))
  val distSlots: Array[Array[Short]] = Array.tabulate(DistStates)(_ => probabilities(DistSlots))

  // One reverse-tree per distance slot from `DistModelStart` up to `DistModelEnd`; sizes double
  // every two slots (2,2,4,4,8,8,16,16,32,32), indexed by `distSlot - DistModelStart`.
  val distSpecial: Array[Array[Short]] = Array(
      probabilities(2), probabilities(2), probabilities(4), probabilities(4),
      probabilities(8), probabilities(8), probabilities(16), probabilities(16),
      probabilities(32), probabilities(32))

  val distAlign: Array[Short] = probabilities(AlignSize)

  // The literal model: 0x300 probabilities per (high bits of the previous byte, low bits of the
  // position) context. Stored flat; `literalIndex` yields the base offset of a context's block.
  val literalProbs: Array[Short] = probabilities(0x300 << (lc + lp))

  def literalIndex(prevByte: Int, position: Int): Int =
    val contextLow = prevByte >>> (8 - literalContextBits)
    val contextHigh = (position & literalPosMask) << literalContextBits
    (contextLow + contextHigh)*0x300

  def reset(): Unit =
    var i = 0
    while i < Reps do { reps(i) = 0; i += 1 }
    state.reset()
    i = 0

    while i < States do
      resetProbabilities(isMatch(i))
      resetProbabilities(isRep0Long(i))
      i += 1

    resetProbabilities(isRep)
    resetProbabilities(isRep0)
    resetProbabilities(isRep1)
    resetProbabilities(isRep2)
    i = 0
    while i < DistStates do { resetProbabilities(distSlots(i)); i += 1 }
    i = 0
    while i < distSpecial.length do { resetProbabilities(distSpecial(i)); i += 1 }
    resetProbabilities(distAlign)
    resetProbabilities(literalProbs)
