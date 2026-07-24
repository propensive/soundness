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

import Lzma.*

// The length decoder: a choice bit selects the low (8), mid (8) or high (256) symbol range, each a
// bit-tree; low/mid are indexed by position-state.
private[pneumatic] final class LengthDecoder extends LengthCoder:
  def decode(rc: RangeDecoder, posState: Int): Int =
    if rc.decodeBit(choice, 0) == 0 then rc.decodeBitTree(low(posState)) + MatchLenMin
    else if rc.decodeBit(choice, 1) == 0 then
      rc.decodeBitTree(mid(posState)) + MatchLenMin + LenLowSymbols
    else
      rc.decodeBitTree(high) + MatchLenMin + LenLowSymbols + LenMidSymbols

// The LZMA symbol decoder: reads literals and matches from the range decoder, writing bytes (and
// match copies) into the sliding window. Driven a window's-worth at a time by the LZMA2 layer,
// which bounds output via `lz.setLimit`.
private[pneumatic] final class LzmaDecoder
  ( lz: LzDecoder, rc: RangeDecoder, lc: Int, lp: Int, pb: Int )
extends LzmaCoder(lc, lp, pb):

  private val matchLengthDecoder = LengthDecoder()
  private val repLengthDecoder = LengthDecoder()

  reset()

  override def reset(): Unit =
    super.reset()
    matchLengthDecoder.reset()
    repLengthDecoder.reset()

  def endMarkerDetected: Boolean = reps(0) == -1

  // Decode until the window has no more space (the LZMA2 chunk's uncompressed length is reached, or
  // the buffer is full and needs flushing). Any match left dangling at the limit is finished first.
  def decode(): Unit =
    lz.repeatPending()

    while lz.hasSpace do
      val posState = lz.pos & posMask

      if rc.decodeBit(isMatch(state.get), posState) == 0 then decodeLiteral()
      else
        val len =
          if rc.decodeBit(isRep, state.get) == 0 then decodeMatch(posState)
          else decodeRepMatch(posState)

        lz.repeat(reps(0), len)

  private def decodeLiteral(): Unit =
    val prevByte = lz.getByte(0)
    val base = literalIndex(prevByte, lz.pos)
    var symbol = 1

    if state.isLiteral then
      while symbol < 0x100 do symbol = (symbol << 1) | rc.decodeBit(literalProbs, base + symbol)
    else
      var matchByte = lz.getByte(reps(0)) << 1
      var continue = true

      while continue && symbol < 0x100 do
        val matchBit = (matchByte >> 8) & 1
        matchByte <<= 1
        val bit = rc.decodeBit(literalProbs, base + ((1 + matchBit) << 8) + symbol)
        symbol = (symbol << 1) | bit
        if matchBit != bit then continue = false

      while symbol < 0x100 do symbol = (symbol << 1) | rc.decodeBit(literalProbs, base + symbol)

    lz.putByte(symbol.toByte)
    state.updateLiteral()

  private def decodeMatch(posState: Int): Int =
    state.updateMatch()
    reps(3) = reps(2)
    reps(2) = reps(1)
    reps(1) = reps(0)

    val len = matchLengthDecoder.decode(rc, posState)
    val distSlot = rc.decodeBitTree(distSlots(distState(len)))

    if distSlot < DistModelStart then reps(0) = distSlot
    else
      val footerBits = (distSlot >> 1) - 1
      reps(0) = (2 | (distSlot & 1)) << footerBits

      if distSlot < DistModelEnd then
        reps(0) += rc.decodeBitTreeReverse(distSpecial(distSlot - DistModelStart))
      else
        reps(0) += rc.decodeDirectBits(footerBits - AlignBits) << AlignBits
        reps(0) += rc.decodeBitTreeReverse(distAlign)

    len

  private def decodeRepMatch(posState: Int): Int =
    if rc.decodeBit(isRep0, state.get) == 0 then
      if rc.decodeBit(isRep0Long(state.get), posState) == 0 then
        state.updateShortRep()
        return 1
    else
      val distance =
        if rc.decodeBit(isRep1, state.get) == 0 then reps(1)
        else if rc.decodeBit(isRep2, state.get) == 0 then
          val d = reps(2)
          reps(2) = reps(1)
          d
        else
          val d = reps(3)
          reps(3) = reps(2)
          reps(2) = reps(1)
          d

      reps(1) = reps(0)
      reps(0) = distance

    state.updateRep()
    repLengthDecoder.decode(rc, posState)
