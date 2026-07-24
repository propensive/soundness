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

// A hash-chain match finder over a fully-buffered input. Every position is keyed by a hash of its
// four leading bytes; positions sharing a hash are threaded newest-first through `chain`, so a
// search walks recent candidates in the dictionary window up to a bounded depth — the hash-chain
// (HC4) family used by the fast LZMA presets — simple and correct, trading some ratio for speed.
private[pneumatic] final class HashChain(data: Array[Byte], dictSize: Int, depth: Int):
  private val length = data.length
  private val hashBits = 17
  private val hashSize = 1 << hashBits
  private val head = new Array[Int](hashSize)
  private val chain = new Array[Int](if length > 0 then length else 1)

  private var i = 0
  while i < hashSize do { head(i) = -1; i += 1 }

  private def hash(pos: Int): Int =
    val value = (data(pos) & 0xff) | ((data(pos + 1) & 0xff) << 8) |
      ((data(pos + 2) & 0xff) << 16) | ((data(pos + 3) & 0xff) << 24)

    (value*0x9e3779b1) >>> (32 - hashBits)

  def insert(pos: Int): Unit =
    if pos + 4 <= length then
      val h = hash(pos)
      chain(pos) = head(h)
      head(h) = pos

  // The longest match at `pos`, as (length, distanceValue) where the LZMA distance is one less than
  // the byte offset. Returns length 0 if nothing usable is found.
  def find(pos: Int, lenLimit: Int): Long =
    if pos + 4 > length || lenLimit < MatchLenMin then 0L else
      var bestLen = 0
      var bestDist = 0
      var candidate = head(hash(pos))
      var remaining = depth

      while candidate >= 0 && (pos - candidate) <= dictSize && remaining > 0 do
        var len = 0
        while len < lenLimit && data(candidate + len) == data(pos + len) do len += 1

        if len > bestLen then
          bestLen = len
          bestDist = pos - candidate - 1
          if len >= lenLimit then remaining = 1

        candidate = chain(candidate)
        remaining -= 1

      (bestLen.toLong << 32) | (bestDist.toLong & 0xffffffffL)

  // The full candidate set at `pos`: for each length reachable (strictly increasing), the nearest
  // distance achieving it, so the caller can weigh the length/distance trade-off by price. Results
  // fill `outLen`/`outDist`; the return value is the count. Read-only (does not insert `pos`).
  def findAll(pos: Int, lenLimit: Int, outLen: Array[Int], outDist: Array[Int]): Int =
    if pos + 4 > length || lenLimit < MatchLenMin then 0 else
      var count = 0
      var maxLen = MatchLenMin - 1
      var candidate = head(hash(pos))
      var remaining = depth

      while candidate >= 0 && (pos - candidate) <= dictSize && remaining > 0 do
        var len = 0
        while len < lenLimit && data(candidate + len) == data(pos + len) do len += 1

        if len > maxLen then
          outLen(count) = len
          outDist(count) = pos - candidate - 1
          count += 1
          maxLen = len
          if len >= lenLimit then remaining = 1

        candidate = chain(candidate)
        remaining -= 1

      count

// The length model encoder: a choice bit chooses the low/mid/high range, then a bit-tree codes the
// symbol within it. Lengths are offset by `MatchLenMin`.
private[pneumatic] final class LengthEncoder extends LengthCoder:
  def encode(rc: RangeEncoder, len: Int, posState: Int): Unit =
    val value = len - MatchLenMin

    if value < LenLowSymbols then
      rc.encodeBit(choice, 0, 0)
      rc.encodeBitTree(low(posState), value)
    else
      rc.encodeBit(choice, 0, 1)
      val mid0 = value - LenLowSymbols

      if mid0 < LenMidSymbols then
        rc.encodeBit(choice, 1, 0)
        rc.encodeBitTree(mid(posState), mid0)
      else
        rc.encodeBit(choice, 1, 1)
        rc.encodeBitTree(high, mid0 - LenMidSymbols)

// The LZMA symbol encoder. It walks the input once, at each step choosing between a literal, an
// ordinary match, and — in normal mode — a repeat-match reusing one of the four recent distances
// (understood by the decoder, and far cheaper in bits than a fresh distance). Every emitted symbol
// keeps `state` and `reps` in step with what the decoder will reconstruct, so matched-context
// literals stay synchronised.
//
// Two strategies: `fast` (presets 0..3) greedily takes the longest match, subject to a
// distance-dependent minimum length that keeps short far matches from costing more than they save;
// `normal` (presets 4..9) additionally prefers repeat-matches when they are competitive and applies
// one-step lazy evaluation, deferring a match when the next position offers a longer one. Both are
// always valid; normal simply compresses better.
private[pneumatic] final class LzmaEncoder
  ( data: Array[Byte], rc: RangeEncoder, lc: Int, lp: Int, pb: Int, finder: HashChain,
    niceLen: Int, normal: Boolean )
extends LzmaCoder(lc, lp, pb):

  private val length = data.length
  private val matchLengthEncoder = LengthEncoder()
  private val repLengthEncoder = LengthEncoder()
  private var readPos = 0

  // Reusable candidate buffers for the price-based normal parser.
  private val candidateLen = new Array[Int](MatchLenMax + 1)
  private val candidateDist = new Array[Int](MatchLenMax + 1)

  reset()

  override def reset(): Unit =
    super.reset()
    matchLengthEncoder.reset()
    repLengthEncoder.reset()

  def pos: Int = readPos
  def hasMore: Boolean = readPos < length

  private def byteAt(dist: Int): Int = data(readPos - dist - 1) & 0xff

  private def distSlot(dist: Int): Int =
    if dist < DistModelStart then dist
    else
      val n = 31 - java.lang.Integer.numberOfLeadingZeros(dist)
      (n << 1) | ((dist >>> (n - 1)) & 1)

  private def minLenFor(dist: Int): Int =
    if dist >= 32768 then 4 else if dist >= 512 then 3 else 2

  // The match length obtainable at repeat distance `repDist` from `at`, capped at `lenLimit`.
  private def repMatchLen(at: Int, repDist: Int, lenLimit: Int): Int =
    val back = at - repDist - 1

    if back < 0 then 0 else
      var l = 0
      while l < lenLimit && data(back + l) == data(at + l) do l += 1
      l

  // The best of the four repeat distances at `at`: the packed (length << 32 | index).
  private def bestRep(at: Int, lenLimit: Int): Long =
    var bestLen = 0
    var bestIndex = 0
    var i = 0

    while i < Reps do
      val l = repMatchLen(at, reps(i), lenLimit)
      if l > bestLen then { bestLen = l; bestIndex = i }
      i += 1

    (bestLen.toLong << 32) | bestIndex.toLong

  def encodeSymbol(): Int =
    val posState = readPos & posMask
    val lenLimit = if length - readPos < MatchLenMax then length - readPos else MatchLenMax

    if normal then
      // Gather the candidate set (read-only) before inserting this position, so it is never a
      // candidate for itself.
      val count = finder.findAll(readPos, lenLimit, candidateLen, candidateDist)
      finder.insert(readPos)
      encodeNormal(posState, lenLimit, count)
    else
      val found = finder.find(readPos, lenLimit)
      finder.insert(readPos)
      encodeFast(posState, (found >>> 32).toInt, (found & 0xffffffffL).toInt)

  private def emitMatch(dist: Int, len: Int, posState: Int): Int =
    rc.encodeBit(isMatch(state.get), posState, 1)
    rc.encodeBit(isRep, state.get, 0)
    encodeMatch(dist, len, posState)
    advance(len)

  private def emitRep(index: Int, len: Int, posState: Int): Int =
    rc.encodeBit(isMatch(state.get), posState, 1)
    rc.encodeBit(isRep, state.get, 1)
    encodeRepMatch(index, len, posState)
    advance(len)

  private def emitLiteral(posState: Int): Int =
    rc.encodeBit(isMatch(state.get), posState, 0)
    encodeLiteral()
    readPos += 1
    1

  private def advance(len: Int): Int =
    var k = 1
    while k < len do { finder.insert(readPos + k); k += 1 }
    readPos += len
    len

  private def encodeFast(posState: Int, matchLen: Int, matchDist: Int): Int =
    if matchLen >= MatchLenMin && matchLen >= minLenFor(matchDist) then
      emitMatch(matchDist, matchLen, posState)
    else
      emitLiteral(posState)

  // Price-based per-step parsing: cost every candidate (literal, each repeat-match, and each
  // fresh-match length/distance trade-off from the finder) in fixed-point bits using the current
  // probability model, and take the option with the lowest cost-per-byte, applying one-step lazy
  // evaluation to fresh matches. Reps are cheap, so they win whenever competitive.
  private def encodeNormal(posState: Int, lenLimit: Int, count: Int): Int =
    val rep = bestRep(readPos, lenLimit)
    val repLen = (rep >>> 32).toInt
    val repIndex = (rep & 0xffffffffL).toInt

    // Start from the literal option; matches must beat it on cost-per-byte.
    var bestKind = 0 // 0 literal, 1 match, 2 rep
    var bestLen = 1
    var bestArg = 0
    var bestCost = literalPrice(readPos)

    if repLen >= MatchLenMin then
      val cost = repPrice(repIndex, repLen, posState)

      if cheaperPerByte(cost, repLen, bestCost, bestLen) then
        bestKind = 2; bestLen = repLen; bestArg = repIndex; bestCost = cost

    var k = 0

    while k < count do
      val len = candidateLen(k)
      val dist = candidateDist(k)

      if len >= MatchLenMin && len >= minLenFor(dist) then
        val cost = matchPrice(dist, len, posState)

        if cheaperPerByte(cost, len, bestCost, bestLen) then
          bestKind = 1; bestLen = len; bestArg = dist; bestCost = cost

      k += 1

    if bestKind == 0 then emitLiteral(posState)
    else if bestKind == 1 && bestLen < niceLen && lazyDefer(bestLen) then emitLiteral(posState)
    else if bestKind == 2 then emitRep(bestArg, bestLen, posState)
    else emitMatch(bestArg, bestLen, posState)

  // Is (costA over lenA) strictly cheaper per byte than (costB over lenB)? Cross-multiplied to keep
  // the comparison exact in integers.
  private def cheaperPerByte(costA: Int, lenA: Int, costB: Int, lenB: Int): Boolean =
    costA.toLong*lenB < costB.toLong*lenA

  // One-step lazy evaluation: if the next position offers a strictly longer fresh match, defer by
  // emitting a literal now. `readPos` has already been inserted, so the lookahead sees it.
  private def lazyDefer(currentLen: Int): Boolean =
    val next = readPos + 1

    if next + 4 > length then false else
      val nextLimit = if length - next < MatchLenMax then length - next else MatchLenMax
      val nextFound = finder.find(next, nextLimit)
      (nextFound >>> 32).toInt > currentLen

  private inline def priceBit(probs: Array[Short], index: Int, bit: Int): Int =
    RangeCoder.bitPrice(probs(index).toInt, bit)

  private def lengthPrice(coder: LengthEncoder, len: Int, posState: Int): Int =
    val l = len - MatchLenMin

    if l < LenLowSymbols then
      priceBit(coder.choice, 0, 0) + RangeCoder.bitTreePrice(coder.low(posState), l)
    else if l < LenLowSymbols + LenMidSymbols then
      priceBit(coder.choice, 0, 1) + priceBit(coder.choice, 1, 0) +
        RangeCoder.bitTreePrice(coder.mid(posState), l - LenLowSymbols)
    else
      priceBit(coder.choice, 0, 1) + priceBit(coder.choice, 1, 1) +
        RangeCoder.bitTreePrice(coder.high, l - LenLowSymbols - LenMidSymbols)

  private def distancePrice(dist: Int, lenState: Int): Int =
    val slot = distSlot(dist)
    var price = RangeCoder.bitTreePrice(distSlots(lenState), slot)

    if slot >= DistModelStart then
      val footerBits = (slot >> 1) - 1
      val base = (2 | (slot & 1)) << footerBits

      if slot < DistModelEnd then
        price += RangeCoder.bitTreeReversePrice(distSpecial(slot - DistModelStart), dist - base)
      else
        price += RangeCoder.directBitsPrice(footerBits - AlignBits)
        price += RangeCoder.bitTreeReversePrice(distAlign, (dist - base) & AlignMask)

    price

  private def matchPrice(dist: Int, len: Int, posState: Int): Int =
    priceBit(isMatch(state.get), posState, 1) + priceBit(isRep, state.get, 0) +
      lengthPrice(matchLengthEncoder, len, posState) + distancePrice(dist, distState(len))

  private def repPrice(index: Int, len: Int, posState: Int): Int =
    var price = priceBit(isMatch(state.get), posState, 1) + priceBit(isRep, state.get, 1)

    if index == 0 then
      price += priceBit(isRep0, state.get, 0) + priceBit(isRep0Long(state.get), posState, 1)
    else
      price += priceBit(isRep0, state.get, 1)

      if index == 1 then price += priceBit(isRep1, state.get, 0)
      else
        price += priceBit(isRep1, state.get, 1) +
          priceBit(isRep2, state.get, if index == 2 then 0 else 1)

    price + lengthPrice(repLengthEncoder, len, posState)

  private def literalPrice(at: Int): Int =
    val prevByte = if at > 0 then data(at - 1) & 0xff else 0
    val base = literalIndex(prevByte, at)
    val target = data(at) & 0xff
    var price = 0
    var context = 1

    if state.isLiteral then
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1
        price += priceBit(literalProbs, base + context, bit)
        context = (context << 1) | bit
        i -= 1
    else
      var matchByte = (data(at - reps(0) - 1) & 0xff) << 1
      var stillMatched = true
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1

        if stillMatched then
          val matchBit = (matchByte >>> 8) & 1
          matchByte <<= 1
          price += priceBit(literalProbs, base + ((1 + matchBit) << 8) + context, bit)
          if matchBit != bit then stillMatched = false
        else
          price += priceBit(literalProbs, base + context, bit)

        context = (context << 1) | bit
        i -= 1

    price

  private def encodeLiteral(): Unit =
    val prevByte = if readPos > 0 then data(readPos - 1) & 0xff else 0
    val base = literalIndex(prevByte, readPos)
    val target = data(readPos) & 0xff
    var context = 1

    if state.isLiteral then
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1
        rc.encodeBit(literalProbs, base + context, bit)
        context = (context << 1) | bit
        i -= 1
    else
      var matchByte = byteAt(reps(0)) << 1
      var stillMatched = true
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1

        if stillMatched then
          val matchBit = (matchByte >>> 8) & 1
          matchByte <<= 1
          rc.encodeBit(literalProbs, base + ((1 + matchBit) << 8) + context, bit)
          if matchBit != bit then stillMatched = false
        else
          rc.encodeBit(literalProbs, base + context, bit)

        context = (context << 1) | bit
        i -= 1

    state.updateLiteral()

  private def encodeMatch(dist: Int, len: Int, posState: Int): Unit =
    state.updateMatch()
    reps(3) = reps(2)
    reps(2) = reps(1)
    reps(1) = reps(0)
    reps(0) = dist

    matchLengthEncoder.encode(rc, len, posState)

    val slot = distSlot(dist)
    rc.encodeBitTree(distSlots(distState(len)), slot)

    if slot >= DistModelStart then
      val footerBits = (slot >> 1) - 1
      val base = (2 | (slot & 1)) << footerBits

      if slot < DistModelEnd then
        rc.encodeBitTreeReverse(distSpecial(slot - DistModelStart), dist - base)
      else
        rc.encodeDirectBits((dist - base) >>> AlignBits, footerBits - AlignBits)
        rc.encodeBitTreeReverse(distAlign, (dist - base) & AlignMask)

  // Encode a repeat-match reusing recent distance `index` (0..3), length `len` (at least 2, never
  // a short-rep). The rep-index bits and the reps rotation mirror the decoder's `decodeRepMatch`
  // exactly, and all sub-bits are coded against the pre-update `state`.
  private def encodeRepMatch(index: Int, len: Int, posState: Int): Unit =
    if index == 0 then
      rc.encodeBit(isRep0, state.get, 0)
      rc.encodeBit(isRep0Long(state.get), posState, 1)
    else
      rc.encodeBit(isRep0, state.get, 1)

      if index == 1 then rc.encodeBit(isRep1, state.get, 0)
      else
        rc.encodeBit(isRep1, state.get, 1)
        rc.encodeBit(isRep2, state.get, if index == 2 then 0 else 1)

      val dist = reps(index)
      var j = index
      while j > 0 do { reps(j) = reps(j - 1); j -= 1 }
      reps(0) = dist

    state.updateRep()
    repLengthEncoder.encode(rc, len, posState)
