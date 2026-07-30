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

// LZMA2 encoding options from a preset level 0..9. `dictSize`, `lc`/`lp`/`pb` are recorded in the
// stream (dictionary-size property byte and the LZMA properties byte), so a decoder recovers
// them; `mode`, `niceLen`, `matchFinder` and `depthLimit` steer only the encoder's search and
// do not affect decodability. Presets 0..3 use the fast greedy encoder; 4..9 use the normal
// price-based encoder (cost-optimal per-step decisions with repeat-matches and lazy evaluation).
private[pneumatic] object Lzma2Options:
  inline val ModeFast = 1
  inline val ModeNormal = 2

  inline val MatchFinderHc4 = 0x04
  inline val MatchFinderBt4 = 0x14

  inline val NiceLenMin = 2
  inline val NiceLenMax = 273

  inline val DictSizeMin = 4096
  inline val DictSizeMax = 768 << 20 // 768 MiB

  inline val LcDefault = 3
  inline val LpDefault = 0
  inline val PbDefault = 2

  private val presetDictSizes: Array[Int]^{} =
    Array.unsafeFrozen:
      scala.Array(
        1 << 18, 1 << 20, 1 << 21, 1 << 22, 1 << 22, 1 << 23, 1 << 23, 1 << 24, 1 << 25, 1 << 26)

  private val fastDepths: Array[Int]^{} =
    Array.unsafeFrozen:
      scala.Array(4, 8, 24, 48)

  def preset(level0: Int): Lzma2Options =
    val level = if level0 < 0 then 0 else if level0 > 9 then 9 else level0
    val dictSize = presetDictSizes(level)

    if level <= 3 then
      val niceLen = if level <= 1 then 128 else 273

      Lzma2Options(dictSize, LcDefault, LpDefault, PbDefault, ModeFast, niceLen, MatchFinderHc4,
          fastDepths(level))
    else
      val niceLen = if level == 4 then 16 else if level == 5 then 32 else 64

      Lzma2Options(dictSize, LcDefault, LpDefault, PbDefault, ModeNormal, niceLen, MatchFinderBt4,
          0)

  // The LZMA properties byte packs lc/lp/pb: p = (pb*5 + lp)*9 + lc, with lc+lp <= 4.
  def propertiesByte(lc: Int, lp: Int, pb: Int): Int = (pb*5 + lp)*9 + lc

  def decodeProperties(byte: Int): (Int, Int, Int) =
    if byte < 0 || byte > (4*5 + 4)*9 + 8 then
      throw IllegalStateException("the LZMA data is corrupt: invalid properties byte")

    var props = byte
    val lc = props % 9
    props /= 9
    val lp = props % 5
    val pb = props / 5

    if lc + lp > 4 then
      throw IllegalStateException("the LZMA data is corrupt: lc + lp exceeds 4")

    (lc, lp, pb)

  // The LZMA2 dictionary-size property byte: the smallest `i` whose size covers `dictSize`.
  def dictSizeToByte(dictSize: Int): Int =
    var i = 0

    while i < 40 do
      if ((2 | (i & 1)).toLong << (i / 2 + 11)) >= (dictSize.toLong & 0xffffffffL) then return i
      i += 1

    40

  def byteToDictSize(byte: Int): Int =
    if byte < 0 || byte > 40 then
      throw IllegalStateException("the LZMA data is corrupt: invalid dictionary-size byte")

    if byte == 40 then DictSizeMax
    else
      val size = ((2 | (byte & 1)).toLong << (byte / 2 + 11))
      if size > DictSizeMax then DictSizeMax else size.toInt

private[pneumatic] case class Lzma2Options
  ( dictSize:    Int,
    lc:          Int,
    lp:          Int,
    pb:          Int,
    mode:        Int,
    niceLen:     Int,
    matchFinder: Int,
    depthLimit:  Int )
