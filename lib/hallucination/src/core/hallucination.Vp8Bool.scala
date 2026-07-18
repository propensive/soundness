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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package hallucination

import anticipation.*

// The VP8 boolean entropy decoder (RFC 6386 §7). This is the canonical bit-exact algorithm; the
// reference (image-rs/image-webp `src/lossy/arithmetic_decoder.rs`) uses a faster but equivalent
// chunked implementation. Reads past the end of the partition yield zero bytes, as libwebp allows.
private[hallucination] final class Vp8Bool(data: Data, start: Int, end: Int):
  private var position = start
  private var value = ((byte(start) << 8) | byte(start + 1))
  private var range = 255
  private var bitCount = 0

  locally { position = start + 2 }

  private inline def byte(index: Int): Int = if index < end then data(index) & 0xff else 0

  def bool(probability: Int): Boolean =
    val split = 1 + (((range - 1)*probability) >> 8)
    val bigSplit = split << 8

    val result =
      if value >= bigSplit then
        range -= split
        value -= bigSplit
        true
      else
        range = split
        false

    while range < 128 do
      value <<= 1
      range <<= 1
      bitCount += 1

      if bitCount == 8 then
        bitCount = 0
        value |= byte(position)
        position += 1

    result

  def flag: Boolean = bool(128)

  def literal(bits: Int): Int =
    var value = 0
    var i = 0

    while i < bits do
      value = (value << 1) | (if flag then 1 else 0)
      i += 1

    value

  // An optional signed value: a flag, then (if set) an `bits`-wide magnitude and a sign.
  def optionalSigned(bits: Int): Int =
    if !flag then 0 else
      val magnitude = literal(bits)

      if flag then -magnitude else magnitude

  // Walks a token tree: `tree` holds branch targets in sibling pairs (positive = next node index
  // ×2, non-positive = negated leaf value); `probs`/`probOffset` give the per-node probability.
  // `startPosition` seeds the walk (2 skips the first decision — used after a zero coefficient).
  def tree(tree: Array[Int], probs: Array[Int], probOffset: Int, startPosition: Int): Int =
    var position = startPosition

    while
      val bit = if bool(probs(probOffset + (position >> 1))) then 1 else 0
      position = tree(position + bit)
      position > 0

    do ()

    -position

  def tree(tree: Array[Int], probs: Array[Int], probOffset: Int): Int =
    this.tree(tree, probs, probOffset, 0)
