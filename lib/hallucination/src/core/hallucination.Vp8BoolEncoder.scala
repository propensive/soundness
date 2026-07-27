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
package hallucination

import scala.math
import proscenium.compat.*

import java.io as ji

import anticipation.*
import rudiments.*
import vacuous.*

import scala.caps

// The VP8 boolean entropy encoder (RFC 6386 §7), ported from image-rs/image-webp
// (`src/lossy/arithmetic_encoder.rs`, MIT/Apache-2.0) — the inverse of `Vp8Bool`.
private[hallucination] final class Vp8BoolEncoder extends caps.Mutable:
  private val out = ji.ByteArrayOutputStream()

  private val buffer: scala.collection.mutable.ArrayBuffer[Int] =
    scala.collection.mutable.ArrayBuffer[Int]()

  private var bottom = 0
  private var range = 255
  private var bitCount = 24

  // A carry propagates by incrementing the most recent sub-255 output byte.
  private def addOneToOutput(): Unit =
    var i = buffer.length - 1
    var done = false

    while i >= 0 && !done do
      if buffer(i) < 255 then
        buffer(i) = buffer(i) + 1
        done = true
      else
        i -= 1

  update def writeFlag(flag: Boolean): Unit = writeBool(flag, 128)

  update def writeBool(bit: Boolean, probability: Int): Unit =
    val split = 1 + (((range - 1)*probability) >> 8)

    if bit then
      bottom += split
      range -= split
    else
      range = split

    while range < 128 do
      range <<= 1

      if (bottom & (1 << 31)) != 0 then addOneToOutput()
      bottom <<= 1
      bitCount -= 1

      if bitCount == 0 then
        buffer += (bottom >>> 24) & 0xff
        bottom &= (1 << 24) - 1
        bitCount = 8

  update def writeLiteral(bits: Int, value: Int): Unit =
    var bit = bits - 1

    while bit >= 0 do
      writeBool(((1 << bit) & value) != 0, 128)
      bit -= 1

  update def writeOptionalSigned(bits: Int, value: Optional[Int]): Unit =
    writeFlag(value.present)

    value.let: v =>
      writeLiteral(bits, math.abs(v))
      writeFlag(v >= 0)

  // Encodes a tree value: locate the leaf `−value`, then emit the root-to-leaf bits.
  update def writeTree(tree: Array[Int], probs: Array[Int], probOffset: Int, value: Int): Unit =
    writeTree(tree, probs, probOffset, value, 0)

  update def writeTree
    ( tree: Array[Int], probs: Array[Int], probOffset: Int, value: Int, startIndex: Int )
  :   Unit =

    var current = indexOf(tree, -value)
    val bits = scala.collection.mutable.ArrayBuffer[(Boolean, Int)]()
    var done = false

    while !done do
      if current == startIndex then
        bits += ((false, probs(probOffset + current/2)))
        done = true
      else if current == startIndex + 1 then
        bits += ((true, probs(probOffset + current/2)))
        done = true
      else
        val bit = current % 2 == 1

        if bit then current -= 1
        bits += ((bit, probs(probOffset + current/2)))
        current = indexOf(tree, current)

    var i = bits.length - 1

    while i >= 0 do
      writeBool(bits(i)(0), bits(i)(1))
      i -= 1

  private def indexOf(tree: Array[Int], value: Int): Int =
    var i = 0

    while tree(i) != value do i += 1
    i

  // Flushes the pending bits and returns the encoded bytes.
  update def bytes: Data =
    if (bottom & (1 << (32 - bitCount))) != 0 then addOneToOutput()
    var v = bottom.toLong & 0xffffffffL
    v <<= (bitCount & 0x7)
    var c = (bitCount >> 3) - 1

    while c >= 0 do
      v <<= 8
      c -= 1

    c = 3

    while c >= 0 do
      buffer += ((v >>> 24) & 0xff).toInt
      v = (v << 8) & 0xffffffffL
      c -= 1

    buffer.foreach(out.write(_))
    out.toByteArray.nn.immutable(using Unsafe)
