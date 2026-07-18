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

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import rudiments.*
import vacuous.*

import RasterError.Reason

// The GIF variant of LZW: codes packed least-significant-bit first, with a variable code width
// growing from `minimum + 1` to 12 bits, and `clear`/`end` control codes. (This differs from
// the TIFF/PDF variant pneumatic implements, which packs codes most-significant-bit first with
// an early width change.)
private[hallucination] object GifLzw:
  // Decodes to one palette index per pixel; `length` bounds the output (trailing data beyond
  // the frame's pixel count is ignored, as decoders conventionally do).
  def decode(minimum: Int, data: Data, length: Int): Array[Byte] raises RasterError =
    val clear = 1 << minimum
    val end = clear + 1
    val prefix = new Array[Int](4096)
    val suffix = new Array[Byte](4096)
    val stack = new Array[Byte](4096)
    val output = new Array[Byte](length)

    for index <- 0 until clear do suffix(index) = index.toByte

    var codeSize = minimum + 1
    var available = end + 1
    var oldCode = -1
    var first = 0
    var outdex = 0
    var bits = 0
    var accumulator = 0
    var index = 0

    while outdex < length do
      while bits < codeSize && index < data.length do
        accumulator |= (data(index)&0xff) << bits
        bits += 8
        index += 1

      if bits < codeSize then abort(RasterError(Gif(), Reason.Truncated))

      var code = accumulator&((1 << codeSize) - 1)
      accumulator >>>= codeSize
      bits -= codeSize

      if code == end then outdex = length
      else if code == clear then
        codeSize = minimum + 1
        available = end + 1
        oldCode = -1
      else if oldCode == -1 then
        output(outdex) = suffix(code)
        outdex += 1
        first = suffix(code)&0xff
        oldCode = code
      else
        val inCode = code
        var depth = 0

        if code >= available then
          // The `KwKwK` case: the code is the one about to be defined.
          stack(depth) = first.toByte
          depth += 1
          code = oldCode

        while code > end do
          stack(depth) = suffix(code)
          depth += 1
          code = prefix(code)

        first = suffix(code)&0xff
        stack(depth) = first.toByte
        depth += 1

        while depth > 0 && outdex < length do
          depth -= 1
          output(outdex) = stack(depth)
          outdex += 1

        if available < 4096 then
          prefix(available) = oldCode
          suffix(available) = first.toByte
          available += 1

          if available == (1 << codeSize) && codeSize < 12 then codeSize += 1

        oldCode = inCode

    output

  // Encodes palette indices, including the initial `clear` and final `end` codes; the caller
  // divides the result into GIF data sub-blocks.
  def encode(minimum: Int, indices: Array[Byte]): Data =
    val clear = 1 << minimum
    val end = clear + 1
    val dictionary = scm.HashMap[Int, Int]()
    val output = scm.ArrayBuilder.ofByte()

    var codeSize = minimum + 1
    var available = end + 1
    var accumulator = 0L
    var bits = 0

    def write(code: Int): Unit =
      if available > (1 << codeSize) && codeSize < 12 then codeSize += 1
      accumulator |= code.toLong << bits
      bits += codeSize

      while bits >= 8 do
        output += (accumulator&0xff).toByte
        accumulator >>>= 8
        bits -= 8

    write(clear)

    if indices.length > 0 then
      var prefix = indices(0)&0xff
      var index = 1

      while index < indices.length do
        val next = indices(index)&0xff
        val key = prefix << 8 | next

        if dictionary.contains(key) then prefix = dictionary(key)
        else
          write(prefix)

          if available == 4096 then
            write(clear)
            dictionary.clear()
            codeSize = minimum + 1
            available = end + 1
          else
            dictionary(key) = available
            available += 1

          prefix = next

        index += 1

      write(prefix)

    write(end)
    if bits > 0 then output += (accumulator&0xff).toByte

    output.result().immutable(using Unsafe)
