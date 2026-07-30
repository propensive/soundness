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
package phoenicia

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// Serialises a table set as an sfnt font file: the header, a directory sorted by tag, and the
// tables themselves, four-byte aligned and zero-padded, with per-table checksums computed and
// head's checksum adjustment set so the whole file sums to the specified constant.
private[phoenicia] object Sfnt:
  def assemble(version: Data, tables: List[(Text, Data)]): Data =
    def padded(length: Int): Int = (length + 3)/4*4

    val sorted = tables.sortBy(_(0).s)
    val count = sorted.length
    val entrySelector = 31 - Integer.numberOfLeadingZeros(count)
    val searchRange = (1 << entrySelector)*16
    val tablesStart = 12 + count*16

    val total = tablesStart + sorted.sumBy: entry => padded(entry(1).length)

    val buffer = new Array[Byte](total)

    def putU16(position: Int, value: Int): Unit =
      buffer(position) = (value >> 8).toByte
      buffer(position + 1) = value.toByte

    def putU32(position: Int, value: Long): Unit =
      buffer(position) = (value >> 24).toByte
      buffer(position + 1) = (value >> 16).toByte
      buffer(position + 2) = (value >> 8).toByte
      buffer(position + 3) = value.toByte

    // The sum of big-endian 32-bit words, over the length rounded up to a word boundary —
    // safe because tables are zero-padded in place.
    def checksum(start: Int, length: Int): Long =
      var sum = 0L
      var position = start
      val end = start + padded(length)

      while position < end do
        val word =
          ((buffer(position) & 0xffL) << 24) | ((buffer(position + 1) & 0xffL) << 16) |
            ((buffer(position + 2) & 0xffL) << 8) | (buffer(position + 3) & 0xffL)

        sum = (sum + word) & 0xffffffffL
        position += 4

      sum

    (0 until 4).each: index => buffer(index) = version(index)

    putU16(4, count)
    putU16(6, searchRange)
    putU16(8, entrySelector)
    putU16(10, count*16 - searchRange)

    var offset = tablesStart
    var headOffset = -1

    sorted.indices.each: index =>
      val (tag, table) = sorted(index)
      val directory = 12 + index*16
      val tagBytes = tag.s.getBytes("US-ASCII").nn

      (0 until 4).each: position => buffer(directory + position) = tagBytes(position)

      System.arraycopy(table.mutable(using Unsafe), 0, buffer, offset, table.length)
      putU32(directory + 4, checksum(offset, table.length))
      putU32(directory + 8, offset.toLong)
      putU32(directory + 12, table.length.toLong)
      if tag == t"head" then headOffset = offset
      offset += padded(table.length)

    // The caller supplies head with its adjustment zeroed, so the directory checksum above
    // is the spec's zero-adjusted one; the adjustment is then patched in afterwards.
    if headOffset >= 0 then putU32(headOffset + 8, (0xb1b0afbaL - checksum(0, total)) & 0xffffffffL)

    buffer.immutable(using Unsafe)
