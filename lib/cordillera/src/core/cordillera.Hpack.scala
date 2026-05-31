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
package cordillera

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

import H2Error.Reason

// HPACK header-block compression and decompression (RFC 7541). A `Hpack` instance
// owns one direction's dynamic table; a connection keeps one for decoding inbound
// header blocks and one for encoding outbound ones, since the tables evolve
// independently as fields are added.
class Hpack(maxTableSize: Int = 4096):
  private val table = HpackTable(maxTableSize)

  // ─── integer representation (RFC 7541 §5.1) ───────────────────────────────
  //
  // An integer uses the low `prefix` bits of the byte at `data(offset)`; if those
  // are all 1 it continues in subsequent 7-bit groups (low 7 bits, high bit =
  // continuation). Returns the value and the index just past the integer.
  private def readInteger(data: Data, offset: Int, prefix: Int)(using Tactic[H2Error])
  :   (Int, Int) =

    val mask = (1 << prefix) - 1
    val first = data(offset) & mask

    if first < mask then (first, offset + 1) else
      var result = mask
      var shift = 0
      var pos = offset + 1
      var continue = true

      while continue do
        if pos >= data.length then abort(H2Error(Reason.Truncated))
        val byte = data(pos) & 0xff
        result += (byte & 0x7f) << shift
        shift += 7
        pos += 1
        continue = (byte & 0x80) != 0
        if shift > 28 then abort(H2Error(Reason.BadInteger))

      (result, pos)

  // Writes `value` into the low `prefix` bits of `first` (its high bits preserved),
  // with continuation bytes as needed.
  private def writeInteger(builder: scm.ArrayBuilder[Byte], first: Int, prefix: Int, value: Int)
  :   Unit =

    val mask = (1 << prefix) - 1

    if value < mask then builder.addOne((first | value).toByte) else
      builder.addOne((first | mask).toByte)
      var rest = value - mask

      while rest >= 0x80 do
        builder.addOne(((rest & 0x7f) | 0x80).toByte)
        rest >>>= 7

      builder.addOne(rest.toByte)

  // ─── string literal (RFC 7541 §5.2) ───────────────────────────────────────
  //
  // A length-prefixed octet sequence; the prefix's high bit flags Huffman coding.
  private def readString(data: Data, offset: Int)(using Tactic[H2Error]): (Text, Int) =
    val huffman = (data(offset) & 0x80) != 0
    val (length, start) = readInteger(data, offset, 7)
    if start + length > data.length then abort(H2Error(Reason.Truncated))
    val raw = data.slice(start, start + length)
    val decoded = if huffman then Huffman.decode(raw) else raw

    (decoded.utf8, start + length)

  private def writeString(builder: scm.ArrayBuilder[Byte], text: Text): Unit =
    val raw = text.s.getBytes("US-ASCII").nn.immutable(using Unsafe)
    val huffed = Huffman.encode(raw)

    // Use whichever encoding is shorter (RFC permits either); flag Huffman in bit 7.
    if huffed.length < raw.length then
      writeInteger(builder, 0x80, 7, huffed.length)
      builder.addAll(huffed.mutable(using Unsafe))
    else
      writeInteger(builder, 0, 7, raw.length)
      builder.addAll(raw.mutable(using Unsafe))

  // ─── decode a complete header block ────────────────────────────────────────

  def decode(data: Data)(using Tactic[H2Error]): List[HpackEntry] =
    val builder = List.newBuilder[HpackEntry]
    var pos = 0

    def nameValue(index: Int, after: Int): (HpackEntry, Int) =
      // `index == 0` → literal name follows; otherwise name comes from the table.
      val (name, valueStart) =
        if index != 0 then (table.lookup(index).lest(H2Error(Reason.BadIndex(index))).name, after)
        else readString(data, after)

      val (value, next) = readString(data, valueStart)
      (HpackEntry(name, value), next)

    while pos < data.length do
      val byte = data(pos) & 0xff

      if (byte & 0x80) != 0 then
        // Indexed header field (§6.1): whole field from the table.
        val (index, next) = readInteger(data, pos, 7)
        builder += table.lookup(index).lest(H2Error(Reason.BadIndex(index)))
        pos = next
      else if (byte & 0x40) != 0 then
        // Literal with incremental indexing (§6.2.1): adds to the dynamic table.
        val (index, after) = readInteger(data, pos, 6)
        val (entry, next) = nameValue(index, after)
        table.add(entry)
        builder += entry
        pos = next
      else if (byte & 0x20) != 0 then
        // Dynamic table size update (§6.3).
        val (newSize, next) = readInteger(data, pos, 5)
        table.resize(newSize)
        pos = next
      else
        // Literal without indexing (§6.2.2) or never-indexed (§6.2.3): 4-bit prefix,
        // not added to the table either way.
        val (index, after) = readInteger(data, pos, 4)
        val (entry, next) = nameValue(index, after)
        builder += entry
        pos = next

    builder.result()

  // ─── encode a header block ──────────────────────────────────────────────────
  //
  // v1 strategy: emit each field as a literal with incremental indexing and a
  // literal (Huffman-or-raw) name and value. Correct and interoperable; does not
  // yet exploit static-table name matches. Pseudo-headers must already be ordered
  // ahead of regular headers by the caller (RFC 7540 §8.1.2.1).
  def encode(headers: List[HpackEntry]): Data =
    val builder = scm.ArrayBuilder.make[Byte]

    headers.each: header =>
      writeInteger(builder, 0x40, 6, 0)
      writeString(builder, header.name)
      writeString(builder, header.value)
      table.add(header)

    builder.result().immutable(using Unsafe)
