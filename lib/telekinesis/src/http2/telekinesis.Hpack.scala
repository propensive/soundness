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
package telekinesis

import scala.collection.mutable as scm

import java.nio.charset.StandardCharsets
import anticipation.*
import denominative.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

import Http2.Error.Reason
import scala.caps

// HPACK header-block compression and decompression (RFC 7541). A `Hpack` instance
// owns one direction's dynamic table; a connection keeps one for decoding inbound
// header blocks and one for encoding outbound ones, since the tables evolve
// independently as fields are added.
object Hpack:
  // Builder mutation lives here rather than in class methods, which would force a
  // `uses` clause onto the class itself.
  private[telekinesis] def encodeEntries(headers: List[Entry], table: Table): Data =
    val buf: ByteBuf^ = ByteBuf()

    // A while-loop rather than `each` (or a local recursion): neither a closure nor a local
    // def may capture the exclusive buffer. The cons extractor walks the list in O(1) per
    // step, where `tail` through `Segmentable` would be O(n).
    var rest: List[Entry] = headers
    var continue: Boolean = true

    while continue do rest match
      case Nil =>
        continue = false

      case header :: tail =>
        writeInteger(buf, 0x40, 6, 0)
        writeString(buf, header.name)
        writeString(buf, header.value)
        table.add(header)
        rest = tail

    buf.data

  // Writes `value` into the low `prefix` bits of `first` (its high bits preserved),
  // with continuation bytes as needed.
  private def writeInteger(buf: ByteBuf^, first: Int, prefix: Int, value: Int): Unit =
    val mask = (1 << prefix) - 1

    if value < mask then buf.add((first | value).toByte) else
      buf.add((first | mask).toByte)
      var rest = value - mask

      while rest >= 0x80 do
        buf.add(((rest & 0x7f) | 0x80).toByte)
        rest >>>= 7

      buf.add(rest.toByte)

  private def writeString(buf: ByteBuf^, text: Text): Unit =
    val raw: Data = Array.unsafeFrozen(text.s.getBytes(StandardCharsets.US_ASCII).nn)
    val huffed: Data = Huffman.encode(raw)

    // Use whichever encoding is shorter (RFC permits either); flag Huffman in bit 7.
    if huffed.length < raw.length then
      writeInteger(buf, 0x80, 7, huffed.length)
      buf.addAll(huffed)
    else
      writeInteger(buf, 0, 7, raw.length)
      buf.addAll(raw)

  // Entry → Hpack.Entry, Table → Hpack.Table
  // An HPACK header field: a name and value. The encoded byte size used for dynamic
  // table accounting is `name + value + 32` (RFC 7541 §4.1).
  case class Entry(name: Text, value: Text):
    def size: Int = name.s.length + value.s.length + 32

  object Table:
    // The 61-entry static table (RFC 7541, Appendix A). Index 1 is element 0 here.
    val static: Array[Entry]^{} = Array[Entry](
      Entry(":authority", ""),
      Entry(":method", "GET"),
      Entry(":method", "POST"),
      Entry(":path", "/"),
      Entry(":path", "/index.html"),
      Entry(":scheme", "http"),
      Entry(":scheme", "https"),
      Entry(":status", "200"),
      Entry(":status", "204"),
      Entry(":status", "206"),
      Entry(":status", "304"),
      Entry(":status", "400"),
      Entry(":status", "404"),
      Entry(":status", "500"),
      Entry("accept-charset", ""),
      Entry("accept-encoding", "gzip, deflate"),
      Entry("accept-language", ""),
      Entry("accept-ranges", ""),
      Entry("accept", ""),
      Entry("access-control-allow-origin", ""),
      Entry("age", ""),
      Entry("allow", ""),
      Entry("authorization", ""),
      Entry("cache-control", ""),
      Entry("content-disposition", ""),
      Entry("content-encoding", ""),
      Entry("content-language", ""),
      Entry("content-length", ""),
      Entry("content-location", ""),
      Entry("content-range", ""),
      Entry("content-type", ""),
      Entry("cookie", ""),
      Entry("date", ""),
      Entry("etag", ""),
      Entry("expect", ""),
      Entry("expires", ""),
      Entry("from", ""),
      Entry("host", ""),
      Entry("if-match", ""),
      Entry("if-modified-since", ""),
      Entry("if-none-match", ""),
      Entry("if-range", ""),
      Entry("if-unmodified-since", ""),
      Entry("last-modified", ""),
      Entry("link", ""),
      Entry("location", ""),
      Entry("max-forwards", ""),
      Entry("proxy-authenticate", ""),
      Entry("proxy-authorization", ""),
      Entry("range", ""),
      Entry("referer", ""),
      Entry("refresh", ""),
      Entry("retry-after", ""),
      Entry("server", ""),
      Entry("set-cookie", ""),
      Entry("strict-transport-security", ""),
      Entry("transfer-encoding", ""),
      Entry("user-agent", ""),
      Entry("vary", ""),
      Entry("via", ""),
      Entry("www-authenticate", "") )

  // The HPACK dynamic table: a FIFO of recently-seen header fields, bounded by a
  // byte-size limit, with oldest-first eviction. Combined with the static table it
  // forms the HPACK address space: index 1..61 is static; 62.. is the dynamic table,
  // most-recently-inserted first (RFC 7541 §2.3.3).
  class Table(initialMaxSize: Int = 4096):
    // Untracked: the dynamic table is confined to its owning `Hpack` codec, which
    // is itself confined to one connection's reader or writer daemon.
    @caps.unsafe.untrackedCaptures
    private val entries: scm.ArrayDeque[Entry] = scm.ArrayDeque.empty[Entry]
    @caps.unsafe.untrackedCaptures
    private var maxSize: Int = initialMaxSize
    @caps.unsafe.untrackedCaptures
    private var currentSize: Int = 0

    def size: Int = currentSize
    def capacity: Int = maxSize

    // Resize (HPACK dynamic-table-size-update); evicts to fit the new bound.
    def resize(newMax: Int): Unit =
      maxSize = newMax
      evict()

    private def evict(): Unit =
      while currentSize > maxSize && entries.nonEmpty do currentSize -= entries.removeLast().size

    // Insert at the front (most recent). An entry larger than the whole table
    // clears it and is itself not stored (RFC 7541 §4.4).
    def add(entry: Entry): Unit =
      currentSize += entry.size
      entries.prepend(entry)
      evict()

    // Resolve an HPACK index (1-based) to its entry: static for 1..61, then dynamic.
    def lookup(index: Int): Optional[Entry] =
      if index >= 1 && index <= Table.static.length then Table.static.readUnchecked(index - 1)
      else
        val dynamicIndex = index - Table.static.length - 1

        if dynamicIndex >= 0 && dynamicIndex < entries.length then entries(dynamicIndex) else Unset

class Hpack(maxTableSize: Int = 4096):
  private val table = Hpack.Table(maxTableSize)

  // ─── integer representation (RFC 7541 §5.1) ───────────────────────────────
  //
  // An integer uses the low `prefix` bits of the byte at `data(offset)`; if those
  // are all 1 it continues in subsequent 7-bit groups (low 7 bits, high bit =
  // continuation). Returns the value and the index just past the integer.
  private def readInteger(data: Data, offset: Int, prefix: Int): (Int, Int) raises Http2.Error =
    val mask = (1 << prefix) - 1
    val first = data.readUnchecked(offset) & mask

    if first < mask then (first, offset + 1) else
      var result = mask
      var shift = 0
      var pos = offset + 1
      var continue = true

      while continue do
        if pos >= data.length then abort(Http2.Error(Reason.Truncated))
        val byte = data.readUnchecked(pos) & 0xff
        result += (byte & 0x7f) << shift
        shift += 7
        pos += 1
        continue = (byte & 0x80) != 0
        if shift > 28 then abort(Http2.Error(Reason.BadInteger))

      (result, pos)

  // ─── string literal (RFC 7541 §5.2) ───────────────────────────────────────
  //
  // A length-prefixed octet sequence; the prefix's high bit flags Huffman coding.
  private def readString(data: Data, offset: Int): (Text, Int) raises Http2.Error =
    val huffman = (data.readUnchecked(offset) & 0x80) != 0
    val (length, start) = readInteger(data, offset, 7)
    if start + length > data.length then abort(Http2.Error(Reason.Truncated))
    val raw: Data = data.segment((start).z till (start + length).z)
    val decoded: Data = if huffman then Huffman.decode(raw) else raw

    (decoded.utf8, start + length)

  // ─── decode a complete header block ────────────────────────────────────────

  def decode(data: Data): List[Hpack.Entry] raises Http2.Error =
    val builder = scala.collection.immutable.List.newBuilder[Hpack.Entry]
    var pos = 0

    def nameValue(index: Int, after: Int): (Hpack.Entry, Int) =
      // `index == 0` → literal name follows; otherwise name comes from the table.
      val (name, valueStart) =
        if index == 0 then readString(data, after)
        else (table.lookup(index).lest(Http2.Error(Reason.BadIndex(index))).name, after)

      val (value, next) = readString(data, valueStart)
      (Hpack.Entry(name, value), next)

    while pos < data.length do
      val byte = data.readUnchecked(pos) & 0xff

      if (byte & 0x80) != 0 then
        // Indexed header field (§6.1): whole field from the table.
        val (index, next) = readInteger(data, pos, 7)
        builder += table.lookup(index).lest(Http2.Error(Reason.BadIndex(index)))
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

    builder.result().to(List)

  // ─── encode a header block ──────────────────────────────────────────────────
  //
  // v1 strategy: emit each field as a literal with incremental indexing and a
  // literal (Huffman-or-raw) name and value. Correct and interoperable; does not
  // yet exploit static-table name matches. Pseudo-headers must already be ordered
  // ahead of regular headers by the caller (RFC 7540 §8.1.2.1).
  // Delegates to the companion: builder mutation inside a class method would
  // force a `uses` clause onto the class itself.
  def encode(headers: List[Hpack.Entry]): Data = Hpack.encodeEntries(headers, table)
