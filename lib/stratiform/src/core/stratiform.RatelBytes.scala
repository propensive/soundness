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
package stratiform

import anticipation.*
import rudiments.*
import zephyrine.*

// The byte-level vocabulary RaTEL's reader and writer share: its structural integers, its
// padding, and the encoding of index keys (RaTEL §3, §9.2).
private[stratiform] object RatelBytes:
  // The header's size, and the magic number `ᾱτελ` which opens it.
  val header: Int = 200

  // The size of the digests section (§5.7): a digest of the body, then one of the header.
  val digests: Int = 2*32

  // NONE (§3): no row, no item, no page.
  val none: Long = 0xffffffffL

  // The flags of the header (§5.1), and the one flag of a row's header (§6.2).
  val digested: Int = 1
  val hashed: Int = 2
  val sealing: Int = 4
  val dirty: Int = 8
  val live: Int = 1

  // The link of the root's row, and of a free row.
  val unlinked: Int = 0xffff

  val magic: Data = Array[Byte](0xb1.toByte, 0xc4.toByte, 0xb5.toByte, 0xbb.toByte)

  def u32(out: Scribe[Byte], value: Long): Unit =
    out.append(value.toByte)
    out.append((value >>> 8).toByte)
    out.append((value >>> 16).toByte)
    out.append((value >>> 24).toByte)

  def u64(out: Scribe[Byte], value: Long): Unit =
    u32(out, value)
    u32(out, value >>> 32)

  // The one big-endian integer of RaTEL: a record index within an index entry, so that
  // entries which are compared bytewise sort numerically.
  def u32be(out: Scribe[Byte], value: Long): Unit =
    out.append((value >>> 24).toByte)
    out.append((value >>> 16).toByte)
    out.append((value >>> 8).toByte)
    out.append(value.toByte)

  def zeros(out: Scribe[Byte], count: Int): Unit = if count > 0 then
    out.append(0.toByte)
    zeros(out, count - 1)

  // Zero bytes up to the next multiple of `multiple`.
  def pad(out: Scribe[Byte], multiple: Int): Unit =
    zeros(out, (multiple - out.mark%multiple)%multiple)

  def utf8(text: Text): Data = Array.collect[Byte](text.s.length + 8)(_.appendUtf8(text.s))

  // A value item of a region (§5.3, §5.4): its length, its bytes, and zero bytes to a
  // multiple of four.
  def item(header: Long, body: Data): Data = Array.collect[Byte](body.length + 8): out =>
    u32(out, header)
    out.append(body)
    pad(out, 4)

  // A growable item of the variable region (§5.4): its capacity and its count, then its
  // elements, of `unit` bytes each, and zero bytes for those it has room for and does not hold.
  def growable(capacity: Long, count: Long, unit: Int, body: Data): Data =
    Array.collect[Byte]((capacity*unit).toInt + 12): out =>
      u32(out, capacity)
      u32(out, count)
      out.append(body)
      zeros(out, ((capacity - count)*unit).toInt)
      pad(out, 4)

  // §9.2: one component of an index key. An absent value sorts below every value; a value of
  // fixed width is its bytes; any other is escaped so that it ends where its terminator is.
  def absent(out: Scribe[Byte]): Unit = out.append(0.toByte)

  def component(out: Scribe[Byte], bytes: Data, fixed: Boolean): Unit =
    out.append(1.toByte)

    if fixed then out.append(bytes) else
      bytes.each: byte =>
        out.append(byte)
        if byte == 0 then out.append(0xff.toByte)

      out.append(0.toByte)
      out.append(0.toByte)

  // The order of two byte sequences, compared as unsigned bytes: a proper prefix sorts first.
  def order(left: Data, right: Data): Int =
    val common = left.length.min(right.length)

    @scala.annotation.tailrec
    def recur(index: Int): Int =
      if index == common then left.length - right.length else
        val difference = left.u8(index) - right.u8(index)
        if difference == 0 then recur(index + 1) else difference

    recur(0)

  // Whether bytes are well-formed UTF-8: no truncated or overlong sequence, no surrogate, and
  // nothing beyond U+10FFFF.
  def utf8(data: Data): Boolean =
    val length = data.length
    def continuation(index: Int): Boolean = index < length && (data.u8(index) & 0xc0) == 0x80

    @scala.annotation.tailrec
    def recur(index: Int): Boolean =
      if index == length then true else
        val lead = data.u8(index)

        if lead < 0x80 then recur(index + 1)
        else if lead >= 0xc2 && lead < 0xe0 then continuation(index + 1) && recur(index + 2)
        else if lead >= 0xe0 && lead < 0xf0 then
          val second = if index + 1 < length then data.u8(index + 1) else 0

          val shortest = (lead != 0xe0 || second >= 0xa0) && (lead != 0xed || second < 0xa0)
          continuation(index + 1) && continuation(index + 2) && shortest && recur(index + 3)
        else if lead >= 0xf0 && lead < 0xf5 then
          val second = if index + 1 < length then data.u8(index + 1) else 0

          val shortest = (lead != 0xf0 || second >= 0x90) && (lead != 0xf4 || second < 0x90)
          val continued = continuation(index + 1) && continuation(index + 2)
          continued && continuation(index + 3) && shortest && recur(index + 4)
        else
          false

    recur(0)
