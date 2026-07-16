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
package zeppelin

import java.io as ji
import java.util.zip as juz

import anticipation.*
import contingency.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import zephyrine.*
import vacuous.*

object Zip:
  type Rules =
    MustNotContain["\\"] & MustNotContain["\""] & MustNotContain["/"] & MustNotContain[":"] &
      MustNotContain["*"] & MustNotContain["?"] & MustNotContain["<"] & MustNotContain[">"] &
      MustNotContain["|"] & MustNotEqual["."] & MustNotEqual[".."]

  inline given compliant: Linux is Compliant on Zip = !!
  inline given compliant2: MacOs is Compliant on Zip = !!
  inline given nominative: Zip is Nominative under Rules = !!
  given submissible: %.type is Submissible on Zip = _ => ()

  given filesystem: Zip is Filesystem:
    type UniqueRoot = false

    val name: Text = "ZIP"
    val separator: Text = "/"
    val self: Text = "."
    val parent: Text = ".."

  given radical: %.type is Radical:
    type Plane = Zip

    def length(text: Text): Int raises PathError = 0
    def decode(text: Text): %.type raises PathError = %
    def encode(root: %.type): Text = t""

  // The compression method actually recorded on an entry.
  enum Method(val id: Int):
    case Stored  extends Method(0)
    case Deflate extends Method(8)

  object Compression:
    given default: Compression = Compression.Deflate(-1)

  // The contextual policy consulted when an `Entry` is created from raw content.
  enum Compression:
    case Stored
    case Deflate(level: Int)

  object Entry:
    def apply[content: Streamable by Data](ref: Path on Zip, content: content)
      ( using Compression )
    :   Entry =

      build(ref, gather(content.lazyList[Data]))

    def apply(ref: Path on Zip, content: () => LazyList[Data])(using Compression): Entry =
      build(ref, gather(content()))

    // Construct an entry from raw bytes, compressing once per the contextual policy.
    def at[content: Streamable by Data, instant: Abstractable across Instants to Long]
      ( ref: Path on Zip, content: content, modified: instant )
        ( using Compression )
    :   Entry =

      val (time, date) = dosDateTime(modified.generic)
      build(ref, gather(content.lazyList[Data]), time, date)

    private def build
      ( ref: Path on Zip, raw: Data, time: Int = epochTime, date: Int = epochDate )
        ( using compression: Compression )
    :   Entry =

      val crc = crc32(raw)
      val uncompressed = raw.length.toLong

      def stored: Entry =
        Entry(ref, Method.Stored, crc, uncompressed, uncompressed, () => LazyList(raw), time, date)

      compression match
        case Compression.Stored => stored

        case Compression.Deflate(level) =>
          val deflated = deflate(raw, level)

          if deflated.length >= raw.length then stored
          else Entry(ref, Method.Deflate, crc, uncompressed, deflated.length.toLong,
              () => LazyList(deflated), time, date)

    // Used by the random-access reader to rebuild an entry from central-directory metadata
    // without recompressing; `storedBytes` reads the already-compressed payload lazily.
    private[zeppelin] def precompressed
      ( ref:              Path on Zip,
       method:           Method,
       crc32:            Int,
       uncompressedSize: Long,
       compressedSize:   Long,
       storedBytes:      () => LazyList[Data],
       dosTime:          Int,
       dosDate:          Int,
       directory:        Boolean,
       comment:          Optional[Text] )
    :   Entry =

      Entry(ref, method, crc32, uncompressedSize, compressedSize, storedBytes, dosTime, dosDate,
          directory, comment)

    given streamable: Entry is Streamable by Data over Credit = entry =>
      entry.contents.iterator.stream

  case class Entry
    ( ref:              Path on Zip,
     method:           Method,
     crc32:            Int,
     uncompressedSize: Long,
     compressedSize:   Long,
     storedBytes:      () => LazyList[Data],
     dosTime:          Int               = Zip.epochTime,
     dosDate:          Int               = Zip.epochDate,
     directory:        Boolean           = false,
     comment:          Optional[Text]    = Unset ):

    // The decompressed content of the entry.
    def contents: LazyList[Data] = method match
      case Method.Stored  => storedBytes()
      case Method.Deflate => LazyList(inflate(gather(storedBytes())))

  // 00:00:00, 1 January 1980 — the minimum value representable in a DOS timestamp.
  private[zeppelin] val epochTime: Int = 0x0000
  private[zeppelin] val epochDate: Int = 0x0021

  private[zeppelin] val localHeaderSig:   Int = 0x04034b50
  private[zeppelin] val dataDescriptorSig:Int = 0x08074b50
  private[zeppelin] val centralHeaderSig: Int = 0x02014b50
  private[zeppelin] val eocdSig:          Int = 0x06054b50
  private[zeppelin] val zip64EocdSig:     Int = 0x06064b50
  private[zeppelin] val zip64LocatorSig:  Int = 0x07064b50

  // Little-endian writers into a mutable array.
  private[zeppelin] def putU16(array: Array[Byte], offset: Int, value: Int): Unit =
    array(offset) = (value & 0xff).toByte
    array(offset + 1) = ((value >> 8) & 0xff).toByte

  private[zeppelin] def putU32(array: Array[Byte], offset: Int, value: Long): Unit =
    array(offset) = (value & 0xff).toByte
    array(offset + 1) = ((value >> 8) & 0xff).toByte
    array(offset + 2) = ((value >> 16) & 0xff).toByte
    array(offset + 3) = ((value >> 24) & 0xff).toByte

  private[zeppelin] def putU64(array: Array[Byte], offset: Int, value: Long): Unit =
    var i = 0

    while i < 8 do
      array(offset + i) = ((value >> (i*8)) & 0xff).toByte
      i += 1

  // Little-endian readers from immutable data.
  private[zeppelin] def u16(data: Data, offset: Int): Int =
    (data(offset) & 0xff) | ((data(offset + 1) & 0xff) << 8)

  private[zeppelin] def u32(data: Data, offset: Int): Long =
    var value = 0L
    var i = 0

    while i < 4 do
      value |= (data(offset + i) & 0xffL) << (i*8)
      i += 1

    value

  private[zeppelin] def u64(data: Data, offset: Int): Long =
    var value = 0L
    var i = 0

    while i < 8 do
      value |= (data(offset + i) & 0xffL) << (i*8)
      i += 1

    value

  private[zeppelin] def dosDateTime(epochMillis: Long): (Int, Int) =
    import java.time as jt

    val instant = jt.Instant.ofEpochMilli(epochMillis)
    val ldt = jt.LocalDateTime.ofInstant(instant, jt.ZoneId.systemDefault()).nn
    val year = (ldt.getYear - 1980).max(0)
    val date = (year << 9) | (ldt.getMonthValue << 5) | ldt.getDayOfMonth
    val time = (ldt.getHour << 11) | (ldt.getMinute << 5) | (ldt.getSecond/2)

    (time, date)

  private[zeppelin] def crc32(data: Data): Int =
    val crc = juz.CRC32()
    crc.update(data.mutable(using Unsafe))
    crc.getValue.toInt

  // Raw RFC-1951 DEFLATE of a single buffer (the algorithm primitive, not the ZIP container).
  private[zeppelin] def deflate(data: Data, level: Int): Data =
    val deflater = juz.Deflater(level, true)
    deflater.setInput(data.mutable(using Unsafe))
    deflater.finish()
    val buffer = new Array[Byte](8192)
    val out = ji.ByteArrayOutputStream()
    while !deflater.finished() do out.write(buffer, 0, deflater.deflate(buffer))
    deflater.end()
    out.toByteArray.nn.immutable(using Unsafe)

  private[zeppelin] def inflate(data: Data): Data =
    val inflater = juz.Inflater(true)
    inflater.setInput(data.mutable(using Unsafe))
    val buffer = new Array[Byte](8192)
    val out = ji.ByteArrayOutputStream()

    while !inflater.finished() && !inflater.needsInput() do
      out.write(buffer, 0, inflater.inflate(buffer))

    inflater.end()
    out.toByteArray.nn.immutable(using Unsafe)

  private[zeppelin] def gather(stream: LazyList[Data]): Data =
    val out = ji.ByteArrayOutputStream()
    stream.each: chunk => out.write(chunk.mutable(using Unsafe))
    out.toByteArray.nn.immutable(using Unsafe)

sealed trait Zip
