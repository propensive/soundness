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
package zeppelin

import java.io as ji
import java.util.zip as juz
import proscenium.compat.*

import anticipation.*
import contingency.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import pneumatic.*
import turbulence.*
import zephyrine.*
import vacuous.*
import fulminate.*
import scala.caps
import java.nio as jn
import java.nio.channels as jnc
import java.nio.file as jnf
import aperture.*

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

    def length(text: Text): Int raises Path.Error = 0
    def decode(text: Text): %.type raises Path.Error = %
    def encode(root: %.type): Text = t""

  // Anchored here so `path.open[Zip]()` and `data.open[Zip]()` resolve with no import.
  given openable: [path: Abstractable across Paths to Text]
  =>  Tactic[Zip.Error]
  =>  Zip.Openable[path] =
    Zip.Openable[path]

  given dataOpenable: Tactic[Zip.Error] => Zip.DataOpenable = Zip.DataOpenable()

  given creatable: [path: Abstractable across Paths to Text]
  =>  Tactic[Zip.Error]
  =>  ZipBuilder.ZipCreatable[path] =
    ZipBuilder.ZipCreatable[path]

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
    def apply[content: Streamable by Data over Credit](ref: Path on Zip, content: content)
      ( using Compression )
    :   Entry =

      build(ref, content.source[Data].memoize)

    // Construct an entry from raw bytes, compressing once per the contextual policy.
    def at[content: Streamable by Data over Credit,
           instant: Abstractable across Instants to Long]
      ( ref: Path on Zip, content: content, modified: instant )
        ( using Compression )
    :   Entry =

      val (time, date) = dosDateTime(modified.generic)
      build(ref, content.source[Data].memoize, time, date)

    private def build
      ( ref: Path on Zip, raw: Data, time: Int = epochTime, date: Int = epochDate )
        ( using compression: Compression )
    :   Entry =

      val crc = crc32(raw)
      val uncompressed = raw.length.toLong

      def stored: Entry =
        Entry(ref, Method.Stored, crc, uncompressed, uncompressed, () => raw.stream, time, date)

      compression match
        case Compression.Stored => stored

        case Compression.Deflate(level) =>
          val deflated = deflate(raw, level)

          if deflated.length >= raw.length then stored
          else Entry(ref, Method.Deflate, crc, uncompressed, deflated.length.toLong,
              () => deflated.stream, time, date)

    // Used by the random-access reader to rebuild an entry from central-directory metadata
    // without recompressing; `storedBytes` reads the already-compressed payload lazily.
    private[zeppelin] def precompressed
      ( ref:              Path on Zip,
       method:           Method,
       crc32:            Int,
       uncompressedSize: Long,
       compressedSize:   Long,
       storedBytes:      () => Stream[Data] over Credit,
       dosTime:          Int,
       dosDate:          Int,
       directory:        Boolean,
       comment:          Optional[Text] )
    :   Entry =

      Entry(ref, method, crc32, uncompressedSize, compressedSize, storedBytes, dosTime, dosDate,
          directory, comment)

    given streamable: Entry is Streamable by Data over Credit = entry => entry.contents

  case class Entry
    ( ref:              Path on Zip,
     method:           Method,
     crc32:            Int,
     uncompressedSize: Long,
     compressedSize:   Long,
     storedBytes:      () => Stream[Data] over Credit,
     dosTime:          Int               = Zip.epochTime,
     dosDate:          Int               = Zip.epochDate,
     directory:        Boolean           = false,
     comment:          Optional[Text]    = Unset,
     alignment:        Int               = 1 ):

    // The decompressed content of the entry: a fresh stream per call, inflated
    // incrementally through the `Deflate` duct, so a payload of any size is read,
    // inflated and consumed in bounded chunks.
    def contents: Stream[Data] over Credit = method match
      case Method.Stored  => storedBytes()
      case Method.Deflate => storedBytes().decompress[Deflate]

    // The same entry, its data required to begin at a byte offset that is a multiple of
    // `multiple` in the written archive — achieved by padding the local header's extra field.
    // Meaningful only for stored (uncompressed) entries, whose bytes a reader may `mmap`
    // directly: an aligned `classes.dex`, `resources.arsc`, or page-aligned (`4096`) native
    // library needs no copy to be memory-mapped. This is the transform Android's `zipalign`
    // performs.
    def aligned(multiple: Int): Entry = copy(alignment = multiple)

    // The same entry, recorded as a directory: its name serializes with a trailing slash and
    // the DOS directory attribute is set. (The case-class `copy` cannot be called from
    // capture-checked code, since it would rethread the `storedBytes` producer through a pure
    // parameter; transforms belong here, beside `aligned`.)
    def asDirectory: Entry = copy(directory = true)

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
  private[zeppelin] def putU16(array: scala.Array[Byte], offset: Int, value: Int): Unit =
    array(offset) = (value & 0xff).toByte
    array(offset + 1) = ((value >> 8) & 0xff).toByte

  private[zeppelin] def putU32(array: scala.Array[Byte], offset: Int, value: Long): Unit =
    array(offset) = (value & 0xff).toByte
    array(offset + 1) = ((value >> 8) & 0xff).toByte
    array(offset + 2) = ((value >> 16) & 0xff).toByte
    array(offset + 3) = ((value >> 24) & 0xff).toByte

  private[zeppelin] def putU64(array: scala.Array[Byte], offset: Int, value: Long): Unit =
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
    crc.update(Array.unsafeJvm(data))
    crc.getValue.toInt

  // Raw RFC-1951 DEFLATE of a single buffer (the algorithm primitive, not the ZIP container).
  private[zeppelin] def deflate(data: Data, level: Int): Data =
    val deflater = juz.Deflater(level, true)
    deflater.setInput(Array.unsafeJvm(data))
    deflater.finish()
    val buffer = new scala.Array[Byte](8192)
    val out = ji.ByteArrayOutputStream()
    while !deflater.finished() do out.write(buffer, 0, deflater.deflate(buffer))
    deflater.end()
    Array.unsafeFrozen(out.toByteArray.nn)

  // ZipError -> Zip.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case DuplicateEntry(path: Path on Zip)   extends Reason(1)
      case NotFound(path: Path on Zip)         extends Reason(2)
      case InvalidName(name: Text)             extends Reason(3)
      case UnsupportedMethod(method: Int)      extends Reason(4)
      case MissingEocd                         extends Reason(5)
      case TruncatedArchive                    extends Reason(6)
      case BadSignature(expected: Int)         extends Reason(7)
      case Zip64Error                          extends Reason(8)
      case WriteUnsupported                    extends Reason(9)
      case AlreadyExists                       extends Reason(10)
      case CannotWrite(detail: Text)           extends Reason(11)

    given communicable: Reason is Communicable =
      case Reason.DuplicateEntry(path)    => m"the path $path is a duplicate entry"
      case Reason.NotFound(path)          => m"path $path was not found in the ZIP file"
      case Reason.InvalidName(name)       => m"the name $name is not valid for a ZIP entry"
      case Reason.UnsupportedMethod(code) => m"the compression method $code is not supported"
      case Reason.MissingEocd             => m"no end-of-central-directory record could be found"
      case Reason.TruncatedArchive        => m"the ZIP archive ended unexpectedly"
      case Reason.BadSignature(expected)  => m"an expected record signature ($expected) was absent"
      case Reason.Zip64Error              => m"the ZIP64 metadata could not be interpreted"
      case Reason.WriteUnsupported        => m"ZIP archives cannot yet be opened for writing"
      case Reason.AlreadyExists           => m"an archive already exists at this path"
      case Reason.CannotWrite(detail)     => m"the archive could not be written: $detail"

  case class Error(reason: Zip.Error.Reason)(using Diagnostics)
  extends fulminate.Error(751, reason.number)(m"the ZIP operation failed because $reason")

  // ZipEvent -> Zip.Event
  object Event:
    given communicable: Zip.Event is Communicable =
      case Wrote(path, entries) => m"wrote $entries entries to the zip archive $path"
      case Read(path, entries)  => m"read $entries entries from the zip archive $path"

  enum Event:
    case Wrote(path: Text, entries: Int) extends Zip.Event, Log.Serialization
    case Read(path: Text, entries: Int) extends Zip.Event, Log.Serialization

  // ZipHandle -> Zip.Handle
  // The scoped capability provided by opening an archive as `Zip`: `path.open[Zip]()`. Unlike a
  // detached `Zipfile` (whose `FileSource` re-opens the file for every read), a `Zip.Handle` reads
  // through a single channel held open for the duration of the block, so entry payloads resolve
  // with no per-read open/close cost — and, correspondingly, must be consumed within the scope.
  // (Zeppelin is not yet capture-checked, so the confinement is enforced only for callers
  // compiled with capture checking; the annotations sharpen when the module joins the rollout.)
  class Handle private[zeppelin] (private[zeppelin] val zipfile: Zipfile)
  extends caps.ExclusiveCapability:
    def entries: List[Zip.Entry] = zipfile.entries
    def entry(ref: Path on Zip): Zip.Entry raises Zip.Error = zipfile.entry(ref)
    def comment: Optional[Text] = zipfile.comment

  // A named class rather than an anonymous given instance, for the reasons documented on
  // galilei's `FileOpenable`. Archives open read-only: a `Write` mode is refused with
  // `Zip.Error.Reason.WriteUnsupported` until writing lands.
  // Opens an in-memory archive; no channel is involved, but access is scoped all the same, for
  // consistency with every other form of `Zip` target.

  // ZipOpenable → Zip.Openable
  class Openable[path: Abstractable across Paths to Text](using Tactic[Zip.Error])
  extends aperture.Openable:

    type Self = path
    type Form = Zip
    type Operand = Nothing
    type Result = Zip.Handle

    def open[grants <: Grant, result]
      ( value: path, mode: Mode granting grants, flags: List[Nothing] )
      ( block: (Zip.Handle & Granting[grants]) ?=> result )
    :   result =

      if mode.atoms.has(Write) then abort(Zip.Error(Zip.Error.Reason.WriteUnsupported))

      val channel =
        jnc.FileChannel.open(jnf.Path.of(value.generic.s), jnf.StandardOpenOption.READ).nn

      try
        val zipfile = Zipfile.parse(Zipfile.ChannelSource(channel))
        block(using new Zip.Handle(zipfile) with Granting[grants] {})
      finally channel.close()

  // ZipDataOpenable → Zip.DataOpenable
  class DataOpenable(using Tactic[Zip.Error]) extends aperture.Openable:
    type Self = Data
    type Form = Zip
    type Operand = Nothing
    type Result = Zip.Handle

    def open[grants <: Grant, result]
      ( value: Data, mode: Mode granting grants, flags: List[Nothing] )
      ( block: (Zip.Handle & Granting[grants]) ?=> result )
    :   result =

      if mode.atoms.has(Write) then abort(Zip.Error(Zip.Error.Reason.WriteUnsupported))
      block(using new Zip.Handle(Zipfile.parse(Zipfile.DataSource(value))) with Granting[grants] {})

sealed trait Zip
