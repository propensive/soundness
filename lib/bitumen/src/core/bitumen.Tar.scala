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
package bitumen


import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import galilei.*
import gossamer.*
import hieroglyph.*, charEncoders.asciiEncoder, textMetrics.uniformMetric
import hypotenuse.*, arithmeticOptions.uncheckedOverflow
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*
import fulminate.*
import hypotenuse.*
import scala.caps
import aperture.*
import pneumatic.*
import denominative.dysasymptotics.linearSize

object Tar:
  // TarRef → Tar.Ref
  type Ref = Relative on Tar

  type Rules =
    MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  inline given compliant:  Linux is Compliant on Tar = !!
  inline given compliant2: MacOs is Compliant on Tar = !!
  inline given nominative: Tar is Nominative under Rules = !!

  given filesystem: Tar is Filesystem:
    type UniqueRoot = false

    val name: Text = "TAR"
    val separator: Text = "/"
    val self: Text = "."
    val parent: Text = ".."

  // Anchored here so `data.open[Tar](...)` resolves with no import. Opening a filesystem
  // *path* as TAR (`path.open[Tar]`) lives in `bitumen.jvm`, alongside the disk backend.
  given dataOpenable: (tarTactic: Tactic[Tar.Error], streamTactic: Tactic[Truncation.Error])
  =>  (Tar.DataOpenable^{tarTactic, streamTactic}) =
    Tar.DataOpenable()

  // The 512-byte USTAR header, held as raw slices: nothing is decoded in the case class.
  object Header:
    val blockSize: Int = 512
    val checksumOffset: Int = 148
    val checksumLength: Int = 8

    def parse(block: Data): Header raises Tar.Error =
      if block.length < blockSize
      then raise(Tar.Error(Tar.Error.Reason.TruncatedStream(blockSize, block.length)))

      Header
        ( name     = block.segment((0).z till (100).z),
          mode     = block.segment((100).z till (108).z),
          uid      = block.segment((108).z till (116).z),
          gid      = block.segment((116).z till (124).z),
          size     = block.segment((124).z till (136).z),
          mtime    = block.segment((136).z till (148).z),
          checksum = block.segment((148).z till (156).z),
          // Total: after a raised `TruncatedStream` the header is parsed best-effort, so a
          // short block yields the default (regular-file) flag rather than an overrun.
          typeFlag = block.at((156).z).or(0),
          linkName = block.segment((157).z till (257).z),
          magic    = block.segment((257).z till (263).z),
          version  = block.segment((263).z till (265).z),
          uname    = block.segment((265).z till (297).z),
          gname    = block.segment((297).z till (329).z),
          devMajor = block.segment((329).z till (337).z),
          devMinor = block.segment((337).z till (345).z),
          prefix   = block.segment((345).z till (500).z) )

    def verifyChecksum(block: Data, recorded: U32): Unit raises Tar.Error =
      var sum: Long = 0L

      // A short block sums fewer bytes and fails the comparison below, rather than overrunning.
      block.iterate(block.extent.capped(blockSize)): index =>
        val i: Int = (index: Ordinal).n0

        val byte: Int =
          if i >= checksumOffset && i < checksumOffset + checksumLength then 0x20
          else block.at(index) & 0xff

        sum = sum + byte

      val actual: U32 = sum.toInt.bits.u32

      if actual != recorded then raise(Tar.Error(Tar.Error.Reason.BadChecksum(recorded, actual)))

    def decodeOctal(data: Data, field: Text): U32 raises Tar.Error =
      // Two-stage scan: leading spaces, then the octal digit run. `prefix(after)` is
      // cumulative, so `digits` spans both stages and its limit is the digit run's end.
      val spaces = data.prefix: index => data.at(index) == ' '.toByte

      val digits = data.prefix(spaces): index =>
        val byte: Byte = data.at(index)
        byte >= '0'.toByte && byte <= '7'.toByte

      if (digits: Interval).size == (spaces: Interval).size
      then raise(Tar.Error(Tar.Error.Reason.BadOctal(field, data)))

      // The one genuinely checked access: the terminator position is one past the digit run,
      // which may be one past the end of the field, so `at` returning `Unset` means a run
      // flush to the field's end — valid, with nothing to check.
      data.at((digits: Interval).limit).let: byte =>
        if byte != 0 && byte != ' '.toByte
        then raise(Tar.Error(Tar.Error.Reason.BadOctal(field, data)))

      var n: Long = 0L

      // `digits` includes the leading spaces (cumulative), which are skipped by value.
      data.iterate(digits): index =>
        val byte: Byte = data.at(index)
        if byte != ' '.toByte then n = n*8L + (byte - '0'.toByte).toLong

      n.toInt.bits.u32

    def decodeNulText(data: Data): Text =
      data.segment(data.prefix { index => data.at(index) != 0 }).utf8

    def isZeroBlock(block: Data): Boolean =
      val zeros: Interval = block.prefix: index => block.at(index) == 0.toByte
      zeros.size >= block.length.min(blockSize)

  case class Header
    ( name:     Data,
      mode:     Data,
      uid:      Data,
      gid:      Data,
      size:     Data,
      mtime:    Data,
      checksum: Data,
      typeFlag: Byte,
      linkName: Data,
      magic:    Data,
      version:  Data,
      uname:    Data,
      gname:    Data,
      devMajor: Data,
      devMinor: Data,
      prefix:   Data )

  // Opens in-memory `Data` as a TAR archive: `data.open[Tar](flags*)`. Archives open read-only.
  class DataOpenable(using Tactic[Tar.Error], Tactic[Truncation.Error]) extends Openable:
    type Self = Data
    type Form = Tar
    type Operand = Tar.Flag
    type Result = Tar.Handle

    def open[grants <: Grant, result]
      ( value: Data, mode: Mode granting grants, flags: List[Tar.Flag] )
      ( block: ((Tar.Handle & Granting[grants])^) ?=> result )
    :   result =

      if mode.atoms.has(Write) then abort(Tar.Error(Tar.Error.Reason.WriteUnsupported))
      val entries = Tar.Handle.entries(value.stream, flags)
      block(using new Tar.Handle(entries) with Granting[grants] {})

  object Entry:
    def apply[data: Streamable by Data over Credit, instant: Abstractable across Instants to Long]
      ( name:  Tar.Ref,
        data:  data,
        mode:  UnixMode          = UnixMode(),
        user:  UnixUser          = UnixUser(0),
        group: UnixGroup         = UnixGroup(0),
        mtime: Optional[instant] = Unset )
    :   Entry =

      val mtimeU32: U32 =
        (mtime.let(_.generic).or(System.currentTimeMillis)/1000).toInt.bits.u32

      Entry.File(name, mode, user, group, mtimeU32, Archive.Body(data.source[Data].memoize))

    private[bitumen] val paxRef: Tar.Ref =
      import strategies.throwUnsafely
      t"PaxHeaders/0".as[Relative on Tar]

    private[bitumen] def sparseExtensionBlocks(segments: List[SparseSegment]): List[Data] =
      if segments.nil then Nil
      else
        val (batch, rest) = segments.snip(21)

        val block: Data = Data.build(512): array =>
          var pos = 0

          batch.foreach: seg =>
            array.place(formatLongOctal(seg.offset, 12), pos.z)
            pos = pos + 12
            array.place(formatLongOctal(seg.length, 12), pos.z)
            pos = pos + 12

          if !rest.nil then array(504) = 1.toByte

        block :: sparseExtensionBlocks(rest)

    private[bitumen] def formatLongOctal(number: Long, width: Int): Data =
      val str: String = java.lang.Long.toOctalString(number).nn
      val pad: Int = (width - 1 - str.length).max(0)
      (("0"*pad) + str).tt.in[Data]

    // Re-blocks arbitrary chunks as 512-byte archive blocks, the final block
    // zero-padded, without regard to the incoming chunk boundaries.
    private[bitumen] def blocks512(chunks: Iterator[Data]): Iterator[Data] =
      new Iterator[Data]:
        @scala.caps.unsafe.untrackedCaptures
        private var chunk: Data = Array.empty[Byte]
        @scala.caps.unsafe.untrackedCaptures
        private var offset: Int = 0

        // Establish a nonempty current chunk, or report exhaustion.
        private def replenish(): Boolean =
          while offset >= chunk.length && chunks.hasNext do
            chunk = chunks.next()
            offset = 0

          offset < chunk.length

        def hasNext: Boolean = replenish()

        def next(): Data =
          val block = Array.allocate[Byte](512)
          var position = 0

          while position < 512 && replenish() do
            val count = (chunk.length - offset).min(512 - position)
            block.place(chunk, offset, position, count)
            offset += count
            position += count

          Array.freeze(block)

  enum Entry(path: Tar.Ref, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32):
    case File
      ( path:  Tar.Ref,
        mode:  UnixMode,
        user:  UnixUser,
        group: UnixGroup,
        mtime: U32,
        data:  Archive.Body,
        pax:   Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Directory
      ( path:  Tar.Ref,
        mode:  UnixMode,
        user:  UnixUser,
        group: UnixGroup,
        mtime: U32,
        pax:   Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Link
      ( path:   Tar.Ref,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        target: Text,
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Symlink
      ( path:   Tar.Ref,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        target: Text,
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case CharSpecial
      ( path:   Tar.Ref,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        device: (U32, U32),
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case BlockSpecial
      ( path:   Tar.Ref,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        device: (U32, U32),
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Fifo
      ( path:  Tar.Ref,
        mode:  UnixMode,
        user:  UnixUser,
        group: UnixGroup,
        mtime: U32,
        pax:   Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Pax(records: Data)
    extends Entry(Entry.paxRef, UnixMode(), UnixUser(0), UnixGroup(0), 0.bits.u32)

    case GnuLong(override val typeFlag: TypeFlag, content: Text)
    extends Entry(Entry.paxRef, UnixMode(), UnixUser(0), UnixGroup(0), 0.bits.u32)

    case Sparse
      ( path:     Tar.Ref,
        mode:     UnixMode,
        user:     UnixUser,
        group:    UnixGroup,
        mtime:    U32,
        realSize: Long,
        segments: List[SparseSegment],
        data:     Archive.Body,
        pax:      Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    def size: U32 = this match
      case file: File      => file.data.size.toInt.bits.u32
      case pax: Pax        => pax.records.length.bits.u32
      case long: GnuLong   => (long.content.in[Data].length + 1).bits.u32
      case sparse: Sparse  => sparse.segments.map(_.length).total.toInt.bits.u32
      case _               => 0

    def dataBlocks: Iterator[Data] = this match
      case file: File      => Entry.blocks512(file.data.chunks)
      case pax: Pax        => Entry.blocks512(Iterator(pax.records))

      case long: GnuLong =>
        Entry.blocks512(Iterator(Array.frozen(long.content.in[Data].readable :+ 0.toByte)))

      case sparse: Sparse =>
        Entry.blocks512(sparse.data.chunks)

      case _ =>
        Iterator.empty

    def typeFlag: TypeFlag = this match
      case _: File         => TypeFlag.File
      case _: Link         => TypeFlag.Link
      case _: Symlink      => TypeFlag.Symlink
      case _: CharSpecial  => TypeFlag.CharSpecial
      case _: BlockSpecial => TypeFlag.BlockSpecial
      case _: Directory    => TypeFlag.Directory
      case _: Fifo         => TypeFlag.Fifo
      case _: Pax          => TypeFlag.NextFile
      case long: GnuLong   => long.typeFlag
      case _: Sparse       => TypeFlag.Sparse

    def entryName: Text = this match
      case directory: Directory => t"${directory.path}/"
      case _: Pax               => t"PaxHeaders/0"
      case _: GnuLong           => t"././@LongLink"
      case other                => this.path.show

    def link: Optional[Text] = this.only:
      case link: Link       => link.target
      case symlink: Symlink => symlink.target

    def deviceNumbers: Optional[(U32, U32)] = this.only:
      case special: CharSpecial  => special.device
      case special: BlockSpecial => special.device

    def format(number: U32, width: Int): Data =
      number.octal.pad(width - 1).in[Data]

    def formatLong(number: Long, width: Int): Data =
      val str: String = java.lang.Long.toOctalString(number).nn
      val pad: Int = (width - 1 - str.length).max(0)
      (("0"*pad) + str).tt.in[Data]

    def header: Data = headerWith(size)

    // The 512-byte USTAR header with an explicit size: the streaming writer
    // backpatches a placeholder header once an entry's body length is known.
    private[bitumen] def headerWith(size0: U32): Data = Data.build(512): array =>
      val nameData = entryName.in[Data]
      array.place(if nameData.length > 100 then nameData.segment((0).z till (100).z) else nameData, Prim)
      array.place(mode.bytes, 100.z)
      array.place(user.bytes, 108.z)
      array.place(group.bytes, 116.z)
      array.place(format(size0, 12), 124.z)
      array.place(format(mtime, 12), 136.z)
      array.place(t"        ".in[Data], 148.z)
      array(156) = typeFlag.id.toByte

      link.let: link =>
        val linkData = link.in[Data]
        array.place(if linkData.length > 100 then linkData.segment((0).z till (100).z) else linkData, 157.z)

      deviceNumbers.let: (devMajor, devMinor) =>
        array.place(format(devMajor, 8), 329.z)
        array.place(format(devMinor, 8), 337.z)

      user.name.let: name =>
        val nameData = name.in[Data]
        array.place(if nameData.length > 32 then nameData.segment((0).z till (32).z) else nameData, 265.z)

      group.name.let: name =>
        val nameData = name.in[Data]
        array.place(if nameData.length > 32 then nameData.segment((0).z till (32).z) else nameData, 297.z)

      array.place(t"ustar\u0000".in[Data], 257.z)
      array.place(t"00".in[Data], 263.z)

      this.only:
        case sparse: Sparse =>
          val inline = sparse.segments.keep(4)
          var pos = 386

          inline.foreach: seg =>
            array.place(formatLong(seg.offset, 12), pos.z)
            pos = pos + 12
            array.place(formatLong(seg.length, 12), pos.z)
            pos = pos + 12

          if sparse.segments.size > 4 then array(482) = 1.toByte
          array.place(formatLong(sparse.realSize, 12), 483.z)

      val total = array.iterator.map(_.bits.u8.u32).reduce(_ + _)
      array.place(format(total, 8), 148.z)

    def serialize: Iterator[Data] = this match
      case sparse: Sparse if sparse.segments.size > 4 =>
        // `Iterator#++` demands an `IterableOnce`, which the opaque `List` is not.
        Iterator(header) ++ Entry.sparseExtensionBlocks(sparse.segments.skip(4)).stdlib ++ dataBlocks

      case _ =>
        Iterator(header) ++ dataBlocks

  // TarError → Tar.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NameTooLong(field: Text, length: Int, maximum: Int) extends Reason(1)
      case BadMagic(actual: Data) extends Reason(2)
      case BadChecksum(expected: U32, actual: U32) extends Reason(3)
      case UnknownTypeFlag(byte: Byte) extends Reason(4)
      case TruncatedStream(needed: Int, got: Int) extends Reason(5)
      case BadOctal(field: Text, data: Data) extends Reason(6)
      case BadPaxRecord(data: Data) extends Reason(7)
      case BadName(text: Text) extends Reason(8)
      case BadSparseMap(text: Text) extends Reason(9)
      case DeviceCreationUnsupported(path: Text) extends Reason(10)
      case WriteUnsupported extends Reason(11)
      case AlreadyExists extends Reason(12)
      case CannotWrite(detail: Text) extends Reason(13)

    given communicable: Reason is Communicable =
      case Reason.NameTooLong(field, length, maximum) =>
        m"the $field field is $length bytes, exceeding the USTAR limit of $maximum bytes"

      case Reason.BadMagic(actual) =>
        m"the USTAR magic bytes are not valid (got ${actual.length} bytes)"

      case Reason.BadChecksum(expected, actual) =>
        m"""
          the header checksum did not match (header recorded $expected but the recomputed value is
          $actual)
        """

      case Reason.UnknownTypeFlag(byte) =>
        val code: Int = byte.toInt & 0xff
        m"the entry type flag $code is not recognised"

      case Reason.TruncatedStream(needed, got) =>
        m"the archive stream ended unexpectedly (needed $needed bytes, got $got)"

      case Reason.BadOctal(field, _) =>
        m"the $field field did not contain a valid octal value"

      case Reason.BadPaxRecord(_) =>
        m"a PAX extended-header record could not be parsed"

      case Reason.BadName(text) =>
        m"the entry name $text is not a valid POSIX relative path"

      case Reason.BadSparseMap(text) =>
        m"the GNU sparse map $text could not be parsed"

      case Reason.DeviceCreationUnsupported(path) =>
        m"the special device entry at $path could not be created on this filesystem"

      case Reason.WriteUnsupported =>
        m"TAR archives cannot yet be opened for writing"

      case Reason.AlreadyExists =>
        m"an archive already exists at this path"

      case Reason.CannotWrite(detail) =>
        m"the archive could not be written: $detail"

  case class Error(reason: Tar.Error.Reason)(using Diagnostics)
  extends fulminate.Error(284, reason.number)(m"the TAR archive could not be read or written because $reason")

  // TarCompression → Tar.Compression
  object Compression

  // TarFlag → Tar.Flag
  // Flags for opening a TAR archive: the compression wrapping the archive, if any. TAR has no
  // self-identifying magic for its compression layer at the API level, so the caller states it:
  // `path.open[Tar](Tar.Flag.Gzip)`.
  enum Flag:
    case Gzip, Zlib, Deflate

  // TarHandle → Tar.Handle
  // The scoped capability provided by opening an archive as `Tar`: `path.open[Tar]()`. TAR is a
  // sequential format, so `entries` parses lazily from the underlying source, one entry per
  // step; payloads must be consumed within the scope, while the source remains open. The
  // iterator is single-pass: an entry passed over remains readable (its body memoizes when the
  // iterator advances), but the sequence itself is not replayable within the scope.
  class Handle private[bitumen] (entries0: Iterator[Tar.Entry]^)
  extends caps.ExclusiveCapability:

    // Reached only through this exclusive handle, which scopes it; its capture
    // of the underlying source is erased here, as the memoizing `LazyList` it
    // replaces erased it implicitly through its pure cells.
    @caps.unsafe.untrackedCaptures
    val entries: Iterator[Tar.Entry] = caps.unsafe.unsafeAssumePure(entries0)

  object Handle:
    private[bitumen] def entries(consume stream: (Stream[Data] over Credit)^, flags: List[Tar.Flag])
      ( using tarTactic: Tactic[Tar.Error], streamTactic: Tactic[Truncation.Error], buffering: Buffering )
    :   Iterator[Tar.Entry]^{tarTactic, streamTactic} =

      Tarfile.read:
        flags.prim match
          case Tar.Flag.Gzip    => stream.decompress[Gzip]
          case Tar.Flag.Zlib    => stream.decompress[Zlib]
          case Tar.Flag.Deflate => stream.decompress[Deflate]
          case _                => stream

case class SparseSegment(offset: Long, length: Long)

sealed trait Tar
