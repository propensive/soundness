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
package bitumen

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import hieroglyph.*, charEncoders.ascii, textMetrics.uniform
import hypotenuse.*, arithmeticOptions.overflow.unchecked
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

object Tar:
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

  object Entry:
    def apply[data: Streamable by Data, instant: Abstractable across Instants to Long]
      ( name:  TarRef,
        data:  data,
        mode:  UnixMode          = UnixMode(),
        user:  UnixUser          = UnixUser(0),
        group: UnixGroup         = UnixGroup(0),
        mtime: Optional[instant] = Unset )
    :   Entry =

      val mtimeU32: U32 =
        (mtime.let(_.generic).or(System.currentTimeMillis)/1000).toInt.bits.u32

      Entry.File(name, mode, user, group, mtimeU32, data.stream[Data])

    private[bitumen] val paxRef: TarRef =
      import strategies.throwUnsafely
      t"PaxHeaders/0".decode[Relative on Tar]

    private[bitumen] def sparseExtensionBlocks(segments: List[SparseSegment]): LazyList[Data] =
      if segments.isEmpty then LazyList()
      else
        val (batch, rest) = segments.splitAt(21)

        val block: Data = Data.build(512): array =>
          var pos = 0

          batch.foreach: seg =>
            array.place(formatLongOctal(seg.offset, 12), pos.z)
            pos = pos + 12
            array.place(formatLongOctal(seg.length, 12), pos.z)
            pos = pos + 12

          if rest.nonEmpty then array(504) = 1.toByte

        block #:: sparseExtensionBlocks(rest)

    private[bitumen] def formatLongOctal(number: Long, width: Int): Data =
      val str: String = java.lang.Long.toOctalString(number).nn
      val pad: Int = (width - 1 - str.length).max(0)
      (("0"*pad) + str).tt.data

  enum Entry(path: TarRef, mode: UnixMode, user: UnixUser, group: UnixGroup, mtime: U32):
    case File
      ( path:  TarRef,
        mode:  UnixMode,
        user:  UnixUser,
        group: UnixGroup,
        mtime: U32,
        data:  LazyList[Data],
        pax:   Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Directory
      ( path:  TarRef,
        mode:  UnixMode,
        user:  UnixUser,
        group: UnixGroup,
        mtime: U32,
        pax:   Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Link
      ( path:   TarRef,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        target: Text,
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Symlink
      ( path:   TarRef,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        target: Text,
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case CharSpecial
      ( path:   TarRef,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        device: (U32, U32),
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case BlockSpecial
      ( path:   TarRef,
        mode:   UnixMode,
        user:   UnixUser,
        group:  UnixGroup,
        mtime:  U32,
        device: (U32, U32),
        pax:    Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)

    case Fifo
      ( path:  TarRef,
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
      ( path:     TarRef,
        mode:     UnixMode,
        user:     UnixUser,
        group:    UnixGroup,
        mtime:    U32,
        realSize: Long,
        segments: List[SparseSegment],
        data:     LazyList[Data],
        pax:      Map[Text, Text] = Map.empty )
    extends Entry(path, mode, user, group, mtime)


    def size: U32 = this match
      case file: File      => file.data.sumBy(_.length).bits.u32
      case pax: Pax        => pax.records.length.bits.u32
      case long: GnuLong   => (long.content.data.length + 1).bits.u32
      case sparse: Sparse  => sparse.segments.map(_.length).sum.toInt.bits.u32
      case _               => 0

    def dataBlocks: LazyList[Data] = this match
      case file: File      => file.data.chunked(512, zeroPadding = true)
      case pax: Pax        => LazyList(pax.records).chunked(512, zeroPadding = true)

      case long: GnuLong =>
        LazyList(long.content.data ++ IArray.fill[Byte](1)(0)).chunked(512, zeroPadding = true)

      case sparse: Sparse =>
        sparse.data.chunked(512, zeroPadding = true)

      case _ =>
        LazyList()

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
      number.octal.pad(width - 1).data

    def formatLong(number: Long, width: Int): Data =
      val str: String = java.lang.Long.toOctalString(number).nn
      val pad: Int = (width - 1 - str.length).max(0)
      (("0"*pad) + str).tt.data

    def header: Data = Data.build(512): array =>
      val nameData = entryName.data
      array.place(if nameData.length > 100 then nameData.slice(0, 100) else nameData, Prim)
      array.place(mode.bytes, 100.z)
      array.place(user.bytes, 108.z)
      array.place(group.bytes, 116.z)
      array.place(format(size, 12), 124.z)
      array.place(format(mtime, 12), 136.z)
      array.place(t"        ".data, 148.z)
      array(156) = typeFlag.id.toByte

      link.let: link =>
        val linkData = link.data
        array.place(if linkData.length > 100 then linkData.slice(0, 100) else linkData, 157.z)

      deviceNumbers.let: (devMajor, devMinor) =>
        array.place(format(devMajor, 8), 329.z)
        array.place(format(devMinor, 8), 337.z)

      user.name.let: name =>
        val nameData = name.data
        array.place(if nameData.length > 32 then nameData.slice(0, 32) else nameData, 265.z)

      group.name.let: name =>
        val nameData = name.data
        array.place(if nameData.length > 32 then nameData.slice(0, 32) else nameData, 297.z)

      array.place(t"ustar\u0000".data, 257.z)
      array.place(t"00".data, 263.z)

      this.only:
        case sparse: Sparse =>
          val inline = sparse.segments.take(4)
          var pos = 386

          inline.foreach: seg =>
            array.place(formatLong(seg.offset, 12), pos.z)
            pos = pos + 12
            array.place(formatLong(seg.length, 12), pos.z)
            pos = pos + 12

          if sparse.segments.length > 4 then array(482) = 1.toByte
          array.place(formatLong(sparse.realSize, 12), 483.z)

      val total = array.iterator.map(_.bits.u8.u32).reduce(_ + _)
      array.place(format(total, 8), 148.z)

    def serialize: LazyList[Data] = this match
      case sparse: Sparse if sparse.segments.length > 4 =>
        header #:: Entry.sparseExtensionBlocks(sparse.segments.drop(4)) #::: dataBlocks

      case _ =>
        header #:: dataBlocks

case class SparseSegment(offset: Long, length: Long)

sealed trait Tar
