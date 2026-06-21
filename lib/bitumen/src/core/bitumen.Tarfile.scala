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
import fulminate.*
import galilei.*
import gossamer.*
import hieroglyph.*, charEncoders.asciiEncoder
import hypotenuse.*
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*

enum LongNameFormat:
  case Pax
  case Gnu

object Tarfile:
  val zeroBlock: Data = IArray.fill[Byte](512)(0)

  given streamable: Tarfile is Streamable by Data = _.blocks

  def read(stream: Stream[Data]): Stream[Tar.Entry] raises TarError =
    readEntries(stream.chunked(512), Map.empty, Map.empty, Unset, Unset)

  def from(stream: Stream[Data]): Tarfile raises TarError = Tarfile(read(stream))

  def fromGzip(stream: Stream[Data]): Stream[Tar.Entry] raises TarError =
    read(stream.decompress[Gzip])

  def fromZlib(stream: Stream[Data]): Stream[Tar.Entry] raises TarError =
    read(stream.decompress[Zlib])

  def fromDeflate(stream: Stream[Data]): Stream[Tar.Entry] raises TarError =
    read(stream.decompress[Deflate])

  def from[plane <: Posix: Filesystem](root: Path on plane)
    ( using DereferenceSymlinks,
            TraversalOrder,
            plane is Explorable )
  :   Tarfile raises IoError raises TarError =

    val entries: LazyList[Tar.Entry] = root.descendants.to(LazyList).map: path =>
      TarFilesystem.entryFor(root, path)

    Tarfile(entries)

  private def readEntries
    ( blocks:        Stream[Data],
      paxOverlay:    Map[Text, Text],
      globalOverlay: Map[Text, Text],
      longName:      Optional[Text],
      longLink:      Optional[Text] )
  :   Stream[Tar.Entry] raises TarError =

    blocks match
      case head #:: tail =>
        if TarHeader.isZeroBlock(head) then Stream() else
          val header = TarHeader.parse(head)
          val checksum = TarHeader.decodeOctal(header.checksum, t"checksum")
          TarHeader.verifyChecksum(head, checksum)
          val size: Int = TarHeader.decodeOctal(header.size, t"size").long.toInt
          val mtime: U32 = TarHeader.decodeOctal(header.mtime, t"mtime")
          val mode = UnixMode.from(TarHeader.decodeOctal(header.mode, t"mode").long.toInt)
          val uid = TarHeader.decodeOctal(header.uid, t"uid").long.toInt
          val gid = TarHeader.decodeOctal(header.gid, t"gid").long.toInt

          val unameText =
            paxOverlay.get("uname".tt).orElse(globalOverlay.get("uname".tt))
            . getOrElse(TarHeader.decodeNulText(header.uname))

          val gnameText =
            paxOverlay.get("gname".tt).orElse(globalOverlay.get("gname".tt))
            . getOrElse(TarHeader.decodeNulText(header.gname))

          val user = UnixUser(uid, if unameText.s.isEmpty then Unset else unameText)
          val group = UnixGroup(gid, if gnameText.s.isEmpty then Unset else gnameText)

          header.typeFlag.toInt & 0xff match
            case 'x' =>
              val (data, rest) = takeData(tail, size)
              val pax = Pax.parse(data)
              readEntries(rest, paxOverlay ++ pax, globalOverlay, longName, longLink)

            case 'g' =>
              val (data, rest) = takeData(tail, size)
              val pax = Pax.parse(data)
              readEntries(rest, paxOverlay, globalOverlay ++ pax, longName, longLink)

            case 'L' =>
              val (data, rest) = takeData(tail, size)
              val name = TarHeader.decodeNulText(data)
              readEntries(rest, paxOverlay, globalOverlay, name, longLink)

            case 'K' =>
              val (data, rest) = takeData(tail, size)
              val link = TarHeader.decodeNulText(data)
              readEntries(rest, paxOverlay, globalOverlay, longName, link)

            case 'S' =>
              val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
              val path = decodePath(nameText)

              val inlineSegments: List[SparseSegment] = readInlineSparseMap(head)
              val isExtended: Boolean = head(482) != 0.toByte
              val realSize: Long = TarHeader.decodeOctal(head.slice(483, 495), t"realsize").long
              val (extSegments, afterExt) = readSparseExtensions(tail, isExtended)
              val (data, rest) = takeData(afterExt, size)

              val allSegments = (inlineSegments ++ extSegments).filter(_.length > 0)

              val extras: Map[Text, Text] =
                (globalOverlay ++ paxOverlay).filter: (k, _) => !structuralPaxKeys.contains(k)

              val entry =
                Tar.Entry.Sparse
                  ( path, mode, user, group, mtime, realSize, allSegments, Stream(data), extras )

              entry #:: readEntries(rest, Map.empty, globalOverlay, Unset, Unset)

            case flag =>
              val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
              val linkText = resolveLink(header, paxOverlay, globalOverlay, longLink)
              val path = decodePath(nameText)

              val extras: Map[Text, Text] =
                (globalOverlay ++ paxOverlay).filter: (k, _) => !structuralPaxKeys.contains(k)

              val (entry, rest) =
                buildEntry(flag, path, mode, user, group, mtime, size, linkText, extras, header,
                  tail)

              entry #:: readEntries(rest, Map.empty, globalOverlay, Unset, Unset)

      case _ =>
        raise(TarError(TarError.Reason.TruncatedStream(512, 0)))
        Stream()

  private def buildEntry
    ( flag:   Int,
      path:   TarRef,
      mode:   UnixMode,
      user:   UnixUser,
      group:  UnixGroup,
      mtime:  U32,
      size:   Int,
      link:   Text,
      extras: Map[Text, Text],
      header: TarHeader,
      blocks: Stream[Data] )
  :   (Tar.Entry, Stream[Data]) raises TarError =

    flag match
      case 0 | '0' | '7' =>
        val (data, rest) = takeData(blocks, size)
        (Tar.Entry.File(path, mode, user, group, mtime, Stream(data), extras), rest)

      case '5' =>
        (Tar.Entry.Directory(path, mode, user, group, mtime, extras), blocks)

      case '1' =>
        (Tar.Entry.Link(path, mode, user, group, mtime, link, extras), blocks)

      case '2' =>
        (Tar.Entry.Symlink(path, mode, user, group, mtime, link, extras), blocks)

      case '3' =>
        val major = TarHeader.decodeOctal(header.devMajor, t"devmajor")
        val minor = TarHeader.decodeOctal(header.devMinor, t"devminor")
        (Tar.Entry.CharSpecial(path, mode, user, group, mtime, (major, minor), extras), blocks)

      case '4' =>
        val major = TarHeader.decodeOctal(header.devMajor, t"devmajor")
        val minor = TarHeader.decodeOctal(header.devMinor, t"devminor")
        (Tar.Entry.BlockSpecial(path, mode, user, group, mtime, (major, minor), extras), blocks)

      case '6' =>
        (Tar.Entry.Fifo(path, mode, user, group, mtime, extras), blocks)

      case other =>
        raise(TarError(TarError.Reason.UnknownTypeFlag(other.toByte)))
        (Tar.Entry.Directory(path, mode, user, group, mtime, extras), blocks)

  private def takeData(blocks: Stream[Data], size: Int): (Data, Stream[Data]) =
    val nBlocks = (size + 511)/512
    val (taken, rest) = blocks.splitAt(nBlocks)
    val concatenated: Data = taken.foldLeft(IArray.empty[Byte])(_ ++ _).slice(0, size)
    (concatenated, rest)

  private def decodeSparseField(data: Data): Long raises TarError =
    var allZero = true
    var i = 0

    while i < data.length && allZero do
      if data(i) != 0.toByte then allZero = false
      i = i + 1

    if allZero then 0L else TarHeader.decodeOctal(data, t"sparse.field").long

  private def readInlineSparseMap(headerBlock: Data): List[SparseSegment] raises TarError =
    val builder = List.newBuilder[SparseSegment]
    var pos = 386
    var i = 0

    while i < 4 do
      val offset = decodeSparseField(headerBlock.slice(pos, pos + 12))
      val length = decodeSparseField(headerBlock.slice(pos + 12, pos + 24))

      if length > 0 then builder += SparseSegment(offset, length)
      pos = pos + 24
      i = i + 1

    builder.result()

  private def readSparseExtensions(blocks: Stream[Data], hasMore: Boolean)
  :   (List[SparseSegment], Stream[Data]) raises TarError =

    if !hasMore then (Nil, blocks) else blocks match
      case head #:: tail =>
        val builder = List.newBuilder[SparseSegment]
        var pos = 0
        var i = 0

        while i < 21 do
          val offset = decodeSparseField(head.slice(pos, pos + 12))
          val length = decodeSparseField(head.slice(pos + 12, pos + 24))

          if length > 0 then builder += SparseSegment(offset, length)
          pos = pos + 24
          i = i + 1

        val moreExtended = head(504) != 0.toByte
        val (rest, afterRest) = readSparseExtensions(tail, moreExtended)
        (builder.result() ++ rest, afterRest)

      case _ =>
        raise(TarError(TarError.Reason.TruncatedStream(512, 0)))
        (Nil, blocks)

  private def resolveName
    ( header:        TarHeader,
      paxOverlay:    Map[Text, Text],
      globalOverlay: Map[Text, Text],
      longName:      Optional[Text] )
  :   Text =

    longName.or:
      paxOverlay.get("path".tt).orElse(globalOverlay.get("path".tt)) match
        case Some(text) => stripTrailingSlash(text)

        case None =>
          val name = TarHeader.decodeNulText(header.name)
          val prefix = TarHeader.decodeNulText(header.prefix)
          stripTrailingSlash(if prefix.s.isEmpty then name else t"$prefix/$name")

  private def resolveLink
    ( header:        TarHeader,
      paxOverlay:    Map[Text, Text],
      globalOverlay: Map[Text, Text],
      longLink:      Optional[Text] )
  :   Text =

    longLink.or:
      paxOverlay.get("linkpath".tt).orElse(globalOverlay.get("linkpath".tt)) match
        case Some(text) => text
        case None       => TarHeader.decodeNulText(header.linkName)

  private def stripTrailingSlash(text: Text): Text =
    if text.s.endsWith("/") then text.s.dropRight(1).nn.tt else text

  private def decodePath(text: Text): TarRef raises TarError =
    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case PathError(_, _) => TarError(TarError.Reason.BadName(text))

    . protect(text.decode[Relative on Tar])

  private val structuralPaxKeys: Set[Text] = Set(t"path", t"linkpath", t"uname", t"gname")

  private def paxRecordsFor(entry: Tar.Entry): List[(Text, Text)] =
    val builder = List.newBuilder[(Text, Text)]
    if entry.entryName.data.length > 100 then builder += ((t"path", entry.entryName))

    entry.link.let: link =>
      if link.data.length > 100 then builder += ((t"linkpath", link))

    val (user, group) = userAndGroup(entry)

    user.name.let: name =>
      if name.data.length > 32 then builder += ((t"uname", name))

    group.name.let: name =>
      if name.data.length > 32 then builder += ((t"gname", name))

    paxOf(entry).foreach: (k, v) =>
      if !structuralPaxKeys.contains(k) then builder += ((k, v))

    builder.result()

  private def userAndGroup(entry: Tar.Entry): (UnixUser, UnixGroup) = entry match
    case f: Tar.Entry.File         => (f.user, f.group)
    case d: Tar.Entry.Directory    => (d.user, d.group)
    case l: Tar.Entry.Link         => (l.user, l.group)
    case s: Tar.Entry.Symlink      => (s.user, s.group)
    case c: Tar.Entry.CharSpecial  => (c.user, c.group)
    case b: Tar.Entry.BlockSpecial => (b.user, b.group)
    case f: Tar.Entry.Fifo         => (f.user, f.group)
    case sp: Tar.Entry.Sparse      => (sp.user, sp.group)
    case _: Tar.Entry.Pax          => (UnixUser(0), UnixGroup(0))
    case _: Tar.Entry.GnuLong      => (UnixUser(0), UnixGroup(0))

  private def paxOf(entry: Tar.Entry): Map[Text, Text] = entry match
    case f: Tar.Entry.File         => f.pax
    case d: Tar.Entry.Directory    => d.pax
    case l: Tar.Entry.Link         => l.pax
    case s: Tar.Entry.Symlink      => s.pax
    case c: Tar.Entry.CharSpecial  => c.pax
    case b: Tar.Entry.BlockSpecial => b.pax
    case f: Tar.Entry.Fifo         => f.pax
    case sp: Tar.Entry.Sparse      => sp.pax
    case _: Tar.Entry.Pax          => Map.empty
    case _: Tar.Entry.GnuLong      => Map.empty

case class Tarfile
  ( entries: LazyList[Tar.Entry], longNameFormat: LongNameFormat = LongNameFormat.Pax ):
  // The raw 512-byte blocks of the archive, including the two trailing zero blocks.
  // Reach this externally through the `Streamable` given, i.e. `tarfile.stream[Data]`.
  private[bitumen] def blocks: LazyList[Data] =
    entries.flatMap(emitEntry) #::: LazyList(Tarfile.zeroBlock, Tarfile.zeroBlock)

  // Compressed views of the archive's TAR stream.
  def gzip: Stream[Data] = this.stream[Data].compress[Gzip]
  def zlib: Stream[Data] = this.stream[Data].compress[Zlib]
  def deflate: Stream[Data] = this.stream[Data].compress[Deflate]

  def extractTo[plane <: Posix: Filesystem](root: Path on plane)
    ( using CreateNonexistentParents on plane, OverwritePreexisting on plane )
  :   Unit raises IoError raises TarError =

    entries.foreach: entry =>
      TarFilesystem.applyEntry(root, entry)

  private def emitEntry(entry: Tar.Entry): LazyList[Data] =
    val longNamePart: LazyList[Data] = longNameFormat match
      case LongNameFormat.Pax => LazyList()

      case LongNameFormat.Gnu =>
        val nameBlocks =
          if entry.entryName.data.length > 100
          then Tar.Entry.GnuLong(TypeFlag.LongName, entry.entryName).serialize
          else LazyList()

        val linkBlocks = entry.link.let: l =>
          if l.data.length > 100 then Tar.Entry.GnuLong(TypeFlag.LongLink, l).serialize
          else LazyList()

        . or(LazyList())

        nameBlocks #::: linkBlocks

    val records = Tarfile.paxRecordsFor(entry).filter: (key, _) =>
      longNameFormat match
        case LongNameFormat.Pax => true
        case LongNameFormat.Gnu => key != t"path" && key != t"linkpath"

    val paxPart: LazyList[Data] =
      if records.nil then LazyList()
      else Tar.Entry.Pax(Pax.records(records)).serialize

    longNamePart #::: paxPart #::: entry.serialize
