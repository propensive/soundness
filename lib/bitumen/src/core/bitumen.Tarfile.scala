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
package bitumen

import proscenium.compat.*
import rudiments.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.asciiEncoder
import hypotenuse.*
import prepositional.*
import serpentine.*
import pneumatic.*
import turbulence.*
import vacuous.*
import zephyrine.*

enum LongNameFormat:
  case Pax
  case Gnu

object Tarfile:
  val zeroBlock: Data = IArray.fill[Byte](512)(0)

  given streamable: Tarfile is Streamable by Data over Credit = tarfile =>
    Stream(tarfile.blocks.iterator)

  def read(stream: Progression[Data]): Progression[Tar.Entry] raises TarError =
    readEntries(Cursor[Data](stream.filter(_.nonEmpty).iterator), Map.empty, Map.empty, Unset,
        Unset)

  // The endpoint form: entries parse lazily straight off a pull endpoint (one
  // forced entry advances the cursor past it), absorbing arbitrary chunk
  // boundaries — the archive need never be materialized. The resulting
  // entries are single-owner: consume them in order, on one thread.
  def read(consume stream: (Stream[Data] over Credit)^): Progression[Tar.Entry] raises TarError =
    readEntries(Cursor[Data](stream), Map.empty, Map.empty, Unset, Unset)

  def from(stream: Progression[Data]): Tarfile raises TarError = Tarfile(read(stream))

  def fromGzip(stream: Progression[Data]): Progression[Tar.Entry] raises TarError =
    read(stream.decompress[Gzip])

  def fromZlib(stream: Progression[Data]): Progression[Tar.Entry] raises TarError =
    read(stream.decompress[Zlib])

  def fromDeflate(stream: Progression[Data]): Progression[Tar.Entry] raises TarError =
    read(stream.decompress[Deflate])

  // Per-entry body accounting for the streaming reader. A file's payload
  // streams lazily off the shared cursor in bounded chunks; forcing the NEXT
  // entry first forces whatever of this body was not yet read (its `Progression`
  // memoizes, so this materializes only skipped-over payloads — an in-order
  // consumer streams with bounded memory, while listing entries before
  // reading a body still works, at the cost of buffering the passed-over
  // bodies, which is what the eager reader always did).
  private class TarBody(cursor: Cursor[Data, {}]^, size: Int, padded: Int)
    ( using Tactic[TarError] ):

    private var consumed: Int = 0
    private val chunkSize: Int = 65536

    // A single memoizing chain per body: every consumer (including `drain`)
    // shares it, so the cursor region is read exactly once.
    lazy val stream: Progression[Data] = chunk(0)

    private def chunk(offset: Int): Progression[Data] = Progression.defer:
      if offset >= size then
        finishPadding()
        Progression()
      else
        val n = (size - offset).min(chunkSize)

        val data =
          cursor.take(abort(TarError(TarError.Reason.TruncatedStream(n, cursor.available))))(n)

        consumed = offset + n
        data #:: chunk(offset + n)

    // Consume the trailing padding once the data region is fully read.
    private def finishPadding(): Unit =
      if consumed < padded then
        cursor.take(abort(TarError(TarError.Reason.TruncatedStream(padded - consumed,
            cursor.available))))(padded - consumed)
        consumed = padded

    // Force the remainder of this body, so the cursor stands at the next
    // header. Already-read chunks are memoized and not re-read.
    private[Tarfile] def drain(): Unit =
      var rest = stream
      while !rest.isEmpty do rest = rest.tail

  private def readEntries
    ( cursor:        Cursor[Data, {}]^,
      paxOverlay:    Map[Text, Text],
      globalOverlay: Map[Text, Text],
      longName:      Optional[Text],
      longLink:      Optional[Text] )
  :   Progression[Tar.Entry] raises TarError = Progression.defer:
    val block = takeBlock(cursor)

    if block.absent then
      // The archive ended without its terminating zero blocks.
      raise(TarError(TarError.Reason.TruncatedStream(512, 0)))
      Progression()
    else
      val head = block.vouch
      if TarHeader.isZeroBlock(head) then Progression() else
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
              val pax = Pax.parse(takeData(cursor, size))
              readEntries(cursor, Map.of(paxOverlay.stdlib ++ pax.stdlib), globalOverlay, longName, longLink)

            case 'g' =>
              val pax = Pax.parse(takeData(cursor, size))
              readEntries(cursor, paxOverlay, Map.of(globalOverlay.stdlib ++ pax.stdlib), longName, longLink)

            case 'L' =>
              val name = TarHeader.decodeNulText(takeData(cursor, size))
              readEntries(cursor, paxOverlay, globalOverlay, name, longLink)

            case 'K' =>
              val link = TarHeader.decodeNulText(takeData(cursor, size))
              readEntries(cursor, paxOverlay, globalOverlay, longName, link)

            case 'S' =>
              val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
              val path = decodePath(nameText)

              val inlineSegments = readInlineSparseMap(head)
              val isExtended: Boolean = head(482) != 0.toByte
              val realSize: Long = TarHeader.decodeOctal(head.slice(483, 495), t"realsize").long
              val extSegments = readSparseExtensions(cursor, isExtended)
              val data = takeData(cursor, size)

              val allSegments = (inlineSegments ++ extSegments).filter(_.length > 0)

              val extras: Map[Text, Text] =
                Map.of((globalOverlay.stdlib ++ paxOverlay.stdlib).filter { (k, _) => !structuralPaxKeys.has(k) })

              val entry =
                Tar.Entry.Sparse
                  ( path, mode, user, group, mtime, realSize, List.of(allSegments), Progression(data), extras )

              entry #:: readEntries(cursor, Map.empty, globalOverlay, Unset, Unset)

            case flag if flag == 0 || flag == '0' || flag == '7' =>
              val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
              val path = decodePath(nameText)

              val extras: Map[Text, Text] =
                Map.of((globalOverlay.stdlib ++ paxOverlay.stdlib).filter { (k, _) => !structuralPaxKeys.has(k) })

              // The body streams off the shared cursor; forcing the tail of
              // this cons (the next entry) drains whatever of it was unread.
              val body = TarBody(cursor, size, ((size + 511)/512)*512)
              val entry = Tar.Entry.File(path, mode, user, group, mtime, body.stream, extras)

              entry #:: Progression.defer:
                body.drain()
                readEntries(cursor, Map.empty, globalOverlay, Unset, Unset)

            case flag =>
              val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
              val linkText = resolveLink(header, paxOverlay, globalOverlay, longLink)
              val path = decodePath(nameText)

              val extras: Map[Text, Text] =
                Map.of((globalOverlay.stdlib ++ paxOverlay.stdlib).filter { (k, _) => !structuralPaxKeys.has(k) })

              val entry =
                buildEntry(flag, path, mode, user, group, mtime, size, linkText, extras, header,
                  cursor)

              entry #:: readEntries(cursor, Map.empty, globalOverlay, Unset, Unset)

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
      cursor: Cursor[Data, {}]^ )
  :   Tar.Entry raises TarError =

    flag match
      case '5' =>
        Tar.Entry.Directory(path, mode, user, group, mtime, extras)

      case '1' =>
        Tar.Entry.Link(path, mode, user, group, mtime, link, extras)

      case '2' =>
        Tar.Entry.Symlink(path, mode, user, group, mtime, link, extras)

      case '3' =>
        val major = TarHeader.decodeOctal(header.devMajor, t"devmajor")
        val minor = TarHeader.decodeOctal(header.devMinor, t"devminor")
        Tar.Entry.CharSpecial(path, mode, user, group, mtime, (major, minor), extras)

      case '4' =>
        val major = TarHeader.decodeOctal(header.devMajor, t"devmajor")
        val minor = TarHeader.decodeOctal(header.devMinor, t"devminor")
        Tar.Entry.BlockSpecial(path, mode, user, group, mtime, (major, minor), extras)

      case '6' =>
        Tar.Entry.Fifo(path, mode, user, group, mtime, extras)

      case other =>
        raise(TarError(TarError.Reason.UnknownTypeFlag(other.toByte)))
        Tar.Entry.Directory(path, mode, user, group, mtime, extras)

  // The next 512-byte block, or `Unset` at clean end-of-archive; a partial
  // block raises. One allocation per header block.
  private def takeBlock(cursor: Cursor[Data, {}]^): Optional[Data] raises TarError =
    if cursor.finished then Unset
    else cursor.take(abort(TarError(TarError.Reason.TruncatedStream(512, cursor.available))))(512)

  // An entry's `size` bytes of data plus its padding, in a single allocation
  // (the block-list fold this replaces reallocated per block).
  private def takeData(cursor: Cursor[Data, {}]^, size: Int): Data raises TarError =
    val padded = ((size + 511)/512)*512

    val data = cursor.take(abort(TarError(TarError.Reason.TruncatedStream(padded,
        cursor.available))))(padded)

    data.slice(0, size)

  private def decodeSparseField(data: Data): Long raises TarError =
    var allZero = true
    var i = 0

    while i < data.length && allZero do
      if data(i) != 0.toByte then allZero = false
      i = i + 1

    if allZero then 0L else TarHeader.decodeOctal(data, t"sparse.field").long

  private def readInlineSparseMap(headerBlock: Data)
  :   scala.collection.immutable.List[SparseSegment] raises TarError =

    val builder = scala.collection.immutable.List.newBuilder[SparseSegment]
    var pos = 386
    var i = 0

    while i < 4 do
      val offset = decodeSparseField(headerBlock.slice(pos, pos + 12))
      val length = decodeSparseField(headerBlock.slice(pos + 12, pos + 24))

      if length > 0 then builder += SparseSegment(offset, length)
      pos = pos + 24
      i = i + 1

    builder.result()

  private def readSparseExtensions(cursor: Cursor[Data, {}]^, hasMore: Boolean)
  :   scala.collection.immutable.List[SparseSegment] raises TarError =

    if !hasMore then Nil.stdlib else
      val block = takeBlock(cursor)

      if block.absent then
        raise(TarError(TarError.Reason.TruncatedStream(512, 0)))
        Nil.stdlib
      else
        val head = block.vouch
        val builder = scala.collection.immutable.List.newBuilder[SparseSegment]
        var pos = 0
        var i = 0

        while i < 21 do
          val offset = decodeSparseField(head.slice(pos, pos + 12))
          val length = decodeSparseField(head.slice(pos + 12, pos + 24))

          if length > 0 then builder += SparseSegment(offset, length)
          pos = pos + 24
          i = i + 1

        val moreExtended = head(504) != 0.toByte
        builder.result() ++ readSparseExtensions(cursor, moreExtended)

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

    . protect(text.as[Relative on Tar])

  private val structuralPaxKeys: Set[Text] = Set(t"path", t"linkpath", t"uname", t"gname")

  // The blocks that precede an entry's own header: GNU long-name/long-link
  // pseudo-entries and/or a PAX extended-header entry, per the format. These
  // depend only on names and attributes — never on the payload size — so the
  // streaming writer can emit them before an unknown-length body.
  private[bitumen] def preamble(entry: Tar.Entry, longNameFormat: LongNameFormat)
  :   Progression[Data] =

    val longNamePart: Progression[Data] = longNameFormat match
      case LongNameFormat.Pax => Progression()

      case LongNameFormat.Gnu =>
        val nameBlocks =
          if entry.entryName.in[Data].length > 100
          then Tar.Entry.GnuLong(TypeFlag.LongName, entry.entryName).serialize
          else Progression()

        val linkBlocks = entry.link.let: l =>
          if l.in[Data].length > 100 then Tar.Entry.GnuLong(TypeFlag.LongLink, l).serialize
          else Progression()

        . or(Progression())

        nameBlocks #::: linkBlocks

    val records = paxRecordsFor(entry).stdlib.filter: (key, _) =>
      longNameFormat match
        case LongNameFormat.Pax => true
        case LongNameFormat.Gnu => key != t"path" && key != t"linkpath"

    val paxPart: Progression[Data] =
      if records.isEmpty then Progression()
      else Tar.Entry.Pax(Pax.records(records)).serialize

    longNamePart #::: paxPart

  private def paxRecordsFor(entry: Tar.Entry): List[(Text, Text)] =
    val builder = scala.collection.immutable.List.newBuilder[(Text, Text)]
    if entry.entryName.in[Data].length > 100 then builder += ((t"path", entry.entryName))

    entry.link.let: link =>
      if link.in[Data].length > 100 then builder += ((t"linkpath", link))

    val (user, group) = userAndGroup(entry)

    user.name.let: name =>
      if name.in[Data].length > 32 then builder += ((t"uname", name))

    group.name.let: name =>
      if name.in[Data].length > 32 then builder += ((t"gname", name))

    paxOf(entry).each: (k, v) =>
      if !structuralPaxKeys.has(k) then builder += ((k, v))

    List.of(builder.result())

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
  ( entries: Progression[Tar.Entry], longNameFormat: LongNameFormat = LongNameFormat.Pax ):
  // The raw 512-byte blocks of the archive, including the two trailing zero blocks.
  // Reach this externally through the `Streamable` given, i.e. `tarfile.source[Data]`.
  private[bitumen] def blocks: Progression[Data] =
    entries.flatMap(emitEntry) #::: Progression(Tarfile.zeroBlock, Tarfile.zeroBlock)

  // Compressed views of the archive's TAR stream.
  def gzip: Progression[Data] = blocks.compress[Gzip]
  def zlib: Progression[Data] = blocks.compress[Zlib]
  def deflate: Progression[Data] = blocks.compress[Deflate]

  private def emitEntry(entry: Tar.Entry): Progression[Data] =
    Tarfile.preamble(entry, longNameFormat) #::: entry.serialize
