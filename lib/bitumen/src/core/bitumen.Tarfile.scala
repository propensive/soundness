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

import rudiments.*


import scala.caps

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
import symbolism.*

enum LongNameFormat:
  case Pax
  case Gnu

object Tarfile:
  val zeroBlock: Data = Array.fill[Byte](512)(0)

  given streamable: Tarfile is Streamable by Data over Credit = tarfile =>
    Stream(tarfile.blocks)

  // The endpoint form: entries parse lazily straight off a pull endpoint (one
  // consumed entry advances the cursor past it), absorbing arbitrary chunk
  // boundaries — the archive need never be materialized. The resulting
  // entries are single-owner: consume them in order, on one thread. Advancing
  // to the next entry first drains whatever of the previous entry's body was
  // not yet read into its memoizing `Tar.Body` — an in-order consumer streams
  // with bounded memory, while listing entries before reading a body still
  // works, at the cost of buffering the passed-over bodies, which is what the
  // eager reader always did.
  // An explicit `Tactic` rather than `raises` sugar: a fresh capability in a
  // context-function result cannot flow to a forwarding caller.
  def read(consume stream: (Stream[Data] over Credit)^)(using tactic: Tactic[Tar.Error])
  :   Iterator[Tar.Entry]^{tactic} =

    // The stream's single ownership passes to the cursor inside the iterator, whose fresh
    // capability is laundered (nothing else can reach it).
    scala.caps.unsafe.unsafeAssumePure:
      scala.caps.unsafe.unsafeAssumeSeparate(entryIterator(Cursor[Data](stream)))

  def from(consume stream: (Stream[Data] over Credit)^)(using Tactic[Tar.Error]): Tarfile =
    // The stream's single ownership passes with this call; the checker cannot see through
    // the consumed parameter's re-use in the nested call.
    scala.caps.unsafe.unsafeAssumeSeparate:
      Tarfile(read(stream).to(List).asInstanceOf[List[Tar.Entry]])

  // Pulls an entry's `size` bytes off the shared cursor in bounded chunks,
  // consuming the trailing block padding after the final one. The closure is
  // handed to `Tar.Body.deferred`, whose memoization guarantees the region is
  // read exactly once, in order.
  private def bodyPull(cursor: Cursor[Data, {}]^, size: Int, padded: Int)
    ( using tactic: Tactic[Tar.Error] )
  :   () ->{cursor, tactic} Optional[Data] =

    @caps.unsafe.untrackedCaptures
    var consumed: Int = 0

    val chunkSize: Int = 65536

    () =>
      if consumed >= size then
        if consumed < padded then
          cursor.take(abort(Tar.Error(Tar.Error.Reason.TruncatedStream(padded - consumed,
              cursor.available))))(padded - consumed)
          consumed = padded
        Unset
      else
        val n = (size - consumed).min(chunkSize)

        // The inline `take` expansion re-infers a fresh `any.rd` on the frozen chunk;
        // the cast reasserts the frozen form, which `take` already guarantees.
        val data =
          cursor.take(abort(Tar.Error(Tar.Error.Reason.TruncatedStream(n, cursor.available))))(n)
          . asInstanceOf[Data]

        consumed += n
        data

  private def entryIterator(cursor: Cursor[Data, {}]^)(using tactic: Tactic[Tar.Error])
  :   Iterator[Tar.Entry]^{cursor, tactic} =

    new Iterator[Tar.Entry]:
      // A stdlib class cannot extend `Stateful`, so its state is untracked
      // (the record-iterator precedent).
      @caps.unsafe.untrackedCaptures
      private var lookahead: Optional[Tar.Entry] = Unset
      @caps.unsafe.untrackedCaptures
      private var unread: Optional[Tar.Body] = Unset
      @caps.unsafe.untrackedCaptures
      private var globalOverlay: Map[Text, Text] = Map.empty
      @caps.unsafe.untrackedCaptures
      private var finished: Boolean = false

      def hasNext: Boolean = !lookahead.absent || (!finished && advance())

      def next(): Tar.Entry = lookahead match
        case entry: Tar.Entry =>
          lookahead = Unset
          entry

        case Unset =>
          if !finished && advance() then next() else panic(m"the archive has no more entries")

      // Parse forward to the next real entry, first draining whatever of the
      // previous entry's body was not yet read, so the cursor stands at the
      // next header. Metadata pseudo-entries (PAX and GNU long-name blocks)
      // accumulate into the overlays consumed by the entry they precede.
      private def advance(): Boolean =
        unread.let(_.drain())
        unread = Unset

        var paxOverlay: Map[Text, Text] = Map.empty
        var longName: Optional[Text] = Unset
        var longLink: Optional[Text] = Unset

        while lookahead.absent && !finished do
          takeBlock(cursor) match
            case Unset =>
              // The archive ended without its terminating zero blocks.
              raise(Tar.Error(Tar.Error.Reason.TruncatedStream(512, 0)))
              finished = true

            case head: Data if TarHeader.isZeroBlock(head) => finished = true

            case head: Data =>
              val header = TarHeader.parse(head)

              val checksummed: Venture[Unit] = venture:
                TarHeader.verifyChecksum(head, TarHeader.decodeOctal(header.checksum, t"checksum"))

              // A block that fails its checksum cannot be trusted for anything — including the
              // size that locates the next header — so parsing on would only manufacture
              // cascade errors from corrupt bytes. Record the checksum error and end the walk.
              if !checksummed.ready then finished = true else
                val size: Int = TarHeader.decodeOctal(header.size, t"size").long.toInt
                val mtime: U32 = TarHeader.decodeOctal(header.mtime, t"mtime")
                val mode = UnixMode.from(TarHeader.decodeOctal(header.mode, t"mode").long.toInt)
                val uid = TarHeader.decodeOctal(header.uid, t"uid").long.toInt
                val gid = TarHeader.decodeOctal(header.gid, t"gid").long.toInt

                val unameText =
                  paxOverlay.at("uname".tt).or(globalOverlay.at("uname".tt))
                  . or(TarHeader.decodeNulText(header.uname))

                val gnameText =
                  paxOverlay.at("gname".tt).or(globalOverlay.at("gname".tt))
                  . or(TarHeader.decodeNulText(header.gname))

                val user = UnixUser(uid, if unameText.s.isEmpty then Unset else unameText)
                val group = UnixGroup(gid, if gnameText.s.isEmpty then Unset else gnameText)

                header.typeFlag.toInt & 0xff match
                  case 'x' =>
                    paxOverlay = paxOverlay + Pax.parse(takeData(cursor, size))

                  case 'g' =>
                    globalOverlay = globalOverlay + Pax.parse(takeData(cursor, size))

                  case 'L' =>
                    longName = TarHeader.decodeNulText(takeData(cursor, size))

                  case 'K' =>
                    longLink = TarHeader.decodeNulText(takeData(cursor, size))

                  case 'S' =>
                    val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
                    val path = decodePath(nameText)

                    val inlineSegments: List[SparseSegment] = readInlineSparseMap(head)
                    val isExtended: Boolean = head.readUnchecked(482) != 0.toByte

                    val realSize: Long =
                      TarHeader.decodeOctal(head.segment((483).z till (495).z), t"realsize").long

                    val extSegments = readSparseExtensions(cursor, isExtended)
                    val data = takeData(cursor, size)

                    val allSegments = (inlineSegments + extSegments).filter(_.length > 0)

                    val overlay: Map[Text, Text] = globalOverlay + paxOverlay

                    val extras: Map[Text, Text] = overlay.filter: (k, _) =>
                      !structuralPaxKeys.has(k)

                    lookahead =
                      Tar.Entry.Sparse
                        ( path, mode, user, group, mtime, realSize, allSegments, Tar.Body(data),
                          extras )

                  case flag if flag == 0 || flag == '0' || flag == '7' =>
                    val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
                    val path = decodePath(nameText)

                    val overlay: Map[Text, Text] = globalOverlay + paxOverlay

                    val extras: Map[Text, Text] = overlay.filter: (k, _) =>
                      !structuralPaxKeys.has(k)

                    // The body pulls off the shared cursor; advancing to the
                    // next entry drains whatever of it remains unread.
                    val body =
                      Tar.Body.deferred:
                        // Erases the two independently-freshened `any.rd`s on the frozen
                        // chunk type (result position vs parameter position).
                        bodyPull(cursor, size, ((size + 511)/512)*512)
                        . asInstanceOf[() => Optional[Data]]

                    unread = body
                    lookahead = Tar.Entry.File(path, mode, user, group, mtime, body, extras)

                  case flag =>
                    val nameText = resolveName(header, paxOverlay, globalOverlay, longName)
                    val linkText = resolveLink(header, paxOverlay, globalOverlay, longLink)
                    val path = decodePath(nameText)

                    val overlay: Map[Text, Text] = globalOverlay + paxOverlay

                    val extras: Map[Text, Text] = overlay.filter: (k, _) =>
                      !structuralPaxKeys.has(k)

                    // Drained here, not in `buildEntry` (a `consume`d cursor cannot cross that
                    // call): zero bytes for the known dataless kinds, and an unknown kind's
                    // declared payload, keeping the cursor synchronized with the next header.
                    val data = takeData(cursor, size)

                    lookahead =
                      buildEntry(flag, path, mode, user, group, mtime, linkText, extras,
                        header, data)

        !lookahead.absent

  private def buildEntry
    ( flag:   Int,
      path:   Tar.Ref,
      mode:   UnixMode,
      user:   UnixUser,
      group:  UnixGroup,
      mtime:  U32,
      link:   Text,
      extras: Map[Text, Text],
      header: TarHeader,
      data:   Data )
  :   Tar.Entry raises Tar.Error =

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
        // An unknown type flag records its error and degrades to a regular file — POSIX's
        // prescribed treatment for unrecognised flags — never a directory, which would corrupt
        // the archive's tree (children attaching beneath it, and `TarFilesystem` creating it).
        raise(Tar.Error(Tar.Error.Reason.UnknownTypeFlag(other.toByte)))
        Tar.Entry.File(path, mode, user, group, mtime, Tar.Body(data), extras)

  // The next 512-byte block, or `Unset` at clean end-of-archive; a partial
  // block raises. One allocation per header block.
  private def takeBlock(cursor: Cursor[Data, {}]^)(using Tactic[Tar.Error]): Optional[Data] =
    if cursor.finished then Unset
    else cursor.take(abort(Tar.Error(Tar.Error.Reason.TruncatedStream(512, cursor.available))))(512)

  // An entry's `size` bytes of data plus its padding, in a single allocation
  // (the block-list fold this replaces reallocated per block).
  private def takeData(cursor: Cursor[Data, {}]^, size: Int)(using Tactic[Tar.Error]): Data =
    val padded = ((size + 511)/512)*512

    val data = cursor.take(abort(Tar.Error(Tar.Error.Reason.TruncatedStream(padded,
        cursor.available))))(padded)

    data.segment((0).z till (size).z)

  private def decodeSparseField(data: Data): Long raises Tar.Error =
    var allZero = true
    var i = 0

    while i < data.length && allZero do
      if data.readUnchecked(i) != 0.toByte then allZero = false
      i = i + 1

    if allZero then 0L else TarHeader.decodeOctal(data, t"sparse.field").long

  private def readInlineSparseMap(headerBlock: Data): List[SparseSegment] raises Tar.Error =
    val builder = scala.collection.immutable.List.newBuilder[SparseSegment]
    var pos = 386
    var i = 0

    while i < 4 do
      val offset = decodeSparseField(headerBlock.segment((pos).z till (pos + 12).z))
      val length = decodeSparseField(headerBlock.segment((pos + 12).z till (pos + 24).z))

      if length > 0 then builder += SparseSegment(offset, length)
      pos = pos + 24
      i = i + 1

    builder.result().to(List)

  private def readSparseExtensions(cursor: Cursor[Data, {}]^, hasMore: Boolean)
    ( using Tactic[Tar.Error] )
  :   List[SparseSegment] =

    if !hasMore then Nil else takeBlock(cursor) match
      case Unset =>
        raise(Tar.Error(Tar.Error.Reason.TruncatedStream(512, 0)))
        Nil

      case head: Data =>
        val builder = scala.collection.immutable.List.newBuilder[SparseSegment]
        var pos = 0
        var i = 0

        while i < 21 do
          val offset = decodeSparseField(head.segment((pos).z till (pos + 12).z))
          val length = decodeSparseField(head.segment((pos + 12).z till (pos + 24).z))

          if length > 0 then builder += SparseSegment(offset, length)
          pos = pos + 24
          i = i + 1

        val moreExtended = head.readUnchecked(504) != 0.toByte
        builder.result().to(List) + readSparseExtensions(cursor, moreExtended)

  private def resolveName
    ( header:        TarHeader,
      paxOverlay:    Map[Text, Text],
      globalOverlay: Map[Text, Text],
      longName:      Optional[Text] )
  :   Text =

    longName.or:
      paxOverlay.at("path".tt).or(globalOverlay.at("path".tt)).lay:
        val name = TarHeader.decodeNulText(header.name)
        val prefix = TarHeader.decodeNulText(header.prefix)
        stripTrailingSlash(if prefix.s.isEmpty then name else t"$prefix/$name")
      . apply: text =>
        stripTrailingSlash(text)

  private def resolveLink
    ( header:        TarHeader,
      paxOverlay:    Map[Text, Text],
      globalOverlay: Map[Text, Text],
      longLink:      Optional[Text] )
  :   Text =

    longLink.or:
      paxOverlay.at("linkpath".tt).or(globalOverlay.at("linkpath".tt))
      . or(TarHeader.decodeNulText(header.linkName))

  private def stripTrailingSlash(text: Text): Text =
    if text.s.endsWith("/") then text.s.dropRight(1).nn.tt else text

  private def decodePath(text: Text): Tar.Ref raises Tar.Error =
    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case Path.Error(_, _) => Tar.Error(Tar.Error.Reason.BadName(text))

    . protect(text.as[Relative on Tar])

  private val structuralPaxKeys: Set[Text] = Set(t"path", t"linkpath", t"uname", t"gname")

  // The blocks that precede an entry's own header: GNU long-name/long-link
  // pseudo-entries and/or a PAX extended-header entry, per the format. These
  // depend only on names and attributes — never on the payload size — so the
  // streaming writer can emit them before an unknown-length body.
  private[bitumen] def preamble(entry: Tar.Entry, longNameFormat: LongNameFormat)
  :   Iterator[Data] =

    val longNamePart: Iterator[Data] = longNameFormat match
      case LongNameFormat.Pax => Iterator.empty

      case LongNameFormat.Gnu =>
        val nameBlocks =
          if entry.entryName.in[Data].length > 100
          then Tar.Entry.GnuLong(TypeFlag.LongName, entry.entryName).serialize
          else Iterator.empty

        val linkBlocks = entry.link.let: l =>
          if l.in[Data].length > 100 then Tar.Entry.GnuLong(TypeFlag.LongLink, l).serialize
          else Iterator.empty

        . or(Iterator.empty)

        nameBlocks ++ linkBlocks

    val records = paxRecordsFor(entry).filter: (key, _) =>
      longNameFormat match
        case LongNameFormat.Pax => true
        case LongNameFormat.Gnu => key != t"path" && key != t"linkpath"

    val paxPart: Iterator[Data] =
      if records.nil then Iterator.empty else Tar.Entry.Pax(Pax.records(records)).serialize

    longNamePart ++ paxPart

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

    paxOf(entry).foreach: (k, v) =>
      if !structuralPaxKeys.has(k) then builder += ((k, v))

    builder.result().to(List)

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
  ( entries: List[Tar.Entry], longNameFormat: LongNameFormat = LongNameFormat.Pax ):
  // The raw 512-byte blocks of the archive, including the two trailing zero blocks.
  // Reach this externally through the `Streamable` given, i.e. `tarfile.source[Data]`.
  private[bitumen] def blocks: Iterator[Data] =
    // The blocks are emitted through a stdlib `Iterator`, which the opaque `List` cannot yield.
    entries.stdlib.iterator.flatMap(emitEntry) ++ Iterator(Tarfile.zeroBlock, Tarfile.zeroBlock)

  // Compressed views of the archive's TAR stream.
  def gzip: (Stream[Data] over Credit)^ = Stream(blocks).compress[Gzip]
  def zlib: (Stream[Data] over Credit)^ = Stream(blocks).compress[Zlib]
  def deflate: (Stream[Data] over Credit)^ = Stream(blocks).compress[Deflate]

  private def emitEntry(entry: Tar.Entry): Iterator[Data] =
    Tarfile.preamble(entry, longNameFormat) ++ entry.serialize
