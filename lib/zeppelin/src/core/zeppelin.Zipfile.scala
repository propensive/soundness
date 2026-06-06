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
package zeppelin

import java.io as ji
import java.nio as jn
import java.nio.channels as jnc
import java.nio.charset as jncs
import java.nio.file as jnf

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

object Zipfile:
  private val u32Max: Long = 0xffffffffL
  private val u16Max: Int  = 0xffff

  given streamable: Zipfile is Streamable by Data = _.serialize

  def write[path: Abstractable across Paths to Text]
     (path: path)(entries: Iterable[Zip.Entry])
  :   Unit raises ZipError =
    checkDuplicates(entries)
    val out = ji.FileOutputStream(ji.File(path.generic.s))
    try Zipfile(entries.to(LazyList)).serialize.each: chunk =>
      out.write(chunk.mutable(using Unsafe))
    finally out.close()

  def read[path: Abstractable across Paths to Text](path: path): Zipfile raises ZipError =
    parse(FileSource(path.generic))

  def read(data: Data): Zipfile raises ZipError = parse(DataSource(data))

  private def checkDuplicates(entries: Iterable[Zip.Entry]): Unit raises ZipError =
    val seen = scala.collection.mutable.HashSet[Text]()
    entries.foreach: entry =>
      if !seen.add(entry.ref.encode)
      then raise(ZipError(ZipError.Reason.DuplicateEntry(entry.ref)))

  // A random-access view of the bytes backing a ZIP archive.
  private trait ByteSource:
    def size: Long
    def read(offset: Long, length: Int): Data

  private class DataSource(data: Data) extends ByteSource:
    def size: Long = data.length.toLong
    def read(offset: Long, length: Int): Data = data.slice(offset.toInt, offset.toInt + length)

  // Re-opens the file for each read, so entries stay detached and reusable with no held handle.
  private class FileSource(filename: Text) extends ByteSource:
    private def open(): jnc.FileChannel =
      jnc.FileChannel.open(jnf.Path.of(filename.s), jnf.StandardOpenOption.READ).nn

    def size: Long =
      val channel = open()
      try channel.size finally channel.close()

    def read(offset: Long, length: Int): Data =
      if length == 0 then IArray.empty[Byte] else
        val channel = open()
        try
          val buffer = jn.ByteBuffer.allocate(length).nn
          var position = offset
          var eof = false
          while buffer.hasRemaining && !eof do
            val count = channel.read(buffer, position)
            if count < 0 then eof = true else position += count
          buffer.array.nn.immutable(using Unsafe)
        finally channel.close()

  private def parse(source: ByteSource): Zipfile raises ZipError =
    val size = source.size
    if size < 22 then raise(ZipError(ZipError.Reason.MissingEocd))

    val windowSize = math.min(size, 22L + u16Max).toInt
    val windowStart = size - windowSize
    val window = source.read(windowStart, windowSize)

    var i = window.length - 22
    while i >= 0 && Zip.u32(window, i) != Zip.eocdSig.toLong do i -= 1
    if i < 0 then raise(ZipError(ZipError.Reason.MissingEocd))

    val eocdOffset = windowStart + i
    var entryCount = Zip.u16(window, i + 10).toLong
    var cdSize = Zip.u32(window, i + 12)
    var cdOffset = Zip.u32(window, i + 16)
    val commentLength = Zip.u16(window, i + 20)
    val comment: Optional[Text] =
      if commentLength == 0 then Unset
      else decodeText(window.slice(i + 22, i + 22 + commentLength))

    // Follow the ZIP64 locator if any EOCD field is saturated.
    if entryCount == u16Max.toLong || cdSize == u32Max || cdOffset == u32Max then
      if eocdOffset >= 20 then
        val locator = source.read(eocdOffset - 20, 20)
        if Zip.u32(locator, 0) == (Zip.zip64LocatorSig.toLong & 0xffffffffL) then
          val zip64Offset = Zip.u64(locator, 8)
          val record = source.read(zip64Offset, 56)
          if Zip.u32(record, 0) == (Zip.zip64EocdSig.toLong & 0xffffffffL) then
            entryCount = Zip.u64(record, 32)
            cdSize = Zip.u64(record, 40)
            cdOffset = Zip.u64(record, 48)
          else raise(ZipError(ZipError.Reason.Zip64Error))

    val central = source.read(cdOffset, cdSize.toInt)
    val builder = List.newBuilder[Zip.Entry]
    var p = 0
    var count = 0L

    while count < entryCount && p + 46 <= central.length do
      if Zip.u32(central, p) != (Zip.centralHeaderSig.toLong & 0xffffffffL)
      then raise(ZipError(ZipError.Reason.BadSignature(Zip.centralHeaderSig)))

      val methodId = Zip.u16(central, p + 10)
      val dosTime = Zip.u16(central, p + 12)
      val dosDate = Zip.u16(central, p + 14)
      val crc = Zip.u32(central, p + 16).toInt
      var compressedSize = Zip.u32(central, p + 20)
      var uncompressedSize = Zip.u32(central, p + 24)
      val nameLength = Zip.u16(central, p + 28)
      val extraLength = Zip.u16(central, p + 30)
      val entryCommentLength = Zip.u16(central, p + 32)
      var localOffset = Zip.u32(central, p + 42)

      val nameStart = p + 46
      val nameBytes = central.slice(nameStart, nameStart + nameLength)
      val extraStart = nameStart + nameLength
      val extra = central.slice(extraStart, extraStart + extraLength)
      val commentStart = extraStart + extraLength
      val entryComment: Optional[Text] =
        if entryCommentLength == 0 then Unset
        else decodeText(central.slice(commentStart, commentStart + entryCommentLength))

      // ZIP64 extended information overrides the saturated fixed fields, in a fixed order.
      var q = 0
      while q + 4 <= extra.length do
        val id = Zip.u16(extra, q)
        val dataSize = Zip.u16(extra, q + 2)
        if id == 1 then
          var r = q + 4
          if uncompressedSize == u32Max && r + 8 <= extra.length then
            uncompressedSize = Zip.u64(extra, r); r += 8
          if compressedSize == u32Max && r + 8 <= extra.length then
            compressedSize = Zip.u64(extra, r); r += 8
          if localOffset == u32Max && r + 8 <= extra.length then
            localOffset = Zip.u64(extra, r); r += 8
        q += 4 + dataSize

      val nameText = decodeText(nameBytes)
      val directory = nameText.ends(t"/")
      val cleanName = if directory then nameText.s.stripSuffix("/").nn.tt else nameText

      val method = methodId match
        case 0 => Zip.Method.Stored
        case 8 => Zip.Method.Deflate
        case other => abort(ZipError(ZipError.Reason.UnsupportedMethod(other)))

      val ref: Path on Zip =
        import errorDiagnostics.empty
        whereas:
          case PathError(_, _)    => ZipError(ZipError.Reason.InvalidName(cleanName))
          case NameError(_, _, _) => ZipError(ZipError.Reason.InvalidName(cleanName))
        . mitigate(cleanName.decode[Path on Zip])

      val payloadOffset = localOffset
      val payloadSize = compressedSize
      val storedBytes: () => Stream[Data] = () =>
        val header = source.read(payloadOffset, 30)
        val headerNameLength = Zip.u16(header, 26)
        val headerExtraLength = Zip.u16(header, 28)
        val start = payloadOffset + 30 + headerNameLength + headerExtraLength
        Stream(source.read(start, payloadSize.toInt))

      builder += Zip.Entry.precompressed(ref, method, crc, uncompressedSize, compressedSize,
          storedBytes, dosTime, dosDate, directory, entryComment)

      p = commentStart + entryCommentLength
      count += 1

    Zipfile(builder.result().to(LazyList), comment)

  private def decodeText(bytes: Data): Text =
    String(bytes.mutable(using Unsafe), jncs.StandardCharsets.UTF_8).nn.tt

  private def nameBytes(entry: Zip.Entry): Data =
    val encoded: Text = entry.ref.encode
    // ZIP entry names are relative; absolute `Path on Zip` values encode with a leading
    // slash, which `java -jar` and the spec reject, so strip it.
    val base: Text = if encoded.starts(t"/") then encoded.skip(1) else encoded
    (if entry.directory then t"$base/" else base).data

  private def textBytes(text: Text): Data = text.data

  private def utf8Flag(name: Data): Int = if name.exists(_ < 0) then 0x800 else 0

  private def localHeader(entry: Zip.Entry, name: Data): Data =
    val zip64 = entry.uncompressedSize > u32Max || entry.compressedSize > u32Max
    val extra: Data =
      if !zip64 then IArray.empty[Byte] else Data.build(20): array =>
        Zip.putU16(array, 0, 1)
        Zip.putU16(array, 2, 16)
        Zip.putU64(array, 4, entry.uncompressedSize)
        Zip.putU64(array, 12, entry.compressedSize)

    Data.build(30 + name.length + extra.length): array =>
      Zip.putU32(array, 0, Zip.localHeaderSig.toLong & 0xffffffffL)
      Zip.putU16(array, 4, if zip64 then 45 else 20)
      Zip.putU16(array, 6, utf8Flag(name))
      Zip.putU16(array, 8, entry.method.id)
      Zip.putU16(array, 10, entry.dosTime)
      Zip.putU16(array, 12, entry.dosDate)
      Zip.putU32(array, 14, entry.crc32 & 0xffffffffL)
      Zip.putU32(array, 18, if zip64 then u32Max else entry.compressedSize)
      Zip.putU32(array, 22, if zip64 then u32Max else entry.uncompressedSize)
      Zip.putU16(array, 26, name.length)
      Zip.putU16(array, 28, extra.length)
      array.place(name, 30.z)
      if extra.length > 0 then array.place(extra, (30 + name.length).z)

  private def centralHeader(entry: Zip.Entry, name: Data, localOffset: Long): Data =
    val needUncompressed = entry.uncompressedSize > u32Max
    val needCompressed = entry.compressedSize > u32Max
    val needOffset = localOffset > u32Max
    val zip64 = needUncompressed || needCompressed || needOffset

    val extra: Data =
      if !zip64 then IArray.empty[Byte] else
        val fields = List.newBuilder[Long]
        if needUncompressed then fields += entry.uncompressedSize
        if needCompressed then fields += entry.compressedSize
        if needOffset then fields += localOffset
        val values = fields.result()

        Data.build(4 + values.length*8): array =>
          Zip.putU16(array, 0, 1)
          Zip.putU16(array, 2, values.length*8)
          var offset = 4
          values.foreach: value =>
            Zip.putU64(array, offset, value)
            offset += 8

    val commentBytes: Data = entry.comment.lay(IArray.empty[Byte])(textBytes)
    val version = if zip64 then 45 else 20

    Data.build(46 + name.length + extra.length + commentBytes.length): array =>
      Zip.putU32(array, 0, Zip.centralHeaderSig.toLong & 0xffffffffL)
      Zip.putU16(array, 4, version)
      Zip.putU16(array, 6, version)
      Zip.putU16(array, 8, utf8Flag(name))
      Zip.putU16(array, 10, entry.method.id)
      Zip.putU16(array, 12, entry.dosTime)
      Zip.putU16(array, 14, entry.dosDate)
      Zip.putU32(array, 16, entry.crc32 & 0xffffffffL)
      Zip.putU32(array, 20, if needCompressed then u32Max else entry.compressedSize)
      Zip.putU32(array, 24, if needUncompressed then u32Max else entry.uncompressedSize)
      Zip.putU16(array, 28, name.length)
      Zip.putU16(array, 30, extra.length)
      Zip.putU16(array, 32, commentBytes.length)
      Zip.putU16(array, 34, 0)
      Zip.putU16(array, 36, 0)
      Zip.putU32(array, 38, if entry.directory then 0x10L else 0L)
      Zip.putU32(array, 42, if needOffset then u32Max else localOffset)
      array.place(name, 46.z)
      var pos = 46 + name.length
      if extra.length > 0 then
        array.place(extra, pos.z)
        pos += extra.length
      if commentBytes.length > 0 then array.place(commentBytes, pos.z)

  private def endRecords
     (entryCount: Long, cdStart: Long, cdSize: Long, comment: Optional[Text])
  :   List[Data] =
    val zip64 = entryCount > u16Max.toLong || cdSize > u32Max || cdStart > u32Max
    val commentBytes: Data = comment.lay(IArray.empty[Byte])(textBytes)

    val eocd: Data = Data.build(22 + commentBytes.length): array =>
      Zip.putU32(array, 0, Zip.eocdSig.toLong & 0xffffffffL)
      Zip.putU16(array, 4, 0)
      Zip.putU16(array, 6, 0)
      Zip.putU16(array, 8, math.min(entryCount, u16Max.toLong).toInt)
      Zip.putU16(array, 10, math.min(entryCount, u16Max.toLong).toInt)
      Zip.putU32(array, 12, math.min(cdSize, u32Max))
      Zip.putU32(array, 16, math.min(cdStart, u32Max))
      Zip.putU16(array, 20, commentBytes.length)
      if commentBytes.length > 0 then array.place(commentBytes, 22.z)

    if !zip64 then List(eocd) else
      val zip64Offset = cdStart + cdSize

      val record: Data = Data.build(56): array =>
        Zip.putU32(array, 0, Zip.zip64EocdSig.toLong & 0xffffffffL)
        Zip.putU64(array, 4, 44L)
        Zip.putU16(array, 12, 45)
        Zip.putU16(array, 14, 45)
        Zip.putU32(array, 16, 0)
        Zip.putU32(array, 20, 0)
        Zip.putU64(array, 24, entryCount)
        Zip.putU64(array, 32, entryCount)
        Zip.putU64(array, 40, cdSize)
        Zip.putU64(array, 48, cdStart)

      val locator: Data = Data.build(20): array =>
        Zip.putU32(array, 0, Zip.zip64LocatorSig.toLong & 0xffffffffL)
        Zip.putU32(array, 4, 0)
        Zip.putU64(array, 8, zip64Offset)
        Zip.putU32(array, 16, 1)

      List(record, locator, eocd)

case class Zipfile(entries: LazyList[Zip.Entry], comment: Optional[Text] = Unset):
  def entry(ref: Path on Zip): Zip.Entry raises ZipError =
    entries.find(_.ref == ref).getOrElse(abort(ZipError(ZipError.Reason.NotFound(ref))))

  def serialize: Stream[Data] =
    val entryList = entries.to(List)
    var offset = 0L
    val builder = List.newBuilder[(Zip.Entry, Data, Data, Long)]

    entryList.foreach: entry =>
      val name = Zipfile.nameBytes(entry)
      val header = Zipfile.localHeader(entry, name)
      builder += ((entry, name, header, offset))
      offset += header.length + entry.compressedSize

    val records = builder.result()
    val cdStart = offset
    val central = records.map((entry, name, _, off) => Zipfile.centralHeader(entry, name, off))
    val cdSize = central.foldLeft(0L)(_ + _.length)
    val tail = Zipfile.endRecords(records.length.toLong, cdStart, cdSize, comment)

    val local: Stream[Data] =
      records.to(LazyList).flatMap((entry, _, header, _) => header #:: entry.storedBytes())

    local #::: central.to(LazyList) #::: tail.to(LazyList)
