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

import soundness.*

import charDecoders.utf8Decoder
import charEncoders.utf8Encoder
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import logging.silentLogging
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import textSanitizers.skipSanitizer

import _root_.java.io as ji
import _root_.java.util.zip as juz

import filesystemBackends.virtualMachine

object Tests extends Suite(m"Zeppelin tests"):
  def run(): Unit =
    val workDir: Path on Linux = temporaryDirectory[Path on Linux]/Uuid().show
    workDir.create[Directory]()

    def zipRef(text: Text): Path on Zip = text.decode[Path on Zip]

    def entry(path: Text, content: Text): Zip.Entry =
      Zip.Entry(zipRef(path), content.data)

    def writeZip(name: Text, entries: Zip.Entry*): Path on Linux =
      val path = workDir/name
      Zipfile.write(path)(entries.to(List))
      path

    def bytesOf(path: Path on Linux): Data = path.open(_.read[Data])

    def readEntries(path: Path on Linux): List[Zip.Entry] = Zipfile.read(path).entries.to(List)

    def names(entries: List[Zip.Entry]): List[Text] = entries.map(_.ref.encode)

    // Build a ZIP with the raw JDK writer so entry names can bypass the type-safe
    // `Path on Zip` validation that Zeppelin's own writer would enforce.
    def writeRawZip(name: Text, entryNames: Text*): Path on Linux =
      val path = workDir/name
      val out = juz.ZipOutputStream(ji.FileOutputStream(ji.File(path.encode.s)))

      entryNames.foreach: entryName =>
        out.putNextEntry(juz.ZipEntry(entryName.s))
        out.write(t"data".data.mutable(using Unsafe))
        out.closeEntry()

      out.close()
      path

    // An independent oracle: the JDK's own ZIP reader, so round-trip tests are not merely
    // checking Zeppelin against itself.
    def jdkNames(path: Path on Linux): List[Text] =
      val zip = juz.ZipFile(ji.File(path.encode.s))

      try
        val builder = List.newBuilder[Text]
        val iterator = zip.entries().nn
        while iterator.hasMoreElements do builder += iterator.nextElement().nn.getName.nn.tt
        builder.result()
      finally zip.close()

    def jdkContent(path: Path on Linux, name: Text): Data =
      val zip = juz.ZipFile(ji.File(path.encode.s))
      try
        val entry = zip.getEntry(name.s).nn
        zip.getInputStream(entry).nn.readAllBytes().nn.immutable(using Unsafe)
      finally zip.close()

    def jdkComment(path: Path on Linux): Optional[Text] =
      val zip = juz.ZipFile(ji.File(path.encode.s))
      try zip.getComment() match
        case null            => Unset
        case text: String    => text.tt
      finally zip.close()

    def writeBytes(name: Text, stream: LazyList[Data]): Path on Linux =
      val path = workDir/name
      val out = ji.FileOutputStream(ji.File(path.encode.s))
      try stream.each { chunk => out.write(chunk.mutable(using Unsafe)) } finally out.close()
      path

    // The general-purpose bit flag of the first local file header.
    def firstFlag(path: Path on Linux): Int =
      val data = bytesOf(path)
      (data(6) & 0xff) | ((data(7) & 0xff) << 8)

    def contains(data: Data, signature: List[Int]): Boolean =
      val window = signature.length
      (0 to data.length - window).exists: i =>
        (0 until window).forall(j => (data(i + j) & 0xff) == signature(j))

    suite(m"Zip.Entry construction and content"):
      test(m"entry built from a path and Text exposes its path"):
        Zip.Entry(zipRef(t"hello.txt"), t"Hello world".data).ref.encode
      . assert(_ == t"hello.txt")

      test(m"entry content reads back as Text"):
        Zip.Entry(zipRef(t"hello.txt"), t"Hello world".data).read[Text]
      . assert(_ == t"Hello world")

      test(m"entry is Streamable by Data"):
        Zip.Entry(zipRef(t"a"), t"xyz".data).stream[Data].read[Text]
      . assert(_ == t"xyz")

      test(m"direct constructor accepts lazily-computed content"):
        val lazyEntry: Zip.Entry = Zip.Entry(zipRef(t"a.txt"), () => LazyList(t"lazy".data))
        lazyEntry.read[Text]
      . assert(_ == t"lazy")

      test(m"a compressible entry is stored with the Deflate method"):
        Zip.Entry(zipRef(t"a.txt"), (t"abcd"*64).data).method
      . assert(_ == Zip.Method.Deflate)

      test(m"the Stored compression policy disables deflation"):
        given Zip.Compression = Zip.Compression.Stored
        Zip.Entry(zipRef(t"a.txt"), (t"abcd"*64).data).method
      . assert(_ == Zip.Method.Stored)

    suite(m"Writing ZIP archives"):
      test(m"single-entry archive begins with the ZIP local-header magic"):
        bytesOf(writeZip(t"one.zip", entry(t"hello.txt", t"Hello world")))
          .slice(0, 4).to(List).map(_.toInt & 0xff)
      . assert(_ == List(0x50, 0x4b, 0x03, 0x04))

      test(m"single entry is visible to the JDK ZIP reader"):
        jdkNames(writeZip(t"one.zip", entry(t"hello.txt", t"Hello world")))
      . assert(_ == List(t"hello.txt"))

      test(m"the JDK reader sees the original content"):
        val path = writeZip(t"content.zip", entry(t"hello.txt", t"Hello world"))
        jdkContent(path, t"hello.txt").to(List)
      . assert(_ == t"Hello world".data.to(List))

      test(m"multiple entries preserve insertion order"):
        jdkNames(writeZip(t"many.zip", entry(t"a.txt", t"A"), entry(t"b.txt", t"B"),
          entry(t"c.txt", t"C")))
      . assert(_ == List(t"a.txt", t"b.txt", t"c.txt"))

      test(m"nested entry paths are preserved"):
        jdkNames(writeZip(t"nested.zip", entry(t"dir/sub/file.txt", t"x")))
      . assert(_ == List(t"dir/sub/file.txt"))

      test(m"an absolute Path on Zip is written without a leading slash"):
        val entry = Zip.Entry(%.on[Zip]/"META-INF"/"MANIFEST.MF", t"Manifest-Version: 1.0".data)
        jdkNames(writeZip(t"abs.zip", entry))
      . assert(_ == List(t"META-INF/MANIFEST.MF"))

      test(m"an empty archive contains no entries"):
        jdkNames(writeZip(t"empty.zip"))
      . assert(_ == Nil)

      test(m"writing two entries with the same path raises DuplicateEntry"):
        import errorDiagnostics.emptyDiagnostics
        capture[ZipError]:
          Zipfile.write(workDir/t"dup.zip")(List(entry(t"x.txt", t"1"), entry(t"x.txt", t"2")))
        . reason
      . assert(_.isInstanceOf[ZipError.Reason.DuplicateEntry])

      test(m"a non-ASCII entry name sets the UTF-8 general-purpose flag"):
        (firstFlag(writeZip(t"utf8.zip", entry(t"café.txt", t"x"))) & 0x800) != 0
      . assert(_ == true)

      test(m"an ASCII entry name leaves the UTF-8 flag clear"):
        (firstFlag(writeZip(t"ascii.zip", entry(t"plain.txt", t"x"))) & 0x800) != 0
      . assert(_ == false)

      test(m"the JDK reader decodes a non-ASCII entry name"):
        jdkNames(writeZip(t"utf8b.zip", entry(t"café.txt", t"x")))
      . assert(_ == List(t"café.txt"))

      test(m"an archive comment round-trips through the JDK reader"):
        val zipfile = Zipfile(LazyList(entry(t"a.txt", t"a")), t"hello comment")
        jdkComment(writeBytes(t"comment.zip", zipfile.serialize))
      . assert(_ == t"hello comment")

    suite(m"Reading ZIP archives"):
      val archive = writeZip(t"rt.zip", entry(t"a.txt", t"alpha"), entry(t"b/c.txt", t"gamma"))

      test(m"round-trips all entry names"):
        names(readEntries(archive))
      . assert(_ == List(t"a.txt", t"b/c.txt"))

      test(m"round-trips entry contents"):
        readEntries(archive).map(_.read[Text])
      . assert(_ == List(t"alpha", t"gamma"))

      test(m"an empty archive yields no entries"):
        readEntries(writeZip(t"empty2.zip")).length
      . assert(_ == 0)

      test(m"reads back binary (non-text) content unchanged"):
        val payload: Data = IArray.tabulate(512)(i => (i%256).toByte)
        val path = workDir/t"bin.zip"
        Zipfile.write(path)(List(Zip.Entry(zipRef(t"blob"), payload)))
        readEntries(path).head.read[Data].to(List)
      . assert(_ == IArray.tabulate(512)(i => (i%256).toByte).to(List))

      test(m"reads back an entry with empty content"):
        readEntries(writeZip(t"emptyfile.zip", entry(t"empty", t""))).head.read[Text]
      . assert(_ == t"")

      test(m"reads a large, highly-compressible payload"):
        val path = workDir/t"big.zip"
        Zipfile.write(path)(List(Zip.Entry(zipRef(t"big.txt"), (t"soundness "*4096).data)))
        readEntries(path).head.read[Text]
      . assert(_ == t"soundness "*4096)

      test(m"looking up an absent entry raises NotFound"):
        import errorDiagnostics.emptyDiagnostics
        capture[ZipError](Zipfile.read(archive).entry(zipRef(t"absent.txt"))).reason
      . assert(_.isInstanceOf[ZipError.Reason.NotFound])

      test(m"reading data that is not a ZIP archive raises MissingEocd"):
        import errorDiagnostics.emptyDiagnostics
        capture[ZipError](Zipfile.read(t"this is not a zip file".data)).reason
      . assert(_ == ZipError.Reason.MissingEocd)

    suite(m"Interoperability with the JDK writer"):
      test(m"reads entry names from an externally (JDK) written archive"):
        names(readEntries(writeRawZip(t"foreign.zip", t"one.txt", t"two.txt")))
      . assert(_ == List(t"one.txt", t"two.txt"))

      test(m"reads content from an externally (JDK) written archive"):
        readEntries(writeRawZip(t"foreign2.zip", t"solo.txt")).head.read[Text]
      . assert(_ == t"data")

      test(m"an entry name with a forbidden character raises InvalidName"):
        import errorDiagnostics.emptyDiagnostics
        capture[ZipError](readEntries(writeRawZip(t"badchar.zip", t"bad:name.txt"))).reason
      . assert(_ == ZipError.Reason.InvalidName(t"bad:name.txt"))

      test(m"a path-traversing entry name raises InvalidName"):
        import errorDiagnostics.emptyDiagnostics
        capture[ZipError](readEntries(writeRawZip(t"slip.zip", t"../escape.txt"))).reason
      . assert(_ == ZipError.Reason.InvalidName(t"../escape.txt"))

    suite(m"Entry reuse between archives"):
      val source = writeZip(t"src.zip", entry(t"x.txt", (t"reuse me "*32)))
      val reused: Zip.Entry = Zipfile.read(source).entries.head
      val target = workDir/t"dst.zip"
      Zipfile.write(target)(List(reused))

      test(m"a reused entry preserves its content"):
        Zipfile.read(target).entries.head.read[Text]
      . assert(_ == t"reuse me "*32)

      test(m"a reused entry is not recompressed (identical compressed size)"):
        Zipfile.read(target).entries.head.compressedSize
      . assert(_ == reused.compressedSize)

    suite(m"ZIP64"):
      // More than 0xFFFF entries forces ZIP64 end-of-central-directory records.
      val many = (0 until 66000).map { i => entry(t"e$i", t"") }
      val path = workDir/t"zip64.zip"
      Zipfile.write(path)(many.to(List))

      test(m"a ZIP64 end-of-central-directory record is emitted"):
        contains(bytesOf(path), List(0x50, 0x4b, 0x06, 0x06))
      . assert(_ == true)

      test(m"a ZIP64 locator is emitted"):
        contains(bytesOf(path), List(0x50, 0x4b, 0x06, 0x07))
      . assert(_ == true)

      test(m"the JDK reader counts all entries in a ZIP64 archive"):
        jdkNames(path).length
      . assert(_ == 66000)

      test(m"the native reader counts all entries in a ZIP64 archive"):
        Zipfile.read(path).entries.length
      . assert(_ == 66000)

    suite(m"Binary prefix"):
      val prefix: Data = IArray.tabulate(64)(i => (i*7).toByte)

      test(m"a binary prefix round-trips"):
        val path = workDir/t"prefixed.zip"
        Zipfile.write(path, prefix)(List(entry(t"a.txt", t"alpha"), entry(t"b.txt", t"beta")))
        Zipfile.read(path).prefix.lay(Nil)(_.to(List))
      . assert(_ == prefix.to(List))

      test(m"entries in a prefixed archive remain readable"):
        val path = workDir/t"prefixed2.zip"
        Zipfile.write(path, prefix)(List(entry(t"a.txt", t"alpha"), entry(t"b.txt", t"beta")))
        Zipfile.read(path).entries.map(_.read[Text]).to(List)
      . assert(_ == List(t"alpha", t"beta"))

      test(m"the prefix precedes the first local header"):
        val path = workDir/t"prefixed3.zip"
        Zipfile.write(path, prefix)(List(entry(t"a.txt", t"alpha")))
        bytesOf(path).slice(0, 64).to(List)
      . assert(_ == prefix.to(List))

      test(m"the JDK reader reads a prefixed archive"):
        val path = workDir/t"prefixed4.zip"
        Zipfile.write(path, prefix)(List(entry(t"a.txt", t"alpha")))
        jdkContent(path, t"a.txt").to(List)
      . assert(_ == t"alpha".data.to(List))

      test(m"an archive with no prefix reports no prefix"):
        Zipfile.read(writeZip(t"noprefix.zip", entry(t"a.txt", t"x"))).prefix
      . assert(_ == Unset)

      test(m"detects and reads a prefix prepended to a JDK-written archive"):
        // The JDK writes offsets relative to the archive start; prepending data makes a
        // self-extracting archive whose offsets must be shifted by the prefix length.
        val inner = bytesOf(writeRawZip(t"inner.zip", t"one.txt", t"two.txt"))
        val stub: Data = t"STUB-PREFIX-DATA".data
        val sfx = workDir/t"sfx.zip"
        val out = ji.FileOutputStream(ji.File(sfx.encode.s))
        out.write(stub.mutable(using Unsafe))
        out.write(inner.mutable(using Unsafe))
        out.close()
        val zip = Zipfile.read(sfx)
        (zip.prefix.lay(Nil)(_.to(List)), zip.entries.map(_.read[Text]).to(List))
      . assert(_ == (t"STUB-PREFIX-DATA".data.to(List), List(t"data", t"data")))
