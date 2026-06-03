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

import soundness.*

import charDecoders.utf8
import charEncoders.utf8
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import logging.silent
import strategies.throwUnsafely
import systems.java
import temporaryDirectories.system
import textSanitizers.skip

import _root_.java.io as ji
import _root_.java.util.zip as juz

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

    def streamEntries(path: Path on Linux): List[Zip.Entry] =
      ZipStream(bytesOf(path)).map(identity).to(List)

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

    def names(entries: List[Zip.Entry]): List[Text] = entries.map(_.ref.encode)

    // An independent oracle: read entry names straight from the JDK's own ZIP reader,
    // so round-trip tests are not merely checking Zeppelin against itself.
    def jdkNames(path: Path on Linux): List[Text] =
      val zip = juz.ZipFile(ji.File(path.encode.s))

      try
        val builder = List.newBuilder[Text]
        val iterator = zip.entries().nn
        while iterator.hasMoreElements do builder += iterator.nextElement().nn.getName.nn.tt
        builder.result()
      finally zip.close()

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
        val lazyEntry: Zip.Entry = Zip.Entry(zipRef(t"a.txt"), () => Stream(t"lazy".data))
        lazyEntry.read[Text]
      . assert(_ == t"lazy")

    suite(m"Writing ZIP archives"):
      test(m"single-entry archive begins with the ZIP local-header magic"):
        bytesOf(writeZip(t"one.zip", entry(t"hello.txt", t"Hello world")))
          .slice(0, 4).to[List].map(_.toInt & 0xff)
      . assert(_ == List(0x50, 0x4b, 0x03, 0x04))

      test(m"single entry is visible to the JDK ZIP reader"):
        jdkNames(writeZip(t"one.zip", entry(t"hello.txt", t"Hello world")))
      . assert(_ == List(t"hello.txt"))

      test(m"multiple entries preserve insertion order"):
        jdkNames(writeZip(t"many.zip", entry(t"a.txt", t"A"), entry(t"b.txt", t"B"),
          entry(t"c.txt", t"C")))
      . assert(_ == List(t"a.txt", t"b.txt", t"c.txt"))

      test(m"nested entry paths are preserved"):
        jdkNames(writeZip(t"nested.zip", entry(t"dir/sub/file.txt", t"x")))
      . assert(_ == List(t"dir/sub/file.txt"))

      test(m"an empty archive contains no entries"):
        jdkNames(writeZip(t"empty.zip"))
      . assert(_ == Nil)

      test(m"writing two entries with the same path raises DuplicateEntry"):
        import errorDiagnostics.empty
        capture[ZipError]:
          Zipfile.write(workDir/t"dup.zip")(List(entry(t"x.txt", t"1"), entry(t"x.txt", t"2")))
        . reason
      . assert(_.isInstanceOf[ZipError.Reason.DuplicateEntry])

    suite(m"Reading ZIP archives with ZipStream"):
      val archive = writeZip(t"rt.zip", entry(t"a.txt", t"alpha"), entry(t"b/c.txt", t"gamma"))

      test(m"round-trips all entry names"):
        names(streamEntries(archive))
      . assert(_ == List(t"a.txt", t"b/c.txt"))

      test(m"round-trips entry contents"):
        streamEntries(archive).map(_.read[Text])
      . assert(_ == List(t"alpha", t"gamma"))

      test(m"an empty archive yields no entries"):
        streamEntries(writeZip(t"empty2.zip")).length
      . assert(_ == 0)

      test(m"reads back binary (non-text) content unchanged"):
        val payload: Data = IArray.tabulate(512)(i => (i%256).toByte)
        val path = workDir/t"bin.zip"
        Zipfile.write(path)(List(Zip.Entry(zipRef(t"blob"), payload)))
        ZipStream(bytesOf(path)).map(_.read[Data]).head.to[List]
      . assert(_ == IArray.tabulate(512)(i => (i%256).toByte).to[List])

      test(m"reads back an entry with empty content"):
        streamEntries(writeZip(t"emptyfile.zip", entry(t"empty", t""))).head.read[Text]
      . assert(_ == t"")

    suite(m"ZipStream filtering and extraction"):
      val archive =
        writeZip(t"ops.zip", entry(t"keep.txt", t"K"), entry(t"drop.txt", t"D"),
          entry(t"keep2.txt", t"K2"))

      test(m"keep retains only entries matching the predicate"):
        ZipStream(bytesOf(archive)).keep(_.encode.starts(t"keep")).map(_.ref.encode).to(List)
      . assert(_ == List(t"keep.txt", t"keep2.txt"))

      test(m"extract returns the requested entry's content"):
        ZipStream(bytesOf(archive)).extract(zipRef(t"drop.txt")).read[Text]
      . assert(_ == t"D")

      test(m"extracting a missing entry raises NotFound"):
        import errorDiagnostics.empty
        capture[ZipError](ZipStream(bytesOf(archive)).extract(zipRef(t"absent.txt"))).reason
      . assert(_.isInstanceOf[ZipError.Reason.NotFound])

      test(m"each visits every entry"):
        var count = 0
        ZipStream(bytesOf(archive)).each(_ => count += 1)
        count
      . assert(_ == 3)

      test(m"map transforms each entry"):
        ZipStream(bytesOf(archive)).map(_.ref.encode).to(List)
      . assert(_ == List(t"keep.txt", t"drop.txt", t"keep2.txt"))

      test(m"reads entry names from an externally (JDK) written archive"):
        names(streamEntries(writeRawZip(t"foreign.zip", t"one.txt", t"two.txt")))
      . assert(_ == List(t"one.txt", t"two.txt"))

      test(m"reads content from an externally (JDK) written archive"):
        streamEntries(writeRawZip(t"foreign2.zip", t"solo.txt")).head.read[Text]
      . assert(_ == t"data")

    suite(m"Path on Zip"):
      test(m"decode then encode round-trips a nested path"):
        t"a/b/c.txt".decode[Path on Zip].encode
      . assert(_ == t"a/b/c.txt")

      test(m"a written entry's path is preserved through a full round-trip"):
        streamEntries(writeZip(t"path.zip", entry(t"deep/nested/leaf.dat", t"."))).head.ref.encode
      . assert(_ == t"deep/nested/leaf.dat")
