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


import soundness.*
import filesystemBackends.javaBaseFilesystem

import java.nio.file as jnf

import galilei.javaPath

import charEncoders.asciiEncoder
import strategies.throwUnsafely
import denominative.dysasymptotics.linearSize

object Tests extends Suite(m"Bitumen Tests"):
  def run(): Unit =
    val helloFile = Tar.Entry.File
                     ( path  = t"hello.txt".as[Relative on Tar],
                       mode  = UnixMode(),
                       user  = UnixUser(0),
                       group = UnixGroup(0),
                       mtime = 0.bits.u32,
                       data  = Tar.Body(t"hello".in[Data]) )

    val emptyDir = Tar.Entry.Directory
                    ( path  = t"data".as[Relative on Tar],
                      mode  = UnixMode(),
                      user  = UnixUser(0),
                      group = UnixGroup(0),
                      mtime = 0.bits.u32 )

    suite(m"USTAR header writing"):
      val blocks = Tarfile(List(helloFile)).source[Data].chain.stdlib.toList
      val header = blocks.head

      test(m"USTAR magic at offset 257"):
        header.readable.slice(257, 263).toList.map(_.toChar)
      . assert(_ == List('u', 's', 't', 'a', 'r', '\u0000'))

      test(m"version 00 at offset 263"):
        header.readable.slice(263, 265).toList.map(_.toChar)
      . assert(_ == List('0', '0'))

      test(m"file type flag at offset 156"):
        header.readable(156).toChar
      . assert(_ == '0')

      test(m"name field starts with hello.txt"):
        header.readable.slice(0, 9).toList.map(_.toChar)
      . assert(_ == List('h', 'e', 'l', 'l', 'o', '.', 't', 'x', 't'))

      test(m"single-file archive emits 4 blocks"):
        blocks.size
      . assert(_ == 4)

      test(m"single-file archive is 2048 bytes"):
        blocks.map(_.readable.length).sum
      . assert(_ == 2048)

    suite(m"Multi-entry archive"):
      val blocks = Tarfile(List(helloFile, emptyDir)).source[Data].chain.stdlib.toList

      test(m"file + directory + two trailing zero blocks = 5 blocks"):
        blocks.size
      . assert(_ == 5)

      test(m"directory entry has type flag 5"):
        blocks(2).readable(156).toChar
      . assert(_ == '5')

      test(m"directory entryName ends with /"):
        blocks(2).readable.slice(0, 5).toList.map(_.toChar)
      . assert(_ == List('d', 'a', 't', 'a', '/'))

    suite(m"PAX extended headers for long names"):
      val longPath: Text = "a".repeat(150).nn.tt
      val longFile = Tar.Entry.File
                      ( path  = longPath.as[Relative on Tar],
                        mode  = UnixMode(),
                        user  = UnixUser(0),
                        group = UnixGroup(0),
                        mtime = 0.bits.u32,
                        data  = Tar.Body() )
      val blocks = Tarfile(List(longFile)).source[Data].chain.stdlib.toList

      test(m"long name: 5 blocks (PAX header/data, regular, 2 zero)"):
        blocks.size
      . assert(_ == 5)

      test(m"PAX header has type flag 'x' at offset 156"):
        blocks(0).readable(156).toChar
      . assert(_ == 'x')

      test(m"PAX header name is PaxHeaders/0"):
        blocks(0).readable.slice(0, 13).toList.map(_.toChar)
      . assert(_ == List('P', 'a', 'x', 'H', 'e', 'a', 'd', 'e', 'r', 's', '/', '0', '\u0000'))

      test(m"PAX record begins with decimal length prefix '160 path='"):
        blocks(1).readable.slice(0, 9).toList.map(_.toChar)
      . assert(_ == List('1', '6', '0', ' ', 'p', 'a', 't', 'h', '='))

      test(m"PAX record ends with newline at offset 159"):
        blocks(1).readable(159).toChar
      . assert(_ == '\n')

      test(m"regular header (block 2) has type flag '0' for the file"):
        blocks(2).readable(156).toChar
      . assert(_ == '0')

      test(m"regular header name bytes 0, 99 both 'a' (truncated to 100)"):
        (blocks(2).readable(0).toChar, blocks(2).readable(99).toChar)
      . assert(_ == ('a', 'a'))

    suite(m"PAX extended headers for long link targets"):
      val longTarget: Text = "b".repeat(150).nn.tt
      val longSymlink = Tar.Entry.Symlink
                         ( path   = t"link".as[Relative on Tar],
                           mode   = UnixMode(),
                           user   = UnixUser(0),
                           group  = UnixGroup(0),
                           mtime  = 0.bits.u32,
                           target = longTarget )
      val blocks = Tarfile(List(longSymlink)).source[Data].chain.stdlib.toList

      test(m"long linkpath: 5 blocks (PAX header/data, symlink, 2 zero)"):
        blocks.size
      . assert(_ == 5)

      test(m"PAX header type flag is 'x'"):
        blocks(0).readable(156).toChar
      . assert(_ == 'x')

      test(m"PAX record uses the linkpath keyword"):
        blocks(1).readable.slice(0, 13).toList.map(_.toChar)
      . assert(_ == List('1', '6', '4', ' ', 'l', 'i', 'n', 'k', 'p', 'a', 't', 'h', '='))

      test(m"regular header (block 2) has type flag '2' for the symlink"):
        blocks(2).readable(156).toChar
      . assert(_ == '2')

      test(m"regular header link target bytes 157, 256 both 'b' (trunc 100)"):
        (blocks(2).readable(157).toChar, blocks(2).readable(256).toChar)
      . assert(_ == ('b', 'b'))

    suite(m"Reader: round-trip a single file through a pull endpoint"):
      val bytes: Data = Tarfile(List(helloFile)).source[Data].memoize
      val entries = Tarfile.read(bytes.readable.grouped(7).map(Array.frozen(_)).iterator.stream).toList

      test(m"streamed reader produces one entry"):
        entries.length
      . assert(_ == 1)

      test(m"streamed file data round-trips across odd chunk boundaries"):
        entries.head match
          case f: Tar.Entry.File => f.data.memoize.readable.to(List).map(_.toChar).mkString
          case _                => ""
      . assert(_ == "hello")

    suite(m"Reader: large payloads stream in bounded chunks"):
      val big: Data = Array.fill[Byte](200000)(42)
      val bigFile = Tar.Entry.File
                     ( path  = t"big.bin".as[Relative on Tar],
                       mode  = UnixMode(),
                       user  = UnixUser(0),
                       group = UnixGroup(0),
                       mtime = 0.bits.u32,
                       data  = Tar.Body(big) )
      val bytes: Data = Tarfile(List(bigFile)).source[Data].memoize

      test(m"a 200kB body arrives as multiple chunks"):
        Tarfile.read(bytes.readable.grouped(8192).map(Array.frozen(_)).iterator.stream).toList.head match
          case f: Tar.Entry.File => f.data.chunks.length
          case _                 => 0
      . assert(_ > 1)

      test(m"the chunks reassemble the payload exactly"):
        Tarfile.read(bytes.readable.grouped(8192).map(Array.frozen(_)).iterator.stream).toList.head match
          case f: Tar.Entry.File => f.data.size
          case _                 => 0
      . assert(_ == 200000)

    suite(m"Reader: round-trip a single file"):
      val bytes = Tarfile(List(helloFile)).source[Data].chain
      val entries = Tarfile.read(bytes.stdlib.iterator.stream).toList

      test(m"reader produces one entry"):
        entries.length
      . assert(_ == 1)

      test(m"entry is a File"):
        entries.head.isInstanceOf[Tar.Entry.File]
      . assert(_ == true)

      test(m"file name round-trips"):
        entries.head.entryName
      . assert(_ == t"hello.txt")

      test(m"file data round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.data.memoize.readable.to(List).map(_.toChar).mkString
          case _                => ""
      . assert(_ == "hello")

    suite(m"Reader: round-trip multiple entries"):
      val bytes = Tarfile(List(helloFile, emptyDir)).source[Data].chain
      val entries = Tarfile.read(bytes.stdlib.iterator.stream).toList

      test(m"two entries"):
        entries.length
      . assert(_ == 2)

      test(m"first is File"):
        entries(0).isInstanceOf[Tar.Entry.File]
      . assert(_ == true)

      test(m"second is Directory"):
        entries(1).isInstanceOf[Tar.Entry.Directory]
      . assert(_ == true)

      test(m"directory name has no trailing slash after read"):
        entries(1) match
          case d: Tar.Entry.Directory => d.path.show
          case _                     => t""
      . assert(_ == t"data")

    suite(m"Reader: PAX long name round-trip"):
      val longPath: Text = "a".repeat(150).nn.tt
      val longFile = Tar.Entry.File
                      ( path  = longPath.as[Relative on Tar],
                        mode  = UnixMode(),
                        user  = UnixUser(0),
                        group = UnixGroup(0),
                        mtime = 0.bits.u32,
                        data  = Tar.Body() )

      val bytes = Tarfile(List(longFile)).source[Data].chain
      val entries = Tarfile.read(bytes.stdlib.iterator.stream).toList

      test(m"reader produces one entry (PAX consumed silently)"):
        entries.length
      . assert(_ == 1)

      test(m"long path is preserved via PAX override"):
        entries.head.entryName
      . assert(_ == longPath)

    suite(m"Reader: PAX long linkpath round-trip"):
      val longTarget: Text = "b".repeat(150).nn.tt
      val longSymlink = Tar.Entry.Symlink
                         ( path   = t"link".as[Relative on Tar],
                           mode   = UnixMode(),
                           user   = UnixUser(0),
                           group  = UnixGroup(0),
                           mtime  = 0.bits.u32,
                           target = longTarget )

      val bytes = Tarfile(List(longSymlink)).source[Data].chain
      val entries = Tarfile.read(bytes.stdlib.iterator.stream).toList

      test(m"single Symlink entry emitted"):
        entries.head.isInstanceOf[Tar.Entry.Symlink]
      . assert(_ == true)

      test(m"linkpath is preserved via PAX override"):
        entries.head match
          case s: Tar.Entry.Symlink => s.target
          case _                   => t""
      . assert(_ == longTarget)

    suite(m"Reader: mode, user, group round-trip"):
      val executable = Tar.Entry.File
                        ( path  = t"bin".as[Relative on Tar],
                          mode  = UnixMode(ownerExec = true, groupExec = true, otherExec = true),
                          user  = UnixUser(1000, t"alice"),
                          group = UnixGroup(1000, t"alice"),
                          mtime = 12345.bits.u32,
                          data  = Tar.Body(t"#!/bin/sh\n".in[Data]) )

      val bytes = Tarfile(List(executable)).source[Data].chain
      val entries = Tarfile.read(bytes.stdlib.iterator.stream).toList

      test(m"mode bits round-trip"):
        entries.head match
          case f: Tar.Entry.File => f.mode.ownerExec && f.mode.groupExec && f.mode.otherExec
          case _                => false
      . assert(_ == true)

      test(m"uid round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.user.value
          case _                => 0
      . assert(_ == 1000)

      test(m"uname round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.user.name.or(t"")
          case _                => t""
      . assert(_ == t"alice")

      test(m"mtime round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.mtime
          case _                => 0.bits.u32
      . assert(_ == 12345.bits.u32)

    suite(m"Reader: error cases"):
      import errorDiagnostics.emptyDiagnostics

      test(m"bad checksum is detected"):
        val good: List[Data] = Tarfile(List(helloFile)).source[Data].chain.stdlib.toList.asInstanceOf[List[Data]]
        val corrupted: List[Data] = Array.frozen(good.stdlib.head.readable.updated(0, ('Z'.toByte: Byte))) :: good.stdlib.tail.to(proscenium.List)
        capture[Tar.Error](Tarfile.read(corrupted.stdlib.iterator.stream).toList).reason
      . assert: r =>
          r.isInstanceOf[Tar.Error.Reason.BadChecksum]
            || r.isInstanceOf[Tar.Error.Reason.BadName]

      case class TarIssues(reasons: List[Tar.Error.Reason] = Nil)(using Diagnostics)
      extends Error(m"${reasons.size} tar issues"):
        def +(reason: Tar.Error.Reason): TarIssues = TarIssues(reasons :+ reason)

      // Inline, with a directly-constructed `Validate`; see rep/DECISIONS.md.
      inline def collectTar(inline block: Unit raises Tar.Error tracks Text): TarIssues =
        Validate[TarIssues, [r] =>> r raises Tar.Error, Text]
          (TarIssues(), { case error: Tar.Error => accrual + error.reason })
        . protect(block)

      test(m"a bad checksum accrues exactly one error, with no parse cascade"):
        // A corrupt block cannot be trusted for anything, so nothing in it is parsed: no
        // BadName/BadOctal cascade follows the checksum failure.
        val good: List[Data] = Tarfile(List(helloFile)).source[Data].chain.stdlib.toList.asInstanceOf[List[Data]]
        val corrupted: List[Data] = Array.frozen(good.stdlib.head.readable.updated(0, ('Z'.toByte: Byte))) :: good.stdlib.tail.to(proscenium.List)
        collectTar { Tarfile.read(corrupted.stdlib.iterator.stream).toList; () }.reasons
      . assert: reasons =>
          reasons.size == 1 && reasons.prim.lay(false)(_.isInstanceOf[Tar.Error.Reason.BadChecksum])

      test(m"an unknown type flag degrades to a file, never a directory"):
        val good: List[Data] = Tarfile(List(helloFile)).source[Data].chain.stdlib.toList.asInstanceOf[List[Data]]
        // Patch the type flag to an unrecognised value and restamp the checksum
        // (bytes 148-156 count as spaces; "%06o\0 " format).
        val flagged = good.stdlib.head.readable.updated(156, 'q'.toByte)

        var sum = 0L
        var i = 0

        while i < 512 do
          sum += (if i >= 148 && i < 156 then 0x20 else flagged(i) & 0xff)
          i += 1

        val octal: String = java.lang.Long.toOctalString(sum).nn
        val padded: String = "000000".substring(octal.length).nn + octal

        var patchedBlock = flagged
        i = 0

        while i < 6 do
          patchedBlock = patchedBlock.updated(148 + i, padded.charAt(i).toByte)
          i += 1

        patchedBlock = patchedBlock.updated(154, 0.toByte).updated(155, ' '.toByte)

        val patched: List[Data] =
          Array.frozen(patchedBlock) :: good.stdlib.tail.to(proscenium.List)

        var entries: scala.collection.immutable.List[bitumen.Tar.Entry] = scala.Nil
        val issues = collectTar:
          entries = Tarfile.read(patched.stdlib.iterator.stream).toList
          ()

        ( issues.reasons.size,
          issues.reasons.prim.let(_.isInstanceOf[Tar.Error.Reason.UnknownTypeFlag]),
          entries.headOption.map(_.isInstanceOf[Tar.Entry.File]) )
      . assert(_ == (1, true, Some(true)))

    suite(m"PAX: long uname / gname round-trip"):
      val longName: Text = "u".repeat(40).nn.tt
      val longGroup: Text = "g".repeat(40).nn.tt

      val file = Tar.Entry.File
                  ( path  = t"a".as[Relative on Tar],
                    mode  = UnixMode(),
                    user  = UnixUser(1000, longName),
                    group = UnixGroup(1000, longGroup),
                    mtime = 0.bits.u32,
                    data  = Tar.Body() )

      val blocks = Tarfile(List(file)).source[Data].chain.stdlib.toList

      test(m"long uname triggers a PAX header (extra block emitted)"):
        blocks.size
      . assert(_ == 5)

      test(m"PAX record contains uname keyword"):
        Array.frozen(blocks(1).readable.slice(0, 80)).utf8.s.contains("uname=")
      . assert(_ == true)

      test(m"PAX record contains gname keyword"):
        Array.frozen(blocks(1).readable.slice(0, 256)).utf8.s.contains("gname=")
      . assert(_ == true)

      test(m"long uname round-trips via reader"):
        val entries = Tarfile.read(blocks.iterator.stream).toList
        entries.head match
          case f: Tar.Entry.File => f.user.name.or(t"")
          case _                => t""
      . assert(_ == longName)

      test(m"long gname round-trips via reader"):
        val entries = Tarfile.read(blocks.iterator.stream).toList
        entries.head match
          case f: Tar.Entry.File => f.group.name.or(t"")
          case _                => t""
      . assert(_ == longGroup)

    suite(m"GNU long-name: writer emits 'L' block and round-trips"):
      val longPath: Text = "a".repeat(150).nn.tt
      val longFile = Tar.Entry.File
                      ( path  = longPath.as[Relative on Tar],
                        mode  = UnixMode(),
                        user  = UnixUser(0),
                        group = UnixGroup(0),
                        mtime = 0.bits.u32,
                        data  = Tar.Body() )

      val tar = Tarfile(List(longFile), LongNameFormat.Gnu)
      val blocks = tar.source[Data].chain.stdlib.toList

      test(m"first block has 'L' type flag at offset 156"):
        blocks(0).readable(156).toChar
      . assert(_ == 'L')

      test(m"first block has name '././@LongLink'"):
        blocks(0).readable.slice(0, 13).toList.map(_.toChar).mkString
      . assert(_ == "././@LongLink")

      test(m"second block holds the long name (first byte 'a')"):
        blocks(1).readable(0).toChar
      . assert(_ == 'a')

      test(m"third block is the regular file header (typeflag '0')"):
        blocks(2).readable(156).toChar
      . assert(_ == '0')

      test(m"long path round-trips via reader honouring 'L'"):
        Tarfile.read(blocks.iterator.stream).toList.head.entryName
      . assert(_ == longPath)

    suite(m"GNU long-name: 'K' block emitted for long link target"):
      val longTarget: Text = "b".repeat(150).nn.tt
      val longSymlink = Tar.Entry.Symlink
                         ( path   = t"link".as[Relative on Tar],
                           mode   = UnixMode(),
                           user   = UnixUser(0),
                           group  = UnixGroup(0),
                           mtime  = 0.bits.u32,
                           target = longTarget )

      val tar = Tarfile(List(longSymlink), LongNameFormat.Gnu)
      val blocks = tar.source[Data].chain.stdlib.toList

      test(m"first block has 'K' type flag"):
        blocks(0).readable(156).toChar
      . assert(_ == 'K')

      test(m"long link target round-trips via reader honouring 'K'"):
        Tarfile.read(blocks.iterator.stream).toList.head match
          case s: Tar.Entry.Symlink => s.target
          case _                   => t""
      . assert(_ == longTarget)

    suite(m"PAX: atime / ctime / sub-second mtime round-trip via .pax field"):
      val file = Tar.Entry.File
                  ( path  = t"clock".as[Relative on Tar],
                    mode  = UnixMode(),
                    user  = UnixUser(0),
                    group = UnixGroup(0),
                    mtime = 1234567890.bits.u32,
                    data  = Tar.Body(),
                    pax   = Map
                             ( t"atime"   -> t"1700000000.500000000",
                               t"ctime"   -> t"1700000001.250000000",
                               t"mtime"   -> t"1234567890.987654321",
                               t"comment" -> t"a test file" ) )

      val bytes = Tarfile(List(file)).source[Data].chain
      val entries = Tarfile.read(bytes.stdlib.iterator.stream).toList

      test(m"atime round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.pax.stdlib.get(t"atime").getOrElse(t"")
          case _                => t""
      . assert(_ == t"1700000000.500000000")

      test(m"ctime round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.pax.stdlib.get(t"ctime").getOrElse(t"")
          case _                => t""
      . assert(_ == t"1700000001.250000000")

      test(m"mtime sub-second portion round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.pax.stdlib.get(t"mtime").getOrElse(t"")
          case _                => t""
      . assert(_ == t"1234567890.987654321")

      test(m"comment round-trips"):
        entries.head match
          case f: Tar.Entry.File => f.pax.stdlib.get(t"comment").getOrElse(t"")
          case _                => t""
      . assert(_ == t"a test file")

    suite(m"External tar reads archives produced by Bitumen"):
      import systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory
      import workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging
      import filesystemOptions.dereferenceSymlinks
      import filesystemOptions.overwritePreexisting
      import filesystemOptions.createNonexistentParents
      import filesystemOptions.deleteRecursively

      val workDir: Path on Linux = temporaryDirectory[Path on Linux] / Uuid().show
      workDir.create[Directory]()

      def writeArchive(tar: Tarfile, name: Text): Path on Linux =
        val path = workDir / name
        if path.existent() then path.delete()
        path.create[File]()
        path.open[File](Write): handle ?=> handle.write(tar)
        path

      def listing(path: Path on Linux): List[Text] =
        Array.unsafeFrozen(sh"tar -tf $path".exec[Text]().s.split('\n').nn).to[List]
          .map(_.nn.tt)
          .filter(!_.s.isEmpty)

      test(m"single-file archive lists hello.txt"):
        listing(writeArchive(Tarfile(List(helloFile)), t"hello.tar"))
      . assert(_ == List(t"hello.txt"))

      test(m"directory archive lists data/"):
        listing(writeArchive(Tarfile(List(emptyDir)), t"dir.tar"))
      . assert(_ == List(t"data/"))

      test(m"file + directory archive lists both"):
        listing(writeArchive(Tarfile(List(helloFile, emptyDir)), t"both.tar"))
      . assert(_ == List(t"hello.txt", t"data/"))

      val longPathA: Text = "a".repeat(150).nn.tt
      val longPathB: Text = "b".repeat(150).nn.tt

      val longFileA = Tar.Entry.File
                       ( path  = longPathA.as[Relative on Tar],
                         mode  = UnixMode(),
                         user  = UnixUser(0),
                         group = UnixGroup(0),
                         mtime = 0.bits.u32,
                         data  = Tar.Body() )

      val longFileB = Tar.Entry.File
                       ( path  = longPathB.as[Relative on Tar],
                         mode  = UnixMode(),
                         user  = UnixUser(0),
                         group = UnixGroup(0),
                         mtime = 0.bits.u32,
                         data  = Tar.Body() )

      test(m"long PAX path is readable by external tar"):
        listing(writeArchive(Tarfile(List(longFileA)), t"longpax.tar"))
      . assert(_ == List(longPathA))

      test(m"long GNU 'L' path is readable by external tar"):
        listing(writeArchive(Tarfile(List(longFileB), LongNameFormat.Gnu), t"longgnu.tar"))
      . assert(_ == List(longPathB))

      val longSymlink = Tar.Entry.Symlink
                         ( path   = t"link".as[Relative on Tar],
                           mode   = UnixMode(),
                           user   = UnixUser(0),
                           group  = UnixGroup(0),
                           mtime  = 0.bits.u32,
                           target = "t".repeat(150).nn.tt )

      test(m"long PAX linkpath is readable by external tar"):
        listing(writeArchive(Tarfile(List(longSymlink)), t"linkpax.tar"))
      . assert(_ == List(t"link"))

      test(m"long GNU 'K' linkpath is readable by external tar"):
        listing(writeArchive(Tarfile(List(longSymlink), LongNameFormat.Gnu), t"linkgnu.tar"))
      . assert(_ == List(t"link"))

    suite(m"Filesystem integration: Tarfile.from / extractTo"):
      import systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory
      import filesystemTraversal.preOrderTraversal
      import filesystemOptions.preserveSymlinks
      import filesystemOptions.overwritePreexisting
      import filesystemOptions.createNonexistentParents
      import filesystemOptions.deleteRecursively

      def freshDir(): Path on Linux =
        val d = temporaryDirectory[Path on Linux] / Uuid().show
        d.create[Directory]()
        d

      test(m"single file round-trips through Tarfile.from / extractTo"):
        val source = freshDir()
        val sourceFile = source / "hello.txt"
        sourceFile.create[File]()
        sourceFile.open[File](Write): handle ?=> handle.write(t"hi there".in[Data])

        // The extension is called directly: fallback from the companion overload no longer
        // re-elaborates under the frozen `Data` stream parameter.
        val tar = bitumen.from(Tarfile)(source)
        val dest = freshDir()
        tar.extractTo(dest)

        val readBackPath: Path on Linux = dest / "hello.txt"
        val readBack = readBackPath.read[Data]
        readBack.utf8.s
      . assert(_ == "hi there")

      test(m"directory hierarchy round-trips"):
        val source = freshDir()
        val sub = source / "sub"
        sub.create[Directory]()
        (sub / "a.txt").create[File]()
        (sub / "a.txt").open[File](Write) { handle ?=> handle.write(t"A".in[Data]) }
        (sub / "b.txt").create[File]()
        (sub / "b.txt").open[File](Write) { handle ?=> handle.write(t"B".in[Data]) }

        val tar = bitumen.from(Tarfile)(source)
        val dest = freshDir()
        tar.extractTo(dest)

        val aPath: Path on Linux = dest / "sub" / "a.txt"
        val bPath: Path on Linux = dest / "sub" / "b.txt"
        val a = aPath.read[Data]
        val b = bPath.read[Data]
        ( a.utf8.s,
          b.utf8.s )
      . assert(_ == ("A", "B"))

      test(m"symlink round-trips"):
        val source = freshDir()
        (source / "real.txt").create[File]()
        (source / "real.txt").open[File](Write) { handle ?=> handle.write(t"realdata".in[Data]) }
        (source / "real.txt").symlinkTo(source / "link.txt")

        val tar = bitumen.from(Tarfile)(source)
        val dest = freshDir()
        tar.extractTo(dest)

        jnf.Files.isSymbolicLink((dest / "link.txt").javaPath)
      . assert(_ == true)

    suite(m"Compression: tar.gzip round-trip"):
      val tar = Tarfile(List(helloFile, emptyDir))

      test(m"gzip round-trips through Tarfile.fromGzip"):
        Tarfile.read(tar.gzip.decompress[Gzip]).toList.map(_.entryName)
      . assert(_ == List(t"hello.txt", t"data/"))

      test(m"zlib round-trips through Tarfile.fromZlib"):
        Tarfile.read(tar.zlib.decompress[Zlib]).toList.map(_.entryName)
      . assert(_ == List(t"hello.txt", t"data/"))

      test(m"deflate round-trips through Tarfile.fromDeflate"):
        Tarfile.read(tar.deflate.decompress[Deflate]).toList.map(_.entryName)
      . assert(_ == List(t"hello.txt", t"data/"))

      test(m"gzipped stream is shorter than uncompressed (for small archive)"):
        val raw = tar.source[Data].chain.stdlib.map(_.readable.length).sum
        val compressed = tar.gzip.chain.stdlib.map(_.readable.length).sum
        compressed < raw
      . assert(_ == true)

    suite(m"Scoped opening"):
      import systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory
      import filesystemOptions.createNonexistentParents
      import filesystemOptions.overwritePreexisting
      import filesystemOptions.deleteRecursively

      val tar = Tarfile(List(helloFile, emptyDir))

      test(m"in-memory data opens as Tar"):
        tar.source[Data].memoize.open[Tar]():
          bitumen.tar.entries.to(List).map(_.entryName)
      . assert(_ == List(t"hello.txt", t"data/"))

      test(m"gzipped data opens as Tar with the Gzip flag"):
        tar.gzip.memoize.open[Tar](Tar.Flag.Gzip):
          bitumen.tar.entries.to(List).map(_.entryName)
      . assert(_ == List(t"hello.txt", t"data/"))

      test(m"an archive file opens as Tar"):
        val workDir: Path on Linux = temporaryDirectory[Path on Linux] / Uuid().show
        workDir.create[Directory]()
        val path: Path on Linux = workDir / "scoped.tar"
        path.open[File](Write, OpenFlag.Create) { handle ?=> handle.write(tar) }

        path.open[Tar]():
          bitumen.tar.entries.to(List).map(_.entryName)
      . assert(_ == List(t"hello.txt", t"data/"))

      test(m"opening for writing is refused"):
        import errorDiagnostics.emptyDiagnostics
        capture[Tar.Error](tar.source[Data].memoize.open[Tar](Write) { () })
        . reason
      . assert(_ == Tar.Error.Reason.WriteUnsupported)

    suite(m"Creating archives"):
      import systems.javaBaseSystem
      import temporaryDirectories.systemTemporaryDirectory
      import filesystemOptions.createNonexistentParents
      import filesystemOptions.overwritePreexisting
      import filesystemOptions.deleteRecursively

      val createDir: Path on Linux = temporaryDirectory[Path on Linux] / Uuid().show
      createDir.create[Directory]()

      test(m"A created archive round-trips through open"):
        val target: Path on Linux = createDir / "made.tar"

        scala.caps.unsafe.unsafeAssumeSeparate:
         target.create[Tar](): builder ?=>
          builder.insert(t"hello.txt".as[Relative on Tar], t"hello".in[Data])

        target.open[Tar]():
          bitumen.tar.entries.to(List).map(_.entryName)
      . assert(_ == List(t"hello.txt"))

      test(m"A gzip-compressed creation round-trips with the Gzip flag"):
        val target: Path on Linux = createDir / "made.tar.gz"

        target.create[Tar](Tar.Flag.Gzip): builder ?=>
          builder.insert(t"data.txt".as[Relative on Tar], t"payload".in[Data])

        target.open[Tar](Tar.Flag.Gzip):
          bitumen.tar.entries.to(List).map(_.entryName)
      . assert(_ == List(t"data.txt"))

      test(m"A scoped entry writer streams an unknown-length body"):
        val target: Path on Linux = createDir / "streamed.tar"

        scala.caps.unsafe.unsafeAssumeSeparate:
         target.create[Tar](): builder ?=>
          builder.file(t"chunks.bin".as[Relative on Tar]): entry ?=>
            for i <- 1 to 5 do entry.put(Array.fill[Byte](1000)(i.toByte))

        target.open[Tar]():
          bitumen.tar.entries.to(List).head match
            case f: Tar.Entry.File => f.data.size
            case _                 => 0
      . assert(_ == 5000)

      test(m"scoped and whole entries interleave in order"):
        val target: Path on Linux = createDir / "mixed.tar"

        scala.caps.unsafe.unsafeAssumeSeparate:
         target.create[Tar](): builder ?=>
          builder.insert(t"first.txt".as[Relative on Tar], t"one".in[Data])
          builder.file(t"second.bin".as[Relative on Tar]): entry ?=>
            entry.put(t"two".in[Data])
          builder.insert(t"third.txt".as[Relative on Tar], t"three".in[Data])

        target.open[Tar]():
          bitumen.tar.entries.to(List).map(_.entryName)
      . assert(_ == List(t"first.txt", t"second.bin", t"third.txt"))

      test(m"a scoped entry writer buffers under compression"):
        val target: Path on Linux = createDir / "streamed.tar.gz"

        target.create[Tar](Tar.Flag.Gzip): builder ?=>
          builder.file(t"z.bin".as[Relative on Tar]): entry ?=>
            entry.put(t"zipped".in[Data])

        target.open[Tar](Tar.Flag.Gzip):
          bitumen.tar.entries.to(List).head match
            case f: Tar.Entry.File => f.data.memoize.readable.to(List).map(_.toChar).mkString
            case _                 => ""
      . assert(_ == "zipped")

      test(m"An exception escaping the creation scope leaves nothing behind"):
        import errorDiagnostics.emptyDiagnostics
        val target: Path on Linux = createDir / "doomed.tar"

        capture[Tar.Error]:
          scala.caps.unsafe.unsafeAssumeSeparate:
           target.create[Tar](): builder ?=>
            builder.insert(t"x".as[Relative on Tar], t"data".in[Data])
            abort(Tar.Error(Tar.Error.Reason.WriteUnsupported))

        target.existent()
      . assert(_ == false)

    suite(m"GNU sparse: round-trip a simple sparse file"):
      val sparseEntry = Tar.Entry.Sparse
                         ( path     = t"sparse.bin".as[Relative on Tar],
                           mode     = UnixMode(),
                           user     = UnixUser(0),
                           group    = UnixGroup(0),
                           mtime    = 0.bits.u32,
                           realSize = 10000L,
                           segments = List
                                       ( SparseSegment(0L, 100L),
                                         SparseSegment(5000L, 200L) ),
                           data     = Tar.Body(Array.fill[Byte](300)('X'.toByte)) )

      val bytes = Tarfile(List(sparseEntry)).source[Data].chain
      val blocks = bytes.stdlib.toList

      test(m"header type flag is 'S'"):
        blocks.head.readable(156).toChar
      . assert(_ == 'S')

      test(m"realsize is recorded at offset 483-494"):
        val realSize = Array.frozen(blocks.head.readable.slice(483, 494)).ascii.s
        java.lang.Long.parseLong(realSize.trim.nn, 8)
      . assert(_ == 10000L)

      test(m"first sparse segment offset+length recorded at 386-409"):
        val ofs = Array.frozen(blocks.head.readable.slice(386, 397)).ascii.s
        val len = Array.frozen(blocks.head.readable.slice(398, 409)).ascii.s
        ( java.lang.Long.parseLong(ofs.trim.nn, 8),
          java.lang.Long.parseLong(len.trim.nn, 8) )
      . assert(_ == (0L, 100L))

      test(m"second sparse segment offset+length recorded at 410-433"):
        val ofs = Array.frozen(blocks.head.readable.slice(410, 421)).ascii.s
        val len = Array.frozen(blocks.head.readable.slice(422, 433)).ascii.s
        ( java.lang.Long.parseLong(ofs.trim.nn, 8),
          java.lang.Long.parseLong(len.trim.nn, 8) )
      . assert(_ == (5000L, 200L))

      test(m"isExtended byte is 0 (fewer than 4 segments)"):
        blocks.head.readable(482).toInt
      . assert(_ == 0)

      val entries = Tarfile.read(bytes.stdlib.iterator.stream).toList

      test(m"reader recovers a Sparse entry"):
        entries.head.isInstanceOf[Tar.Entry.Sparse]
      . assert(_ == true)

      test(m"reader recovers the real size"):
        entries.head match
          case sp: Tar.Entry.Sparse => sp.realSize
          case _                   => 0L
      . assert(_ == 10000L)

      test(m"reader recovers all segments"):
        entries.head match
          case sp: Tar.Entry.Sparse => sp.segments
          case _                   => Nil
      . assert(_ == List(SparseSegment(0L, 100L), SparseSegment(5000L, 200L)))

    suite(m"GNU sparse: more than 4 segments triggers extension blocks"):
      val manySegments = (0 until 10).toList.map: i =>
        SparseSegment((i*1000L), 50L)

      val sparseEntry = Tar.Entry.Sparse
                         ( path     = t"bigsparse.bin".as[Relative on Tar],
                           mode     = UnixMode(),
                           user     = UnixUser(0),
                           group    = UnixGroup(0),
                           mtime    = 0.bits.u32,
                           realSize = 10000L,
                           segments = manySegments.to(proscenium.List),
                           data     = Tar.Body(Array.fill[Byte](500)('X'.toByte)) )

      val bytes = Tarfile(List(sparseEntry)).source[Data].chain
      val blocks = bytes.stdlib.toList

      test(m"header isExtended byte is 1"):
        blocks.head.readable(482).toInt
      . assert(_ == 1)

      test(m"extension block (block 1) is emitted before data"):
        blocks(1).readable(0).toChar
      . assert(_ == '0') // first segment's offset starts with '0' octal

      test(m"reader recovers all 10 segments"):
        Tarfile.read(bytes.stdlib.iterator.stream).toList.head match
          case sp: Tar.Entry.Sparse => sp.segments.stdlib.length
          case _                   => 0
      . assert(_ == 10)
