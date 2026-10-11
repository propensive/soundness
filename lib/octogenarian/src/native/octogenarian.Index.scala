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
package octogenarian

import java.nio.ByteBuffer

import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import galilei.*
import gastronomy.*
import gossamer.*
import monotonous.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*

import sortingAlgorithms.timsort
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.overwritePreexisting
import filesystemOptions.moveAtomically
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.deleteRecursively
import alphabets.hexLowerCase
import cryptoPermits.permitDeprecatedCrypto
import providers.javaBaseProvider

// The index (`DIRC`), versions 2, 3 and 4: the staged tree with the stat data that lets a status
// skip hashing unchanged files. Read whole and written whole, through `index.lock`, with the
// racy-git rule applied: an entry whose modification time is not older than the index file's
// cannot be trusted and is re-hashed.
object Index:
  // One index entry. `stage` is 0 for a merged path and 1 to 3 for the base, ours and theirs of
  // a conflict; `mode` is the full octal mode as an integer.
  case class Entry
    ( path:         Text,
      mode:         Int,
      hash:         Git.Hash,
      stage:        Int,
      changedSec:   Int,
      changedNanos: Int,
      modifiedSec:  Int,
      modifiedNanos: Int,
      device:       Int,
      inode:        Int,
      user:         Int,
      group:        Int,
      size:         Int,
      assumeValid:  Boolean  = false,
      skipWorktree: Boolean  = false,
      intentToAdd:  Boolean  = false )

  // An extension block preserved as it was read.
  case class Extension(signature: Text, data: Data)

  case class Contents(version: Int, entries: List[Entry], extensions: List[Extension]):
    def staged: List[Entry] = entries.filter(_.stage == 0)
    def at(path: Text): Optional[Entry] = entries.reap { case entry if entry.path == path && entry.stage == 0 => entry }

  val empty: Contents = Contents(2, Nil, Nil)

  val regular: Int = 0x81a4      // 100644
  val executable: Int = 0x81ed   // 100755
  val symlink: Int = 0xa000      // 120000
  val gitlink: Int = 0xe000      // 160000

  def read(gitDir: Path on Linux)(using Tactic[Git.Error], Tactic[Io.Error]): Contents =
    val file = gitDir / "index"
    if !file.existent() then empty else parse(file.read[Data])

  def parse(data: Data)(using Tactic[Git.Error]): Contents =
    val array = data.asInstanceOf[scala.Array[Byte]]
    val buffer = ByteBuffer.wrap(array).nn
    if array.length < 12 || buffer.getInt(0) != 0x44495243 then abort(Git.Error(Git.Error.Reason.Unexpected, t"the index is not a DIRC file"))
    val version = buffer.getInt(4)
    val count = buffer.getInt(8)
    if version < 2 || version > 4 then abort(Git.Error(Git.Error.Reason.Unsupported(t"index version $version")))

    def nul(from: Int): Int = if from >= array.length || array(from) == 0 then from else nul(from + 1)

    // Version 4 prefix-compresses names: a varint count of bytes to strip from the previous
    // name, then the suffix.
    def varint(position: Int, value: Int): (Int, Int) =
      val byte = array(position) & 0xff
      val next = (value << 7) | (byte & 0x7f)
      if (byte & 0x80) == 0 then (next, position + 1) else varint(position + 1, next + 1)

    def entries(position: Int, remaining: Int, previous: Text, acc: List[Entry]): (List[Entry], Int) =
      if remaining == 0 then (acc.reverse, position) else
        val flags = buffer.getShort(position + 60) & 0xffff
        val extended = version >= 3 && (flags & 0x4000) != 0
        val extendedFlags = if extended then buffer.getShort(position + 62) & 0xffff else 0
        val nameStart = position + (if extended then 64 else 62)

        val (name, next) =
          if version == 4 then
            val (strip, suffixStart) = varint(nameStart, 0)
            val end = nul(suffixStart)
            val kept = previous.keep(previous.length - strip)
            (t"$kept${String(array, suffixStart, end - suffixStart, "UTF-8").nn.tt}", end + 1)
          else
            val end = nul(nameStart)
            val name = String(array, nameStart, end - nameStart, "UTF-8").nn.tt
            val length = end - position + 1
            val padded = (length + 7) / 8 * 8
            (name, position + padded)

        val hash = Git.Hash.unsafe(Objects.hex(array, position + 40, 20))

        val entry =
          Entry
            ( name,
              buffer.getInt(position + 24),
              hash,
              (flags >> 12) & 3,
              buffer.getInt(position),
              buffer.getInt(position + 4),
              buffer.getInt(position + 8),
              buffer.getInt(position + 12),
              buffer.getInt(position + 16),
              buffer.getInt(position + 20),
              buffer.getInt(position + 28),
              buffer.getInt(position + 32),
              buffer.getInt(position + 36),
              (flags & 0x8000) != 0,
              (extendedFlags & 0x4000) != 0,
              (extendedFlags & 0x2000) != 0 )

        entries(next, remaining - 1, name, entry :: acc)

    val (parsed, afterEntries) = entries(12, count, t"", Nil)

    def extensions(position: Int, acc: List[Extension]): List[Extension] =
      if position + 8 > array.length - 20 then acc.reverse else
        val signature = String(array, position, 4, "UTF-8").nn.tt
        val length = buffer.getInt(position + 4)
        val content = java.util.Arrays.copyOfRange(array, position + 8, position + 8 + length).nn
        extensions(position + 8 + length, Extension(signature, content.asInstanceOf[Data]) :: acc)

    Contents(version, parsed, extensions(afterEntries, Nil))

  // Extensions which describe the entries and would be wrong after a rewrite are dropped; the
  // rest (such as `REUC`, the resolve-undo record) are copied through.
  private val volatile: Set[Text] = Set(t"TREE", t"UNTR", t"FSMN", t"EOIE", t"IEOT")

  def render(contents: Contents): Data =
    val extended = contents.entries.exists { entry => entry.skipWorktree || entry.intentToAdd }
    val version = if extended then 3.max(contents.version.min(3)) else 2
    val out = java.io.ByteArrayOutputStream()
    val sorted = contents.entries.order(entry => (entry.path.s, entry.stage))

    def int(value: Int): Unit =
      out.write((value >>> 24) & 0xff); out.write((value >>> 16) & 0xff)
      out.write((value >>> 8) & 0xff); out.write(value & 0xff)

    def short(value: Int): Unit =
      out.write((value >>> 8) & 0xff); out.write(value & 0xff)

    def entry(entry: Entry): Unit =
      int(entry.changedSec); int(entry.changedNanos); int(entry.modifiedSec); int(entry.modifiedNanos)
      int(entry.device); int(entry.inode); int(entry.mode); int(entry.user); int(entry.group); int(entry.size)
      out.write(Objects.unhex(entry.hash.text))
      val name = entry.path.s.getBytes("UTF-8").nn
      val useExtended = entry.skipWorktree || entry.intentToAdd
      val flags = (if entry.assumeValid then 0x8000 else 0) | (if useExtended then 0x4000 else 0) |
        ((entry.stage & 3) << 12) | name.length.min(0xfff)
      short(flags)
      if useExtended then short((if entry.skipWorktree then 0x4000 else 0) | (if entry.intentToAdd then 0x2000 else 0))
      out.write(name)
      val length = (if useExtended then 64 else 62) + name.length + 1
      val padding = (length + 7) / 8 * 8 - length + 1

      def pad(n: Int): Unit = if n > 0 then
        out.write(0)
        pad(n - 1)

      pad(padding)

    out.write(0x44); out.write(0x49); out.write(0x52); out.write(0x43)
    int(version)
    int(sorted.fold(0) { (count, _) => count + 1 })
    sorted.each(entry(_))

    contents.extensions.each: extension =>
      if !volatile.has(extension.signature) then
        out.write(extension.signature.s.getBytes("UTF-8").nn)
        int(extension.data.length)
        out.write(extension.data.asInstanceOf[scala.Array[Byte]])

    val body = out.toByteArray.nn.asInstanceOf[Data]
    val trailer = Objects.unhex(body.digest[Sha1].serialize[Hex])
    Store.concatenate(body, trailer.asInstanceOf[Data])

  // Written through `index.lock`, which another git process would also take.
  def write(gitDir: Path on Linux, contents: Contents)(using Tactic[Git.Error], Tactic[Io.Error]): Unit =
    val file: Path on Linux = gitDir / "index"
    val lockFile: Path on Linux = gitDir / "index.lock"

    try lockFile.open[File](Write, OpenFlag.Create, OpenFlag.Exclusive) { handle ?=> () }
    catch case error: Io.Error => abort(Git.Error(Git.Error.Reason.Locked(file.encode)))

    try
      lockFile.write(render(contents))
      lockFile.moveTo(file)
    finally if lockFile.existent() then lockFile.delete()

  // The modification time of the index file itself, in seconds, for the racy-git rule.
  def modified(gitDir: Path on Linux)(using Tactic[Io.Error]): Long =
    val file = gitDir / "index"
    if !file.existent() then 0L
    else summon[FilesystemBackend on Linux].stat(file, true).modified / 1000L

  // An entry describing a working file as it is now.
  def entryFor(worktree: Path on Linux, path: Text, hash: Git.Hash, mode: Int)
    ( using Tactic[Io.Error] )
  :   Entry =

    val file = Refstore.descend(worktree, path.cut(t"/"))
    val backend = summon[FilesystemBackend on Linux]
    val stat = backend.stat(file, false)
    val identity = backend.identity(file, false)
    val seconds = (stat.modified / 1000L).toInt
    val nanos = ((stat.modified % 1000L) * 1000000L).toInt

    Entry
      ( path, mode, hash, 0, seconds, nanos, seconds, nanos,
        identity.let(_.device.toInt).or(0), identity.let(_.inode.toInt).or(0),
        stat.user.or(0), stat.group.or(0), stat.size.toInt )

  // Whether the stat data recorded for an entry still describes the file: if not, the content
  // must be hashed to know whether it changed. The racy-git rule treats an entry written in the
  // same second as the index as unverified.
  def fresh(entry: Entry, worktree: Path on Linux, indexModified: Long)(using Tactic[Io.Error]): Boolean =
    val file = Refstore.descend(worktree, entry.path.cut(t"/"))
    if !file.existent() then false else
      val stat = summon[FilesystemBackend on Linux].stat(file, false)
      val seconds = (stat.modified / 1000L).toInt
      val nanos = ((stat.modified % 1000L) * 1000000L).toInt

      stat.size.toInt == entry.size && seconds == entry.modifiedSec && nanos == entry.modifiedNanos &&
        entry.modifiedSec.toLong < indexModified
