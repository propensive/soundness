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

import scala.caps

import java.io as ji
import java.nio.file as jnf

import scala.collection.mutable as scm

import anticipation.*
import aperture.*
import contingency.*
import galilei.CreateFlag
import gossamer.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import pneumatic.*
import turbulence.*
import vacuous.*
import zephyrine.*

// The scoped writer lent by `builder.file(name)`: body chunks stream to the
// archive as they are written, with no per-entry buffering on an uncompressed
// target. Confined to its block; nothing of it may be retained.
class TarEntryWriter private[bitumen] (put0: Data => Unit) extends caps.ExclusiveCapability:
  def put(chunk: Data): Unit = put0(chunk)

  def write[data: Streamable by Data over Credit as streamable](data: data)(using Buffering)
  :   Unit =

    zephyrine.toProgression(streamable.stream(data)).foreach(put0(_))

// The authoring handle provided by `path.create[Tar](flags*)`. TAR permits duplicate names
// (later entries supersede on extraction), so nothing is checked at insert.
//
// On an UNCOMPRESSED target, the builder writes straight through to the temporary sibling as
// entries are inserted: a file whose payload is lazy streams to disk in bounded chunks, its
// header written as a placeholder and backpatched with the real size and checksum once the
// body has passed — so an entry of unknown length (`builder.file(...)`) streams end-to-end.
// A COMPRESSED target cannot be backpatched through the compressor, so entries accumulate
// and serialize when the scope closes, buffering each payload as the eager writer always
// did. Either way the target appears atomically, or not at all.
class TarBuilder private[bitumen]
  ( sink: Optional[ji.RandomAccessFile], format: LongNameFormat )
  ( using Tactic[TarError] )
extends caps.ExclusiveCapability:
  private var stack: List[Tar.Entry] = Nil

  def insert(entry: Tar.Entry): Unit = sink.lay(stack ::= entry)(writeEntry(_, entry))

  def insert[data: Streamable by Data over Credit as streamable](name: TarRef, data: data)
  :   Unit =

    insert(Tar.Entry.File
      ( name, UnixMode(), UnixUser(0), UnixGroup(0), 0.bits.u32,
        streamable.stream(data).toProgression ))

  // Author one entry with a streamed, unknown-length body: the block writes
  // chunks through the lent `TarEntryWriter`. On an uncompressed target the
  // body goes straight to disk and the header is backpatched; on a compressed
  // target the body is buffered and inserted whole when the block returns.
  def file[result]
    ( name:  TarRef,
      mode:  UnixMode  = UnixMode(),
      user:  UnixUser  = UnixUser(0),
      group: UnixGroup = UnixGroup(0),
      mtime: U32       = 0.bits.u32 )
    ( block: TarEntryWriter^ ?=> result )
  :   result =

    sink.lay:
      val buffer = scm.ArrayBuffer[Data]()
      val outcome = block(using TarEntryWriter(buffer += _))
      insert(Tar.Entry.File(name, mode, user, group, mtime, buffer.to(Progression)))
      outcome

    . apply: out =>
        val probe = Tar.Entry.File(name, mode, user, group, mtime, Progression())
        Tarfile.preamble(probe, format).each { chunk => write(out, chunk) }

        val headerPosition = out.getFilePointer
        write(out, Tarfile.zeroBlock)

        var count: Long = 0
        val outcome = block(using TarEntryWriter { chunk => write(out, chunk); count += chunk.length })

        pad(out, count)
        val end = out.getFilePointer
        out.seek(headerPosition)
        write(out, probe.headerWith(count.toInt.bits.u32))
        out.seek(end)
        outcome

  private def writeEntry(out: ji.RandomAccessFile, entry: Tar.Entry): Unit =
    entry match
      case file: Tar.Entry.File =>
        // Stream the body with a backpatched header, so a lazy payload is
        // never held in memory.
        Tarfile.preamble(entry, format).each { chunk => write(out, chunk) }
        val headerPosition = out.getFilePointer
        write(out, Tarfile.zeroBlock)

        var count: Long = 0
        file.data.each { chunk => write(out, chunk); count += chunk.length }

        pad(out, count)
        val end = out.getFilePointer
        out.seek(headerPosition)
        write(out, file.headerWith(count.toInt.bits.u32))
        out.seek(end)

      case other =>
        Tarfile.preamble(entry, format).each { chunk => write(out, chunk) }
        other.serialize.each { chunk => write(out, chunk) }

  private def pad(out: ji.RandomAccessFile, count: Long): Unit =
    val remainder = (count%512).toInt
    if remainder != 0 then write(out, Tarfile.zeroBlock.slice(0, 512 - remainder))

  private def write(out: ji.RandomAccessFile, chunk: Data): Unit =
    try out.write(chunk.mutable(using Unsafe))
    catch case error: ji.IOException =>
      abort(TarError(TarError.Reason.CannotWrite(error.getMessage.nn.tt)))

  // The two terminating zero blocks, in streaming mode.
  private[bitumen] def finish(): Unit = sink.let: out =>
    write(out, Tarfile.zeroBlock)
    write(out, Tarfile.zeroBlock)

  private[bitumen] def tarfile(format: LongNameFormat): Tarfile =
    Tarfile(stack.stdlib.reverse.to(Progression), format)

object TarBuilder:
  class TarCreatable[path: Abstractable across Paths to Text](using Tactic[TarError])
  extends Creatable:

    type Self = path
    type Form = Tar
    type Operand = CreateFlag | TarFlag | LongNameFormat
    type Grants = Grant.Read & Grant.Write
    type Result = TarBuilder

    def create[result]
      ( value: path, flags: List[CreateFlag | TarFlag | LongNameFormat] )
      ( block: ((TarBuilder & Granting[Grant.Read & Grant.Write])^) ?=> result )
    :   result =

      val format = flags.stdlib.collectFirst { case format: LongNameFormat => format }
        . getOrElse(LongNameFormat.Pax)

      val compression = flags.stdlib.collectFirst { case flag: TarFlag => flag }
      val createFlags = flags.stdlib.collect { case flag: CreateFlag => flag }

      compression match
        case Some(tarFlag) =>
          // A compressed target cannot be backpatched through the compressor:
          // entries accumulate and serialize at scope close, as before.
          val builder = new TarBuilder(Unset, format) with Granting[Grant.Read & Grant.Write] {}
          val outcome = block(using builder)
          val tarfile = builder.tarfile(format)

          val stream: Progression[Data] = tarFlag match
            case TarFlag.Gzip    => tarfile.gzip
            case TarFlag.Zlib    => tarfile.zlib
            case TarFlag.Deflate => tarfile.deflate

          commit(value.generic, List.of(createFlags), stream)
          outcome

        case None =>
          // Streaming mode: the temporary sibling opens up front and entries
          // write through as they are inserted, headers backpatched; the
          // target still appears atomically, or not at all.
          val filename = value.generic
          val target = jnf.Path.of(filename.s).nn

          if !createFlags.contains(CreateFlag.Replace) && jnf.Files.exists(target)
          then abort(TarError(TarError.Reason.AlreadyExists))

          try
            if createFlags.contains(CreateFlag.Parents) then
              Option(target.toAbsolutePath.nn.getParent).foreach(jnf.Files.createDirectories(_))

            val temporary =
              target.resolveSibling(t".${filename.s.split('/').nn.last.nn}.part".s).nn

            try
              val out = ji.RandomAccessFile(temporary.toFile, "rw")

              val outcome =
                try
                  val builder =
                    new TarBuilder(out, format) with Granting[Grant.Read & Grant.Write] {}

                  val result = block(using builder)
                  builder.finish()
                  result
                finally out.close()

              jnf.Files.move(temporary, target, jnf.StandardCopyOption.ATOMIC_MOVE,
                jnf.StandardCopyOption.REPLACE_EXISTING)

              outcome
            catch case throwable: Throwable =>
              try jnf.Files.deleteIfExists(temporary) catch case _: Exception => ()
              throw throwable
          catch
            case error: ji.IOException =>
              abort(TarError(TarError.Reason.CannotWrite(error.getMessage.nn.tt)))

  // Serialize to a hidden temporary sibling, then move atomically onto the target.
  private def commit(filename: Text, flags: List[CreateFlag], stream: Progression[Data])
    ( using Tactic[TarError] )
  :   Unit =

    val target = jnf.Path.of(filename.s).nn

    if !flags.stdlib.contains(CreateFlag.Replace) && jnf.Files.exists(target)
    then abort(TarError(TarError.Reason.AlreadyExists))

    try
      if flags.stdlib.contains(CreateFlag.Parents) then
        Option(target.toAbsolutePath.nn.getParent).foreach(jnf.Files.createDirectories(_))

      val temporary = target.resolveSibling(t".${filename.s.split('/').nn.last.nn}.part".s).nn

      try
        val out = ji.FileOutputStream(temporary.toFile)

        try stream.each { chunk => out.write(chunk.mutable(using Unsafe)) }
        finally out.close()

        jnf.Files.move(temporary, target, jnf.StandardCopyOption.ATOMIC_MOVE,
          jnf.StandardCopyOption.REPLACE_EXISTING)
      catch case throwable: Throwable =>
        try jnf.Files.deleteIfExists(temporary) catch case _: Exception => ()
        throw throwable
    catch
      case error: ji.IOException =>
        abort(TarError(TarError.Reason.CannotWrite(error.getMessage.nn.tt)))
