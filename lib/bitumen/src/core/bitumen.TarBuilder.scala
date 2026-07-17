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

import java.io as ji
import java.nio.file as jnf

import anticipation.*
import aperture.*
import contingency.*
import galilei.CreateFlag
import gossamer.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// The authoring handle provided by `path.create[Tar](flags*)`: bitumen's first writer.
// Entries accumulate in insertion order; TAR permits duplicate names (later entries
// supersede on extraction), so nothing is checked at insert. The archive is serialized only
// when the creation scope closes — compressed per any `TarFlag`, long names encoded per any
// `LongNameFormat` flag — to a temporary sibling moved atomically onto the target, so an
// exception escaping the scope leaves nothing behind.
class TarBuilder private[bitumen] () extends caps.ExclusiveCapability:
  private var stack: List[Tar.Entry] = Nil

  def insert(entry: Tar.Entry): Unit = stack ::= entry

  def insert[data: Streamable by Data over Credit as streamable](name: TarRef, data: data)
  :   Unit =

    insert(Tar.Entry.File
      ( name, UnixMode(), UnixUser(0), UnixGroup(0), 0.bits.u32,
        streamable.stream(data).toLazyList ))

  private[bitumen] def tarfile(format: LongNameFormat): Tarfile =
    Tarfile(stack.reverse.to(LazyList), format)

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

      val builder = new TarBuilder with Granting[Grant.Read & Grant.Write] {}
      val outcome = block(using builder)

      val format = flags.collectFirst { case format: LongNameFormat => format }
        . getOrElse(LongNameFormat.Pax)

      val tarfile = builder.tarfile(format)

      val stream: LazyList[Data] = flags.collectFirst:
        case TarFlag.Gzip    => tarfile.gzip
        case TarFlag.Zlib    => tarfile.zlib
        case TarFlag.Deflate => tarfile.deflate

      . getOrElse(tarfile.source[Data].toLazyList)

      commit(value.generic, flags.collect { case flag: CreateFlag => flag }, stream)
      outcome

  // Serialize to a hidden temporary sibling, then move atomically onto the target.
  private def commit(filename: Text, flags: List[CreateFlag], stream: LazyList[Data])
    ( using Tactic[TarError] )
  :   Unit =

    val target = jnf.Path.of(filename.s).nn

    if !flags.contains(CreateFlag.Replace) && jnf.Files.exists(target)
    then abort(TarError(TarError.Reason.AlreadyExists))

    try
      if flags.contains(CreateFlag.Parents) then
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
