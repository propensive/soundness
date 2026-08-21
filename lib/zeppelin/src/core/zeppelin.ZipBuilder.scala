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
package zeppelin

import scala.caps

import java.io as ji
import java.nio.file as jnf

import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import galilei.CreateFlag
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import hieroglyph.*, charEncoders.utf8Encoder
import turbulence.*
import vacuous.*
import zephyrine.*

// The authoring handle provided by `path.create[Zip]()` (and, refined with manifest support,
// `path.create[Jar]()`). Entries accumulate in insertion order; a duplicate name is an error
// at the offending `insert`, not at commit. The archive is serialized only when the creation
// scope closes, to a temporary sibling moved atomically onto the target, so an exception
// escaping the scope leaves nothing behind.
class ZipBuilder private[zeppelin] (using Tactic[Zip.Error])
extends caps.ExclusiveCapability:

  private var stack: List[Zip.Entry] = Nil
  private var names: Set[Text] = Set()
  private var remark: Optional[Text] = Unset

  def insert(entry: Zip.Entry): Unit =
    if names.has(entry.ref.encode)
    then abort(Zip.Error(Zip.Error.Reason.DuplicateEntry(entry.ref)))

    names = Set.of(names.stdlib + entry.ref.encode)
    stack ::= entry

  def insert[content: Streamable by Data over Credit](ref: Path on Zip, content: content)
    ( using Zip.Compression )
  :   Unit =

    insert(Zip.Entry(ref, content))

  def comment(text: Text): Unit = remark = text

  private[zeppelin] def zipfile: Zipfile = Zipfile(stack.reverse, remark, Unset)

class JarBuilder private[zeppelin] (using Tactic[Zip.Error]) extends ZipBuilder:

  // Writes `META-INF/MANIFEST.MF` from the given attributes, with values wrapped at 72 bytes
  // per the JAR specification. Call it first if the manifest should lead the archive, as
  // convention prefers.
  def manifest(attributes: (Text, Text)*)(using Zip.Compression): Unit =
    val lines = attributes.to(List).map { (key, value) => wrap(t"$key: $value") }
    val text = lines.join(t"", t"\r\n", t"\r\n\r\n")
    insert(Zip.Entry(ZipBuilder.manifestRef, text))

  private def wrap(line: Text): Text =
    if line.s.length <= 70 then line else t"${line.s.take(70)}\r\n ${wrap(line.s.drop(70).tt)}"

object ZipBuilder:
  private[zeppelin] val manifestRef: Path on Zip =
    Path[Zip, Text, Tuple](t"", List(t"MANIFEST.MF", t"META-INF"))

  // Creation instances for the `Zip` and `Jar` forms. The default `make` (a discarded
  // builder) writes a valid empty archive. `CreateFlag.Parents` and `CreateFlag.Replace`
  // govern the destination as they do for filesystem entries.
  class ZipCreatable[path: Abstractable across Paths to Text](using Tactic[Zip.Error])
  extends Creatable:

    type Self = path
    type Form = Zip
    type Operand = CreateFlag
    type Grants = Grant.Read & Grant.Write
    type Result = ZipBuilder

    def create[result]
      ( value: path, flags: List[CreateFlag] )
      ( block: (ZipBuilder & Granting[Grant.Read & Grant.Write]) ?=> result )
    :   result =

      val builder = new ZipBuilder with Granting[Grant.Read & Grant.Write] {}
      val outcome = block(using builder)
      commit(value.generic, flags, builder.zipfile)
      outcome

  class JarCreatable[path: Abstractable across Paths to Text](using Tactic[Zip.Error])
  extends Creatable:

    type Self = path
    type Form = Jar
    type Operand = CreateFlag
    type Grants = Grant.Read & Grant.Write
    type Result = JarBuilder

    def create[result]
      ( value: path, flags: List[CreateFlag] )
      ( block: (JarBuilder & Granting[Grant.Read & Grant.Write]) ?=> result )
    :   result =

      val builder = new JarBuilder with Granting[Grant.Read & Grant.Write] {}
      val outcome = block(using builder)
      commit(value.generic, flags, builder.zipfile)
      outcome

  // Serialize to a hidden temporary sibling, then move atomically onto the target.
  private def commit(filename: Text, flags: List[CreateFlag], zipfile: Zipfile)
    ( using Tactic[Zip.Error] )
  :   Unit =

    val target = jnf.Path.of(filename.s).nn

    if !flags.has(CreateFlag.Replace) && jnf.Files.exists(target)
    then abort(Zip.Error(Zip.Error.Reason.AlreadyExists))

    try
      if flags.has(CreateFlag.Parents) then
        Option(target.toAbsolutePath.nn.getParent).foreach(jnf.Files.createDirectories(_))

      val parent = target.toAbsolutePath.nn.getParent
      val temporary = target.resolveSibling(t".${filename.s.split('/').nn.last.nn}.part".s).nn

      try
        val out = ji.FileOutputStream(temporary.toFile)

        try
          zipfile.serialize.drain: region =>
            range =>
              val interval: Interval = range
              out.write(unsafely(region.raw.asInstanceOf[scala.Array[Byte]]), interval.start.n0,
                  interval.size)
        finally out.close()

        jnf.Files.move(temporary, target, jnf.StandardCopyOption.ATOMIC_MOVE,
          jnf.StandardCopyOption.REPLACE_EXISTING)
      catch case throwable: Throwable =>
        try jnf.Files.deleteIfExists(temporary) catch case _: Exception => ()
        throw throwable
    catch
      case error: ji.IOException =>
        abort(Zip.Error(Zip.Error.Reason.CannotWrite(error.getMessage.nn.tt)))
