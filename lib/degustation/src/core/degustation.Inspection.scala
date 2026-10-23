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
package degustation

import java.nio.file as jnf
import java.util.zip as juz

import scala.jdk.CollectionConverters.IteratorHasAsScala

import anticipation.*
import contingency.*
import denominative.nil
import fulminate.*
import gossamer.*
import murmuration.*
import rudiments.{at, each, prim, seek}
import symbolism.*
import vacuous.*

import Inspection.Error.Reason

// The filesystem face of `Degustation.atomize`: atomizes a set of `.tasty` files on disk against
// a dependency classpath of directories and jars, with the JDK's own classes behind them. The
// classpath must carry everything the files reference, including the Scala library; at assembly
// and publish time the buildpath supplies it. No compiler is involved: the files are read with
// the TASTy reader and resolved against the classpath by `Universe`.
object Inspection:

  def atomize(tastyFiles: List[Text], classpath: List[Text])
  :   List[ScalaAtom] raises Inspection.Error =

    val entries = classpath.map(entry => jnf.Paths.get(entry.s).nn)
    val directories = entries.filter(jnf.Files.isDirectory(_))
    val jars = entries.filter(entry => !jnf.Files.isDirectory(entry)).map(Jar(_))
    val sources: List[ClassSource] = directories.map(Directory(_)) + jars + List(Platform)

    try
      val files = tastyFiles.map: file =>
        val path = jnf.Paths.get(file.s).nn
        val data = read(path)
        (relativePath(path, data, directories), data)

      Degustation.atomize(files, ClassSource(sources.stdlib*))

    catch
      case error: Degustation.Unencodable =>
        if error.construct.s.startsWith("duplicate key ")
        then abort(Inspection.Error(Reason.DuplicateKey(error.construct.s.drop("duplicate key ".length).tt)))
        else abort(Inspection.Error(Reason.Unencodable(error.construct)))

      case error: Erasure.Unresolved  => abort(Inspection.Error(Reason.Unencodable(error.detail)))
      case error: Classfile.Malformed => abort(Inspection.Error(Reason.InspectionFailed(error.detail)))

      case error: java.io.IOException =>
        abort(Inspection.Error(Reason.InspectionFailed(error.getMessage.nn.tt)))

    finally jars.each(_.close())

  // A file's classpath-relative path: relative to the classpath directory containing it when one
  // does, else its package's directory (read from the file itself) and its own name.
  private def relativePath(file: jnf.Path, data: Data, directories: List[jnf.Path]): Text =
    directories.seek(directory => file.startsWith(directory)) match
      case directory: jnf.Path =>
        directory.relativize(file).nn.toString.replace(jnf.FileSystems.getDefault.nn.getSeparator.nn, "/").nn.tt

      case _ =>
        val scope = Universe.index(t"", Unpickler(data))
        val packages = scope.owners.values.sweep { case pkg: Text => pkg }
        val name = file.getFileName.nn.toString.tt

        packages.prim match
          case pkg: Text if pkg != t"" && pkg != t"<empty>" => t"${pkg.s.replace('.', '/').nn}/$name"
          case _                                           => name

  private def read(path: jnf.Path): Data = data(jnf.Files.readAllBytes(path).nn)

  private def data(bytes: scala.Array[Byte]): Data =
    Data.build(bytes.length)(array => java.lang.System.arraycopy(bytes, 0, array, 0, bytes.length))

  private class Directory(root: jnf.Path) extends ClassSource:
    def read(path: Text): Optional[Data] =
      val file = root.resolve(path.s).nn
      if jnf.Files.isRegularFile(file) then Inspection.read(file) else Unset

    def list(directory: Text): List[Text] =
      val dir = root.resolve(directory.s).nn

      if jnf.Files.isDirectory(dir) then
        val stream = jnf.Files.list(dir).nn
        try List.from(stream.iterator.nn.asScala.map(_.nn.getFileName.nn.toString.tt))
        finally stream.close()
      else List()

  private class Jar(path: jnf.Path) extends ClassSource:
    private val zip = juz.ZipFile(path.toFile)

    private lazy val entries: Map[Text, List[Text]] =
      val names = List.from(zip.entries.nn.asScala.map(_.nn.getName.nn))

      names.group: name =>
        name.lastIndexOf('/') match
          case -1    => t""
          case index => name.substring(0, index).nn.tt
      . map: group =>
          group.map(name => name.substring(name.lastIndexOf('/') + 1).nn.tt)

    def read(path: Text): Optional[Data] =
      Optional(zip.getEntry(path.s)).let: entry =>
        val stream = zip.getInputStream(entry).nn
        try data(stream.readAllBytes().nn) finally stream.close()

    def list(directory: Text): List[Text] = entries.at(directory).or(List())

    def close(): Unit = zip.close()

  // The JDK's own classes, read through the platform class loader.
  private object Platform extends ClassSource:
    def read(path: Text): Optional[Data] =
      Optional(ClassLoader.getSystemResourceAsStream(path.s)).let: stream =>
        try data(stream.readAllBytes().nn) finally stream.close()

    def list(directory: Text): List[Text] = List()

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case InspectionFailed(detail: Text)  extends Reason(1)
      case Unencodable(construct: Text)    extends Reason(2)
      case DuplicateKey(key: Text)         extends Reason(3)

    given communicable: Reason is Communicable =
      case Reason.InspectionFailed(detail) => m"the TASTy could not be read: $detail"
      case Reason.Unencodable(construct)   => m"no canonical encoding is defined for $construct"
      case Reason.DuplicateKey(key)        => m"the key $key was produced twice"

  case class Error(reason: Inspection.Error.Reason)(using Diagnostics)
  extends fulminate.Error(642, reason.number)(m"the Scala discipline failed because $reason")
