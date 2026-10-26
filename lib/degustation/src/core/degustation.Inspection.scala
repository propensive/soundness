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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import murmuration.*
import nomenclature.*
import prepositional.*
import rudiments.{at, head, seek, to}
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*
import zeppelin.*

import Inspection.Error.Reason

// The filesystem face of `Degustation.atomize`: atomizes a set of `.tasty` files on disk against
// a dependency classpath of directories and jars. The classpath must carry everything the files
// reference, including the Scala library; at assembly and publish time the buildpath supplies it.
// No compiler is involved: the files are read with the TASTy reader and resolved against the
// classpath by `Universe`. Files are read through the filesystem backend in scope and jars through
// zeppelin, so atomization runs wherever a backend does; the JDK's own classes come from the
// running JVM where there is one, and must otherwise be on the classpath as a jar.
object Inspection:

  def atomize(tastyFiles: List[Text], classpath: List[Text])
    ( using backend: FilesystemBackend on Linux )
  :   List[ScalaAtom] raises Inspection.Error =

    import errorDiagnostics.emptyDiagnostics

    given silent: (Zip.Event is Loggable) = Loggable.silent[Zip.Event]

    def failed(detail: Text): Inspection.Error = Inspection.Error(Reason.InspectionFailed(detail))

    mitigate:
      case Io.Error(_, _, _, _) => failed(t"a file could not be read")
      case Path.Error(_, _)     => failed(t"a path was malformed")
      case Name.Error(_, _, _)  => failed(t"a name was malformed")
      case Zip.Error(_)         => failed(t"a jar was malformed")

    . protect:
        val entries: List[Path on Linux] = classpath.map(_.as[Path on Linux])

        val directories = entries.filter(backend.stat(_, true).entry == galilei.Directory)

        val jars = entries.filter(!directories.has(_)).map(Jar(_))

        val sources: List[ClassSource] =
          directories.map(Directory(_)) + jars + PlatformClasses.sources

        try
          val files = tastyFiles.map: file =>
            val path = file.as[Path on Linux]
            val data = read(path)
            (relativePath(path, data, directories), data)

          Degustation.atomize(files, ClassSource(sources.stdlib*))

        catch
          case error: Degustation.Unencodable =>
            val duplicate = "duplicate key "

            val reason =
              if error.construct.s.startsWith(duplicate)
              then Reason.DuplicateKey(error.construct.s.drop(duplicate.length).tt)
              else Reason.Unencodable(error.construct)

            abort(Inspection.Error(reason))

          case error: Erasure.Unresolved =>
            abort(Inspection.Error(Reason.Unencodable(error.detail)))

          case error: Classfile.Malformed =>
            abort(Inspection.Error(Reason.InspectionFailed(error.detail)))

  // A file's classpath-relative path: relative to the classpath directory containing it when one
  // does, else its package's directory (read from the file itself) and its own name.
  private def relativePath(file: Path on Linux, data: Data, directories: List[Path on Linux])
  :   Text =

    val encoded = file.encode

    val containing: Optional[Text] =
      directories.map(_.encode.s + "/").seek(encoded.s.startsWith(_)).let(_.tt)

    containing match
      case prefix: Text => encoded.s.drop(prefix.s.length).tt

      case _ =>
        val scope = Universe.index(t"", Unpickler(data))
        val packages = scope.owners.values.sweep { case pkg: Text => pkg }
        val name = encoded.s.drop(encoded.s.lastIndexOf('/') + 1).tt

        packages.head match
          case pkg: Text if pkg != t"" && pkg != t"<empty>" =>
            t"${pkg.s.replace('.', '/').nn}/$name"

          case _ => name

  private def read(path: Path on Linux)(using backend: FilesystemBackend on Linux)
  :   Data raises Io.Error =

    backend.expanse(path): view => view.read(0L, view.size.toInt)

  // A classpath directory, read through the filesystem backend.
  private class Directory(root: Path on Linux)
    ( using backend: FilesystemBackend on Linux, ioTactic: Tactic[Io.Error],
            nameTactic: Tactic[Name.Error] )
  extends ClassSource:
    private def resolve(relative: Text): Path on Linux =
      val names: List[Text] = relative.cut(t"/").filter(_ != t"")
      names.fold(root): (path: Path on Linux, name: Text) => path / name

    def read(path: Text): Optional[Data] =
      val file = resolve(path)
      if !backend.exists(file, true) then Unset else Inspection.read(file)

    def list(directory: Text): List[Text] =
      val dir = resolve(directory)
      if !backend.exists(dir, true) then List() else backend.children(dir).to[List]

  // A jar on the classpath, read through zeppelin: the central directory is parsed once, and each
  // entry's payload is read from the file when it is asked for.
  private class Jar(path: Path on Linux)
    ( using FilesystemBackend on Linux, Tactic[Io.Error], Tactic[Zip.Error],
            (Zip.Event is Loggable) )
  extends ClassSource:
    private lazy val zipfile: Zipfile = Zipfile.read(path)

    private lazy val entries: Map[Text, Zip.Entry] =
      def named(entry: Zip.Entry): (Text, Zip.Entry) =
        (entry.ref.encode.s.stripPrefix("/").tt, entry)

      zipfile.entries.filter(!_.directory).map(named).to[Map]

    private lazy val directories: Map[Text, List[Text]] =
      def directoryOf(name: Text): Text = name.s.lastIndexOf('/') match
        case -1    => t""
        case index => name.s.substring(0, index).nn.tt

      def simpleName(name: Text): Text = name.s.substring(name.s.lastIndexOf('/') + 1).nn.tt

      entries.keys.to[List].group(directoryOf).map(_.map(simpleName))

    def read(path: Text): Optional[Data] =
      entries.at(path).let: entry => summon[Data is Aggregable by Data].accept(entry.contents)

    def list(directory: Text): List[Text] = directories.at(directory).or(List())

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
