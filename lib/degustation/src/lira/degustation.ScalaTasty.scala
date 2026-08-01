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
import fulminate.*
import gossamer.*
import reliquary.*
import rudiments.*

import errorDiagnostics.emptyDiagnostics

// The `scala-tasty/1` discipline, adapted to reliquary's SPI. It claims `.tasty` files — the
// interface carrier shared by the `jvm`, `sjsir` and `nir` universes — for atomization, and
// claims the derived binaries (`.class`, `.sjsir`, `.nir`) *atomless*: their interface is
// exactly the TASTy's, so they contribute no atoms of their own and never fall through to
// `opaque/1`, where every rebuild would register as a major event.
object ScalaTasty extends Discipline:
  def id: Text = t"scala-tasty/1"

  private val atomless: scala.List[String] = scala.List(".class", ".sjsir", ".nir")

  def claims(path: TreePath, data: Data): Boolean =
    val name = path.text.s
    name.endsWith(".tasty") || atomless.exists: suffix => name.endsWith(suffix)

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises DisciplineError =

    val tasty = content.stdlib.filter: pair => pair(0).text.s.endsWith(".tasty")

    if tasty.isEmpty then Atomization.of(id, List()) else
      // The compiler's unpickler reads files, so the claimed `.tasty` content is written to a
      // throwaway directory for the duration of the inspection.
      val directory = java.nio.file.Files.createTempDirectory("degustation").nn

      try
        val files = tasty.map: pair =>
          val target = directory.resolve(pair(0).text.s).nn
          java.nio.file.Files.createDirectories(target.getParent.nn)
          java.nio.file.Files.write(target, Array.unsafeJvm(pair(1)))
          Text(target.toString)

        val scalaAtoms =
          mitigate:
            case DegustationError(reason) =>
              DisciplineError(id, DisciplineError.Reason.Malformed(t"$reason"))

          . protect(Inspection.atomize(List.from(files), context.classpath))

        val atoms = scalaAtoms.map: atom =>
          val references = atom.references.map:
            case ScalaReference.Own(key)     => AtomReference.Own(key)
            case ScalaReference.Foreign(key) => AtomReference.Foreign(key)

          Atom
            ( atom.key,
              if atom.replaceable then AtomClass.Replaceable else AtomClass.Rigid,
              LiraHash(LiraHash.Domain.Atom(id), atom.encoding),
              references )

        Atomization.of(id, atoms)

      finally
        val paths = java.nio.file.Files.walk(directory).nn

        paths.sorted(java.util.Comparator.reverseOrder).nn.forEach: path =>
          java.nio.file.Files.deleteIfExists(path)

        paths.close()
