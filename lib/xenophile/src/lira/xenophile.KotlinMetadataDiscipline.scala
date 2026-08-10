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
package xenophile

import scala.collection.immutable.List as SList

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import reliquary.*
import rudiments.*

// The `kotlin-metadata/1` discipline, adapted to reliquary's SPI: the Kotlin declaration
// surface carried by `@Metadata` annotations on JVM classfiles, atomized per `kotlin.md`.
//
// Claiming order beside `classfile/1` is load-bearing (`kotlin.md` §3): both claim `.class`
// files, this discipline only those carrying the annotation, so a registry must list
// `kotlin-metadata/1` first or it is left nothing to claim. The claiming test itself is a
// constant-pool scan for the annotation's descriptor — cheap, and confirmed properly against
// the loaded class before any atom is emitted.
object KotlinMetadataDiscipline extends Discipline:
  def id: Text = t"kotlin-metadata/1"

  def claims(path: TreePath, data: Data): Boolean =
    path.text.s.endsWith(".class") && carries(data)

  // ISO-8859-1 maps bytes to chars one-to-one, so a string search over the classfile's bytes
  // finds the descriptor wherever the constant pool holds it.
  private def carries(data: Data): Boolean =
    String(Array.unsafeJvm(data), "ISO-8859-1").contains("Lkotlin/Metadata;")

  // `{jvm, host}`: the metadata rides in JVM classfiles, and the `host` inclusion admits
  // contracts carried as API-stub classfiles — the anticipated Android surface (hosts.md §3).
  def domain: Discipline.Domain = Discipline.Domain.Realms(Set(t"jvm", t"host"))

  // Membership, as `classfile.md` §6: a Kotlin call site resolves members through the receiver.
  def keying: Discipline.Keying = Discipline.Keying.Membership

  def guarantees(realm: Text): Set[Discipline.Guarantee] =
    Set(Discipline.Guarantee.Recompilation)

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises DisciplineError =

    val classes: SList[(Text, Data)] = content.stdlib.map: (path, data) =>
      val binary = Text(path.text.s.stripSuffix(".class").nn.replace("/", ".").nn)
      (binary, data)

    Atomization.of(id,
        KotlinMetadataAtomizer.atomize(classes, context.classpath.stdlib))
