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
package mandible

import anticipation.*
import contingency.*
import gossamer.*
import reliquary.*
import rudiments.*

// The `jsig/1` discipline (`jsig.md`): the declared signature surface of Java classfiles, as
// the discipline of host contracts carried as API stubs — `ct.sym`'s signature files,
// `android.jar`'s stub classes — and of any content whose contract is what can be *compiled*
// against rather than the linkage of shipped bytecode.
//
// It shares `classfile/1`'s canonical encoding (the full fold, since generic signatures are
// precisely the recompilation surface it certifies) and differs in claim: recompilation and
// presence, never linkage — LIRA §11.2 requirement 7 is why the two cannot be one discipline.
// The closure obligation is correspondingly softer: a supertype outside the claimed content is
// a boundary, not an error, since a contract that is not the JDK's cannot be faulted for
// `java.lang.Object`'s absence; producers harvest closures whole, and a supertype *within* the
// claimed content that fails to read still fails the atomization.
object JsigDiscipline extends Discipline:
  def id: Text = t"jsig/1"

  // Host contracts carried as signature stubs are the motivating case; the `jvm` inclusion
  // admits a library whose interface carrier genuinely is Java signatures.
  def domain: Discipline.Domain = Discipline.Domain.Realms(Set(t"jvm", t"host"))

  // A call site names the receiver, so a type's contract surface includes what it presents —
  // sound here as it is for `classfile/1`, and required for the same reason.
  def keying: Discipline.Keying = Discipline.Keying.Membership

  def guarantees(realm: Text): Set[Discipline.Guarantee] =
    Set(Discipline.Guarantee.Recompilation)

  // `.sig` is the JDK's own spelling for signature classfiles; the format is the classfile
  // format, without `Code`.
  def claims(path: TreePath, data: Data): Boolean =
    path.text.s.endsWith(".sig") || path.text.s.endsWith(".class")

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises Discipline.Error =

    val classes = content.stdlib.map: (path, data) =>
      // Same erasure; the parser reads the bytes and retains nothing of them.
      val surface = ClassSurface(data.asInstanceOf[scala.IArray[Byte]])
      surface.name -> surface

    . to(scala.collection.immutable.Map)

    val outcome = ClassfileAtomizer.atomize(Map.of(classes), context.classpath)

    // `outcome.unresolved` is deliberately not consulted: a supertype outside the claimed
    // content contributes nothing to presented sets (`jsig.md` §4), exactly as a metadata-less
    // supertype contributes nothing to `kotlin-metadata/1`.
    Atomization.of(id, outcome.atoms)
