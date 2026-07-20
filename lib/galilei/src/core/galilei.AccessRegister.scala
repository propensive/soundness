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
package galilei

import proscenium.compat.*

import anticipation.*
import aperture.*
import gossamer.*

// The process-global register of open directory scopes, acquired when a directory is opened
// and released when its scope ends. Conflicts are detected on the real (symlink-resolved)
// path, over subtree overlap: any number of overlapping `Read` and `Write` opens may coexist,
// but an `Exclusive` open conflicts with any other open of an overlapping subtree. No type
// system can see that two independently-opened handles denote overlapping trees, so this is
// checked at runtime, at the `open` call — the one place a conflict can be acted upon.
//
// This arbitrates only between scopes within one JVM: it says nothing about other processes
// (OS advisory locks may complement it later), and file opens do not yet participate.
object AccessRegister:
  private case class Registration(real: Text, atoms: Set[Mode])

  private var registrations: List[Registration] = Nil

  // The filesystem root would defeat the `+ "/"` prefix test, so it is normalized to empty.
  private def normalize(real: Text): Text = if real == t"/" then t"" else real

  private def overlapping(left: Text, right: Text): Boolean =
    left == right || left.starts(t"$right/") || right.starts(t"$left/")

  def acquire(real: Text, atoms: Set[Mode]): Boolean = synchronized:
    val real2 = normalize(real)

    val conflict = registrations.stdlib.exists: registration =>
      overlapping(real2, registration.real)
        && (atoms.contains(Exclusive) || registration.atoms.contains(Exclusive))

    if conflict then false else
      registrations ::= Registration(real2, atoms)
      true

  def release(real: Text, atoms: Set[Mode]): Unit = synchronized:
    val real2 = normalize(real)

    def remove(list: List[Registration]): List[Registration] = list match
      case Nil                                                => Nil
      case head :: tail if head == Registration(real2, atoms) => tail
      case head :: tail                                       => head :: remove(tail)

    registrations = remove(registrations)
