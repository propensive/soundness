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

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import reliquary.*
import rudiments.*

import errorDiagnostics.emptyDiagnostics

// The `webidl/1` discipline, adapted to reliquary's SPI: the Web IDL of a browser host
// contract, atomized per `webidl.md`. Its domain is the single realm `{host}` — a browser is a
// host, not a universe, and a `js`-universe library's carrier is `.d.ts` — so a release
// carrying it is a host contract (L135) and L127 rejects a library that declares it.
object WebIdlDiscipline extends Discipline:
  def id: Text = t"webidl/1"

  def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".idl")

  def domain: Discipline.Domain = Discipline.Domain.Realms(Set(t"host"))
  def keying: Discipline.Keying = Discipline.Keying.Declaration

  // Recompilation, for consumers type-checking against declarations generated from the IDL;
  // there is no linkage in a browser to protect, and behavior is never certified (§18).
  def guarantees(realm: Text): Set[Discipline.Guarantee] =
    Set(Discipline.Guarantee.Recompilation)

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises DisciplineError =

    val definitions = content.stdlib.flatMap: (path, data) =>
      val source = Text(String(Array.unsafeJvm(data), "UTF-8"))

      mitigate:
        case WebIdlError(reason) =>
          DisciplineError(id, DisciplineError.Reason.Malformed(t"${path.text}: $reason"))

      . protect(WebIdlParser.parse(source).stdlib)

    // Partial/mixin resolution spans the whole claimed set: a partial in one file completes an
    // interface in another, exactly as the platform's own IDL is distributed.
    Atomization.of(id, WebIdlAtomizer.atomize(List.from(definitions)))
