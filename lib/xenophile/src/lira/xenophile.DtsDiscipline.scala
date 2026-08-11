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
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

import fulminate.*

import errorDiagnostics.emptyDiagnostics

// The `dts/1` discipline: the declared TypeScript surface of a release, as atoms.
//
// Named for the carrier, not the language (§11.1). A `.d.ts` file is the declaration surface a
// TypeScript consumer compiles against, whatever produced the JavaScript beneath it — hand-written
// JavaScript, TypeScript, or a compiler targeting neither.
object DtsDiscipline extends Discipline:
  def id: Text = t"dts/1"

  // Universal, for want of a universe to name. The base schema's universes are `jvm`, `sjsir` and
  // `nir` (§9.4 reserves `js` for a future schema layer), and §11.3 already admits foreign
  // JavaScript content in any section under `opaque/1` — so there is nothing narrower to scope to
  // that would not simply exclude the discipline from every release that exists. Universality has
  // a second effect worth having: it brings `.d.ts` under the cross-section invariant (§9.6), and
  // a declared TypeScript surface that differed per universe would be a defect, not a feature.
  def domain: Discipline.Domain = Discipline.Domain.Universal

  // TypeScript references name the declaring interface, class or alias, and structural
  // assignability is checked against that declaration rather than resolved through a receiver at
  // a call site. Declaration keying is sound exactly where that holds (§11.2 requirement 4), and
  // it holds here because this discipline certifies recompilation only.
  def keying: Discipline.Keying = Discipline.Keying.Declaration

  // Recompilation alone. `.d.ts` declarations are erased before anything runs: there is no late
  // linking for them to protect, so certifying linkage would be a claim about a mechanism that
  // does not exist (§11.2 requirement 7).
  def guarantees(universe: Text): Set[Discipline.Guarantee] =
    Set(Discipline.Guarantee.Recompilation)

  def claims(path: TreePath, data: Data): Boolean = path.text.s.endsWith(".d.ts")

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises DisciplineError =

    val atoms = scala.collection.mutable.ListBuffer[Atom]()

    content.stdlib.foreach: (path, data) =>
      val source = Text(String(Array.unsafeJvm(data), "UTF-8"))

      val declarations =
        mitigate:
          case Typescript.Error(reason) =>
            DisciplineError(id, DisciplineError.Reason.Malformed(t"${path.text}: $reason"))

        . protect(Typescript.Parser.parse(source))

      // A declaration a consumer of the module cannot name is not part of its contract. In a
      // module that is the unexported declarations; in a global script there are none.
      declarations.stdlib.filter(_.exported).foreach: declaration =>
        atoms ++= DtsAtomizer.atomize(declaration).stdlib

    Atomization.of(id, List.from(atoms.toList))
