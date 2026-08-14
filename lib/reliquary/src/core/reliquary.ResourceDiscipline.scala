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
package reliquary

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

import Lira.Manifest.{Resource, ResourceMode}

// The normative `resource/1` discipline (§11.4): non-code content addressed by classpath-style
// name, whose *name* may be part of a module's contract even where its bytes are not. It is the
// one registered discipline whose claiming takes input beyond the tree itself — the manifest's
// `resource` records, an authorial claim like `owns` — which is unproblematic because
// atomization only ever runs where the manifest is in hand (§16, step 4).
//
// In the claiming order it follows every language discipline and precedes the `opaque/1`
// fallback, so an item under a scanned directory that a language discipline claims goes to that
// discipline, and only the remainder are atomless.
case class ResourceDiscipline(resources: List[Resource]) extends Discipline:
  def id: Text = t"resource/1"

  // Universal: resources are content, not a representation of one universe. Keys are paths, so
  // keying is by declaration in the only sense available. Rigid resource atoms certify presence
  // at both levels — a name that resolves keeps resolving — and nothing about content, since
  // resource bytes are behavior and no discipline certifies that (§18).
  def domain: Discipline.Domain = Discipline.Domain.Universal
  def keying: Discipline.Keying = Discipline.Keying.Declaration

  def guarantees(universe: Text): Set[Discipline.Guarantee] =
    Set(Discipline.Guarantee.Linkage, Discipline.Guarantee.Recompilation)

  private def named(mode: ResourceMode): List[TreePath] =
    List.from(resources.stdlib.filter(_.mode == mode).map(_.path))

  def exports: List[TreePath] = named(ResourceMode.Export)
  def tracked: List[TreePath] = named(ResourceMode.Track)
  def scanned: List[TreePath] = named(ResourceMode.Scan)

  // A scan claims every item whose path has the declared directory plus `/` as a prefix. The
  // directory itself is not an item, and a scanned directory may be empty in any or all
  // universes.
  def underScan(path: TreePath): Boolean =
    scanned.stdlib.exists: directory => path.text.s.startsWith(directory.text.s + "/")

  def claims(path: TreePath, data: Data): Boolean =
    exports.stdlib.exists(_.text == path.text)
    || tracked.stdlib.exists(_.text == path.text)
    || underScan(path)

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises Discipline.Error =

    val atoms = content.stdlib.flatMap: (path, data) =>
      if exports.stdlib.exists(_.text == path.text) then
        // The value hashes the path alone, so the atom asserts presence and not content: bytes
        // may differ per universe while L108 still requires the name in every one of them.
        scala.List(Atom(path.text, Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(id), path.bytes)))
      else if tracked.stdlib.exists(_.text == path.text) then
        // Content-hashed and replaceable: an edit is replaceable churn, a minor event that marks
        // consumers whose used-sets contain the atom as stale (§13.4). Resources create no
        // linkage, so replaceability soundness is trivial and the reference list is empty.
        scala.List(Atom(path.text, Atom.Class.Replaceable, Lira.Hash(Lira.Hash.Domain.Atom(id), data)))
      else scala.List()  // claimed by a scan: atomless

    Atomization.of(id, List.from(atoms))

object ResourceDiscipline:
  // L124: declarations must be well-formed — no path declared twice, and no `export` or `track`
  // path lying under a declared `scan` directory — so §11.2's partition has a single claimant by
  // construction.
  def check(resources: List[Resource]): Unit raises Lira.Error =
    val discipline = ResourceDiscipline(resources)
    val paths = resources.stdlib.map(_.path.text)

    paths.groupBy(identity).foreach: (path, group) =>
      if group.size > 1
      then abort(Lira.Error(Lira.Error.Reason.BadResource(t"$path is declared twice")))

    (discipline.exports.stdlib ++ discipline.tracked.stdlib).foreach: path =>
      if discipline.underScan(path)
      then abort(Lira.Error(Lira.Error.Reason.BadResource(t"${path.text} lies under a scan")))
