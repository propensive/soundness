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
import prepositional.*
import rudiments.*
import vacuous.*

object Discipline:
  // What a compiler-based discipline needs beyond the content itself: which section is being
  // atomized — its world and, where the release offers alternative dependency vectors (§9.5),
  // its integration — and the dependency classpath materialized from the buildpath (entries as
  // textual paths, so the core stays platform-light). The classpath is a property of the
  // integration, which is why atomization is per-section and not per-universe.
  case class Context
    ( world:       Text,
      integration: Optional[Text] = Unset,
      classpath:   List[Text]     = List() )

  // The worlds a discipline atomizes at all (§11.2, requirement 1): universes, and possibly the
  // `host` world (hosts.md). A universal discipline atomizes whatever world it is given — which
  // is `dts/1`'s case, and brings host contracts within its reach; a world-specific one names
  // its worlds, and the cross-section invariant (§9.6) is vacuous outside them. Claiming
  // nothing in a world outside the domain is not the same as claiming content atomless.
  enum Domain:
    case Universal
    case Worlds(names: Set[Text])

    def covers(world: Text): Boolean = this match
      case Universal     => true
      case Worlds(names) => names.stdlib.contains(world)

  // Whether an atom's key names the type that *declares* a member, or every type that *presents*
  // it after inheritance (§11.2, requirement 4). Declaration keying is sound exactly where every
  // reference a certified consumer can make resolves through the declaring type.
  enum Keying:
    case Declaration, Membership

  // What a discipline's rigid atoms certify (§11.2 requirement 7, §11.5). Behavior is not a
  // certifiable level and so has no case here: no hash scheme certifies it (§18).
  enum Guarantee:
    case Linkage, Recompilation

  // An ordered set of disciplines. Content is claimed by the first discipline whose `claims`
  // accepts it; anything left unclaimed falls to `opaque/1` (§11.3), so nothing in a LIRA file
  // is ever outside the compatibility algebra. A discipline out of its domain claims nothing,
  // ahead of any question of what it would have claimed.
  // The claiming order of §11.4 is structural rather than a convention the caller must remember:
  // language disciplines first, then `resource/1` over the manifest's claims, then the
  // `opaque/1` fallback. So an item under a scanned directory that a language discipline claims
  // goes to that discipline, and only the remainder are atomless.
  class Registry(disciplines: List[Discipline], resources: List[LiraManifest.Resource] = List()):
    // The language disciplines alone, without the two the registry supplies itself.
    def declared: List[Discipline] = disciplines

    def all: List[Discipline] =
      List.from(disciplines.stdlib :+ ResourceDiscipline(resources) :+ OpaqueDiscipline)

    def atomize(content: List[(TreePath, Data)], context: Context)
    :   List[Atomization] raises DisciplineError =

      var remaining = content.stdlib

      val results = all.stdlib.map: discipline =>
        val (claimed, rest) =
          if !discipline.domain.covers(context.world) then (scala.Nil, remaining)
          else remaining.partition: (path, data) => discipline.claims(path, data)

        remaining = rest
        (discipline, claimed)

      List.from:
        results.filter { (_, claimed) => !claimed.isEmpty }.map: (discipline, claimed) =>
          discipline.atomize(List.from(claimed), context)

// A named, versioned canonicalization procedure (§11): the single language-specific plug-in
// point of the format. Atomization must be a pure function of the content's semantic model —
// independent of file ordering, timestamps, tool versions and fresh names — and obey the
// folding principle (§10.3).
trait Discipline:
  def id: Text
  def claims(path: TreePath, data: Data): Boolean

  // The three declarative requirements of §11.2. `domain` scopes the cross-section invariant
  // (§9.6) and decides L127; `keying` and `guarantees` are contracts a discipline states and
  // this core takes on trust — neither is checkable by machine, and both change what a reader
  // may conclude from a grade (§12.4).
  def domain: Discipline.Domain
  def keying: Discipline.Keying
  def guarantees(universe: Text): Set[Discipline.Guarantee]

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises DisciplineError
