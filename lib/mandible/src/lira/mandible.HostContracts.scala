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
import fulminate.*
import gossamer.*
import reliquary.*
import revolution.Semver
import rudiments.*
import vacuous.*

import Lira.Error.Reason

// One harvested release of a host's surface: the vendor's name for it (which becomes the
// release's tag, LIRA §12.6) and its content tree.
case class HostRelease(tag: Text, content: List[(TreePath, Data)])

// Builds the release sequence of a host-contract module from harvested surfaces: each release
// is atomized under `jsig/1`, graded against its predecessor by the ordinary algebra, given
// the derived version §12.5 dictates, threaded onto the lineage — a major beginning a fresh
// one — and assembled as a complete, tagged `.lira` host contract.
//
// A major is a removal in the host's history (the JDK 9 and 11 removals are the canonical
// cases), and L110 requires the operator to sanction it: `allowMajor` is that sanction, per
// tag, so an unexpected removal fails loudly rather than silently fracturing the lineage.
object HostContracts:

  def assemble
    ( module:     Text,
      releases:   List[HostRelease],
      toolchain:  List[Lira.Manifest.Tool],
      allowMajor: Text -> Boolean            = { _ => false },
      sign:       Lira.Manifest -> Lira.Manifest = { manifest => manifest } )
    ( using Tactic[Lira.Error], Tactic[Discipline.Error] )
  :   List[(Text, Data)] =

    val registry = Discipline.Registry(List(JsigDiscipline))
    val context = Discipline.Context(t"host")
    val results = scala.collection.mutable.ListBuffer[(Text, Data)]()

    var previous: Optional[List[Atomization]] = Unset
    var lineage: List[Data] = List()
    var version: Semver = Semver(0, 1, 0)

    var todo = releases.stdlib

    while todo.nonEmpty do
      val release = todo.head
      todo = todo.tail
      val atomizations = registry.atomize(release.content, context)
      val snapshot = Snapshot(atomizations)

      previous.let: before =>
        Grade.between(before, atomizations) match
          case Grade.Patch =>
            version = Semver(version.major, version.minor, version.patch + 1)

          case Grade.Minor =>
            version = Semver(version.major, version.minor + 1, 0)
            lineage = (lineage.stdlib :+ snapshot).to(List)

          case Grade.Major =>
            if !allowMajor(release.tag)
            then abort(Lira.Error(Reason.UngradedSuccessor(release.tag)))

            // §12.5: in the 0 series the minor conventionally carries breaking steps.
            version =
              if version.major == 0 then Semver(0, version.minor + 1, 0)
              else Semver(version.major + 1, 0, 0)

            lineage = List(snapshot)

      . or:
          lineage = List(snapshot)

      val bytes =
        LiraAssembler.assemble
          ( module,
            List(LiraAssembler.SectionInput(t"host", release.content)),
            registry,
            version   = version,
            tag       = List(release.tag),
            lineage   = lineage,
            toolchain = toolchain,
            sign      = sign )

      results += ((release.tag, bytes))
      previous = atomizations

    results.toList.to(List)
