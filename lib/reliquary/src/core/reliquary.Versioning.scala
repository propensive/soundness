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
import gossamer.*
import contingency.*
import denominative.*
import revolution.*
import vacuous.*

import Lira.Error.Reason

// The decorative-version projection and the lineage-extension rules. Every consumer decision is
// made on hashes; the `major.minor.patch` number is a human-readable projection of the lineage
// structure — but under the assignment model it is *derived* from the algebra at publication,
// so a published release's version always agrees with its lineage.
object Versioning:

  def compatibility(grade: Grade): Compatibility = grade match
    case Grade.Patch => Compatibility.Internal
    case Grade.Minor => Compatibility.Additions
    case Grade.Major => Compatibility.Breaking

  // The version the algebra assigns to a successor of the given grade.
  def expected(previous: Semver, grade: Grade): Semver = previous.next(compatibility(grade))

  // LIRA versions are strictly numeric: no prerelease or build suffixes. Development state is
  // expressed by the *absence* of a version, not by a suffix.
  def numeric(version: Semver): Boolean = version.prerelease.nil && version.build.nil

  // L110: a lineage may be extended only by a patch (unchanged) or minor (appended snapshot)
  // successor; a major successor begins a fresh lineage, and only on explicit request.
  def extendLineage(lineage: List[Data], snapshot: Data, grade: Grade, forceMajor: Boolean = false)
  :   List[Data] raises Lira.Error =

    grade match
      case Grade.Patch => lineage
      case Grade.Minor => List.from(lineage.stdlib :+ snapshot)

      case Grade.Major =>
        if !forceMajor then abort(Lira.Error(Reason.UngradedSuccessor(t"the release")))
        List(snapshot)

  // The §12.4 comparison, as warn-only advisories: a declared version that is not numeric, or
  // that is not the projection of the grade from the previous published version.
  def advisories(declared: Semver, previous: Optional[Semver], grade: Grade): List[Lira.Advisory] =
    val numericAdvisory =
      if numeric(declared) then scala.Nil else scala.List(Lira.Advisory.NotNumeric(declared))

    val projection = previous match
      case previous: Semver =>
        val expectation = expected(previous, grade)

        if declared == expectation then scala.Nil
        else scala.List(Lira.Advisory.VersionMismatch(declared, expectation))

      case _ => scala.Nil

    List.from(numericAdvisory ++ projection)
