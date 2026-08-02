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
import vacuous.*

import LiraError.Reason

object Buildpath:

  // The publication rules for one manifest against the set of already-published releases (the
  // local registry stand-in until the distribution network exists). A release that fails any of
  // these is a development release and must stay one.
  def publishable(manifest: LiraManifest, published: List[LiraManifest]): Unit raises LiraError =
    // L117: a published release carries a strictly numeric version.
    val version = manifest.version.or(abort(LiraError(Reason.VersionRequired)))
    if !Versioning.numeric(version) then abort(LiraError(Reason.VersionRequired))

    manifest.dependency.stdlib.foreach: dependency =>
      // L118: build pins are development-only; publication requires snapshot requirements.
      dependency.build.let: _ => abort(LiraError(Reason.BuildPinned(dependency.module)))

      // L119: every dependency snapshot must appear in the lineage of a published release.
      def matches(candidate: LiraManifest): Boolean =
        candidate.module == dependency.module && candidate.version.present &&
          Lineage.contains(candidate.lineage, dependency.api)

      val satisfied = published.stdlib.exists(matches)

      if !satisfied then abort(LiraError(Reason.UnpublishedDependency(dependency.module)))

    // L120 (manifest-only part): for a stable series (major ≥ 1) the minor number is the count
    // of minor steps in the lineage. The 0 series is exempt: there, breaking changes bump the
    // minor and begin fresh lineages, so the minor is not a projection of lineage length.
    if version.major >= 1 && version.minor != manifest.lineage.stdlib.size - 1
    then
      val expected = s"${version.major}.${manifest.lineage.stdlib.size - 1}.${version.patch}"
      abort(LiraError(Reason.VersionProjection(Text(expected))))

// A set of releases intended for joint use (§13). It is unordered — the coherence rules make
// ordering irrelevant — and every rule here is decidable from manifests alone, without reading
// any payload. Closure and satisfaction are evaluated per requested universe, since a
// dependency may be scoped to the universes whose implementations need it.
case class Buildpath(releases: List[LiraManifest]):

  def apply(module: Text): Optional[LiraManifest] =
    releases.stdlib.find(_.module == module).getOrElse(Unset)

  // The lira#1 reverse lookup: given the hash of a canonical derivative artifact (a classpath
  // JAR), find the release one of whose sections declares it.
  def byDerivative(hash: Data): Optional[LiraManifest] =
    def declares(manifest: LiraManifest): Boolean =
      manifest.section.stdlib.exists: section =>
        section.derivative.let { declared => Blob.compare(declared, hash) == 0 }.or(false)

    releases.stdlib.find(declares).getOrElse(Unset)

  // §13.3 validity for one universe. Diamond dependencies resolve by construction: requirements
  // on two snapshots of one module are jointly satisfiable iff some lineage contains both.
  def validate(universe: Text): List[LiraAdvisory] raises LiraError =
    val all = releases.stdlib

    // L111: at most one release per module.
    all.groupBy(_.module).foreach: (module, group) =>
      if group.size > 1 then abort(LiraError(Reason.DuplicateModule(module)))

    // L112: `owns` claims pairwise disjoint; a namespace and any dotted extension of it clash.
    val claims = all.flatMap: manifest =>
      manifest.owns.stdlib.map: namespace => (manifest.module, namespace)

    claims.zipWithIndex.foreach: (left, index) =>
      claims.drop(index + 1).foreach: right =>
        if left(0) != right(0) then
          val one = left(1).s
          val two = right(1).s

          if one == two || one.startsWith(two + ".") || two.startsWith(one + ".")
          then abort(LiraError(Reason.NamespaceClash(left(1))))

    val advisories = scala.collection.mutable.ArrayBuffer[LiraAdvisory]()

    all.foreach: manifest =>
      manifest.dependency.stdlib.foreach: dependency =>
        val applies =
          dependency.universe.stdlib.isEmpty || dependency.universe.stdlib.contains(universe)

        if applies then
          // L113: closure — the dependency must be present.
          val candidate = apply(dependency.module) match
            case candidate: LiraManifest => candidate

            case _ =>
              abort(LiraError(Reason.AbsentDependency(dependency.module)))

          // L114: satisfaction — the required snapshot must appear in the candidate's lineage,
          // and a development-time build pin must match the candidate's implementation identity.
          if !Lineage.contains(candidate.lineage, dependency.api)
          then abort(LiraError(Reason.Unsatisfiable(dependency.module)))

          dependency.build.let: build =>
            if Blob.compare(build, candidate.payload.hash) != 0
            then abort(LiraError(Reason.Unsatisfiable(dependency.module)))

          // The decorative version hint has no authority; disagreement is advisory.
          dependency.version.let: hint =>
            candidate.version.let: actual =>
              if hint != actual then advisories += LiraAdvisory.VersionMismatch(hint, actual)

    List.from(advisories)
