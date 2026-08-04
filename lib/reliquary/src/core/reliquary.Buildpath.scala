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

// An assignment (§13.3): one integration per release, under which a buildpath's validity is
// decided. A release declaring no integrations maps to `Unset`, its single implicit one.
case class Assignment(choices: List[(Text, Optional[Text])]):
  def apply(module: Text): Optional[Text] =
    choices.stdlib.find(_(0) == module).map(_(1)).getOrElse(Unset)

// A set of releases intended for joint use (§13). It is unordered — the coherence rules make
// ordering irrelevant — and every rule here is decidable from manifests alone, without reading
// any payload. Closure and satisfaction are evaluated per requested universe and per the
// integration the assignment chose, since a dependency may be scoped to either axis.
case class Buildpath(releases: List[LiraManifest]):

  def apply(module: Text): Optional[LiraManifest] =
    releases.stdlib.find(_.module == module).getOrElse(Unset)

  // The lira#1 reverse lookup: given the hash of a canonical derivative artifact (a classpath
  // JAR), find the release and the section declaring it — the section, not just the release,
  // because a derivative belongs to one (universe, integration) cell, so the hash identifies
  // which integration the artifact is (§13.6).
  def byDerivative(hash: Data): Optional[(LiraManifest, Section)] =
    val found = releases.stdlib.flatMap: manifest =>
      manifest.section.stdlib.collect:
        case section if section.derivative.let(Blob.compare(_, hash) == 0).or(false) =>
          (manifest, section)

    found.headOption.getOrElse(Unset)

  // The integrations a release offers, in canonical order (§13.3): ascending `rank`, then
  // ascending `id`. A release declaring none offers exactly one, its implicit `Unset`. An
  // unranked integration sorts after every ranked one rather than at zero, so that leaving
  // `rank` off never silently promotes an alternative above the publisher's preferred build.
  def candidates(manifest: LiraManifest): scala.List[Optional[Text]] =
    if manifest.integration.stdlib.isEmpty then scala.List(Unset) else
      manifest.integration.stdlib.sortBy: integration =>
        (integration.rank.or(Long.MaxValue), integration.id.s)

      . map { integration => integration.id }.toList

  // §13.3: valid for a universe iff *some* assignment makes it so.
  //
  // The search collapses. A buildpath is a *fixed* set of releases, and every rule an assignment
  // can affect — closure (4) and satisfaction (5) — is a property of one release together with
  // its own choice: which release provides a module is decided by the buildpath, never by an
  // integration. So the choices are independent, and the canonical assignment is found by taking,
  // for each release, the first of its candidates whose own dependencies hold. There is no
  // backtracking and no combinatorial blow-up, and the result is canonical by construction
  // because `candidates` is already in (rank, id) order.
  //
  // Coupling would appear only in a resolver that also chose *which* releases to include, since
  // an integration can then pull a module in; that is dependency resolution proper, and it is
  // outside §13.3, which audits a buildpath it is handed.
  def resolve(universe: Text): Assignment raises LiraError =
    val chosen = releases.stdlib.sortBy(_.module.s).map: manifest =>
      candidates(manifest).find(satisfies(universe, manifest, _)) match
        case scala.Some(candidate) => (manifest.module, candidate)
        case _                     => abort(LiraError(Reason.NoAssignment))

    Assignment(List.from(chosen))

  // Whether one release's dependencies hold under one choice of its integration: rules 4 and 5,
  // as a predicate rather than a diagnosis. `audit` reports which rule failed and why.
  private def satisfies(universe: Text, manifest: LiraManifest, integration: Optional[Text])
  :   Boolean =

    manifest.dependency.stdlib.forall: dependency =>
      if !dependency.applies(universe, integration) then true else
        apply(dependency.module) match
          case candidate: LiraManifest =>
            Lineage.contains(candidate.lineage, dependency.api)
            && dependency.build.let(Blob.compare(_, candidate.payload.hash) == 0).or(true)

          case _ => false

  // §13.3 structural rules, which no assignment can affect: at most one release per module
  // (L111) and pairwise-disjoint `owns` claims (L112).
  private def structural(): Unit raises LiraError =
    val all = releases.stdlib

    all.groupBy(_.module).foreach: (module, group) =>
      if group.size > 1 then abort(LiraError(Reason.DuplicateModule(module)))

    val claims = all.flatMap: manifest =>
      manifest.owns.stdlib.map: namespace => (manifest.module, namespace)

    claims.zipWithIndex.foreach: (left, index) =>
      claims.drop(index + 1).foreach: right =>
        if left(0) != right(0) then
          val one = left(1).s
          val two = right(1).s

          if one == two || one.startsWith(two + ".") || two.startsWith(one + ".")
          then abort(LiraError(Reason.NamespaceClash(left(1))))

  // Closure (L113) and satisfaction (L114) under one assignment, reporting the precise rule that
  // fails. `resolve` answers only whether *some* assignment works; this says why a given one does
  // not, which is what a diagnostic needs.
  private def audit(universe: Text, assignment: Assignment)
  :   List[LiraAdvisory] raises LiraError =

    val advisories = scala.collection.mutable.ArrayBuffer[LiraAdvisory]()

    releases.stdlib.foreach: manifest =>
      manifest.dependency.stdlib.foreach: dependency =>
        if dependency.applies(universe, assignment(manifest.module)) then
          val candidate = apply(dependency.module) match
            case candidate: LiraManifest => candidate

            case _ =>
              abort(LiraError(Reason.AbsentDependency(dependency.module)))

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

  // §13.3 validity for one universe, with the assignment that establishes it. Diamond
  // dependencies resolve by construction: requirements on two snapshots of one module are
  // jointly satisfiable iff some lineage contains both.
  //
  // Where no release declares an integration the assignment is unique, so the search is skipped
  // entirely and the rules read exactly as they did before integrations existed — including the
  // diagnostics, which name the failing dependency rather than reporting that no assignment
  // exists.
  def resolved(universe: Text): (Assignment, List[LiraAdvisory]) raises LiraError =
    structural()

    val assignment =
      if releases.stdlib.forall(_.integration.stdlib.isEmpty) then Assignment(List())
      else resolve(universe)

    (assignment, audit(universe, assignment))

  def validate(universe: Text): List[LiraAdvisory] raises LiraError = resolved(universe)(1)
