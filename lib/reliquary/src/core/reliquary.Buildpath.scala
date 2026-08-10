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
import anticipation.*
import contingency.*
import gossamer.*
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

    // L142: a tag names exactly one release of its module, forever. A tag carried by any other
    // published release of the module is a reassignment; and a re-signed manifest for the
    // *same* release (the same implementation identity) may add tags but never drop one a
    // published manifest carries.
    val siblings = published.stdlib.filter(_.module == manifest.module)

    manifest.tag.stdlib.foreach: tag =>
      val elsewhere = siblings.exists: sibling =>
        sibling.tag.stdlib.contains(tag)
          && Blob.compare(sibling.payload.hash, manifest.payload.hash) != 0

      if elsewhere then abort(LiraError(Reason.TagReassigned(tag)))

    siblings.filter { sibling => Blob.compare(sibling.payload.hash, manifest.payload.hash) == 0 }
    . foreach: sibling =>
        sibling.tag.stdlib.foreach: tag =>
          if !manifest.tag.stdlib.contains(tag)
          then abort(LiraError(Reason.TagReassigned(tag)))

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
  // §13.3: each release serves one universe of the target — the primary, unless a dependency
  // record naming the release carries `serves` (§13.2), in which case the universe that record
  // names. Closure and compatibility quantify over the records applicable to the universe a
  // release serves, which is what makes a cross-universe dependency's own dependencies resolve
  // in *its* universe rather than the target's.
  def serving(primary: Text, module: Text): Text =
    releases.stdlib.flatMap: manifest =>
      manifest.dependency.stdlib.filter(_.module == module).flatMap(_.serves.option)

    . headOption.getOrElse(primary)

  // A pin is how a consumer states a preference the manifests cannot imply (§13.3): the named
  // release takes the named integration, the remaining releases their canonical choices.
  def resolve(universe: Text, pins: List[(Text, Text)] = List()): Assignment raises LiraError =
    val chosen = releases.stdlib.sortBy(_.module.s).map: manifest =>
      val pinned = pins.stdlib.find(_(0) == manifest.module).map(_(1))

      val options: scala.List[Optional[Text]] = pinned match
        case scala.Some(id) =>
          if !manifest.integration.stdlib.exists(_.id == id)
          then abort(LiraError(Reason.BadIntegration(t"the pin names undeclared $id")))
          scala.List(id)

        case _ => candidates(manifest)

      options.find(satisfies(serving(universe, manifest.module), manifest, _)) match
        case scala.Some(candidate) => (manifest.module, candidate)

        // Because the choices are independent, a failure is always one release's: there is no
        // combination to report, only the module none of whose integrations hold.
        case _ => abort(LiraError(Reason.NoAssignment(manifest.module)))

    Assignment(List.from(chosen))

  // §13.2 satisfaction, plus §13.4 spanning: the required snapshot must appear in the candidate's
  // lineage, or one of the snapshots the dependent has *proven* it spans must. A span is a
  // publisher's recorded proof that its used-set is contained in that release's atom set, so a
  // module that spans a dependency's major boundary resolves against either side of it without a
  // variant compilation — which is the cheaper answer §9.5 tells producers to prefer.
  //
  // Spans are taken on trust here, because they are not decidable from one manifest: proving one
  // requires atomizing the candidate's payload, so it is a publish-time check a registry makes
  // across two releases (§16), not something buildpath validation can redo from manifests alone.
  private def requirementMet(dependency: LiraManifest.Dependency, candidate: LiraManifest)
  :   Boolean =

    Lineage.contains(candidate.lineage, dependency.api)
    || dependency.spans.stdlib.exists(Lineage.contains(candidate.lineage, _))

  // Whether one release's dependencies hold under one choice of its integration: rules 4 and 5,
  // as a predicate rather than a diagnosis. `audit` reports which rule failed and why.
  private def satisfies(universe: Text, manifest: LiraManifest, integration: Optional[Text])
  :   Boolean =

    manifest.dependency.stdlib.forall: dependency =>
      if !dependency.applies(universe, integration) then true else
        apply(dependency.module) match
          case candidate: LiraManifest =>
            requirementMet(dependency, candidate)
            && dependency.build.let(Blob.compare(_, candidate.payload.hash) == 0).or(true)

          case _ => false

  // §13.3 structural rules, which no assignment can affect: at most one release per module
  // (L111) and pairwise-disjoint `owns` claims (L112). Host contracts are exempt from rules 2–3
  // (hosts.md §8): a contract describes an environment and contributes no namespace claims and
  // no resources, so one mistakenly placed among the releases must not clash with anything.
  private def structural(): Unit raises LiraError =
    val all = releases.stdlib

    all.groupBy(_.module).foreach: (module, group) =>
      if group.size > 1 then abort(LiraError(Reason.DuplicateModule(module)))

    val libraries = all.filter { manifest => !manifest.hostContract }

    val claims = libraries.flatMap: manifest =>
      manifest.owns.stdlib.map: namespace => (manifest.module, namespace)

    claims.zipWithIndex.foreach: (left, index) =>
      claims.drop(index + 1).foreach: right =>
        if left(0) != right(0) then
          val one = left(1).s
          val two = right(1).s

          if one == two || one.startsWith(two + ".") || two.startsWith(one + ".")
          then abort(LiraError(Reason.NamespaceClash(left(1))))

    // L126: `export` and `track` paths are pairwise disjoint across modules, so a classpath-style
    // reference resolves to exactly one module. `scan` directories are exempt — cross-module
    // aggregation under a shared directory is precisely their purpose.
    val named = libraries.flatMap: manifest =>
      manifest.resource.stdlib
        . filter(_.mode != LiraManifest.ResourceMode.Scan)
        . map: resource => (manifest.module, resource.path.text)

    named.groupBy(_(1)).foreach: (path, group) =>
      if group.map(_(0)).distinct.size > 1 then abort(LiraError(Reason.ResourceClash(path)))

  // Closure (L113) and satisfaction (L114) under one assignment, reporting the precise rule that
  // fails. `resolve` answers only whether *some* assignment works; this says why a given one does
  // not, which is what a diagnostic needs.
  private def audit(universe: Text, joins: List[Text], assignment: Assignment)
  :   List[LiraAdvisory] raises LiraError =

    val advisories = scala.collection.mutable.ArrayBuffer[LiraAdvisory]()

    releases.stdlib.foreach: manifest =>
      val served = serving(universe, manifest.module)

      manifest.dependency.stdlib.foreach: dependency =>
        if dependency.applies(served, assignment(manifest.module)) then
          val candidate = apply(dependency.module) match
            case candidate: LiraManifest => candidate

            case _ =>
              abort(LiraError(Reason.AbsentDependency(dependency.module)))

          // Rule 4's serves clause: a join edge to a universe the target does not include, or
          // to content the candidate does not offer, fails closure exactly as an absent module
          // does (§13.2, §13.3).
          dependency.serves.let: target =>
            if target != universe && !joins.stdlib.contains(target)
            then abort(LiraError(Reason.AbsentDependency(dependency.module)))

            if !candidate.section.stdlib.exists(_.realm == target)
            then abort(LiraError(Reason.AbsentDependency(dependency.module)))

          if !requirementMet(dependency, candidate)
          then abort(LiraError(Reason.Unsatisfiable(dependency.module)))

          dependency.build.let: build =>
            if Blob.compare(build, candidate.payload.hash) != 0
            then abort(LiraError(Reason.Unsatisfiable(dependency.module)))

          // The decorative version hint has no authority; disagreement is advisory.
          dependency.version.let: hint =>
            candidate.version.let: actual =>
              if hint != actual then advisories += LiraAdvisory.VersionMismatch(hint, actual)

    List.from(advisories)

  // The sections a target and an assignment select (§13.3, §13.5): per release, the section for
  // the universe that release serves and its assigned integration.
  private def selected(universe: Text, assignment: Assignment): scala.List[Section] =
    releases.stdlib.flatMap: manifest =>
      val served = serving(universe, manifest.module)

      manifest.section.stdlib.filter: section =>
        section.realm == served
        && section.integration.option == assignment(manifest.module).option

  // The host-contract modules the selected sections' `requires` records name — the modules rule
  // 7 needs contracts for, and what `HostPending` reports when validation runs without any.
  def requiredContracts(universe: Text, assignment: Assignment): List[Text] =
    List.from:
      selected(universe, assignment).flatMap: section =>
        section.requires.stdlib.map(_.module)

      . distinct

  // §13.3 rule 7 (L136, L137): every `requires` record of every selected section must be
  // satisfied by the given contracts, per hosts.md §7 — the required snapshot appears in the
  // named contract's lineage, or the requirement's used-set is contained in a contract's atom
  // set. The latter extends across contracts of *different* modules, which is sound because
  // atoms are content-addressed and module-blind; it needs the contract atom sets and the Uses
  // blobs, which live in payloads, so callers supply them as lookups and a lookup left unset
  // simply forgoes spanning. Aggregation (hosts.md §10) needs no extra pass: the contracts are
  // one release per module, so requirements are jointly satisfiable iff each is individually —
  // by lineage, both snapshots must be in the one given lineage; by spanning, a union of
  // used-sets is contained in a contract's atoms iff each member is.
  def hostRequirements
    ( universe:   Text,
      assignment: Assignment,
      contracts:  List[LiraManifest],
      atoms:      Text => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      used:       Data => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset } )
  :   Unit raises LiraError =

    contracts.stdlib.foreach: contract =>
      if !contract.hostContract
      then abort(LiraError(Reason.NotHostContract(contract.module)))

    selected(universe, assignment).foreach: section =>
      section.requires.stdlib.foreach: requirement =>
        // L137, from the other end: a requirement naming a module whose releases are ordinary
        // libraries is a category error, checkable wherever that module's manifest is in hand.
        apply(requirement.module).let: release =>
          if !release.hostContract
          then abort(LiraError(Reason.NotHostContract(requirement.module)))

        val named = contracts.stdlib.find(_.module == requirement.module)

        val byLineage = named.exists: contract =>
          Lineage.contains(contract.lineage, requirement.api)

        val bySpanning = requirement.uses.let: usesHash =>
          used(usesHash).let: usedSet =>
            contracts.stdlib.exists: contract =>
              atoms(contract.module).let(usedSet.subsetOf(_)).or(false)

          . or(false)

        . or(false)

        if !byLineage && !bySpanning
        then abort(LiraError(Reason.UnsatisfiedRequirement(requirement.module)))

  // §13.3 validity for one universe, with the assignment that establishes it. Diamond
  // dependencies resolve by construction: requirements on two snapshots of one module are
  // jointly satisfiable iff some lineage contains both.
  //
  // Where no release declares an integration the assignment is unique, so the search is skipped
  // entirely and the rules read exactly as they did before integrations existed — including the
  // diagnostics, which name the failing dependency rather than reporting that no assignment
  // exists.
  //
  // Rule 7 runs against the given host contracts; where none are given the rule is left
  // *pending*, not passed, and the `HostPending` advisory names the contracts still needed —
  // the mode report §13.3 requires.
  //
  // `joins` completes the target (§13.3): the universes that join the primary one, which rule
  // 4's serves clause admits join edges into. `pins` are the consumer's integration choices;
  // unpinned releases take their canonical ones. `profiles` supplies rule 6's implementations:
  // every profile declared by any release imposes its buildpath predicates over the whole path,
  // and only an implemented profile can be checked here — an unimplementable declared profile
  // is a registry's refusal at publish time (L140), not re-litigated on every resolution.
  def resolved
    ( universe:  Text,
      joins:     List[Text]         = List(),
      pins:      List[(Text, Text)] = List(),
      contracts: List[LiraManifest] = List(),
      atoms:     Text => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      used:      Data => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      profiles:  EcosystemProfile.Registry = EcosystemProfile.Registry(List()) )
  :   (Assignment, List[LiraAdvisory]) raises LiraError =

    structural()

    val assignment =
      if pins.stdlib.isEmpty && releases.stdlib.forall(_.integration.stdlib.isEmpty)
      then Assignment(List())
      else resolve(universe, pins)

    val advisories = audit(universe, joins, assignment)

    // Rule 6: profile coherence over the whole buildpath.
    releases.stdlib.flatMap(_.profile.stdlib.map(_.id)).distinct.foreach: id =>
      profiles(id).let: profile =>
        val details = profile.coherence(releases).stdlib

        if !details.isEmpty
        then abort(LiraError(Reason.ProfileViolated(id, Text(details.map(_.s).mkString("; ")))))

    val required = requiredContracts(universe, assignment)

    if contracts.stdlib.isEmpty then
      if required.stdlib.isEmpty then (assignment, advisories)
      else
        val pending = List(LiraAdvisory.HostPending(required))
        (assignment, List.from(advisories.stdlib ++ pending.stdlib))
    else
      hostRequirements(universe, assignment, contracts, atoms, used)
      (assignment, advisories)

  def validate
    ( universe:  Text,
      joins:     List[Text]         = List(),
      pins:      List[(Text, Text)] = List(),
      contracts: List[LiraManifest] = List(),
      atoms:     Text => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      used:      Data => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      profiles:  EcosystemProfile.Registry = EcosystemProfile.Registry(List()) )
  :   List[LiraAdvisory] raises LiraError =
    resolved(universe, joins, pins, contracts, atoms, used, profiles)(1)
