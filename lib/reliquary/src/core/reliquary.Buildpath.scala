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
import denominative.{nil, size}
import gossamer.*
import rudiments.*
import symbolism.*
import vacuous.*

import Lira.Error.Reason
import denominative.dysasymptotics.linearSize

object Buildpath:

  // The publication rules for one manifest against the set of already-published releases (the
  // local registry stand-in until the distribution network exists). A release that fails any of
  // these is a development release and must stay one.
  def publishable(manifest: Lira.Manifest, published: List[Lira.Manifest]): Unit raises Lira.Error =
    // L117: a published release carries a strictly numeric version.
    val version = manifest.version.or(abort(Lira.Error(Reason.VersionRequired)))
    if !Versioning.numeric(version) then abort(Lira.Error(Reason.VersionRequired))

    // L142: a tag names exactly one release of its module, forever. A tag carried by any other
    // published release of the module is a reassignment; and a re-signed manifest for the
    // *same* release (the same implementation identity) may add tags but never drop one a
    // published manifest carries.
    val siblings = published.filter(_.module == manifest.module)

    manifest.tag.each: tag =>
      val elsewhere = siblings.exists: sibling =>
        sibling.tag.has(tag) && Blob.compare(sibling.payload.hash, manifest.payload.hash) != 0

      if elsewhere then abort(Lira.Error(Reason.TagReassigned(tag)))

    siblings.filter { sibling => Blob.compare(sibling.payload.hash, manifest.payload.hash) == 0 }
    . each: sibling =>
        sibling.tag.each: tag =>
          if !manifest.tag.has(tag) then abort(Lira.Error(Reason.TagReassigned(tag)))

    manifest.dependency.each: dependency =>
      // L118: build pins are development-only; publication requires snapshot requirements. The
      // pin is bound to a typed local first: reading `Optional[Data]` — a union over a
      // capture-annotated type — directly inside a lambda whose enclosing call still has live
      // type variables crashes the compiler's implicit-scope collection.
      val build: Optional[Data] = dependency.build
      build.let: _ => abort(Lira.Error(Reason.BuildPinned(dependency.module)))

      // L119: every dependency snapshot must appear in the lineage of a published release.
      def matches(candidate: Lira.Manifest): Boolean =
        candidate.module == dependency.module && candidate.version.present &&
          Lineage.contains(candidate.lineage, dependency.api)

      val satisfied = published.exists(matches)

      if !satisfied then abort(Lira.Error(Reason.UnpublishedDependency(dependency.module)))

    // L120 (manifest-only part): for a stable series (major ≥ 1) the minor number is the count
    // of minor steps in the lineage — the rule is stated as a count, which is why the linear
    // `size` of the lineage is taken here rather than restructured away. The 0 series is exempt:
    // there, breaking changes bump the minor and begin fresh lineages, so the minor is not a
    // projection of lineage length.
    if version.major >= 1 && version.minor != manifest.lineage.size - 1
    then
      val expected = s"${version.major}.${manifest.lineage.size - 1}.${version.patch}"
      abort(Lira.Error(Reason.VersionProjection(Text(expected))))

// An assignment (§13.3): one integration per release, under which a buildpath's validity is
// decided. A release declaring no integrations maps to `Unset`, its single implicit one.
case class Assignment(choices: List[(Text, Optional[Text])]):
  def apply(module: Text): Optional[Text] =
    choices.seek(_(0) == module).let(_(1))

// A set of releases intended for joint use (§13). It is unordered — the coherence rules make
// ordering irrelevant — and every rule here is decidable from manifests alone, without reading
// any payload. Closure and satisfaction are evaluated per requested universe and per the
// integration the assignment chose, since a dependency may be scoped to either axis.
case class Buildpath(releases: List[Lira.Manifest]):

  def apply(module: Text): Optional[Lira.Manifest] = releases.seek(_.module == module)

  // The lira#1 reverse lookup: given the hash of a canonical derivative artifact (a classpath
  // JAR), find the release and the section declaring it — the section, not just the release,
  // because a derivative belongs to one (universe, integration) cell, so the hash identifies
  // which integration the artifact is (§13.6).
  def byDerivative(hash: Data): Optional[(Lira.Manifest, Section)] =
    val found = releases.flatMap: manifest =>
      manifest.section.sweep:
        case section if section.derivative.let(Blob.compare(_, hash) == 0).or(false) =>
          (manifest, section)

    found.prim

  // The integrations a release offers, in canonical order (§13.3): ascending `rank`, then
  // ascending `id`. A release declaring none offers exactly one, its implicit `Unset`. An
  // unranked integration sorts after every ranked one rather than at zero, so that leaving
  // `rank` off never silently promotes an alternative above the publisher's preferred build.
  // Named rather than written as an inline sort key: reading the `Optional` rank's default
  // inside a lambda still under its caller's live type variables crashes implicit-scope
  // collection, and an `Ordering` value is typed once, outside any such lambda.
  private given integrationOrder: Ordering[Lira.Manifest.Integration] =
    Ordering.by[Lira.Manifest.Integration, (Long, String)]: integration =>
      val rank: Long = integration.rank.or(Long.MaxValue)
      (rank, integration.id.s)

  def candidates(manifest: Lira.Manifest): List[Optional[Text]] =
    if manifest.integration.nil then List(Unset) else
      val ordered: List[Lira.Manifest.Integration] = manifest.integration.sort
      ordered.map: integration => integration.id

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
    releases.flatMap: manifest =>
      manifest.dependency.filter: dependency =>
        dependency.module == module && dependency.serves.present

    . prim.let(_.serves).or(primary)

  // A pin is how a consumer states a preference the manifests cannot imply (§13.3): the named
  // release takes the named integration, the remaining releases their canonical choices.
  def resolve(universe: Text, pins: List[(Text, Text)] = List()): Assignment raises Lira.Error =
    val chosen: List[(Text, Optional[Text])] = releases.order(_.module.s).map: manifest =>
      val pinned = pins.seek(_(0) == manifest.module).let(_(1))

      val options: List[Optional[Text]] = pinned match
        case id: Text =>
          if !manifest.integration.exists(_.id == id)
          then abort(Lira.Error(Reason.BadIntegration(t"the pin names undeclared $id")))
          List(id)

        case _ => candidates(manifest)

      // A candidate may itself be `Unset` (the implicit integration), so the first satisfying
      // one is taken by filtering and destructuring: an `Optional`-returning `seek` could not
      // tell "found `Unset`" from "found nothing".
      options.filter(satisfies(serving(universe, manifest.module), manifest, _)) match
        case candidate :: _ => (manifest.module, candidate)

        // Because the choices are independent, a failure is always one release's: there is no
        // combination to report, only the module none of whose integrations hold.
        case _ => abort(Lira.Error(Reason.NoAssignment(manifest.module)))

    Assignment(chosen)

  // §13.2 satisfaction, plus §13.4 spanning: the required snapshot must appear in the candidate's
  // lineage, or one of the snapshots the dependent has *proven* it spans must. A span is a
  // publisher's recorded proof that its used-set is contained in that release's atom set, so a
  // module that spans a dependency's major boundary resolves against either side of it without a
  // variant compilation — which is the cheaper answer §9.5 tells producers to prefer.
  //
  // Spans are taken on trust here, because they are not decidable from one manifest: proving one
  // requires atomizing the candidate's payload, so it is a publish-time check a registry makes
  // across two releases (§16), not something buildpath validation can redo from manifests alone.
  private def requirementMet(dependency: Lira.Manifest.Dependency, candidate: Lira.Manifest)
  :   Boolean =

    Lineage.contains(candidate.lineage, dependency.api)
    || dependency.spans.exists(Lineage.contains(candidate.lineage, _))

  // Whether one release's dependencies hold under one choice of its integration: rules 4 and 5,
  // as a predicate rather than a diagnosis. `audit` reports which rule failed and why.
  private def satisfies(universe: Text, manifest: Lira.Manifest, integration: Optional[Text])
  :   Boolean =

    manifest.dependency.all: dependency =>
      if !dependency.applies(universe, integration) then true else
        apply(dependency.module) match
          case candidate: Lira.Manifest =>
            // Bound to a typed local for the same reason as the build pin in `publishable`.
            val build: Optional[Data] = dependency.build

            requirementMet(dependency, candidate)
            && build.let(Blob.compare(_, candidate.payload.hash) == 0).or(true)

          case _ => false

  // §13.3 structural rules, which no assignment can affect: at most one release per module
  // (L111) and pairwise-disjoint `owns` claims (L112). Host contracts are exempt from rules 2–3
  // (hosts.md §8): a contract describes an environment and contributes no namespace claims and
  // no resources, so one mistakenly placed among the releases must not clash with anything.
  private def structural(): Unit raises Lira.Error =
    releases.group(_.module).each: (module, group) =>
      group match
        case _ :: _ :: _ => abort(Lira.Error(Reason.DuplicateModule(module)))
        case _           => ()

    val libraries = releases.filter { manifest => !manifest.hostContract }

    val claims = libraries.flatMap: manifest =>
      manifest.owns.map: namespace => (manifest.module, namespace)

    // The pairwise sweep as a recursion over suffixes: each claim is compared with every later
    // one, which is what the positional `drop(index + 1)` expressed before.
    def pairwise(remaining: List[(Text, Text)]): Unit = remaining match
      case left :: rest =>
        rest.each: right =>
          if left(0) != right(0) then
            val one = left(1).s
            val two = right(1).s

            if one == two || one.startsWith(two + ".") || two.startsWith(one + ".")
            then abort(Lira.Error(Reason.NamespaceClash(left(1))))

        pairwise(rest)

      case _ => ()

    pairwise(claims)

    // L126: `export` and `track` paths are pairwise disjoint across modules, so a classpath-style
    // reference resolves to exactly one module. `scan` directories are exempt — cross-module
    // aggregation under a shared directory is precisely their purpose.
    val named = libraries.flatMap: manifest =>
      manifest.resource
        . filter(_.mode != Lira.Manifest.ResourceMode.Scan)
        . map: resource => (manifest.module, resource.path.text)

    named.group(_(1)).each: (path, group) =>
      group.map(_(0)).distinct match
        case _ :: _ :: _ => abort(Lira.Error(Reason.ResourceClash(path)))
        case _           => ()

  // Closure (L113) and satisfaction (L114) under one assignment, reporting the precise rule that
  // fails. `resolve` answers only whether *some* assignment works; this says why a given one does
  // not, which is what a diagnostic needs.
  private def audit(universe: Text, joins: List[Text], assignment: Assignment)
  :   List[Lira.Advisory] raises Lira.Error =

    val advisories = scala.collection.mutable.ArrayBuffer[Lira.Advisory]()

    releases.each: manifest =>
      val served = serving(universe, manifest.module)

      manifest.dependency.each: dependency =>
        if dependency.applies(served, assignment(manifest.module)) then
          val candidate = apply(dependency.module) match
            case candidate: Lira.Manifest => candidate

            case _ =>
              abort(Lira.Error(Reason.AbsentDependency(dependency.module)))

          // Rule 4's serves clause: a join edge to a universe the target does not include, or
          // to content the candidate does not offer, fails closure exactly as an absent module
          // does (§13.2, §13.3).
          dependency.serves.let: target =>
            if target != universe && !joins.has(target)
            then abort(Lira.Error(Reason.AbsentDependency(dependency.module)))

            if !candidate.section.exists(_.realm == target)
            then abort(Lira.Error(Reason.AbsentDependency(dependency.module)))

          if !requirementMet(dependency, candidate)
          then abort(Lira.Error(Reason.Unsatisfiable(dependency.module)))

          dependency.build.let: build =>
            if Blob.compare(build, candidate.payload.hash) != 0
            then abort(Lira.Error(Reason.Unsatisfiable(dependency.module)))

          // The decorative version hint has no authority; disagreement is advisory.
          dependency.version.let: hint =>
            candidate.version.let: actual =>
              if hint != actual then advisories += Lira.Advisory.VersionMismatch(hint, actual)

    advisories.to(List)

  // The sections a target and an assignment select (§13.3, §13.5): per release, the section for
  // the universe that release serves and its assigned integration.
  private def selected(universe: Text, assignment: Assignment): List[Section] =
    releases.flatMap: manifest =>
      val served = serving(universe, manifest.module)

      manifest.section.filter: section =>
        section.realm == served
        && section.integration.option == assignment(manifest.module).option

  // The host-contract modules the selected sections' `requires` records name — the modules rule
  // 7 needs contracts for, and what `HostPending` reports when validation runs without any.
  def requiredContracts(universe: Text, assignment: Assignment): List[Text] =
    selected(universe, assignment).flatMap(_.requires.map(_.module)).distinct

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
      contracts:  List[Lira.Manifest],
      atoms:      Text => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      used:       Data => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset } )
  :   Unit raises Lira.Error =

    contracts.each: contract =>
      if !contract.hostContract then abort(Lira.Error(Reason.NotHostContract(contract.module)))

    selected(universe, assignment).each: section =>
      section.requires.each: requirement =>
        // L137, from the other end: a requirement naming a module whose releases are ordinary
        // libraries is a category error, checkable wherever that module's manifest is in hand.
        apply(requirement.module).let: release =>
          if !release.hostContract
          then abort(Lira.Error(Reason.NotHostContract(requirement.module)))

        val named = contracts.seek(_.module == requirement.module)

        val byLineage = named.lay(false): contract =>
          Lineage.contains(contract.lineage, requirement.api)

        val bySpanning = requirement.uses.let: usesHash =>
          used(usesHash).let: usedSet =>
            contracts.exists: contract => atoms(contract.module).let(usedSet.subsetOf(_)).or(false)

          . or(false)

        . or(false)

        if !byLineage && !bySpanning
        then abort(Lira.Error(Reason.UnsatisfiedRequirement(requirement.module)))

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
      contracts: List[Lira.Manifest] = List(),
      atoms:     Text => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      used:      Data => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      profiles:  EcosystemProfile.Registry = EcosystemProfile.Registry(List()) )
  :   (Assignment, List[Lira.Advisory]) raises Lira.Error =

    structural()

    val assignment =
      if pins.nil && releases.all(_.integration.nil)
      then Assignment(List())
      else resolve(universe, pins)

    val advisories = audit(universe, joins, assignment)

    // Rule 6: profile coherence over the whole buildpath.
    releases.flatMap(_.profile.map(_.id)).distinct.each: id =>
      profiles(id).let: profile =>
        val details = profile.coherence(releases)

        if !details.nil then abort(Lira.Error(Reason.ProfileViolated(id, details.join(t"; "))))

    val required = requiredContracts(universe, assignment)

    if contracts.nil then
      if required.nil then (assignment, advisories)
      else
        val pending = List(Lira.Advisory.HostPending(required))
        (assignment, advisories + pending)
    else
      hostRequirements(universe, assignment, contracts, atoms, used)
      (assignment, advisories)

  def validate
    ( universe:  Text,
      joins:     List[Text]         = List(),
      pins:      List[(Text, Text)] = List(),
      contracts: List[Lira.Manifest] = List(),
      atoms:     Text => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      used:      Data => Optional[scala.collection.immutable.Set[Text]] = { _ => Unset },
      profiles:  EcosystemProfile.Registry = EcosystemProfile.Registry(List()) )
  :   List[Lira.Advisory] raises Lira.Error =
    resolved(universe, joins, pins, contracts, atoms, used, profiles)(1)
