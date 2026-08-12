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
import fulminate.*
import gossamer.*
import revolution.Semver
import rudiments.*
import vacuous.*

import Lira.Error.Reason
import errorDiagnostics.emptyDiagnostics

// The language-blind producer: given one body of content per section — the first is the root —
// it atomizes every section under the discipline registry, enforces the cross-section API
// invariant (L108), computes minimal overlays, derivative hashes, the snapshot and lineage, and
// assembles the complete `.lira` file. Everything here is deterministic for fixed inputs.
object LiraAssembler:

  // One cell of the (realm × integration) matrix (§9.5). `integration` is absent where the
  // release declares none, which is its single implicit integration. `requires` names the host
  // contracts this section's code assumes (hosts.md §6) — authorial, so it arrives as input
  // rather than being computed.
  case class SectionInput
    ( realm:       Text,
      content:     List[(TreePath, Data)],
      integration: Optional[Text] = Unset,
      requires:    List[Lira.Manifest.Requires] = List() )

  def assemble
    ( module:      Text,
      sections:    List[SectionInput],
      disciplines: Discipline.Registry,
      version:     Optional[Semver]              = Unset,
      tag:         List[Text]                    = List(),
      lineage:     List[Data]                    = List(),
      toolchain:   List[Lira.Manifest.Tool]       = List(),
      owns:        List[Text]                    = List(),
      profile:     List[Lira.Manifest.Profile]     = List(),
      integration: List[Lira.Manifest.Integration] = List(),
      resource:    List[Lira.Manifest.Resource]    = List(),
      dependency:  List[Lira.Manifest.Dependency]  = List(),
      delta:       Optional[Lira.Delta]            = Unset,
      profiles:    EcosystemProfile.Registry      = EcosystemProfile.Registry(List()),
      predecessor: Optional[EcosystemProfile.Evidence] = Unset,
      classpath:   SectionInput => List[Text]     = { _ => List() },
      report:      Text => Unit                   = { _ => () },
      sign:        Lira.Manifest => Lira.Manifest  = identity(_) )
  :   Data raises Lira.Error raises DisciplineError =

    val inputs = sections.stdlib

    if inputs.isEmpty
    then abort(Lira.Error(Reason.InvalidManifest(t"a release needs at least one section")))

    def treeOf(input: SectionInput): Lira.Tree =
      Lira.Tree.of:
        input.content.map: pair => TreeEntry(pair(0), Lira.Hash(Lira.Hash.Domain.Blob, pair(1)))

    // Each section's content is atomized independently; the atom sets must be identical, as
    // (discipline, key, class, value hash), for the release to present one API on every universe
    // and under every integration (L108). The classpath is a property of the section, since it
    // is exactly what distinguishes one integration from another.
    // L124 before anything else: an ill-formed set of claims makes the partition ambiguous, so
    // there is nothing sensible to atomize.
    ResourceDiscipline.check(resource)

    val registry = Discipline.Registry(disciplines.declared, resource)

    val atomized = inputs.map: input =>
      val context = Discipline.Context(input.realm, input.integration, classpath(input))
      (input.realm, registry.atomize(input.content, context))

    // The same per-section view a discipline is given, for the profile predicates below. Built
    // here so that a profile checking structural invariants over a universe's content (§11.6,
    // clause 2) sees exactly what the disciplines saw, including the integration's classpath.
    val profileSections = List.from:
      inputs.map: input =>
        EcosystemProfile.Section
          (input.realm, input.content, input.integration, classpath(input))

    // L125: an `export` or `track` declaration must be effective — a declared path that resolves
    // to no item in any section, or that some other discipline claims, is an assembly-time
    // error. A presence guarantee over nothing, or over content whose contract another
    // discipline already carries, is never what the author meant.
    val resourceDiscipline = ResourceDiscipline(resource)

    (resourceDiscipline.exports.stdlib ++ resourceDiscipline.tracked.stdlib).foreach: path =>
      val present = inputs.exists: input =>
        input.content.stdlib.exists: pair => pair(0).text == path.text

      if !present
      then abort(Lira.Error(Reason.IneffectiveResource(path.text)))

      val claimedByOther = disciplines.declared.stdlib.exists: discipline =>
        inputs.exists: input =>
          input.content.stdlib.exists: pair =>
            pair(0).text == path.text && discipline.claims(pair(0), pair(1))

      if claimedByOther
      then abort(Lira.Error(Reason.IneffectiveResource(path.text)))

    // L127: a declared discipline must atomize some universe this release carries. An
    // atomization of nothing is not a claim about anything. The rule quantifies over the
    // *declared* disciplines: `resource/1` and `opaque/1` are the registry's own and universal,
    // so the question never arises for them.
    val universes = inputs.map(_.realm).toSet

    registry.declared.stdlib.foreach: discipline =>
      if !universes.exists(discipline.domain.covers)
      then abort(Lira.Error(Reason.InapplicableDiscipline(discipline.id)))

    def summary(atomizations: List[Atomization])
    :   scala.collection.immutable.Set[(Text, Text, AtomClass, Text)] =

      atomizations.stdlib.flatMap: atomization =>
        atomization.atoms.stdlib.map: atom =>
          (atomization.discipline, atom.key, atom.atomClass, Lira.Hash.text(atom.valueHash))

      . toSet

    val rootAtoms = atomized.head(1)
    val rootSummary = summary(rootAtoms)

    atomized.drop(1).foreach: pair =>
      val difference = summary(pair(1)).diff(rootSummary) ++ rootSummary.diff(summary(pair(1)))

      difference.headOption match
        case scala.Some(sample) =>
          abort(Lira.Error(Reason.ApiDivergence(t"${pair(0)} differs at ${sample(1)}")))

        case scala.None => ()

    // Blobs: every content item, every tree, every metadata blob — deduplicated by the stream.
    val contentBlobs = inputs.flatMap: input => input.content.stdlib.map(_(1))

    val store = Blobstore:
      List.from(contentBlobs.map { data => Blob(Lira.Hash(Lira.Hash.Domain.Blob, data), data) })

    val atomsBlobs = rootAtoms.stdlib.map: atomization => AtomsBlob.encode(atomization)

    val api = List.from:
      rootAtoms.stdlib.zip(atomsBlobs).map: pair =>
        Lira.Manifest.Api(pair(0).discipline, Lira.Hash(Lira.Hash.Domain.Blob, pair(1)))

    val rootTree = treeOf(inputs.head)

    val builtSections = inputs.zipWithIndex.map: (input, index) =>
      val target = treeOf(input)

      val (tree, delete) =
        if index == 0 then (target, List[TreePath]()) else Overlay.diff(rootTree, target)

      val section =
        Section
          ( realm       = input.realm,
            integration = input.integration,
            tree        = Lira.Hash(Lira.Hash.Domain.Blob, tree.encode),
            delete      = delete,
            derivative  = Derivative.hash(target, store),
            requires    = input.requires )

      (section, tree.encode)

    val deltaBlob = delta.let(_.encode)
    val snapshot = Snapshot(rootAtoms)
    val fullLineage = if lineage.stdlib.isEmpty then List(snapshot) else lineage
    Lineage.check(fullLineage, snapshot)

    val manifest =
      Lira.Manifest
        ( module      = module,
          version     = version,
          tag         = tag,
          lineage     = fullLineage,
          toolchain   = toolchain,
          owns        = owns,
          resource    = resource,
          api         = api,
          profile     = profile,
          integration = integration,
          dependency  = dependency,
          delta       = deltaBlob.let { data => Lira.Hash(Lira.Hash.Domain.Blob, data) },
          section     = List.from(builtSections.map(_(0))),
          payload     = Lira.Manifest.Payload(t"brotli", 0L, Lira.Hash(Lira.Hash.Domain.Blob,
              Array.freeze(Array[Byte](0)))) )

    // The producer never emits a file a consumer would reject: L131/L133/L135 are decidable
    // from the manifest, so they are checked here rather than discovered at install time.
    Verification.integrations(manifest)
    Verification.hostShape(manifest)

    // L140: the assembler is about to sign every declared profile as a checked claim, so a
    // declared profile it cannot check is refused outright — an unverifiable claim is worse
    // than an absent one (§16). This holds with or without a predecessor: implementability is
    // not a property of the step.
    profile.stdlib.foreach: record =>
      if profiles(record.id).absent
      then abort(Lira.Error(Reason.UnimplementedClaim(record.id)))

    // L128/L130. Profile predicates are diachronic, so they can only run where the caller has the
    // predecessor's content in hand; with no predecessor there is no step to check, which is the
    // ordinary case for the first release of a lineage. The audit runs against the assembled
    // manifest because a profile may impose predicates over `toolchain` records (§13.3), and it
    // runs before signing, so nothing unverified is ever signed.
    predecessor.let: previous =>
      val evidence = EcosystemProfile.Evidence(profileSections, manifest)
      val audit = EcosystemProfile.audit(profiles, profile, previous, evidence)
      audit.advisories.stdlib.foreach(report(_))

    val blobs = contentBlobs ++ builtSections.map(_(1)) ++ atomsBlobs ++ deltaBlob.option.toList

    Lira.assemble(sign(manifest), List.from(blobs))
