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

import LiraError.Reason
import errorDiagnostics.emptyDiagnostics

// The language-blind producer: given one body of content per section — the first is the root —
// it atomizes every section under the discipline registry, enforces the cross-section API
// invariant (L108), computes minimal overlays, derivative hashes, the snapshot and lineage, and
// assembles the complete `.lira` file. Everything here is deterministic for fixed inputs.
object LiraAssembler:

  // One cell of the (universe × integration) matrix (§9.5). `integration` is absent where the
  // release declares none, which is its single implicit integration.
  case class SectionInput
    ( universe:    Text,
      content:     List[(TreePath, Data)],
      integration: Optional[Text] = Unset )

  def assemble
    ( module:      Text,
      sections:    List[SectionInput],
      disciplines: Discipline.Registry,
      version:     Optional[Semver]              = Unset,
      lineage:     List[Data]                    = List(),
      toolchain:   List[LiraManifest.Tool]       = List(),
      owns:        List[Text]                    = List(),
      profile:     List[LiraManifest.Profile]     = List(),
      integration: List[LiraManifest.Integration] = List(),
      resource:    List[LiraManifest.Resource]    = List(),
      dependency:  List[LiraManifest.Dependency]  = List(),
      delta:       Optional[LiraDelta]            = Unset,
      classpath:   SectionInput => List[Text]     = { _ => List() },
      sign:        LiraManifest => LiraManifest  = identity(_) )
  :   Data raises LiraError raises DisciplineError =

    val inputs = sections.stdlib

    if inputs.isEmpty
    then abort(LiraError(Reason.InvalidManifest(t"a release needs at least one section")))

    def treeOf(input: SectionInput): LiraTree =
      LiraTree.of:
        input.content.map: pair => TreeEntry(pair(0), LiraHash(LiraHash.Domain.Blob, pair(1)))

    // Each section's content is atomized independently; the atom sets must be identical, as
    // (discipline, key, class, value hash), for the release to present one API on every universe
    // and under every integration (L108). The classpath is a property of the section, since it
    // is exactly what distinguishes one integration from another.
    // L124 before anything else: an ill-formed set of claims makes the partition ambiguous, so
    // there is nothing sensible to atomize.
    ResourceDiscipline.check(resource)

    val registry = Discipline.Registry(disciplines.declared, resource)

    val atomized = inputs.map: input =>
      val context = Discipline.Context(input.universe, input.integration, classpath(input))
      (input.universe, registry.atomize(input.content, context))

    // L125: an `export` or `track` declaration must be effective — a declared path that resolves
    // to no item in any section, or that some other discipline claims, is an assembly-time
    // error. A presence guarantee over nothing, or over content whose contract another
    // discipline already carries, is never what the author meant.
    val resourceDiscipline = ResourceDiscipline(resource)

    (resourceDiscipline.exports.stdlib ++ resourceDiscipline.tracked.stdlib).foreach: path =>
      val present = inputs.exists: input =>
        input.content.stdlib.exists: pair => pair(0).text == path.text

      if !present
      then abort(LiraError(Reason.IneffectiveResource(path.text)))

      val claimedByOther = disciplines.declared.stdlib.exists: discipline =>
        inputs.exists: input =>
          input.content.stdlib.exists: pair =>
            pair(0).text == path.text && discipline.claims(pair(0), pair(1))

      if claimedByOther
      then abort(LiraError(Reason.IneffectiveResource(path.text)))

    // L127: a declared discipline must atomize some universe this release carries. An
    // atomization of nothing is not a claim about anything.
    val universes = inputs.map(_.universe).toSet

    registry.all.stdlib.foreach: discipline =>
      if !universes.exists(discipline.domain.covers)
      then abort(LiraError(Reason.InapplicableDiscipline(discipline.id)))

    def summary(atomizations: List[Atomization])
    :   scala.collection.immutable.Set[(Text, Text, AtomClass, Text)] =

      atomizations.stdlib.flatMap: atomization =>
        atomization.atoms.stdlib.map: atom =>
          (atomization.discipline, atom.key, atom.atomClass, LiraHash.text(atom.valueHash))

      . toSet

    val rootAtoms = atomized.head(1)
    val rootSummary = summary(rootAtoms)

    atomized.drop(1).foreach: pair =>
      val difference = summary(pair(1)).diff(rootSummary) ++ rootSummary.diff(summary(pair(1)))

      difference.headOption match
        case scala.Some(sample) =>
          abort(LiraError(Reason.ApiDivergence(t"${pair(0)} differs at ${sample(1)}")))

        case scala.None => ()

    // Blobs: every content item, every tree, every metadata blob — deduplicated by the stream.
    val contentBlobs = inputs.flatMap: input => input.content.stdlib.map(_(1))

    val store = Blobstore:
      List.from(contentBlobs.map { data => Blob(LiraHash(LiraHash.Domain.Blob, data), data) })

    val atomsBlobs = rootAtoms.stdlib.map: atomization => AtomsBlob.encode(atomization)

    val api = List.from:
      rootAtoms.stdlib.zip(atomsBlobs).map: pair =>
        LiraManifest.Api(pair(0).discipline, LiraHash(LiraHash.Domain.Blob, pair(1)))

    val rootTree = treeOf(inputs.head)

    val builtSections = inputs.zipWithIndex.map: (input, index) =>
      val target = treeOf(input)

      val (tree, delete) =
        if index == 0 then (target, List[TreePath]()) else Overlay.diff(rootTree, target)

      val section =
        Section
          ( universe    = input.universe,
            integration = input.integration,
            tree        = LiraHash(LiraHash.Domain.Blob, tree.encode),
            delete      = delete,
            derivative  = Derivative.hash(target, store) )

      (section, tree.encode)

    val deltaBlob = delta.let(_.encode)
    val snapshot = Snapshot(rootAtoms)
    val fullLineage = if lineage.stdlib.isEmpty then List(snapshot) else lineage
    Lineage.check(fullLineage, snapshot)

    val manifest =
      LiraManifest
        ( module      = module,
          version     = version,
          lineage     = fullLineage,
          toolchain   = toolchain,
          owns        = owns,
          resource    = resource,
          api         = api,
          profile     = profile,
          integration = integration,
          dependency  = dependency,
          delta       = deltaBlob.let { data => LiraHash(LiraHash.Domain.Blob, data) },
          section     = List.from(builtSections.map(_(0))),
          payload     = LiraManifest.Payload(t"brotli", 0L, LiraHash(LiraHash.Domain.Blob,
              Array.freeze(Array[Byte](0)))) )

    // The producer never emits a file a consumer would reject: L131/L133 are decidable from the
    // manifest, so they are checked here rather than discovered at install time.
    Verification.integrations(manifest)

    val blobs = contentBlobs ++ builtSections.map(_(1)) ++ atomsBlobs ++ deltaBlob.option.toList

    Lira.assemble(sign(manifest), List.from(blobs))
