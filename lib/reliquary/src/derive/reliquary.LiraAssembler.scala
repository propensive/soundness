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

// The language-blind producer: given one body of content per universe — the first is the root —
// it atomizes every universe under the discipline registry, enforces the cross-universe API
// invariant (L108), computes minimal overlays, derivative hashes, the snapshot and lineage, and
// assembles the complete `.lira` file. Everything here is deterministic for fixed inputs.
object LiraAssembler:

  case class SectionInput(universe: Text, content: List[(TreePath, Data)])

  def assemble
    ( module:      Text,
      sections:    List[SectionInput],
      disciplines: Discipline.Registry,
      version:     Optional[Semver]              = Unset,
      lineage:     List[Data]                    = List(),
      toolchain:   List[LiraManifest.Tool]       = List(),
      owns:        List[Text]                    = List(),
      dependency:  List[LiraManifest.Dependency] = List(),
      delta:       Optional[LiraDelta]           = Unset,
      classpath:   Text => List[Text]            = { _ => List() },
      sign:        LiraManifest => LiraManifest  = identity(_) )
  :   Data raises LiraError raises DisciplineError =

    val inputs = sections.stdlib

    if inputs.isEmpty
    then abort(LiraError(Reason.InvalidManifest(t"a release needs at least one section")))

    def treeOf(input: SectionInput): LiraTree =
      LiraTree.of:
        input.content.map: pair => TreeEntry(pair(0), LiraHash(LiraHash.Domain.Blob, pair(1)))

    // Each universe's content is atomized independently; the atom sets must be identical, as
    // (discipline, key, class, value hash), for the release to present one API (L108).
    val atomized = inputs.map: input =>
      val context = Discipline.Context(input.universe, classpath(input.universe))
      (input.universe, disciplines.atomize(input.content, context))

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
          ( universe   = input.universe,
            tree       = LiraHash(LiraHash.Domain.Blob, tree.encode),
            delete     = delete,
            derivative = Derivative.hash(target, store) )

      (section, tree.encode)

    val deltaBlob = delta.let(_.encode)
    val snapshot = Snapshot(rootAtoms)
    val fullLineage = if lineage.stdlib.isEmpty then List(snapshot) else lineage
    Lineage.check(fullLineage, snapshot)

    val manifest =
      LiraManifest
        ( module     = module,
          version    = version,
          lineage    = fullLineage,
          toolchain  = toolchain,
          owns       = owns,
          api        = api,
          dependency = dependency,
          delta      = deltaBlob.let { data => LiraHash(LiraHash.Domain.Blob, data) },
          section    = List.from(builtSections.map(_(0))),
          payload    = LiraManifest.Payload(t"brotli", 0L, LiraHash(LiraHash.Domain.Blob,
              Array.freeze(Array[Byte](0)))) )

    val blobs = contentBlobs ++ builtSections.map(_(1)) ++ atomsBlobs ++ deltaBlob.option.toList

    Lira.assemble(sign(manifest), List.from(blobs))
