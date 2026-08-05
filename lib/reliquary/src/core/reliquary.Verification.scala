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
import gossamer.*
import rudiments.*
import vacuous.*

import LiraError.Reason

// Verification is re-execution of the construction, bottom-up (§16). `install` performs the
// language-blind steps every consumer runs: payload decompression and hashing (1), blob-stream
// integrity and reference resolution (2), tree and overlay rules (3), and the snapshot/lineage
// check (5). Re-atomization (4), lineage-step grading (6) and signatures (7) are layered on
// separately: the first two need disciplines and the predecessor, the last needs key material.
object Verification:

  // `materialized` is keyed by the whole section, not by universe alone: a release offering
  // alternative dependency vectors (§9.5) has one entry per (universe, integration) cell.
  case class Report
    ( blobstore:     Blobstore,
      atomizations:  List[Atomization],
      materialized:  List[(Section, LiraTree)],
      advisories:    List[LiraAdvisory] ):

    def tree(universe: Text, integration: Optional[Text]): Optional[LiraTree] =
      materialized.stdlib.find: pair =>
        pair(0).universe == universe && pair(0).integration.option == integration.option

      . map(_(1)).getOrElse(Unset)

  // L131/L133: the integration declarations must be well-formed — ids unique, every section's
  // integration declared, no two sections sharing a (universe, integration) key — and every
  // declared integration must be realized by at least one section. Decidable from the manifest
  // alone, so every consumer checks it, not just a registry.
  def integrations(manifest: LiraManifest): Unit raises LiraError =
    val declared = manifest.integration.stdlib.map(_.id)

    declared.groupBy(identity).foreach: (id, group) =>
      if group.size > 1
      then abort(LiraError(Reason.BadIntegration(t"the integration $id is declared twice")))

    manifest.section.stdlib.foreach: section =>
      section.integration.let: id =>
        if !declared.contains(id)
        then abort(LiraError(Reason.BadIntegration(t"the section names undeclared $id")))

      if section.integration.absent && !declared.isEmpty
      then abort(LiraError(Reason.BadIntegration(t"a section names no integration")))

    manifest.section.stdlib.groupBy(_.key).foreach: (key, group) =>
      if group.size > 1
      then abort(LiraError(Reason.BadIntegration(t"two sections share universe ${key(0)}")))

    declared.foreach: id =>
      val realized = manifest.section.stdlib.exists: section =>
        section.integration.let(_ == id).or(false)

      if !realized then abort(LiraError(Reason.UnrealizedIntegration(id)))

  def install(lira: Lira): Report raises LiraError =
    val manifest = lira.manifest
    integrations(manifest)
    ResourceDiscipline.check(manifest.resource)

    // Steps 1–2: decompress within the declared length, verify the payload hash, and re-derive
    // every blob identity while checking stream order (L102–L105).
    val stream =
      LiraPayload.decompress(lira.compressed, manifest.payload.length, manifest.payload.hash)

    val store = BlobStream.read(stream)
    val referenced = scala.collection.mutable.Set[Text]()

    def resolve(hash: Data): Data raises LiraError =
      referenced += LiraHash.text(hash)
      store.resolve(hash)

    // Step 4's input: the declared atom listings must at least resolve and parse; comparing
    // them against re-atomized content is the publish-time extension.
    val atomizations = List.from:
      manifest.api.stdlib.map: api => AtomsBlob.decode(resolve(api.atoms))

    manifest.delta.let: hash => LiraDelta.decode(resolve(hash))

    manifest.dependency.stdlib.foreach: dependency => dependency.uses.let(resolve(_))

    // Step 3: tree path rules (L106) on every section; every tree blob and content blob must
    // resolve (L104); overlays of known universes materialize against the root under the
    // minimality rules (L107). Unknown universes stay opaque (§9.4).
    val trees = manifest.section.stdlib.map: section =>
      (section, LiraTree.decode(resolve(section.tree)))

    for
      pair  <- trees
      entry <- pair(1).entries.stdlib
    do resolve(entry.blob)

    val materialized =
      if trees.isEmpty then scala.Nil
      else
        val (rootSection, rootTree) = trees.head
        val known = trees.tail.filter: pair => pair(0).known.present

        val results = known.map: pair =>
          (pair(0), Overlay.materialize(rootTree, pair(0).delete, pair(1)))

        (rootSection, rootTree) :: results.toList

    // Step 5: the snapshot recomputed from the atom listings must equal the last lineage entry.
    Lineage.check(manifest.lineage, Snapshot(atomizations))

    val unreferenced = store.unreferenced(Set.from(referenced))

    val advisories =
      if unreferenced.stdlib.isEmpty then List()
      else List(LiraAdvisory.UnreferencedBlobs(unreferenced))

    Report(store, atomizations, List.from(materialized), advisories)
