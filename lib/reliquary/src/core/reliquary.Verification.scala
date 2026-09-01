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
      materialized:  List[(Section, Lira.Tree)],
      advisories:    List[Lira.Advisory] ):

    def tree(universe: Text, integration: Optional[Text]): Optional[Lira.Tree] =
      materialized.seek: pair =>
        pair(0).realm == universe && pair(0).integration.option == integration.option

      . let(_(1))

  // L131/L133: the integration declarations must be well-formed — ids unique, every section's
  // integration declared, no two sections sharing a (universe, integration) key — and every
  // declared integration must be realized by at least one section. Decidable from the manifest
  // alone, so every consumer checks it, not just a registry.
  def integrations(manifest: Lira.Manifest): Unit raises Lira.Error =
    val declared = manifest.integration.map(_.id)

    declared.group: id => id
    . each: (id, group) =>
        if group.size > 1
        then abort(Lira.Error(Reason.BadIntegration(t"the integration $id is declared twice")))

    manifest.section.each: section =>
      // Bound to a typed local before it is read: an `Optional` field read inside a lambda whose
      // caller still has live type variables crashes the compiler's implicit-scope collection.
      val integration: Optional[Text] = section.integration

      integration.let: id =>
        if !declared.has(id)
        then abort(Lira.Error(Reason.BadIntegration(t"the section names undeclared $id")))

      if integration.absent && !declared.nil
      then abort(Lira.Error(Reason.BadIntegration(t"a section names no integration")))

    manifest.section.group(_.key).each: (key, group) =>
      if group.size > 1
      then abort(Lira.Error(Reason.BadIntegration(t"two sections share universe ${key(0)}")))

    declared.each: id =>
      val realized = manifest.section.exists(_.integration.let(_ == id).or(false))

      if !realized then abort(Lira.Error(Reason.UnrealizedIntegration(id)))

  // L135 (§9.4, hosts.md §4): a release carrying a `host` section is a host contract, and its
  // shape is fixed — exactly that one section, no integrations, no dependencies, and no
  // `requires` records on the section. The exclusions are not arbitrary: an integration is an
  // alternative dependency vector and a contract has no dependencies to vary, and a contract
  // requiring a host would make satisfaction recursive. Decidable from the manifest alone.
  def hostShape(manifest: Lira.Manifest): Unit raises Lira.Error =
    if manifest.hostContract then
      def bad(detail: Text): Nothing = abort(Lira.Error(Reason.BadHostContract(detail)))
      if manifest.section.size != 1 then bad(t"it carries more than one section")
      if !manifest.integration.nil then bad(t"it declares integrations")
      if !manifest.dependency.nil then bad(t"it declares dependencies")

      manifest.section.each: section =>
        if !section.requires.nil then bad(t"its section carries requirements")

  def install(lira: Lira): Report raises Lira.Error =
    val manifest = lira.manifest
    integrations(manifest)
    hostShape(manifest)
    ResourceDiscipline.check(manifest.resource)

    // Steps 1–2: decompress within the declared length, verify the payload hash, and re-derive
    // every blob identity while checking stream order (L102–L105).
    val stream =
      Lira.Payload.decompress(lira.compressed, manifest.payload.length, manifest.payload.hash)

    val store = BlobStream.read(stream)
    val referenced = scala.collection.mutable.Set[Text]()

    def resolve(hash: Data): Data raises Lira.Error =
      referenced += Lira.Hash.text(hash)
      store.resolve(hash)

    // Step 4's input: the declared atom listings must at least resolve and parse; comparing
    // them against re-atomized content is the publish-time extension.
    val atomizations = manifest.api.map: api => AtomsBlob.decode(resolve(api.atoms))

    manifest.delta.let: hash => Lira.Delta.decode(resolve(hash))

    manifest.dependency.each: dependency =>
      // Bound to a typed local before the `Optional` is read: `Data` is capture-annotated, and
      // reading such a union inside a lambda whose caller still has live type variables crashes
      // the compiler's implicit-scope collection.
      val uses: Optional[Data] = dependency.uses
      uses.let(resolve(_))

    // Step 3: tree path rules (L106) on every section; every tree blob and content blob must
    // resolve (L104); overlays of known universes materialize against the root under the
    // minimality rules (L107). Unknown universes stay opaque (§9.4).
    val trees = manifest.section.map: section => (section, Lira.Tree.decode(resolve(section.tree)))

    trees.each: pair => pair(1).entries.each: entry => resolve(entry.blob)

    val materialized: List[(Section, Lira.Tree)] = trees match
      case (rootSection, rootTree) :: rest =>
        val known = rest.filter: pair => pair(0).known.present

        val results = known.map: pair =>
          (pair(0), Overlay.materialize(rootTree, pair(0).delete, pair(1)))

        (rootSection, rootTree) :: results

      case _ => Nil

    // Step 5: the snapshot recomputed from the atom listings must equal the last lineage entry.
    Lineage.check(manifest.lineage, Snapshot(atomizations))

    val unreferenced = store.unreferenced(referenced.to(Set))

    val advisories =
      if unreferenced.nil then List() else List(Lira.Advisory.UnreferencedBlobs(unreferenced))

    Report(store, atomizations, materialized, advisories)

  // Step 4 (publish-time): re-atomization of every materialized section under the *declared*
  // disciplines, in the manifest's `api`-record order — so the claiming order is the declared
  // one (L134), never an accident of the caller's list. A declared discipline with no
  // implementation fails the release (L140): an unverifiable claim is worse than an absent one,
  // because consumers cannot tell the two apart from the manifest. The recomputed atomization
  // must equal the declared listings on the root section (L141) and be identical across every
  // other section (L108), and no declared discipline may be inapplicable (L127).
  def reatomize
    ( manifest:        Lira.Manifest,
      report:          Report,
      implementations: List[Discipline],
      classpath:       (Text, Optional[Text]) => List[Text] = { (_, _) => List() } )
  :   Unit raises Lira.Error raises Discipline.Error =

    val resourceId = ResourceDiscipline(manifest.resource).id
    val declaredIds = manifest.api.map(_.discipline)

    val languages = declaredIds.filter: id => id != resourceId && id != OpaqueDiscipline.id

    val resolved = languages.map: id =>
      implementations.seek(_.id == id).or(abort(Lira.Error(Reason.UnimplementedClaim(id))))

    val registry = Discipline.Registry(resolved, manifest.resource)
    val universes = manifest.section.map(_.realm).to[Set]

    resolved.foreach: discipline =>
      if !universes.exists(discipline.domain.covers)
      then abort(Lira.Error(Reason.InapplicableDiscipline(discipline.id)))

    def summary(atomizations: List[Atomization])
    :   Map[Text, Set[(Text, Atom.Class, Text)]] =

      atomizations.map: atomization =>
        val atoms = atomization.atoms.map: atom =>
          (atom.key, atom.atomClass, Lira.Hash.text(atom.valueHash))

        (atomization.discipline, atoms.to[Set])

      . to[Map]

    val declared = summary(report.atomizations)

    val computed = report.materialized.map: (section, tree) =>
      val content = tree.entries.map: entry =>
        (entry.path, report.blobstore.resolve(entry.blob))

      val context =
        Discipline.Context
          (section.realm, section.integration, classpath(section.realm,
              section.integration))

      (section, summary(registry.atomize(content, context)))

    computed match
      case (_, root) :: rest =>
        (declared.keys + root.keys).each: id =>
          if declared.at(id).or(Set()) != root.at(id).or(Set())
          then abort(Lira.Error(Reason.AtomsMismatch(id)))

        rest.each: (section, other) =>
          if other != root
          then abort(Lira.Error(Reason.ApiDivergence(t"${section.realm} differs from the root")))

      case _ => ()

  // Step 4's sibling for profiles (§11.6, L128/L130). `install` stays language-blind, exactly as
  // it does for re-atomization and lineage-step grading, and this recovers the per-section
  // content a profile's predicates read — which the report holds only as tree entries and blob
  // hashes. `classpath` supplies the materialized dependency vector per (universe, integration)
  // cell, since a profile checking a universe's structure needs the same view a discipline had.
  def evidence
    ( manifest:  Lira.Manifest,
      report:    Report,
      classpath: (Text, Optional[Text]) => List[Text] = { (_, _) => List() } )
  :   EcosystemProfile.Evidence raises Lira.Error =

    val sections = report.materialized.map: (section, tree) =>
      val content = tree.entries.map: entry =>
        (entry.path, report.blobstore.resolve(entry.blob))

      EcosystemProfile.Section
        (section.realm, content, section.integration, classpath(section.realm,
            section.integration))

    EcosystemProfile.Evidence(sections, manifest)
