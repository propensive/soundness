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
package mandible

import anticipation.*
import denominative.nil
import contingency.*
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

// The `jvm/1` ecosystem profile (LIRA Appendix D): the JVM's linkage predicates, checked against
// the predecessor release and reported *separately* from the grade.
//
// It runs the same atomizer as `ClassfileDiscipline`, over the same content, and reaches the same
// conclusions about what the bytecode contract does. The difference is entirely in where the
// answer goes. The discipline's atoms enter the snapshot, and the snapshot is API identity, so a
// release whose bridge methods merely moved would acquire a new identity and fail dependency
// satisfaction for consumers who only recompile. The profile's findings enter the `breaks
// linkage` record instead (§12.4), which is read by exactly the consumers a linkage break
// affects — those pinned to prebuilt bytecode — and by nobody else.
//
// This is what Appendix D.1 means by the two levels diverging in both directions. A release can
// be a clean minor by the core algebra and still fail these predicates; it can also break
// recompilation, graded a major, while every predicate here passes.
object JvmProfile extends EcosystemProfile:
  def id: Text = t"jvm/1"

  // Linkage alone. `tasty/1` certifies recompilation over the same release, and this profile
  // adds no predicate about it: a profile adds guarantees, never subtracts them (L129).
  def certifies: Set[Discipline.Guarantee] = Set(Discipline.Guarantee.Linkage)

  private val universe: Text = t"jvm"

  // The atoms `classfile/1` would produce for one release's `jvm` section, as a lookup by key.
  // An absent section — a release that carries no `jvm` universe at all — has no linkage surface
  // and yields nothing to compare.
  private def surface(evidence: EcosystemProfile.Evidence)
  :   Map[Text, Atom] raises Discipline.Error =

    evidence.section(universe).lay(Map.empty[Text, Atom]): section =>
      val classes: List[(TreePath, Data)] =
        section.content.filter: pair => pair(0).text.s.endsWith(".class")

      if classes.nil then Map.empty else
        val surfaces: Map[Text, ClassSurface] = classes.map: (path, data) =>
          // Same erasure; the parser reads the bytes and retains nothing of them.
          val surface = ClassSurface(data.asInstanceOf[scala.IArray[Byte]])
          surface.name -> surface

        . to[Map]

        val outcome =
          ClassfileAtomizer.atomize(surfaces, section.classpath, ClassfileAtomizer.Fold.Linkage)

        outcome.unresolved.prim.let: name =>
          abort(Discipline.Error(id, Discipline.Error.Reason.Unresolved(name)))

        outcome.atoms.map { atom => atom.key -> atom }.to[Map]

  def check(previous: EcosystemProfile.Evidence, next: EcosystemProfile.Evidence)
  :   List[EcosystemProfile.Violation] raises Discipline.Error =

    val before = surface(previous)
    val after = surface(next)
    val violations = scala.collection.mutable.ListBuffer[EcosystemProfile.Violation]()

    def violate(detail: Text): Unit =
      violations += EcosystemProfile.Violation(Discipline.Guarantee.Linkage, detail)

    before.each: (key, atom) =>
      after.at(key) match
        case replacement: Atom =>
          // A rigid atom whose value moved is a member whose flags, descriptor, generic signature
          // or throws clause changed under a name consumers still resolve. Narrowing
          // accessibility is the case that matters most (D.2, predicate 4), and it lands here
          // because access flags fold into the atom's value.
          if atom.atomClass == Atom.Class.Rigid
          && Lira.Hash.text(atom.valueHash) != Lira.Hash.text(replacement.valueHash)
          then violate(t"$key no longer has the shape compiled consumers resolved")

        // D.2, predicates 1, 2 and 4 at once. A key is `owner#name:descriptor`, so a member that
        // disappears, changes descriptor, or moves to a type that no longer presents it all show
        // up here as a missing key — including the bridges and mixin forwarders a compiled
        // consumer may have bound to, which is why the atomizer keeps them.
        case _ => violate(t"$key is no longer presented")

    // D.2, predicate 5: the ecosystem's toolchain predicate (§13.3). TASTy readability is
    // versioned and not universally backward-compatible, so a release must carry TASTy the
    // consumer's compiler can read — a release that records no toolchain at all makes the claim
    // uncheckable rather than false, and is reported as such.
    next.manifest.let: manifest =>
      if manifest.toolchain.nil
      then violate(t"the release records no toolchain, so TASTy readability cannot be checked")

    violations.toList.to(List)

  // D.2, predicate 3: `static final` constant values that javac may already have inlined into
  // consumers. These are *not* linkage violations — a changed constant leaves every descriptor
  // resolvable — but a consumer holding the old value computes with it until it recompiles. The
  // core algebra already says the right thing here (the atoms are replaceable, so the step is a
  // minor and stale used-sets are marked, §13.4), and repeating it as a break would overstate
  // the finding. They are surfaced separately, for reporting.
  def constants(previous: EcosystemProfile.Evidence, next: EcosystemProfile.Evidence)
  :   List[Text] raises Discipline.Error =

    val before = surface(previous)
    val after = surface(next)

    val changed: List[Optional[Text]] = before.sweep:
      case (key, atom) if atom.atomClass == Atom.Class.Replaceable =>
        after.at(key).let: replacement =>
          if Lira.Hash.text(replacement.valueHash) != Lira.Hash.text(atom.valueHash) then key
          else Unset

    changed.sweep { case key: Text => key }.order(_.s)

  // §7's SHOULD: changed constants are surfaced through the audit's advisory channel, so a
  // publisher sees them without any bespoke call.
  override def advisories
    ( previous: EcosystemProfile.Evidence, next: EcosystemProfile.Evidence )
  :   List[Text] raises Discipline.Error =

    // The interpolation is a named `def`, not the lambda's body: expanded inside the lambda,
    // the `t` macro's `Showable` search runs while the rebuilt shape is still an uninstantiated
    // type variable, which is the `wildApprox` crash.
    def advice(key: Text): Text =
      t"the constant $key changed value; consumers that inlined it are stale until recompiled"

    constants(previous, next).map(advice(_))

  // §6's toolchain predicate, at the scope where it is stated: every release on a buildpath
  // must carry metadata a consuming compiler can read, and a release recording no toolchain at
  // all makes the claim uncheckable rather than false — reported as a violation, per release.
  // Which TASTy versions a *particular* consumer's compiler reads is that consumer's knowledge,
  // not the manifests', so the window comparison belongs to the consuming tool; what is
  // buildpath-decidable is that every release states what produced it.
  override def coherence(releases: List[Lira.Manifest]): List[Text] =

    // A named `def` for the same reason as `advisories` above: the `t` macro cannot expand
    // inside a lambda whose rebuilt shape is still an uninstantiated type variable.
    def unstated(manifest: Lira.Manifest): Text =
      t"${manifest.module} records no toolchain, so TASTy readability cannot be checked"

    val undeclared: List[Lira.Manifest] = releases.filter(_.toolchain.nil)

    undeclared.map(unstated(_))
