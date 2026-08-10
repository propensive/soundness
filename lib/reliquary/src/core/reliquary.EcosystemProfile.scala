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
import rudiments.*
import vacuous.*

import LiraError.Reason

object EcosystemProfile:
  // One section's content, in the form a profile predicate reads it. The same shape a discipline
  // is handed, because a profile that checks structural invariants over a universe's content
  // (§11.6, clause 2) needs exactly what a discipline over that universe would need.
  case class Section
    ( realm:       Text,
      content:     List[(TreePath, Data)],
      integration: Optional[Text] = Unset,
      classpath:   List[Text]     = List() )

  // Everything a profile may examine about *one* release. Unlike a discipline, whose atomization
  // is a function of one release's content alone, profile predicates are diachronic: they compare
  // a release against its predecessor. That difference is the whole reason this is a separate
  // SPI and not another `Discipline`.
  case class Evidence
    ( sections: List[Section],
      manifest: Optional[LiraManifest] = Unset ):

    def section(realm: Text): Optional[Section] =
      sections.stdlib.find { section => section.realm == realm }.getOrElse(Unset)

  // A predicate failure, at the guarantee level it breaks. A profile reports what it found; the
  // audit below decides whether the release accounted for it.
  case class Violation(level: Discipline.Guarantee, detail: Text)

  // What an audit found short of a violation: the profiles it could not check, and the advisory
  // findings its profiles surfaced (`jvm/1`'s changed constants are the motivating case) — real
  // information for a publisher, but no failure of any predicate.
  case class Audit(unchecked: List[Text], advisories: List[Text])

  class Registry(profiles: List[EcosystemProfile]):
    def all: List[EcosystemProfile] = profiles

    def apply(id: Text): Optional[EcosystemProfile] =
      profiles.stdlib.find { profile => profile.id == id }.getOrElse(Unset)

  // L128 and L130, the two rules that make a `profile` record a claim rather than decoration.
  //
  // A release declares the profiles it satisfies and, per §12.4, the guarantee levels this step
  // *breaks*. Checking is therefore not a matter of computing the `breaks` list — the author
  // states it — but of confirming that it accounts for what the predicates actually found:
  //
  //   - a violation at a level the profile certifies, which the release does not record, is
  //     **L130**: the step does not preserve the level and says nothing about it;
  //   - a violation at a level the profile does not certify at all is **L128**: the profile
  //     claims a predicate it does not enforce, which is a broken profile, not a broken release.
  //
  // A declared profile with no implementation in the registry is *not* an error. It is a gap in
  // the checking tool, not a defect in the release, and `unchecked` names them so a caller that
  // must not proceed on unverified claims — a registry, per §16 — can refuse.
  def audit
    ( registry: Registry,
      declared: List[LiraManifest.Profile],
      previous: Evidence,
      next:     Evidence )
  :   Audit raises LiraError raises DisciplineError =

    val unchecked = scala.collection.mutable.ListBuffer[Text]()
    val advisories = scala.collection.mutable.ListBuffer[Text]()

    declared.stdlib.foreach: record =>
      registry(record.id) match
        case profile: EcosystemProfile =>
          val recorded = record.breaks.stdlib.map(guarantee(_)).toSet
          val violations = profile.check(previous, next).stdlib

          // Every offense is gathered before either abort, so the error a publisher sees names
          // the whole finding for its rule, not merely the first violation encountered.
          val uncertified = violations.filter: violation =>
            !profile.certifies.stdlib.contains(violation.level)

          if !uncertified.isEmpty
          then
            val details = Text(uncertified.map(_.detail.s).mkString("; "))
            abort(LiraError(Reason.ProfileViolated(record.id, details)))

          val unrecorded = violations.map(_.level).distinct.filter: level =>
            !recorded.contains(level)

          if !unrecorded.isEmpty
          then
            val levels = Text(unrecorded.map(keyword(_).s).mkString(", "))
            abort(LiraError(Reason.UnrecordedBreak(record.id, levels)))

          advisories ++= profile.advisories(previous, next).stdlib

        case _ => unchecked += record.id

    Audit(List.from(unchecked.toList), List.from(advisories.toList))

  // Both vocabularies omit `behavior` for the same reason — no hash scheme certifies it (§11.5,
  // §18) — so the mapping is total in both directions.
  private def guarantee(level: LiraManifest.Guarantee): Discipline.Guarantee = level match
    case LiraManifest.Guarantee.Linkage       => Discipline.Guarantee.Linkage
    case LiraManifest.Guarantee.Recompilation => Discipline.Guarantee.Recompilation

  private def keyword(level: Discipline.Guarantee): Text = level match
    case Discipline.Guarantee.Linkage       => t"linkage"
    case Discipline.Guarantee.Recompilation => t"recompilation"

// A named, versioned set of predicates an ecosystem imposes in addition to those of the core
// specification (§11.6). Profiles add predicates and add guarantees; they never subtract (L129),
// so a file the core rejects is rejected under every profile.
//
// The alternative to a profile is a universe-specific discipline, and §11.6 explains why it is
// the wrong default: atoms feed the snapshot, and the snapshot is API identity, so folding an
// ecosystem's linkage surface into atoms means a release whose source interface is unchanged but
// whose bridge methods moved acquires a new identity and breaks dependency satisfaction for
// every consumer — including those who only ever recompile. A profile keeps the snapshot at the
// recompilation level, where it is the useful identity, and records linkage breakage separately,
// where it can be acted on by the consumers it actually affects.
trait EcosystemProfile:
  def id: Text

  // The levels this profile's predicates cover (§11.6, clause 3) — typically levels the
  // release's disciplines do *not* certify, which is the point of having a profile at all.
  def certifies: Set[Discipline.Guarantee]

  def check(previous: EcosystemProfile.Evidence, next: EcosystemProfile.Evidence)
  :   List[EcosystemProfile.Violation] raises DisciplineError

  // Findings short of violations, surfaced for reporting (`jvm.md` §7's changed constants are
  // the motivating case). Advisory only: nothing here affects the audit's verdict.
  def advisories(previous: EcosystemProfile.Evidence, next: EcosystemProfile.Evidence)
  :   List[Text] raises DisciplineError =
    List()

  // §13.3 rule 6: the predicates this profile imposes over a whole buildpath, decidable from
  // manifests alone — a profile predicate requiring payload inspection is a publish-time check
  // (§16), not a buildpath rule. Toolchain coherence (`jvm.md` §6) is the motivating case.
  // Returns violation details; an empty list is coherence.
  def coherence(releases: List[LiraManifest]): List[Text] = List()
