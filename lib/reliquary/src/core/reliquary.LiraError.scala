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
import fulminate.*

// The validity rules of the LIRA specification, one `Reason` per L-code. Warn-only findings
// (decorative-version mismatches, unreferenced blobs) are never raised as errors; they are
// reported as `LiraAdvisory` values instead.
object LiraError:
  enum Reason(val number: Int) extends Clarification:
    case InvalidManifest(detail: Text)       extends Reason(101)
    case PayloadLength(limit: Long)          extends Reason(102)
    case InvalidBlobStream(detail: Text)     extends Reason(103)
    case MissingBlob(hash: Text)             extends Reason(104)
    case PayloadHash                         extends Reason(105)
    case InvalidTree(detail: Text)           extends Reason(106)
    case OverlayNotMinimal(path: Text)       extends Reason(107)
    case ApiDivergence(detail: Text)         extends Reason(108)
    case LineageMismatch                     extends Reason(109)
    case UngradedSuccessor                   extends Reason(110)
    case DuplicateModule(module: Text)       extends Reason(111)
    case NamespaceClash(space: Text)         extends Reason(112)
    case AbsentDependency(module: Text)      extends Reason(113)
    case Unsatisfiable(module: Text)         extends Reason(114)
    case BadDirective                        extends Reason(115)
    case SigilSpecified                      extends Reason(116)
    case VersionRequired                     extends Reason(117)
    case BuildPinned(module: Text)           extends Reason(118)
    case UnpublishedDependency(module: Text) extends Reason(119)
    case VersionProjection(expected: Text)   extends Reason(120)
    case BadSignature(signer: Text)           extends Reason(121)
    case UnknownAlgorithm(name: Text)         extends Reason(122)
    case UnknownKey(fingerprint: Text)        extends Reason(123)
    case BadResource(detail: Text)            extends Reason(124)
    case IneffectiveResource(path: Text)      extends Reason(125)
    case ResourceClash(path: Text)            extends Reason(126)
    case InapplicableDiscipline(id: Text)     extends Reason(127)
    case ProfileViolated(id: Text, detail: Text) extends Reason(128)
    // L129 is structural — a profile can only add predicates through the `EcosystemProfile`
    // SPI, never remove a core check — so this reason is reserved for a future profile-loading
    // mechanism that could observe a weakening, and is deliberately never constructed today.
    case ProfileWeakens(id: Text)             extends Reason(129)
    case UnrecordedBreak(id: Text, level: Text) extends Reason(130)
    case BadIntegration(detail: Text)         extends Reason(131)
    case NoAssignment(module: Text)           extends Reason(132)
    case UnrealizedIntegration(id: Text)      extends Reason(133)
    case BadDerivative(universe: Text)        extends Reason(138)
    case MalformedPayload(detail: Text)       extends Reason(139)
    case UnimplementedClaim(id: Text)         extends Reason(140)
    case AtomsMismatch(id: Text)              extends Reason(141)

  given communicable: Reason is Communicable =
    case Reason.InvalidManifest(detail)       => m"the manifest is invalid: $detail"
    case Reason.PayloadLength(limit)          => m"the payload length is not the declared $limit"
    case Reason.InvalidBlobStream(detail)     => m"the blob stream is invalid: $detail"
    case Reason.MissingBlob(hash)             => m"the blob $hash is absent from the payload"
    case Reason.PayloadHash                   => m"the payload hash is not its declared value"
    case Reason.InvalidTree(detail)           => m"a tree metadata blob is invalid: $detail"
    case Reason.OverlayNotMinimal(path)       => m"the overlay is not minimal at $path"
    case Reason.ApiDivergence(detail)         => m"the sections differ in API: $detail"
    case Reason.LineageMismatch               => m"the last lineage entry is not this snapshot"
    case Reason.UngradedSuccessor             => m"the release is not a patch or minor successor"
    case Reason.DuplicateModule(module)       => m"the buildpath contains $module more than once"
    case Reason.NamespaceClash(space)         => m"the namespace $space is claimed twice"
    case Reason.AbsentDependency(module) =>
      m"the buildpath does not supply $module in the required universe"
    case Reason.Unsatisfiable(module)         => m"no release of $module satisfies the requirement"
    case Reason.BadDirective                  => m"the interpreter directive is not byte-exact"
    case Reason.SigilSpecified                => m"a lira manifest must not specify a sigil"
    case Reason.VersionRequired               => m"a published release must carry a version"
    case Reason.BuildPinned(module)           => m"the dependency $module is pinned to a build"
    case Reason.UnpublishedDependency(module) => m"the dependency $module is unpublished"
    case Reason.VersionProjection(expected)   => m"the version is not the projection $expected"
    case Reason.BadSignature(signer)          => m"the signature by $signer does not verify"
    case Reason.UnknownAlgorithm(name)        => m"the signature algorithm $name is unknown"
    case Reason.UnknownKey(fingerprint)       => m"no key matches the fingerprint $fingerprint"
    case Reason.BadResource(detail)           => m"the resource claims are ill-formed: $detail"

    case Reason.IneffectiveResource(path) =>
      m"the resource $path resolves to nothing, or to content another discipline claims"

    case Reason.ResourceClash(path)           => m"the resource path $path is claimed twice"

    case Reason.InapplicableDiscipline(id) =>
      m"the discipline $id atomizes no universe this release carries"

    case Reason.ProfileViolated(id, detail) => m"the profile $id is violated: $detail"
    case Reason.ProfileWeakens(id)          => m"the profile $id weakens a core requirement"

    case Reason.UnrecordedBreak(id, level) =>
      m"the step does not preserve $level but the profile $id does not record it"

    case Reason.BadIntegration(detail)      => m"the integrations are ill-formed: $detail"
    case Reason.NoAssignment(module) =>
      m"no integration of $module is satisfiable on this buildpath"
    case Reason.UnrealizedIntegration(id)   => m"the integration $id has no section"

    case Reason.BadDerivative(universe) =>
      m"the declared derivative hash of the $universe section does not recompute"

    case Reason.MalformedPayload(detail)    => m"the payload is malformed: $detail"

    case Reason.UnimplementedClaim(id) =>
      m"the declared discipline or profile $id has no implementation to check it"

    case Reason.AtomsMismatch(id) =>
      m"the declared $id atom listing does not recompute from the content"

case class LiraError(reason: LiraError.Reason)(using Diagnostics)
extends Error(640, reason.number)(m"the LIRA operation failed because $reason")
