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

import Lira.Error.Reason
import anticipation.*
import contingency.*
import fulminate.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import pneumatic.*
import revolution.*
import stratiform.*
import turbulence.*
import vacuous.*

import zephyrine.*

object Lira:
  // The interpreter directive's payload, as the parser stores it (without the `#!`). The full
  // first line of every `.lira` file is byte-fixed (§5.1, L115).
  val directive: Text = t"/usr/bin/env lira"

  private val directiveBytes: Data =
    charEncoders.utf8Encoder.encoded(t"#!/usr/bin/env lira\n")

  private val separatorBytes: Data = charEncoders.utf8Encoder.encoded(t"\n##\n")

  // Locates the document separator: the first line that is exactly `##`. §5.2 fixes the byte
  // layout so this split needs no TEL parsing — which is essential, since everything after the
  // separator is binary.
  private def separatorIndex(data: Data): Optional[Int] =
    var index = 0

    while index + separatorBytes.length <= data.length do
      var offset = 0

      while offset < separatorBytes.length && data(index + offset) == separatorBytes(offset) do
        offset += 1

      if offset == separatorBytes.length then return index
      index += 1

    Unset

  private def slice(data: Data, from: Int, until: Int): Data =
    val buffer = Array.allocate[Byte](until - from)
    System.arraycopy(Array.unsafeJvm(data), from, buffer.raw, 0, until - from)
    Array.freeze(buffer)

  def read(data: Data): Lira raises Lira.Error =
    // Step 0 (§16): the directive is checked byte-for-byte before anything is parsed (L115).
    if data.length < directiveBytes.length then abort(Lira.Error(Reason.BadDirective))

    var index = 0

    while index < directiveBytes.length do
      if data(index) != directiveBytes(index) then abort(Lira.Error(Reason.BadDirective))
      index += 1

    val separator = separatorIndex(data) match
      case position: Int => position

      case _ =>
        abort(Lira.Error(Reason.InvalidManifest(t"the document separator is missing")))

    val manifestData = slice(data, 0, separator + 1)
    val compressed = slice(data, separator + separatorBytes.length, data.length)

    val document =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case Tel.Error(reason, _) => Lira.Error(Reason.InvalidManifest(t"$reason"))

      . protect(manifestData.utf8.load[Tel])

    // L116: the pragma must not specify a sigil; the separator is therefore always `##`.
    document.metadata.pragma.let: pragma =>
      if pragma.sigil.present then abort(Lira.Error(Reason.SigilSpecified))

    val tel = document.root

    import Tels.Decoder.validate

    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case Tel.Error(reason, _) => Lira.Error(Reason.InvalidManifest(t"$reason"))

    . protect(tel.validate(using Lira.Schemas.lira, Lira.Validators.registry))

    Lira(Lira.Manifest.decode(tel), tel, compressed)

  // Assembles a complete `.lira` file: the blob stream is built from `blobs` (deduplicated and
  // sorted), compressed, and described by a payload record that replaces whatever `manifest`
  // carried. Byte-deterministic for fixed inputs and toolchain (§17).
  def assemble(manifest: Lira.Manifest, blobs: List[Data]): Data =
    val stream = BlobStream.write(blobs)
    val compressed = Lira.Payload.compress(stream)

    val payload =
      Lira.Manifest.Payload(t"brotli", stream.length.toLong, Lira.Payload.hash(stream))

    val text = manifest.copy(payload = payload).render
    val manifestData = charEncoders.utf8Encoder.encoded(text)
    val buffer = Array.allocate[Byte](manifestData.length + 3 + compressed.length)
    System.arraycopy(Array.unsafeJvm(manifestData), 0, buffer.raw, 0, manifestData.length)
    buffer(manifestData.length) = '#'.toByte
    buffer(manifestData.length + 1) = '#'.toByte
    buffer(manifestData.length + 2) = '\n'.toByte

    System.arraycopy
      ( Array.unsafeJvm(compressed), 0, buffer.raw, manifestData.length + 3, compressed.length )

    Array.freeze(buffer)

  // LiraAdvisory → Lira.Advisory
  // Warn-only findings (§12.4 and §8.2): never raised as errors, reported alongside successful
  // operations for tools to surface.
  enum Advisory:
    case NotNumeric(version: Semver)
    case VersionMismatch(declared: Semver, expected: Semver)
    case UnreferencedBlobs(hashes: List[Text])

    // §13.3 rule 7 was left pending: the buildpath was validated without a host contract, and the
    // named modules' requirements remain unchecked. A tool MUST report which mode it validated in,
    // since a buildpath can cohere as a library composition and still be unsatisfiable on the
    // host a consumer intends.
    case HostPending(modules: List[Text])

  // LiraDelta → Lira.Delta
  object Delta:

    // The atom-level change record of one lineage step (§12.3): the atoms added, and the
    // replaceable atoms replaced. Deltas make staleness computable (§13.4) and allow a verifier
    // holding consecutive releases to check a lineage step exactly.
    def compute(previous: List[Atomization], next: List[Atomization]): Lira.Delta =
      def flat(atomizations: List[Atomization]): scala.List[Atom] =
        atomizations.stdlib.flatMap(_.atoms.stdlib)

      val before = flat(previous)
      val after = flat(next)
      val beforeHashes = before.map { atom => Lira.Hash.text(atom.valueHash) }.toSet

      val added = after
        . filter: atom => !beforeHashes.contains(Lira.Hash.text(atom.valueHash))
        . map(_.valueHash)
        . sortWith: (a, b) => Blob.compare(a, b) < 0

      val beforeReplaceable = before.filter(_.atomClass == Atom.Class.Replaceable)

      val afterReplaceable =
        scala.collection.immutable.Map.from:
          after.filter(_.atomClass == Atom.Class.Replaceable).map: atom => (atom.key, atom)

      val replaced = beforeReplaceable
        . flatMap: atom =>
            afterReplaceable.get(atom.key) match
              case scala.Some(successor)
                if Blob.compare(successor.valueHash, atom.valueHash) != 0 =>
                scala.List(Replacement(atom.valueHash, successor.valueHash))

              case _ => scala.Nil

        . sortWith: (a, b) => Blob.compare(a.old, b.old) < 0

      Lira.Delta(List.from(added), List.from(replaced))

    def decode(data: Data): Lira.Delta raises Lira.Error =
      import Tels.Decoder.validate

      val document =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case Tel.Error(reason, _) =>
            Lira.Error(Reason.InvalidManifest(t"the delta blob is invalid: $reason"))

        . protect:
            val tel = data.read[Tel]
            tel.validate(using Lira.Schemas.delta, Lira.Validators.registry)
            tel

      def bad(detail: Text): Lira.Error =
        import errorDiagnostics.emptyDiagnostics
        Lira.Error(Reason.InvalidManifest(t"the delta blob is invalid: $detail"))

      def hash(text: Text): Data =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case Base256.Error(_) => bad(t"a hash is malformed")

        . protect(Base256.decodeStrict(text))

      def texts(compound: Tel.Compound): scala.collection.immutable.Vector[Text] =
        compound.atoms.readable.collect:
          case Tel.Atom.Inline(text, _)  => text
          case Tel.Atom.Source(text)     => text
          case Tel.Atom.Literal(_, text) => text

        . toVector

      val compounds = document.childCompounds.readable

      val added = compounds.filter(_.keyword == t"add").toVector.map: compound =>
        val atoms = texts(compound)
        if atoms.length != 1 then abort(bad(t"an add row does not have exactly one atom"))
        hash(atoms(0))

      val replaced = compounds.filter(_.keyword == t"replace").toVector.map: compound =>
        val atoms = texts(compound)
        if atoms.length != 2 then abort(bad(t"a replace row does not have exactly two atoms"))
        Replacement(hash(atoms(0)), hash(atoms(1)))

      Lira.Delta(List.from(added), List.from(replaced))

  case class Delta(add: List[Data], replace: List[Replacement]):

    // Canonical serialization: `add` rows in ascending hash order, then `replace` rows in
    // ascending old-hash order, under the pinned `lira-delta` schema signature.
    def encode: Data =
      val addRows = add.stdlib.map: hash => s"add ${Lira.Hash.text(hash)}"

      val replaceRows = replace.stdlib.map: replacement =>
        s"replace ${Lira.Hash.text(replacement.old)}  ${Lira.Hash.text(replacement.next)}"

      val rows = addRows ++ replaceRows
      val header = s"tel 1.0 ${Lira.Schemas.deltaSignature}"
      val body = rows.mkString("\n")
      val text = Text(if rows.isEmpty then s"$header\n" else s"$header\n\n$body\n")
      charEncoders.utf8Encoder.encoded(text)

  // LiraError → Lira.Error
  // The validity rules of the LIRA specification, one `Reason` per L-code. Warn-only findings
  // (decorative-version mismatches, unreferenced blobs) are never raised as errors; they are
  // reported as `Lira.Advisory` values instead.
  object Error:
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
      case UngradedSuccessor(subject: Text)    extends Reason(110)
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
      case BadHostContract(detail: Text)        extends Reason(135)
      case UnsatisfiedRequirement(module: Text) extends Reason(136)
      case NotHostContract(module: Text)        extends Reason(137)
      case BadDerivative(universe: Text)        extends Reason(138)
      case MalformedPayload(detail: Text)       extends Reason(139)
      case UnimplementedClaim(id: Text)         extends Reason(140)
      case AtomsMismatch(id: Text)              extends Reason(141)
      case TagReassigned(tag: Text)             extends Reason(142)

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
      case Reason.UngradedSuccessor(subject) =>
        m"$subject is not a patch or minor successor to its predecessor"
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
      case Reason.BadHostContract(detail)     => m"the host contract is ill-formed: $detail"

      case Reason.UnsatisfiedRequirement(module) =>
        m"no given host contract satisfies the requirement on $module"

      case Reason.NotHostContract(module) =>
        m"the requirement names $module, whose releases are not host contracts"

      case Reason.BadDerivative(universe) =>
        m"the declared derivative hash of the $universe section does not recompute"

      case Reason.MalformedPayload(detail)    => m"the payload is malformed: $detail"

      case Reason.UnimplementedClaim(id) =>
        m"the declared discipline or profile $id has no implementation to check it"

      case Reason.AtomsMismatch(id) =>
        m"the declared $id atom listing does not recompute from the content"

      case Reason.TagReassigned(tag) =>
        m"the tag $tag already names a different release of this module"

  case class Error(reason: Lira.Error.Reason)(using Diagnostics)
  extends fulminate.Error(640, reason.number)(m"the LIRA operation failed because $reason")

  // LiraHash → Lira.Hash
  // Domain-separated hashing per §7.1 of the LIRA specification: every hash the format defines is
  // `BLAKE3-256(utf8(domain) ++ 0x00 ++ content)`, with the domain string carrying the `lira/1`
  // format epoch. Atom domains additionally carry the full discipline identifier, so atoms from
  // different disciplines — or different versions of one discipline — can never collide.
  object Hash:
    val epoch: Text = t"lira/1"
    val size: Int = 32

    // The `0x00` byte separating the domain from the content; a fresh byte array is
    // zero-initialized, so freezing a unit array yields it directly.
    private val separator: Data = Array.freeze(Array.allocate[Byte](1))

    enum Domain:
      case Blob, Snapshot, Manifest, Key, Derivative
      case Atom(discipline: Text)

      def text: Text = this match
        case Blob             => t"$epoch:blob"
        case Snapshot         => t"$epoch:snapshot"
        case Domain.Manifest  => t"$epoch:manifest"
        case Key              => t"$epoch:key"
        case Derivative       => t"$epoch:derivative"
        case Atom(discipline) => t"$epoch:atom:$discipline"

    def apply(domain: Domain, content: Data): Data =
      val prefix: Data = charEncoders.utf8Encoder.encoded(domain.text)
      val buffer = Array.allocate[Byte](prefix.length + 1 + content.length)
      System.arraycopy(Array.unsafeJvm(prefix), 0, buffer.raw, 0, prefix.length)
      System.arraycopy(Array.unsafeJvm(content), 0, buffer.raw, prefix.length + 1, content.length)

      Blake3.hashOf(Array.freeze(buffer))

    // The textual form of any LIRA hash: 32 BASE-256 characters (§7).
    def text(hash: Data): Text = Base256.encode(hash)

    // The hash of the empty byte string in the blob domain, pinned as a golden value guarding the
    // stability of the domain-separation construction itself.
    val emptyBlob: Text = t"ǢjЪ6ДľIẈḟžЭŠГȕJЂĘґƟḁsЬțДǶṛḠẄήϋƧЪ"

  // LiraManifest → Lira.Manifest
  object Manifest:
    case class Tool(name: Text, version: Text, flag: List[Text] = List())
    case class Api(discipline: Text, atoms: Data)

    // The guarantee levels of §11.5 that can be claimed or broken. Behavior is absent by design:
    // no hash scheme certifies it (§18), so it is not expressible in a `breaks` field.
    enum Guarantee:
      case Linkage, Recompilation

      def keyword: Text = this match
        case Linkage       => t"linkage"
        case Recompilation => t"recompilation"

    object Guarantee:
      def parse(keyword: Text): Optional[Guarantee] = keyword.s match
        case "linkage"       => Linkage
        case "recompilation" => Recompilation
        case _               => Unset

    // An ecosystem profile this release claims to satisfy (§11.6), with the guarantee levels its
    // lineage step did not preserve (§12.4). `breaks` being empty means the step preserved every
    // level the profile certifies — the whole value of the record is that its absence means
    // something, so a level not preserved and not listed is invalid (L130).
    case class Profile(id: Text, breaks: List[Guarantee] = List())

    // One alternative dependency vector the release was built against (§9.5). `rank` orders the
    // canonical assignment (§13.3), lower first; a rank left unset sorts after every declared one,
    // so a publisher who ranks nothing gets declaration-order-independent, id-ordered resolution.
    // `label` is prose and carries no authority (§14). It is one atom even when it contains
    // spaces: `render` separates it from its keyword by a hard-space run, which switches TEL into
    // hard-space mode for the rest of the line (TEL §10.3), so the single spaces within it are
    // content rather than phrase separators.
    case class Integration(id: Text, rank: Optional[Long] = Unset, label: Optional[Text] = Unset)

    // `serves` names the universe in which the dependency itself offers its content, when that
    // differs from the universes the depending sections consume it in — the join case (§13.2): a
    // Scala module whose `sjsir` build invokes a TypeScript module declares `universe sjsir` and
    // `serves js`, and the two universes meet at a bundler join. A dependency without `serves` is
    // satisfied in the same universe it applies to.
    case class Dependency
      ( module:      Text,
        api:         Data,
        version:     Optional[Semver] = Unset,
        build:       Optional[Data]   = Unset,
        universe:    List[Text]       = List(),
        serves:      Optional[Text]   = Unset,
        integration: List[Text]       = List(),
        uses:        Optional[Data]   = Unset,
        spans:       List[Data]       = List() ):

      // §13.2: the two scopes are independent and conjunctive. An empty list on either axis means
      // "every value of that axis", which is how a dependency common to all of them is declared
      // once and unscoped.
      def applies(universe: Text, integration: Optional[Text]): Boolean =
        val universeApplies = this.universe.stdlib.isEmpty || this.universe.stdlib.contains(universe)

        val integrationApplies =
          this.integration.stdlib.isEmpty
          || integration.let { id => this.integration.stdlib.contains(id) }.or(false)

        universeApplies && integrationApplies

    // One host requirement of a section (hosts.md §6): the host contract's module name and the
    // required contract snapshot, satisfied by lineage membership or by spanning where a Uses
    // blob is attached (hosts.md §7). Authorial: no verifier can decide that code needs what it
    // declares (§16), which is why the environment itself is probed at a third moment.
    case class Requires
      ( module:  Text,
        api:     Data,
        version: Optional[Semver] = Unset,
        uses:    Optional[Data]   = Unset )

    // How a declared resource participates in the algebra (§11.4). `Export` guarantees the name is
    // present; `Track` additionally tracks the bytes as replaceable churn; `Scan` claims a whole
    // directory atomless, so nothing under it is contractual.
    enum ResourceMode:
      case Export, Track, Scan

      def keyword: Text = this match
        case Export => t"export"
        case Track  => t"track"
        case Scan   => t"scan"

    object ResourceMode:
      def parse(keyword: Text): Optional[ResourceMode] = keyword.s match
        case "export" => Export
        case "track"  => Track
        case "scan"   => Scan
        case _        => Unset

    // One resource claim (§11.4): an authorial statement, like `owns`, that parameterizes the
    // `resource/1` discipline's claiming.
    case class Resource(mode: ResourceMode, path: TreePath)

    case class Payload(compression: Text, length: Long, hash: Data)
    case class Signature(signer: Text, algorithm: Text, key: Data, value: Text)

    private def bad(detail: Text): Lira.Error =
      import errorDiagnostics.emptyDiagnostics
      Lira.Error(Reason.InvalidManifest(detail))

    private def texts(compound: Tel.Compound): scala.collection.immutable.Vector[Text] =
      compound.atoms.readable.collect:
        case Tel.Atom.Inline(text, _)  => text
        case Tel.Atom.Source(text)     => text
        case Tel.Atom.Literal(_, text) => text

      . toVector

    private def one(compound: Tel.Compound): Text raises Lira.Error =
      val atoms = texts(compound)
      if atoms.length != 1 then abort(bad(t"the ${compound.keyword} field needs exactly one atom"))
      atoms(0)

    private def hash(text: Text): Data raises Lira.Error =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case Base256.Error(_) => bad(t"a hash is malformed")

      . protect(Base256.decodeStrict(text))

    private def semver(text: Text): Semver raises Lira.Error =
      val parts = text.s.split("\\.", -1).nn
      if parts.length != 3 then abort(bad(t"the version is not `major.minor.patch`"))
      Semver(parts(0).nn.toLong, parts(1).nn.toLong, parts(2).nn.toLong)

    private def children(compound: Tel.Compound): scala.collection.immutable.Vector[Tel.Compound] =
      compound.children.readable.flatMap(_.compounds.readable).toVector

    private def field(compounds: scala.collection.immutable.Vector[Tel.Compound], keyword: Text)
    :   Optional[Text] raises Lira.Error =

      compounds.filter(_.keyword == keyword) match
        case scala.collection.immutable.Vector()         => Unset
        case scala.collection.immutable.Vector(compound) => one(compound)

        case _ =>
          abort(bad(t"the $keyword field appears more than once"))

    private def required(compounds: scala.collection.immutable.Vector[Tel.Compound], keyword: Text)
    :   Text raises Lira.Error =

      field(compounds, keyword).or(abort(bad(t"the $keyword field is missing")))

    private def repeated(compounds: scala.collection.immutable.Vector[Tel.Compound], keyword: Text)
    :   scala.collection.immutable.Vector[Text] raises Lira.Error =

      compounds.filter(_.keyword == keyword).flatMap: compound =>
        val atoms = texts(compound)
        if atoms.isEmpty then abort(bad(t"the $keyword field needs at least one atom"))
        atoms

    // Extracts the typed view from a type-assigned manifest document.
    def decode(tel: Tel): Lira.Manifest raises Lira.Error =
      val top = tel.childCompounds.readable.toVector

      val toolchain = top.filter(_.keyword == t"toolchain").map: compound =>
        val fields = children(compound)

        Tool
          ( required(fields, t"name"),
            required(fields, t"version"),
            List.from(repeated(fields, t"flag")) )

      val api = top.filter(_.keyword == t"api").map: compound =>
        val fields = children(compound)
        Api(required(fields, t"discipline"), hash(required(fields, t"atoms")))

      val dependency = top.filter(_.keyword == t"dependency").map: compound =>
        val fields = children(compound)

        Dependency
          ( module      = required(fields, t"module"),
            api         = hash(required(fields, t"api")),
            version     = field(fields, t"version").let(semver(_)),
            build       = field(fields, t"build").let(hash(_)),
            universe    = List.from(repeated(fields, t"universe")),
            serves      = field(fields, t"serves"),
            integration = List.from(repeated(fields, t"integration")),
            uses        = field(fields, t"uses").let(hash(_)),
            spans       = List.from(repeated(fields, t"spans").map(hash(_))) )

      val resource = top.filter(_.keyword == t"resource").map: compound =>
        val mode = texts(compound) match
          case scala.collection.immutable.Vector(mode) =>
            ResourceMode.parse(mode).or(abort(bad(t"$mode is not a resource mode")))

          case _ =>
            abort(bad(t"a resource needs exactly one mode"))

        Resource(mode, TreePath(required(children(compound), t"path")))

      val profile = top.filter(_.keyword == t"profile").map: compound =>
        val fields = children(compound)

        val breaks = repeated(fields, t"breaks").map: keyword =>
          Guarantee.parse(keyword).or(abort(bad(t"$keyword is not a guarantee level")))

        Profile(required(fields, t"id"), List.from(breaks))

      val integration = top.filter(_.keyword == t"integration").map: compound =>
        val fields = children(compound)

        Integration
          ( id    = required(fields, t"id"),
            rank  = field(fields, t"rank").let { text => text.s.toLong },
            label = field(fields, t"label") )

      val section = top.filter(_.keyword == t"section").map: compound =>
        val realm = texts(compound) match
          case scala.collection.immutable.Vector(realm) => realm

          case _ =>
            abort(bad(t"a section needs exactly one realm"))

        val fields = children(compound)

        val requires = fields.filter(_.keyword == t"requires").map: requirement =>
          val subfields = children(requirement)

          Requires
            ( module  = required(subfields, t"module"),
              api     = hash(required(subfields, t"api")),
              version = field(subfields, t"version").let(semver(_)),
              uses    = field(subfields, t"uses").let(hash(_)) )

        Section
          ( realm       = realm,
            integration = field(fields, t"integration"),
            tree        = hash(required(fields, t"tree")),
            delete      = List.from(repeated(fields, t"delete").map(TreePath(_))),
            derivative  = field(fields, t"derivative").let(hash(_)),
            requires    = List.from(requires) )

      val payload = top.filter(_.keyword == t"payload").toList match
        case scala.List(compound) =>
          val fields = children(compound)

          Payload
            ( required(fields, t"compression"),
              required(fields, t"length").s.toLong,
              hash(required(fields, t"hash")) )

        case _ => abort(bad(t"the payload record is missing or repeated"))

      val signature = top.filter(_.keyword == t"signature").map: compound =>
        val fields = children(compound)

        Signature
          ( required(fields, t"signer"),
            required(fields, t"algorithm"),
            hash(required(fields, t"key")),
            required(fields, t"value") )

      Lira.Manifest
        ( module      = required(top, t"module"),
          version     = field(top, t"version").let(semver(_)),
          tag         = List.from(repeated(top, t"tag")),
          lineage     = List.from(repeated(top, t"lineage").map(hash(_))),
          toolchain   = List.from(toolchain),
          owns        = List.from(repeated(top, t"owns")),
          resource    = List.from(resource),
          api         = List.from(api),
          profile     = List.from(profile),
          integration = List.from(integration),
          dependency  = List.from(dependency),
          delta       = field(top, t"delta").let(hash(_)),
          section     = List.from(section),
          payload     = payload,
          signature   = List.from(signature) )

  // The typed view of a `.lira` manifest (§14). Decoding always retains the parsed `Tel` alongside
  // (in `Lira`): signing and reserialization operate on the TEL semantic model; this class is the
  // ergonomic projection. A manifest without a `version` is a development release, identified
  // purely by its hashes and unpublishable until a version is assigned.
  case class Manifest
    ( module:      Text,
      version:     Optional[Semver]                = Unset,
      tag:         List[Text]                      = List(),
      lineage:     List[Data],
      toolchain:   List[Lira.Manifest.Tool]         = List(),
      owns:        List[Text]                      = List(),
      resource:    List[Lira.Manifest.Resource]     = List(),
      api:         List[Lira.Manifest.Api],
      profile:     List[Lira.Manifest.Profile]      = List(),
      integration: List[Lira.Manifest.Integration]  = List(),
      dependency:  List[Lira.Manifest.Dependency]   = List(),
      delta:       Optional[Data]                  = Unset,
      section:     List[Section],
      payload:     Lira.Manifest.Payload,
      signature:   List[Lira.Manifest.Signature]    = List() ):

    // The root section is the first (§9.1); overlays materialize against it.
    def root: Optional[Section] = if section.stdlib.isEmpty then Unset else section.stdlib.head

    def development: Boolean = version.absent

    // A release carrying a `host` section is a host contract (§9.4, hosts.md §4) — recognizable
    // from its manifest alone, which is what makes L137 checkable at resolution time.
    def hostContract: Boolean = section.stdlib.exists(_.realm == t"host")

    // The canonical text of the whole file's manifest part: directive, pragma, one blank line,
    // then the compounds in schema order, LF-terminated. Deterministic; `Lira.read` accepts any
    // conforming formatting, but everything reliquary writes is in this form.
    def render: Text =
      val lines = scala.collection.mutable.ArrayBuffer[String]()
      lines += "#!/usr/bin/env lira"
      lines += s"tel 1.0 ${Lira.Schemas.liraSignature}"
      lines += ""
      lines += s"module $module"

      version.let: v => lines += s"version ${v.major}.${v.minor}.${v.patch}"
      tag.stdlib.foreach: name => lines += s"tag $name"
      lineage.stdlib.foreach: hash => lines += s"lineage ${Lira.Hash.text(hash)}"

      toolchain.stdlib.foreach: tool =>
        lines += "toolchain"
        lines += s"  name ${tool.name}"
        lines += s"  version ${tool.version}"
        tool.flag.stdlib.foreach: flag => lines += s"  flag $flag"

      owns.stdlib.foreach: space => lines += s"owns $space"

      resource.stdlib.foreach: resource =>
        lines += s"resource ${resource.mode.keyword}"
        lines += s"  path ${resource.path.text}"

      api.stdlib.foreach: api =>
        lines += "api"
        lines += s"  discipline ${api.discipline}"
        lines += s"  atoms ${Lira.Hash.text(api.atoms)}"

      profile.stdlib.foreach: profile =>
        lines += "profile"
        lines += s"  id ${profile.id}"
        profile.breaks.stdlib.foreach: level => lines += s"  breaks ${level.keyword}"

      integration.stdlib.foreach: integration =>
        lines += "integration"
        lines += s"  id ${integration.id}"
        integration.rank.let: rank => lines += s"  rank $rank"
        // Two spaces, not one: the hard-space run makes the whole remainder of the line one atom
        // (TEL §10.3), which is what lets a label contain spaces.
        integration.label.let: label => lines += s"  label  $label"

      dependency.stdlib.foreach: dependency =>
        lines += "dependency"
        lines += s"  module ${dependency.module}"
        lines += s"  api ${Lira.Hash.text(dependency.api)}"

        dependency.version.let: version =>
          lines += s"  version ${version.major}.${version.minor}.${version.patch}"

        dependency.build.let: build => lines += s"  build ${Lira.Hash.text(build)}"
        dependency.universe.stdlib.foreach: universe => lines += s"  universe $universe"
        dependency.serves.let: serves => lines += s"  serves $serves"

        dependency.integration.stdlib.foreach: integration =>
          lines += s"  integration $integration"

        dependency.uses.let: uses => lines += s"  uses ${Lira.Hash.text(uses)}"
        dependency.spans.stdlib.foreach: spans => lines += s"  spans ${Lira.Hash.text(spans)}"

      delta.let: delta => lines += s"delta ${Lira.Hash.text(delta)}"

      section.stdlib.foreach: section =>
        lines += s"section ${section.realm}"
        section.integration.let: id => lines += s"  integration $id"
        lines += s"  tree ${Lira.Hash.text(section.tree)}"
        section.delete.stdlib.foreach: path => lines += s"  delete ${path.text}"
        section.derivative.let: hash => lines += s"  derivative ${Lira.Hash.text(hash)}"

        section.requires.stdlib.foreach: requirement =>
          lines += "  requires"
          lines += s"    module ${requirement.module}"
          lines += s"    api ${Lira.Hash.text(requirement.api)}"

          requirement.version.let: version =>
            lines += s"    version ${version.major}.${version.minor}.${version.patch}"

          requirement.uses.let: uses => lines += s"    uses ${Lira.Hash.text(uses)}"

      lines += "payload"
      lines += s"  compression ${payload.compression}"
      lines += s"  length ${payload.length}"
      lines += s"  hash ${Lira.Hash.text(payload.hash)}"

      signature.stdlib.foreach: signature =>
        lines += "signature"
        lines += s"  signer ${signature.signer}"
        lines += s"  algorithm ${signature.algorithm}"
        lines += s"  key ${Lira.Hash.text(signature.key)}"
        lines += s"  value ${signature.value}"

      Text(lines.mkString("", "\n", "\n"))

  // LiraPayload → Lira.Payload
  // The compression envelope (§8.1) around the blob stream. The compressed bytes are not part of
  // any identity: `payload.hash` is the blob-domain hash of the *decompressed* stream (§8.4), so
  // two files differing only in compressor output carry equal implementation identities. The
  // declared decompressed length is enforced as a hard limit (L102) and the declared hash is
  // verified (L105).
  //
  // pneumatic's Brotli engine materializes the whole stream to decode it, so the length cap is
  // currently enforced on the materialized result; when a streaming Brotli decoder lands, this is
  // the seam at which decompression should abort mid-stream instead.
  object Payload:

    def compress(blobStream: Data): Data = blobStream.compress[Brotli]

    def hash(blobStream: Data): Data = Lira.Hash(Lira.Hash.Domain.Blob, blobStream)

    def decompress(compressed: Data, length: Long, declaredHash: Data): Data raises Lira.Error =
      val result =
        try compressed.decompress[Brotli] catch case error: Exception =>
          abort(Lira.Error(Reason.MalformedPayload(t"the payload does not decompress")))

      if result.length.toLong != length then abort(Lira.Error(Reason.PayloadLength(length)))
      if Blob.compare(hash(result), declaredHash) != 0 then abort(Lira.Error(Reason.PayloadHash))

      result

  // LiraRealm → Lira.Realm
  object Realm:
    def parse(keyword: Text): Optional[Lira.Realm] = keyword.s match
      case "jvm"   => Jvm
      case "sjsir" => Sjsir
      case "nir"   => Nir
      case "host"  => Host
      case _       => Unset

  // The realms of the base `lira` schema (§9.4): the three universes of the motivating ecosystem —
  // the library-composition realms a section's content belongs to — and `host`, the one realm that
  // is not a universe, holding a host contract's capability content (hosts.md). The universe
  // vocabulary is open — universes beyond these arrive as TEL schema layers, and sections of
  // unknown realms are held opaque and never materialized.
  enum Realm:
    case Jvm, Sjsir, Nir, Host

    def keyword: Text = this match
      case Jvm   => t"jvm"
      case Sjsir => t"sjsir"
      case Nir   => t"nir"
      case Host  => t"host"

    // Whether independently-published libraries compose in this realm: true of every realm except
    // `host`, whose sections are never materialized onto any artifact path (§13.5).
    def universe: Boolean = this != Host

  // LiraSchemas → Lira.Schemas
  // Hand-encoded `Tels` values for the `lira` schema and its five metadata-blob schemas, following
  // the precedent of `Tels.Axiom`: the Scala literals below are primary, and each mirrors a
  // canonical `.tel` document (at `res/test/reliquary/`) verbatim; the test suite asserts the two
  // stay in agreement and pins each schema's signature as a golden value.
  //
  // The schemas encode the LIRA specification's §14: realms are `jvm | sjsir | nir | host` — the
  // three universes of the motivating ecosystem, and the one realm that is not a universe, which
  // holds a host contract's content (hosts.md); a `Section` is keyed by realm and integration
  // (§9.5), may carry a `derivative` hash (its canonical derived JAR), and may carry `requires`
  // records naming the host contracts its code assumes; a `version` is optional (a release
  // without one is a development release) and strictly numeric; a `Dependency` may be scoped to
  // particular universes or integrations, or pinned to an exact `build` during development; and a
  // `Profile` records the ecosystem predicates a release claims, with the guarantee levels its
  // last step did not preserve (§11.6, §12.4).
  object Schemas:
    import Tels.{Field, Polarity, RecordDefinition, Reference, ScalarDefinition, SelectDefinition,
        SelectRef, Struct, Type, Variant}

    import Polarity.{Implicit, Loose}

    private def field
      ( keyword:    String,
        fieldType:  Type,
        required:   Polarity = Implicit,
        repeatable: Polarity = Implicit )
    :   Field =

      Field(required, repeatable, Text(keyword), fieldType, Unset)

    private def selectRef(reference: String, required: Polarity = Implicit): SelectRef =
      SelectRef(required, Implicit, Text(reference))

    private def record(name: String, members: Tels.Member*): RecordDefinition =
      RecordDefinition(Text(name), Array.from(members), Array.empty[Text])

    private def scalar(name: String, validator: String): ScalarDefinition =
      ScalarDefinition(Text(name), Array(Text(validator)))

    private def select(name: String, variants: Variant*): SelectDefinition =
      SelectDefinition(Text(name), Array.from(variants), Array.empty[Text])

    private def variant(keyword: String): Variant = Variant(Text(keyword), Tels.Flag)

    private val hash:         Type = Reference(t"Hash")
    private val moduleName:   Type = Reference(t"ModuleName")
    private val namespace:    Type = Reference(t"Namespace")
    private val semver:       Type = Reference(t"Semver")
    private val natural:      Type = Reference(t"Natural")
    private val disciplineId: Type = Reference(t"DisciplineId")
    private val identifier:   Type = Reference(t"Identifier")
    private val profileId:    Type = Reference(t"ProfileId")
    private val guarantee:    Type = Reference(t"Guarantee")
    private val string:       Type = Reference(t"String")
    private val treePath:     Type = Reference(t"TreePath")
    private val tagName:      Type = Reference(t"TagName")

    private val hashScalar: ScalarDefinition = scalar("Hash", "base-256-hash")

    // `Tels.Reconstructor.fromTel` prefixes every reconstructed schema's scalars with the TEL
    // built-ins, so the hand-encoded values carry them identically for structural equality.
    private val builtins: Array[ScalarDefinition] = Array(
      scalar("Identifier", "identifier"),
      scalar("TypeName", "type-name"),
      scalar("Sigil", "sigil"),
      scalar("String", "string"))

    val lira: Tels = Tels(
      name     = t"lira",
      document = Struct(
        members = Array(
          field("module", moduleName),
          field("version", semver, required = Loose),
          field("tag", tagName, required = Loose, repeatable = Loose),
          field("lineage", hash, repeatable = Loose),
          field("toolchain", Reference(t"Tool"), repeatable = Loose),
          field("owns", namespace, required = Loose, repeatable = Loose),
          field("resource", Reference(t"Resource"), required = Loose, repeatable = Loose),
          field("api", Reference(t"Api"), repeatable = Loose),
          field("profile", Reference(t"Profile"), required = Loose, repeatable = Loose),
          field("integration", Reference(t"Integration"), required = Loose, repeatable = Loose),
          field("dependency", Reference(t"Dependency"), required = Loose, repeatable = Loose),
          field("delta", hash, required = Loose),
          field("section", Reference(t"Section"), repeatable = Loose),
          field("payload", Reference(t"Payload")),
          field("signature", Reference(t"Signature"), required = Loose, repeatable = Loose)),
        validators = Array.empty[Text]),
      layers   = Array.empty[Tels.Layer],
      sigil    = Unset,
      records  = Array(
        record("Tool",
          field("name", identifier),
          field("version", string),
          field("flag", identifier, required = Loose, repeatable = Loose)),

        record("Api",
          field("discipline", disciplineId),
          field("atoms", hash)),

        record("Resource",
          selectRef("ResourceMode"),
          field("path", treePath)),

        record("Profile",
          field("id", profileId),
          field("breaks", guarantee, required = Loose, repeatable = Loose)),

        record("Integration",
          field("id", identifier),
          field("rank", natural, required = Loose),
          field("label", string, required = Loose)),

        record("Dependency",
          field("module", moduleName),
          field("api", hash),
          field("version", semver, required = Loose),
          field("build", hash, required = Loose),
          field("universe", identifier, required = Loose, repeatable = Loose),
          field("serves", identifier, required = Loose),
          field("integration", identifier, required = Loose, repeatable = Loose),
          field("uses", hash, required = Loose),
          field("spans", hash, required = Loose, repeatable = Loose)),

        record("Requires",
          field("module", moduleName),
          field("api", hash),
          field("version", semver, required = Loose),
          field("uses", hash, required = Loose)),

        record("Section",
          selectRef("Realm"),
          field("integration", identifier, required = Loose),
          field("tree", hash),
          field("delete", string, required = Loose, repeatable = Loose),
          field("derivative", hash, required = Loose),
          field("requires", Reference(t"Requires"), required = Loose, repeatable = Loose)),

        record("Payload",
          field("compression", identifier),
          field("length", natural),
          field("hash", hash)),

        record("Signature",
          field("signer", string),
          field("algorithm", identifier),
          field("key", hash),
          field("value", string))),
      scalars  = Array.frozen(builtins.readable ++ Array(
        hashScalar,
        scalar("ModuleName", "module-name"),
        scalar("Namespace", "namespace"),
        scalar("Semver", "semver"),
        scalar("TagName", "tag-name"),
        scalar("Natural", "natural"),
        scalar("DisciplineId", "discipline-id"),
        scalar("ProfileId", "profile-id"),
        scalar("TreePath", "tree-path"),
        scalar("Guarantee", "guarantee")).readable),
      selects  = Array(
        select("Realm",
          variant("jvm"),
          variant("sjsir"),
          variant("nir"),
          variant("host")),

        select("ResourceMode",
          variant("export"),
          variant("track"),
          variant("scan"))))

    val tree: Tels = Tels(
      name     = t"lira-tree",
      document = Struct(
        members    = Array(field("entry", Reference(t"Entry"),
            required = Loose, repeatable = Loose)),
        validators = Array.empty[Text]),
      layers   = Array.empty[Tels.Layer],
      sigil    = Unset,
      records  = Array(
        record("Entry",
          field("path", Reference(t"TreePath")),
          field("blob", hash))),
      scalars  = Array.frozen(builtins.readable ++ Array(hashScalar, scalar("TreePath", "tree-path")).readable),
      selects  = Array.empty[SelectDefinition])

    val atoms: Tels = Tels(
      name     = t"lira-atoms",
      document = Struct(
        members = Array(
          field("discipline", disciplineId),
          field("atom", Reference(t"Atom"), required = Loose, repeatable = Loose)),
        validators = Array.empty[Text]),
      layers   = Array.empty[Tels.Layer],
      sigil    = Unset,
      records  = Array(
        record("Atom",
          field("class", Reference(t"AtomClass")),
          field("hash", hash),
          field("key", string))),
      scalars  = Array.frozen(builtins.readable ++ Array(
        hashScalar,
        scalar("DisciplineId", "discipline-id"),
        scalar("AtomClass", "atom-class")).readable),
      selects  = Array.empty[SelectDefinition])

    val uses: Tels = Tels(
      name     = t"lira-uses",
      document = Struct(
        members = Array(
          field("module", moduleName),
          field("atom", hash, required = Loose, repeatable = Loose)),
        validators = Array.empty[Text]),
      layers   = Array.empty[Tels.Layer],
      sigil    = Unset,
      records  = Array.empty[RecordDefinition],
      scalars  = Array.frozen(builtins.readable ++ Array(hashScalar, scalar("ModuleName", "module-name")).readable),
      selects  = Array.empty[SelectDefinition])

    val delta: Tels = Tels(
      name     = t"lira-delta",
      document = Struct(
        members = Array(
          field("add", hash, required = Loose, repeatable = Loose),
          field("replace", Reference(t"Replacement"), required = Loose, repeatable = Loose)),
        validators = Array.empty[Text]),
      layers   = Array.empty[Tels.Layer],
      sigil    = Unset,
      records  = Array(
        record("Replacement",
          field("old", hash),
          field("new", hash))),
      scalars  = Array.frozen(builtins.readable ++ Array(hashScalar).readable),
      selects  = Array.empty[SelectDefinition])

    // The capability listing of a host contract with no formal carrier (hosts.md §5): the single
    // tree item at the path `capabilities`, claimed by `capability/1`. Rows are sorted by
    // ascending name with no duplicates; `probe` is advisory and enters no atom.
    val capabilities: Tels = Tels(
      name     = t"lira-capabilities",
      document = Struct(
        members    = Array(field("capability", Reference(t"Capability"),
            required = Loose, repeatable = Loose)),
        validators = Array.empty[Text]),
      layers   = Array.empty[Tels.Layer],
      sigil    = Unset,
      records  = Array(
        record("Capability",
          field("name", identifier),
          field("version", string, required = Loose),
          field("probe", string, required = Loose))),
      scalars  = builtins,
      selects  = Array.empty[SelectDefinition])

    // The BASE-256 schema signatures of the six canonical documents, pinned as golden values (the
    // test suite recomputes each from its `res/test/reliquary/*.tel` mirror and checks agreement).
    // A conforming document of each schema carries its signature on the pragma line.
    val liraSignature:  Text = t"ῘΔìẅḍβlίZOǒžAζȉḠẌLŠῺẃȕЊTȧGƜ2ДNΫΫA"
    val treeSignature:  Text = t"ǨẙơẗỵclϋẁЫĥᾸMôĮẍOώżӯάǢЗĆӸkҚțȐωǢέӫ"
    val atomsSignature: Text = t"2ӪççÃ5AḟǑXϋƤzᾱĺHϕЂẌǒEẂẁĮί9ḀẘΊÐιЪp"
    val usesSignature:  Text = t"şşCȧOӖGҐΪḍḋjΊӁῚƟȐЌĥέȦЬƜδĻĘ1Ȑḟ6ӟÔḍ"
    val deltaSignature: Text = t"gЪΪΞKῺκḢҚdḣulƒjazỲύþῺѝgļEvḞϕϊḟẉtǣ"
    val capabilitiesSignature: Text = t"ẋƒҢιƟžŀæДNGqЌλ1ḞλſẉûÙῡẂȧώẆlώĘdSỲÔ"

  // LiraTree → Lira.Tree
  object Tree:
    val empty: Lira.Tree = Lira.Tree(List())

    // Establishes the §9.2 invariants: rows sorted in ascending bytewise UTF-8 path order, paths
    // unique. Accepts entries in any order; sorting here is what makes tree serialization a pure
    // function of the mapping.
    def of(entries: List[TreeEntry]): Lira.Tree raises Lira.Error =
      val sorted = entries.stdlib.sortWith: (a, b) => TreePath.compare(a.path, b.path) < 0

      sorted.zip(sorted.drop(1)).foreach: (a, b) =>
        if a.path == b.path
        then abort(Lira.Error(Reason.InvalidTree(t"the path ${a.path.text} appears twice")))

      Lira.Tree(List.from(sorted))

    // Parses and checks a Tree metadata blob: a TEL document under the `lira-tree` schema, whose
    // pragma carries that schema's signature.
    def decode(data: Data): Lira.Tree raises Lira.Error =
      given Tel.Validator.Registry = Lira.Validators.registry

      import Tels.Decoder.validate

      val document =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case Tel.Error(reason, _) =>
            Lira.Error(Reason.InvalidTree(t"the document is invalid: $reason"))

        . protect:
            val tel = data.read[Tel]
            tel.validate(using Lira.Schemas.tree, Lira.Validators.registry)
            tel

      val compounds = document.childCompounds.readable.filter(_.keyword == t"entry").toVector

      val entries = compounds.map: compound =>
        val atoms = compound.atoms.readable.collect:
          case Tel.Atom.Inline(text, _)  => text
          case Tel.Atom.Source(text)     => text
          case Tel.Atom.Literal(_, text) => text

        if atoms.length != 2
        then abort(Lira.Error(Reason.InvalidTree(t"an entry does not have exactly two atoms")))

        val path = TreePath(atoms(0))

        val hash =
          import errorDiagnostics.emptyDiagnostics

          mitigate:
            case Base256.Error(_) => Lira.Error(Reason.InvalidTree(t"a blob hash is malformed"))

          . protect(Base256.decodeStrict(atoms(1)))

        TreeEntry(path, hash)

      var index = 1

      while index < entries.length do
        val order = TreePath.compare(entries(index - 1).path, entries(index).path)

        val detail =
          if order == 0 then t"the path ${entries(index).path.text} appears twice"
          else t"rows are not in ascending path order"

        if order >= 0 then abort(Lira.Error(Reason.InvalidTree(detail)))
        index += 1

      Lira.Tree(List.from(entries))

  // A section's mapping from paths to blobs (§9.2), with rows in ascending bytewise path order.
  case class Tree private(entries: List[TreeEntry]):
    lazy val index: scala.collection.immutable.Map[Text, TreeEntry] =
      scala.collection.immutable.Map.from:
        entries.stdlib.map: entry => (entry.path.text, entry)

    def get(path: TreePath): Optional[TreeEntry] = index.get(path.text).getOrElse(Unset)

    // The canonical serialization: the pragma line carrying the `lira-tree` schema signature, one
    // `entry` row per mapping in tree order, hard-space separated, LF line endings. Deterministic
    // by construction, so a tree blob's hash is a pure function of the mapping.
    def encode: Data =
      val rows = entries.stdlib.map: entry =>
        s"entry ${entry.path.text}  ${Lira.Hash.text(entry.blob)}"

      val body = rows.mkString("\n")
      val text = Text(s"tel 1.0 ${Lira.Schemas.treeSignature}\n\n$body\n")
      charEncoders.utf8Encoder.encoded(text)

  // LiraValidators → Lira.Validators
  // Scalar validators for the `lira` schema family, composed over the TEL built-ins. Each grammar
  // here is normative for the LIRA specification:
  //
  //  - `base-256-hash`:  exactly 32 characters, each a member of the BASE-256 alphabet
  //  - `module-name`:    kebab-case segments joined by `/` or `.` (e.g. `soundness.dev/gossamer`)
  //  - `namespace`:      dotted package-style segments (letters, digits, `_`; no leading digit)
  //  - `semver`:         exactly `major.minor.patch`, each a natural; no prerelease/build suffixes
  //  - `natural`:        a decimal natural with no superfluous leading zero
  //  - `discipline-id`:  `<kebab-name>/<positive integer>`, e.g. `tasty/1`
  //  - `profile-id`:     the same grammar as a discipline (§11.6), e.g. `jvm/1`
  //  - `guarantee`:      `linkage` or `recompilation` (§11.5; behavior is not certifiable)
  //  - `tree-path`:      relative `/`-separated path; no empty, `.` or `..` segments
  //  - `atom-class`:     `rigid` or `replaceable`
  object Validators:
    import Tel.Validator.{Diagnostic, Registry, Request, Response}

    val registry: Registry = Registry.withFallback:
      new Registry:
        override def apply(request: Request): Response = request match
          case Request.Scalar(method, value) => method.s match
            case "base-256-hash" => base256Hash(value)
            case "module-name"   => moduleName(value)
            case "namespace"     => namespace(value)
            case "semver"        => semver(value)
            case "natural"       => natural(value)
            case "discipline-id" => disciplineId(value)
            case "profile-id"    => profileId(value)
            case "guarantee"     => guarantee(value)
            case "tree-path"     => treePath(value)
            case "atom-class"    => atomClass(value)
            case "tag-name"      => tagName(value)
            case _               => unknown(method)

          case Request.Struct(method, _) => unknown(method)

    private def unknown(method: Text): Response =
      Response.Invalid(Diagnostic.Scalar(t"unknown validator '${method}'"))

    private def fail(message: Text, span: (Int, Int)): Response =
      Response.Invalid(Diagnostic.Scalar(message, span))

    private def base256Hash(value: Text): Response =
      if value.s.length != Lira.Hash.size
      then fail(t"a hash must be exactly ${Lira.Hash.size} BASE-256 characters", (0, value.s.length))
      else safely(Base256.decodeStrict(value)) match
        case Unset => fail(t"the hash contains characters outside the BASE-256 alphabet", (0, 32))
        case _     => Response.Valid

    private def kebabChar(c: Char): Boolean =
      c == '-' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

    // One kebab-case segment: lowercase ASCII letters and digits, single hyphens within.
    private def kebab(s: String): Boolean =
      val shape = !s.isEmpty && !s.startsWith("-") && !s.endsWith("-") && !s.contains("--")
      shape && s.forall(kebabChar)

    private def moduleName(value: Text): Response =
      val s = value.s
      def good(part: String | Null): Boolean = kebab(part.nn)

      if s.isEmpty then fail(t"the module name must not be empty", (0, 0))
      else if !s.split("[/.]", -1).nn.forall(good)
      then fail(t"each `/`- or `.`-separated segment must be kebab-case", (0, s.length))
      else Response.Valid

    // A tag name (§12.6): a letter followed by letters, digits, `-` and `.` — `jdk-19`,
    // `scala-3.9`.
    private def tagName(value: Text): Response =
      val s = value.s

      def tagChar(c: Char): Boolean =
        c == '-' || c == '.' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')

      if s.isEmpty then fail(t"the tag must not be empty", (0, 0))
      else if !s.charAt(0).isLetter
      then fail(t"a tag must start with a letter", (0, 1))
      else if !s.forall(tagChar)
      then fail(t"a tag may contain only letters, digits, `-` and `.`", (0, s.length))
      else Response.Valid

    private def namespaceChar(c: Char): Boolean =
      c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    private def namespace(value: Text): Response =
      val s = value.s

      def segment(part: String | Null): Boolean =
        val text = part.nn
        val leading = !text.isEmpty && !(text.charAt(0) >= '0' && text.charAt(0) <= '9')
        leading && text.forall(namespaceChar)

      if s.isEmpty then fail(t"the namespace must not be empty", (0, 0))
      else if !s.split("\\.", -1).nn.forall(segment)
      then fail(t"each dotted segment must be a package-style identifier", (0, s.length))
      else Response.Valid

    private def digit(c: Char): Boolean = c >= '0' && c <= '9'

    private def naturalNumber(s: String | Null): Boolean =
      val text = s.nn
      !text.isEmpty && text.forall(digit) && (text == "0" || !text.startsWith("0"))

    private def natural(value: Text): Response =
      if naturalNumber(value.s) then Response.Valid
      else fail(t"a natural number with no leading zero is required", (0, value.s.length))

    private def semver(value: Text): Response =
      val parts = value.s.split("\\.", -1).nn

      if parts.length != 3 || !parts.forall(naturalNumber)
      then fail(t"the version must be `major.minor.patch`, each a natural", (0, value.s.length))
      else Response.Valid

    private def disciplineId(value: Text): Response =
      val parts = value.s.split("/", -1).nn

      if parts.length != 2 || !kebab(parts(0).nn) || !naturalNumber(parts(1)) || parts(1) == "0"
      then fail(t"a discipline is identified as `<name>/<positive integer>`", (0, value.s.length))
      else Response.Valid

    // §11.6: a profile is identified on the same terms as a discipline, and must likewise bump its
    // version on any change to a predicate.
    private def profileId(value: Text): Response =
      val parts = value.s.split("/", -1).nn

      if parts.length != 2 || !kebab(parts(0).nn) || !naturalNumber(parts(1)) || parts(1) == "0"
      then fail(t"a profile is identified as `<name>/<positive integer>`", (0, value.s.length))
      else Response.Valid

    // §11.5 names three guarantee levels, but only two can be claimed or broken: behavior is not
    // certified by any hash scheme, so it is not expressible in a `breaks` field.
    private def guarantee(value: Text): Response =
      if value.s == "linkage" || value.s == "recompilation" then Response.Valid
      else fail(t"a guarantee level is `linkage` or `recompilation`", (0, value.s.length))

    private def treePath(value: Text): Response =
      val s = value.s
      def segment(part: String | Null): Boolean = !part.nn.isEmpty && part != "." && part != ".."

      if s.isEmpty then fail(t"the path must not be empty", (0, 0))
      else if !s.split("/", -1).nn.forall(segment)
      then fail(t"the path must be relative, with no empty, `.` or `..` segments", (0, s.length))
      else Response.Valid

    private def atomClass(value: Text): Response =
      if value.s == "rigid" || value.s == "replaceable" then Response.Valid
      else fail(t"the atom class must be `rigid` or `replaceable`", (0, value.s.length))

// A read `.lira` file: the typed manifest, the parsed TEL document it projects (the semantic
// model that signing and reserialization operate on), and the still-compressed payload.
case class Lira(manifest: Lira.Manifest, tel: Tel, compressed: Data)
