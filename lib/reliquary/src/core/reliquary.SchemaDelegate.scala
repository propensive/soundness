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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package reliquary

import anticipation.*
import contingency.*
import denominative.nil
import distillate.*
import enigmatic.Signing
import gossamer.*
// By-name rather than `rudiments.*`: this file indexes a frozen `Data` array by `Int`
// (`bytesEqual` below), which the wildcard's `Deindex` extension makes unresolvable.
import revolution.Semver
import rudiments.{distinct, filter, has, map, seek}
import stratiform.*
import symbolism.*
import turbulence.*
import vacuous.*

// The LIRA-backed step 4 of TEL schema resolution (tel.md §8.2): the
// `Tels.Resolution.Delegate` implementation serving `:version`/`:tag`
// references and signature-form lookups from LIRA releases.
//
// A TEL schema module follows the `tels/1` discipline: its payload is
// the single tree item at the fixed path `schema.tel`, conforming to
// the `tels` meta-schema, published under the module name the schema
// declares. Every answer is grounded in a verified release: the
// manifest signature is checked first — a store index is a cache, not
// an authority, and an unsigned manifest is an unpublished development
// release that no selector can match — and the payload is materialized
// through `Verification.install` before the schema body is extracted.
object SchemaDelegate:
  // The tels/1 fixed payload path.
  val schemaPath: Text = t"schema.tel"

  // The release-store seam: reliquary keeps releases as in-memory
  // `Lira` values, so the resolver is parameterized over whatever holds
  // them locally. The same trait is the extension point for network
  // resolution, which arrives in a later round.
  trait Releases:
    def apply(module: Text): List[Lira]
    def modules: List[Text]

case class SchemaDelegate
  ( local:   SchemaDelegate.Releases,
    keyring: ManifestSigning.Keyring,
    schemes: Text => Optional[Signing],
    network: Optional[SchemaDelegate.Releases] = Unset )
extends Tels.Resolution.Delegate:

  import Tels.Resolution.{Body, Error as ResolutionError}
  import ResolutionError.Reason

  def bySelector(reference: Tel.Pragma.Reference): Optional[Body] raises ResolutionError =
    val candidates = releasesOf(t"${reference.domain}/${reference.name}")

    val matching: List[Lira] = reference.selector match
      case Tel.Pragma.Reference.Selector.Version(major, minor, patch) =>
        candidates.filter: lira =>
          // The `Optional` field is bound to a typed local as the lambda's first statement:
          // reading it directly inside a combinator lambda trips the `wildApprox` assertion.
          val declared: Optional[Semver] = lira.manifest.version

          declared.let: version =>
            version.major == major && version.minor == minor && version.patch == patch

          . or(false)

      case Tel.Pragma.Reference.Selector.Tag(name) =>
        candidates.filter(_.manifest.tag.has(name))

      // A bare reference is local-only by design and never reaches the
      // delegate; answering nothing keeps that invariant even if called.
      case _ =>
        Nil

    matching match
      case Nil            => Unset
      case release :: Nil => Body(schemaBody(release))

      case _ =>
        // A version names exactly one published release and tags are
        // signed, unique and immutable within a module (L117, L142), so
        // two matches mean the store's content cannot be trusted.
        abort(ResolutionError(Reason.Unverified(
          t"more than one release matches the selector ${reference.text}")))

  def bySignature(signature: Data, reference: Optional[Tel.Pragma.Reference])
  :   Optional[Body] raises ResolutionError =

    val modules: List[Text] = reference match
      case reference: Tel.Pragma.Reference =>
        List(t"${reference.domain}/${reference.name}")

      case _ =>
        // Annotated so `+`'s result type is pinned before `distinct`'s implicit search runs.
        val known: List[Text] = local.modules + network.let(_.modules).or(List())
        known.distinct

    var found: Optional[Data] = Unset
    var candidates = 0

    // The two loops below stop at the first hit, which needs a stepwise `Iterator`; the native
    // `List` has no cursor of its own, so the traversal crosses through `stdlib` here.
    val moduleIterator = modules.stdlib.iterator

    while moduleIterator.hasNext && found.absent do
      val releaseIterator = releasesOf(moduleIterator.next()).stdlib.iterator

      while releaseIterator.hasNext && found.absent do
        // Skip releases without a tels payload or failing verification
        // when sweeping; with a reference, failures still surface as
        // the disagreement check below.
        safely(schemaBody(releaseIterator.next())) match
          case body: Data =>
            candidates += 1
            if serves(body, signature) then found = body

          case _ => ()

    found match
      case body: Data =>
        Body(body)

      case _ =>
        // The signature is authoritative: a reference whose lineage
        // holds schema releases, none serving the signature, disagrees
        // with it, and resolution must fail rather than fall back.
        if reference.present && candidates > 0
        then abort(ResolutionError(Reason.ReferenceDisagrees))
        else Unset

  // Whether a schema body's component hashes decompose the claimed
  // signature exactly: the decoded sequence must re-encode to the
  // identical palimpsest.
  private def serves(body: Data, signature: Data): Boolean =
    safely:
      val tel = body.read[Tel]
      val components = SchemaSignature.componentHashes(tel, Tels.Axiom.tels)
      val decoded = SchemaSignature.decode(signature, components(0) :: components(1))
      bytesEqual(SchemaSignature.encode(decoded), signature)
    . or(false)

  private def releasesOf(module: Text): List[Lira] =
    local(module) + network.let(_(module)).or(List())

  // Verify the release's manifest signature — mandatory, and vacuous
  // verification is not enough: an unsigned manifest is an unpublished
  // development release — then materialize the payload and extract the
  // `schema.tel` body, checked against the tels meta-schema.
  private def schemaBody(lira: Lira): Data raises ResolutionError =
    import fulminate.errorDiagnostics.emptyDiagnostics

    if lira.manifest.signature.nil
    then abort(ResolutionError(Reason.Unverified(
      t"the release manifest of ${lira.manifest.module} carries no signature")))

    val body: Data =
      mitigate:
        case error: Lira.Error => ResolutionError(Reason.Unverified(error.message.text))

      . protect:
          ManifestSigning.verify(lira.manifest, keyring, schemes)
          val report = Verification.install(lira)
          val path = TreePath(SchemaDelegate.schemaPath)

          val found: List[Optional[TreeEntry]] = report.materialized.map(_(1).get(path))
          val entry: Optional[TreeEntry] = found.seek(_.present)

          entry match
            case entry: TreeEntry => report.blobstore.resolve(entry.blob)
            case _ => abort(ResolutionError(Reason.NotSchema(
              t"the release ${lira.manifest.module} has no ${SchemaDelegate.schemaPath} payload")))

    mitigate:
      case error: Tel.Error =>
        ResolutionError(Reason.NotSchema(error.message.text))

    . protect:
        import Tels.Decoder.validate
        body.read[Tel].validate(using Tels.Axiom.tels)

    body

  private def bytesEqual(a: Data, b: Data): Boolean =
    a.length == b.length && {
      var i     = 0
      var equal = true

      while equal && i < a.length do
        if a(i) != b(i) then equal = false
        i += 1

      equal
    }
