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
import enigmatic.Signing
import fulminate.*
import gossamer.*
import hieroglyph.*
import stratiform.*
import turbulence.*
import vacuous.*

import Lira.Error.Reason

// Manifest signing (§15). The signed message is
//
//   hash("lira/1:manifest", BinTEL(manifest with all signature fields removed))
//
// Signing the canonical BinTEL encoding — never source text — makes signatures immune to
// reformatting, and removing the signature fields first means signing and counter-signing never
// perturb the signed bytes. The payload is covered transitively through `payload.hash`; every
// metadata blob and section is covered through the hash tower.
object ManifestSigning:

  // The verifier's trusted public keys, matched by `lira/1:key` fingerprint. Key distribution
  // is out of band (§15.3); keys are in the algorithm's standard encoding as produced by
  // enigmatic (SPKI for ML-DSA).
  case class Keyring(keys: List[Data]):
    def find(fingerprint: Data): Optional[Data] =
      def matches(key: Data): Boolean =
        Blob.compare(fingerprint, ManifestSigning.fingerprint(key)) == 0

      keys.stdlib.find(matches).getOrElse(Unset)

  def fingerprint(publicKey: Data): Data = Lira.Hash(Lira.Hash.Domain.Key, publicKey)

  // The signing input: the manifest's semantic model, minus signatures, canonically encoded.
  // The typed manifest is re-rendered and re-assigned rather than mutated as presentation —
  // BinTEL encodes only the semantic model, so this is signature-equivalent for every
  // conforming manifest of the base schema.
  def input(manifest: Lira.Manifest): Data raises Lira.Error =
    val stripped = manifest.copy(signature = List())
    val data = charEncoders.utf8Encoder.encoded(stripped.render)

    val bytes =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case _: Tel.Error    => Lira.Error(Reason.InvalidManifest(t"the manifest does not re-parse"))
        case _: Bintel.Error => Lira.Error(Reason.InvalidManifest(t"the manifest does not encode"))

      . protect:
          val document = data.utf8.load[Tel]
          val element = Tel.Type.assign(document.root, Lira.Schemas.lira)
          Bintel.encode(element, Lira.Schemas.lira)

    Lira.Hash(Lira.Hash.Domain.Manifest, bytes)

  // Appends one signature record; the existing records are untouched, so co-signing is stable.
  def sign
    ( manifest:   Lira.Manifest,
      signer:     Text,
      algorithm:  Text,
      scheme:     Signing,
      privateKey: Data,
      publicKey:  Data )
  :   Lira.Manifest raises Lira.Error =

    val value = Base256.encode(scheme.sign(input(manifest), privateKey))
    val record = Lira.Manifest.Signature(signer, algorithm, fingerprint(publicKey), value)
    manifest.copy(signature = List.from(manifest.signature.stdlib :+ record))

  // Verification step 7 (§16): every signature present must verify; a signature whose algorithm
  // the verifier does not implement is rejected, never ignored (§15.1).
  def verify(manifest: Lira.Manifest, keyring: Keyring, scheme: Text => Optional[Signing])
  :   Unit raises Lira.Error =

    val message = input(manifest)

    manifest.signature.stdlib.foreach: record =>
      val signing = scheme(record.algorithm) match
        case signing: Signing => signing
        case _                => abort(Lira.Error(Reason.UnknownAlgorithm(record.algorithm)))

      val publicKey = keyring.find(record.key).or:
        abort(Lira.Error(Reason.UnknownKey(Lira.Hash.text(record.key))))

      val signature =
        import errorDiagnostics.emptyDiagnostics

        mitigate:
          case _: Base256.Error => Lira.Error(Reason.BadSignature(record.signer))

        . protect(Base256.decodeStrict(record.value))

      if !signing.verify(message, signature, publicKey)
      then abort(Lira.Error(Reason.BadSignature(record.signer)))
