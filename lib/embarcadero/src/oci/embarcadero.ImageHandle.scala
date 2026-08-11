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
package embarcadero

import scala.caps
import proscenium.compat.*

import anticipation.*
import aperture.*
import bitumen.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import jacinta.*
import pneumatic.*
import prepositional.*
import turbulence.*
import vacuous.*
import zephyrine.*, parsing.trackPositions

// The scoped capability provided by opening an oci-archive as `Image`:
// `path.open[Image]()`. The entries are materialized from bitumen's sequential reader,
// so blobs are found by name over the entry list; their memoized bodies are retained
// for the scope's duration, the flat cost of TAR's sequentiality. Archives written by
// `Image.archive` place `index.json` before the blobs, so the metadata path is cheap.
class ImageHandle private[embarcadero] (entries: List[Tar.Entry])
extends caps.ExclusiveCapability:

  // The body of the blob addressed by a canonical `sha256:<hex>` digest: its stored
  // (for layers: compressed) chunks — undecoded and unverified.
  private def body(digest: Text)(using Tactic[OciError]): Tar.Body =
    if !digest.s.startsWith("sha256:")
    then abort(OciError(OciError.Reason.UnsupportedDigest(digest.cut(t":").stdlib.head)))

    val name = t"blobs/sha256/${digest.s.stripPrefix("sha256:").tt}"

    entries.stdlib.collectFirst { case file: Tar.Entry.File if file.entryName == name => file.data }
    . getOrElse(abort(OciError(OciError.Reason.MissingBlob(digest))))

  // The blob addressed by a canonical `sha256:<hex>` digest, as a stream of its stored
  // (for layers: compressed) chunks — undecoded and unverified. (An explicit `Tactic`
  // rather than `raises` sugar: a fresh capability in a context-function result cannot
  // flow to a forwarding caller.)
  def blob(digest: Text)(using Tactic[OciError]): (Stream[Data] over Credit)^ =
    body(digest).stream

  // The decoded top-level index, after validating the `oci-layout` marker.
  def index(using Tactic[OciError]): Index =
    val layoutBytes = document(t"oci-layout", OciError.Reason.MissingLayout)

    val layout = decode(t"oci-layout"):
      import strategies.throwUnsafely
      layoutBytes.read[Json].as[OciLayout]

    if !layout.imageLayoutVersion.s.startsWith("1.")
    then abort(OciError(OciError.Reason.UnsupportedLayout(layout.imageLayoutVersion)))

    val indexBytes = document(t"index.json", OciError.Reason.MissingIndex)

    decode(t"index.json"):
      import strategies.throwUnsafely
      indexBytes.read[Json].as[Index]

  // The index's first manifest, or the one a descriptor selects; the manifest blob is
  // digest-verified against its descriptor before decoding.
  def manifest(using Tactic[OciError]): Oci.Manifest =
    val descriptor =
      index.manifests.stdlib.headOption.getOrElse(abort(OciError(OciError.Reason.NoManifest)))

    manifest(descriptor)

  def manifest(descriptor: Descriptor)(using Tactic[OciError]): Oci.Manifest =
    val bytes = verified(descriptor)

    decode(descriptor.digest):
      import strategies.throwUnsafely
      bytes.read[Json].as[Oci.Manifest]

  // The decoded image config for a manifest (by default, the first).
  def imageConfig(using Tactic[OciError]): ImageConfig = imageConfig(manifest)

  def imageConfig(manifest: Oci.Manifest)(using Tactic[OciError]): ImageConfig =
    val bytes = verified(manifest.config)

    decode(manifest.config.digest):
      import strategies.throwUnsafely
      bytes.read[Json].as[ImageConfig]

  // The decoded Wasm artifact config for a manifest (by default, the first).
  def wasmConfig(using Tactic[OciError]): WasmConfig = wasmConfig(manifest)

  def wasmConfig(manifest: Oci.Manifest)(using Tactic[OciError]): WasmConfig =
    val bytes = verified(manifest.config)

    decode(manifest.config.digest):
      import strategies.throwUnsafely
      bytes.read[Json].as[WasmConfig]

  // The decoded config blob for a manifest (by default, the first), in whichever form the
  // config descriptor's media type says it takes. This is the entry point for a reader
  // that does not already know which kind of artifact it has opened — a runtime deciding
  // whether to unpack a rootfs or instantiate a component.
  def config(using Tactic[OciError]): Oci.Config = config(manifest)

  def config(manifest: Oci.Manifest)(using Tactic[OciError]): Oci.Config =
    if manifest.config.mediaType == media"application/vnd.wasm.config.v0+json"
    then wasmConfig(manifest) else imageConfig(manifest)

  // A layer's stored blob, verbatim: for OCI layers, the gzip-compressed tar.
  def compressed(descriptor: Descriptor)(using Tactic[OciError])
  :   (Stream[Data] over Credit)^ =

    blob(descriptor.digest)

  // A layer's content as the uncompressed tar byte stream, decompressing according to
  // the descriptor's media type; unrecognised types stream verbatim.
  def layer(descriptor: Descriptor)(using Tactic[OciError]): (Stream[Data] over Credit)^ =
    if descriptor.mediaType.suffixes.stdlib.contains(Media.Suffix.Gzip)
    then compressed(descriptor).decompress[Gzip]
    else compressed(descriptor)

  // A blob gathered eagerly and checked against its descriptor's digest and size — the
  // opt-in verified path, since checking a stream would force draining it.
  def verified(descriptor: Descriptor)(using Tactic[OciError]): Data =
    val bytes = body(descriptor.digest).memoize
    val digest = sha256(bytes)

    if digest != descriptor.digest
    then abort(OciError(OciError.Reason.DigestMismatch(descriptor.digest, digest)))

    if bytes.length.toLong != descriptor.size
    then abort:
      OciError:
        OciError.Reason.InvalidBlob
          ( descriptor.digest, t"its size is ${bytes.length}, not ${descriptor.size}" )

    bytes

  // The gathered bytes of a named top-level document (`oci-layout` or `index.json`).
  private def document(name: Text, reason: OciError.Reason)(using Tactic[OciError]): Data =
    entries.stdlib.collectFirst { case file: Tar.Entry.File if file.entryName == name => file.data }
    . getOrElse(abort(OciError(reason)))
    . memoize

  // Runs a JSON decode, translating any failure — parse, JSON or media-type errors,
  // thrown under the call site's `throwUnsafely` — to an `InvalidBlob` on the given
  // label. The decoder is derived under a throwing strategy because the derivation's
  // codec thunks cannot capture a scoped tactic capability.
  private def decode[doc](label: Text)(body: => doc)(using Tactic[OciError]): doc =
    try body catch case error: Error =>
      abort(OciError(OciError.Reason.InvalidBlob(label, error.message.text)))

// The `oci-layout` marker document at the archive root.
private[embarcadero] case class OciLayout(imageLayoutVersion: Text)

// A named class rather than an anonymous given instance, for the reasons documented on
// galilei's `FileOpenable`. Opening in-memory `Data` as an OCI image; opening a filesystem
// *path* (`ImageOpenable`) lives in the JVM-only source set. Read-only for now.
class ImageDataOpenable(using Tactic[OciError], Tactic[Tar.Error], Tactic[StreamError])
extends Openable:

  type Self = Data
  type Form = Image
  type Operand = Nothing
  type Result = ImageHandle

  def open[grants <: Grant, result]
    ( value: Data, mode: Mode granting grants, flags: List[Nothing] )
    ( block: ((ImageHandle & Granting[grants])^) ?=> result )
  :   result =

    if mode.atoms.stdlib.contains(Write) then abort(OciError(OciError.Reason.WriteUnsupported))
    block(using new ImageHandle(Tarfile.read(value.stream).to(List).asInstanceOf[List[bitumen.Tar.Entry]]) with Granting[grants] {})
