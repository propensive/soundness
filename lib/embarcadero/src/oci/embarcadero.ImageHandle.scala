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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import anticipation.*
import aperture.*
import bitumen.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import jacinta.*
import prepositional.*
import turbulence.*
import vacuous.*
import zephyrine.*, parsing.trackPositions

// The scoped capability provided by opening an oci-archive as `Image`:
// `path.open[Image]()`. The entries come from bitumen's memoized sequential reader, so
// blobs are found by name over the entry list; scanning past a blob retains its chunks
// for the scope's duration, the flat cost of TAR's sequentiality. Archives written by
// `Image.archive` place `index.json` before the blobs, so the metadata path is cheap.
class ImageHandle private[embarcadero] (entries: LazyList[Tar.Entry])
extends caps.ExclusiveCapability:

  // The blob addressed by a canonical `sha256:<hex>` digest, as a stream of its stored
  // (for layers: compressed) chunks — undecoded and unverified.
  def blob(digest: Text): LazyList[Data] raises OciError =
    if !digest.s.startsWith("sha256:")
    then abort(OciError(OciError.Reason.UnsupportedDigest(digest.cut(t":").head)))

    val name = t"blobs/sha256/${digest.s.stripPrefix("sha256:").tt}"

    entries.collectFirst { case file: Tar.Entry.File if file.entryName == name => file.data }
    . getOrElse(abort(OciError(OciError.Reason.MissingBlob(digest))))

  // The decoded top-level index, after validating the `oci-layout` marker.
  def index: Index raises OciError =
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
  def manifest: Oci.Manifest raises OciError =
    val descriptor =
      index.manifests.headOption.getOrElse(abort(OciError(OciError.Reason.NoManifest)))

    manifest(descriptor)

  def manifest(descriptor: Descriptor): Oci.Manifest raises OciError =
    val bytes = verified(descriptor)

    decode(descriptor.digest):
      import strategies.throwUnsafely
      bytes.read[Json].as[Oci.Manifest]

  // The decoded image config for a manifest (by default, the first).
  def imageConfig: ImageConfig raises OciError = imageConfig(manifest)

  def imageConfig(manifest: Oci.Manifest): ImageConfig raises OciError =
    val bytes = verified(manifest.config)

    // Field-wise, not `as[ImageConfig]`: jacinta's derivation currently fails on an
    // `Optional` *product* field (`config`) inside a derived product decoder, though
    // each field type decodes directly.
    decode(manifest.config.digest):
      import strategies.throwUnsafely
      import dynamicJsonAccess.enabled
      val json = bytes.read[Json]

      ImageConfig
        ( architecture = json.architecture.as[Text],
          os           = json.os.as[Text],
          rootfs       = json.rootfs.as[RootFs],
          config       = json.config.as[Optional[ContainerConfig]],
          history      = json.history.as[Optional[List[History]]],
          created      = json.created.as[Optional[Text]],
          author       = json.author.as[Optional[Text]] )

  // A layer's stored blob, verbatim: for OCI layers, the gzip-compressed tar.
  def compressed(descriptor: Descriptor): LazyList[Data] raises OciError =
    blob(descriptor.digest)

  // A layer's content as the uncompressed tar byte stream, decompressing according to
  // the descriptor's media type; unrecognised types stream verbatim.
  def layer(descriptor: Descriptor): LazyList[Data] raises OciError =
    if descriptor.mediaType.suffixes.contains(Media.Suffix.Gzip)
    then compressed(descriptor).decompress[Gzip]
    else compressed(descriptor)

  // A blob gathered eagerly and checked against its descriptor's digest and size — the
  // opt-in verified path, since checking a stream would force draining it.
  def verified(descriptor: Descriptor): Data raises OciError =
    val bytes = gather(blob(descriptor.digest))
    val digest = sha256(bytes)

    if digest != descriptor.digest
    then abort(OciError(OciError.Reason.DigestMismatch(descriptor.digest, digest)))

    if bytes.length.toLong != descriptor.size
    then abort:
      OciError:
        OciError.Reason.InvalidBlob
          ( descriptor.digest, t"its size is ${bytes.length}, not ${descriptor.size}" )

    bytes

  private def gather(stream: LazyList[Data]): Data =
    stream.foldLeft(IArray.empty[Byte])(_ ++ _)

  // The gathered bytes of a named top-level document (`oci-layout` or `index.json`).
  private def document(name: Text, reason: OciError.Reason): Data raises OciError =
    val bytes =
      entries.collectFirst { case file: Tar.Entry.File if file.entryName == name => file.data }
      . getOrElse(abort(OciError(reason)))

    gather(bytes)

  // Runs a JSON decode, translating any failure — parse, JSON or media-type errors,
  // thrown under the call site's `throwUnsafely` — to an `InvalidBlob` on the given
  // label. The decoder is derived under a throwing strategy because the derivation's
  // codec thunks cannot capture a scoped tactic capability.
  private def decode[doc](label: Text)(body: => doc): doc raises OciError =
    try body catch case error: Error =>
      abort(OciError(OciError.Reason.InvalidBlob(label, error.message.text)))

// The `oci-layout` marker document at the archive root.
private[embarcadero] case class OciLayout(imageLayoutVersion: Text)

// Named classes rather than anonymous given instances, for the reasons documented on
// galilei's `FileOpenable`. Archives open read-only: a `Write` mode is refused with
// `OciError.Reason.WriteUnsupported`. The TAR bracket is delegated to bitumen's
// `TarOpenable`, which holds the file open for the scope's duration.
class ImageOpenable[path: Abstractable across Paths to Text]
  ( using Tactic[OciError], Tactic[TarError], Tactic[StreamError] )
extends Openable:

  type Self = path
  type Form = Image
  type Operand = Nothing
  type Result = ImageHandle

  def open[grants <: Grant, result]
    ( value: path, mode: Mode granting grants, flags: List[Nothing] )
    ( block: ((ImageHandle & Granting[grants])^) ?=> result )
  :   result =

    if mode.atoms.contains(Write) then abort(OciError(OciError.Reason.WriteUnsupported))

    TarOpenable[path]().open(value, mode, Nil): tar ?=>
      block(using new ImageHandle(tar.entries) with Granting[grants] {})

class ImageDataOpenable(using Tactic[OciError], Tactic[TarError], Tactic[StreamError])
extends Openable:

  type Self = Data
  type Form = Image
  type Operand = Nothing
  type Result = ImageHandle

  def open[grants <: Grant, result]
    ( value: Data, mode: Mode granting grants, flags: List[Nothing] )
    ( block: ((ImageHandle & Granting[grants])^) ?=> result )
  :   result =

    if mode.atoms.contains(Write) then abort(OciError(OciError.Reason.WriteUnsupported))
    block(using new ImageHandle(Tarfile.read(LazyList(value))) with Granting[grants] {})
