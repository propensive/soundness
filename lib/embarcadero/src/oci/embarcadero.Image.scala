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

import aperture.*
import fulminate.*
import jacinta.*
import pneumatic.*
import proscenium.compat.*

import anticipation.*
import bitumen.*
import contingency.*
import distillate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import hypotenuse.*
import prepositional.*
import rudiments.map
import scala.caps
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

object Image:
  // Anchored here so `data.open[Image]()` resolves with no import. Opening a filesystem
  // *path* as an image (`path.open[Image]`) lives in the JVM-only source set.
  given dataOpenable
  :   ( ociTactic: Tactic[Oci.Error], tarTactic: Tactic[Tar.Error],
        streamTactic: Tactic[Truncation.Error] )
  =>  ( Image.DataOpenable^{ociTactic, tarTactic, streamTactic} ) =
    Image.DataOpenable()

  // Assembles an image from its layers and optional runtime configuration,
  // computing the config blob, manifest and index (with all digests/sizes).
  def apply
    ( layers:       List[Layer],
      config:       Optional[ContainerConfig] = Unset,
      architecture: Text                      = t"amd64",
      os:           Text                      = t"linux",
      annotations:  Optional[Map[Text, Text]] = Unset )
  :   Image =

    val imageConfig =
      Image.Config
        ( architecture = architecture,
          os           = os,
          rootfs       = RootFs(t"layers", layers.map(_.diffId)),
          config       = config )

    assemble
      ( layers,
        imageConfig,
        render(imageConfig),
        media"application/vnd.oci.image.config.v1+json",
        annotations )

  // Assembles a Wasm OCI Artifact: a single, uncompressed `application/wasm` layer
  // carrying `component`, under a config blob that names the WASI generation and the
  // Component Model interfaces the component exports and imports. The manifest, index
  // and archive are the ordinary OCI ones — only the two media types and the shape of
  // the config document distinguish this from an image with a filesystem.
  def wasm
    ( component:    Data,
      exports:      List[Text]                = Nil,
      imports:      List[Text]                = Nil,
      target:       Optional[Text]            = Unset,
      architecture: Text                      = t"wasm",
      os:           Text                      = t"wasip2",
      annotations:  Optional[Map[Text, Text]] = Unset )
  :   Image =

    val layer = Layer.blob(component, media"application/wasm")

    val wasmConfig =
      WasmConfig
        ( architecture = architecture,
          os           = os,
          layerDigests = List(layer.digest),
          component    = WasmComponent(exports, imports, target) )

    assemble
      ( List(layer),
        wasmConfig,
        render(wasmConfig),
        media"application/vnd.wasm.config.v0+json",
        annotations )

  // The assembly shared by every artifact form: descriptor the config blob, wrap it and
  // the layers in a manifest, and name that manifest from a single-entry index.
  private def assemble
    ( layers:      List[Layer],
      config:      Oci.Config,
      configBytes: Data,
      configType:  MediaType,
      annotations: Optional[Map[Text, Text]] )
  :   Image =

    val configDescriptor = descriptorOf(configType, configBytes)

    val manifestType = media"application/vnd.oci.image.manifest.v1+json"

    val manifest =
      Oci.Manifest(2, manifestType, configDescriptor, layers.map(_.descriptor), annotations)

    val manifestBytes      = render(manifest)
    val manifestDescriptor = descriptorOf(manifestType, manifestBytes)

    val indexType  = media"application/vnd.oci.image.index.v1+json"
    val index      = Index(2, indexType, List(manifestDescriptor))
    val indexBytes = render(index)

    Image(layers, config, configBytes, configDescriptor, manifest, manifestBytes,
        manifestDescriptor, index, indexBytes)

  // ImageConfig → Image.Config
  // An OCI image config document (media type `MediaType.config`). It records the
  // target platform, the rootfs `diff_id` chain, and the optional runtime
  // configuration and build history.
  case class Config
    ( architecture: Text,
      os:           Text,
      rootfs:       RootFs,
      config:       Optional[ContainerConfig] = Unset,
      history:      Optional[List[History]]   = Unset,
      created:      Optional[Text]            = Unset,
      author:       Optional[Text]            = Unset )
  derives CanEqual

  // ImageHandle → Image.Handle
  // The scoped capability provided by opening an oci-archive as `Image`:
  // `path.open[Image]()`. The entries are materialized from bitumen's sequential reader,
  // so blobs are found by name over the entry list; their memoized bodies are retained
  // for the scope's duration, the flat cost of TAR's sequentiality. Archives written by
  // `Image.archive` place `index.json` before the blobs, so the metadata path is cheap.
  class Handle private[embarcadero] (entries: List[Tar.Entry])
  extends caps.ExclusiveCapability:

    // The body of the blob addressed by a canonical `sha256:<hex>` digest: its stored
    // (for layers: compressed) chunks — undecoded and unverified.
    private def body(digest: Text)(using Tactic[Oci.Error]): Tar.Body =
      if !digest.s.startsWith("sha256:")
      then abort(Oci.Error(Oci.Error.Reason.UnsupportedDigest(digest.cut(t":").stdlib.head)))

      val name = t"blobs/sha256/${digest.s.stripPrefix("sha256:").tt}"

      entries.stdlib.collectFirst { case file: Tar.Entry.File if file.entryName == name => file.data }
      . getOrElse(abort(Oci.Error(Oci.Error.Reason.MissingBlob(digest))))

    // The blob addressed by a canonical `sha256:<hex>` digest, as a stream of its stored
    // (for layers: compressed) chunks — undecoded and unverified. (An explicit `Tactic`
    // rather than `raises` sugar: a fresh capability in a context-function result cannot
    // flow to a forwarding caller.)
    def blob(digest: Text)(using Tactic[Oci.Error]): (Stream[Data] over Credit)^ =
      body(digest).stream

    // The decoded top-level index, after validating the `oci-layout` marker.
    def index(using Tactic[Oci.Error]): Index =
      val layoutBytes = document(t"oci-layout", Oci.Error.Reason.MissingLayout)

      val layout = decode(t"oci-layout"):
        import strategies.throwUnsafely
        layoutBytes.read[Json].as[OciLayout]

      if !layout.imageLayoutVersion.s.startsWith("1.")
      then abort(Oci.Error(Oci.Error.Reason.UnsupportedLayout(layout.imageLayoutVersion)))

      val indexBytes = document(t"index.json", Oci.Error.Reason.MissingIndex)

      decode(t"index.json"):
        import strategies.throwUnsafely
        indexBytes.read[Json].as[Index]

    // The index's first manifest, or the one a descriptor selects; the manifest blob is
    // digest-verified against its descriptor before decoding.
    def manifest(using Tactic[Oci.Error]): Oci.Manifest =
      val descriptor =
        index.manifests.stdlib.headOption.getOrElse(abort(Oci.Error(Oci.Error.Reason.NoManifest)))

      manifest(descriptor)

    def manifest(descriptor: Descriptor)(using Tactic[Oci.Error]): Oci.Manifest =
      val bytes = verified(descriptor)

      decode(descriptor.digest):
        import strategies.throwUnsafely
        bytes.read[Json].as[Oci.Manifest]

    // The decoded image config for a manifest (by default, the first).
    def imageConfig(using Tactic[Oci.Error]): Image.Config = imageConfig(manifest)

    def imageConfig(manifest: Oci.Manifest)(using Tactic[Oci.Error]): Image.Config =
      val bytes = verified(manifest.config)

      decode(manifest.config.digest):
        import strategies.throwUnsafely
        bytes.read[Json].as[Image.Config]

    // The decoded Wasm artifact config for a manifest (by default, the first).
    def wasmConfig(using Tactic[Oci.Error]): WasmConfig = wasmConfig(manifest)

    def wasmConfig(manifest: Oci.Manifest)(using Tactic[Oci.Error]): WasmConfig =
      val bytes = verified(manifest.config)

      decode(manifest.config.digest):
        import strategies.throwUnsafely
        bytes.read[Json].as[WasmConfig]

    // The decoded config blob for a manifest (by default, the first), in whichever form the
    // config descriptor's media type says it takes. This is the entry point for a reader
    // that does not already know which kind of artifact it has opened — a runtime deciding
    // whether to unpack a rootfs or instantiate a component.
    def config(using Tactic[Oci.Error]): Oci.Config = config(manifest)

    def config(manifest: Oci.Manifest)(using Tactic[Oci.Error]): Oci.Config =
      if manifest.config.mediaType == media"application/vnd.wasm.config.v0+json"
      then wasmConfig(manifest) else imageConfig(manifest)

    // A layer's stored blob, verbatim: for OCI layers, the gzip-compressed tar.
    def compressed(descriptor: Descriptor)(using Tactic[Oci.Error])
    :   (Stream[Data] over Credit)^ =

      blob(descriptor.digest)

    // A layer's content as the uncompressed tar byte stream, decompressing according to
    // the descriptor's media type; unrecognised types stream verbatim.
    def layer(descriptor: Descriptor)(using Tactic[Oci.Error]): (Stream[Data] over Credit)^ =
      if descriptor.mediaType.suffixes.stdlib.contains(Media.Suffix.Gzip)
      then compressed(descriptor).decompress[Gzip]
      else compressed(descriptor)

    // A blob gathered eagerly and checked against its descriptor's digest and size — the
    // opt-in verified path, since checking a stream would force draining it.
    def verified(descriptor: Descriptor)(using Tactic[Oci.Error]): Data =
      val bytes = body(descriptor.digest).memoize
      val digest = sha256(bytes)

      if digest != descriptor.digest
      then abort(Oci.Error(Oci.Error.Reason.DigestMismatch(descriptor.digest, digest)))

      if bytes.length.toLong != descriptor.size
      then abort:
        Oci.Error:
          Oci.Error.Reason.InvalidBlob
            ( descriptor.digest, t"its size is ${bytes.length}, not ${descriptor.size}" )

      bytes

    // The gathered bytes of a named top-level document (`oci-layout` or `index.json`).
    private def document(name: Text, reason: Oci.Error.Reason)(using Tactic[Oci.Error]): Data =
      entries.stdlib.collectFirst { case file: Tar.Entry.File if file.entryName == name => file.data }
      . getOrElse(abort(Oci.Error(reason)))
      . memoize

    // Runs a JSON decode, translating any failure — parse, JSON or media-type errors,
    // thrown under the call site's `throwUnsafely` — to an `InvalidBlob` on the given
    // label. The decoder is derived under a throwing strategy because the derivation's
    // codec thunks cannot capture a scoped tactic capability.
    private def decode[doc](label: Text)(body: => doc)(using Tactic[Oci.Error]): doc =
      try body catch case error: Error =>
        abort(Oci.Error(Oci.Error.Reason.InvalidBlob(label, error.message.text)))

  // ImageDataOpenable → Image.DataOpenable
  // A named class rather than an anonymous given instance, for the reasons documented on
  // galilei's `FileOpenable`. Opening in-memory `Data` as an OCI image; opening a filesystem
  // *path* (`ImageOpenable`) lives in the JVM-only source set. Read-only for now.
  class DataOpenable(using Tactic[Oci.Error], Tactic[Tar.Error], Tactic[Truncation.Error])
  extends Openable:

    type Self = Data
    type Form = Image
    type Operand = Nothing
    type Result = Image.Handle

    def open[grants <: Grant, result]
      ( value: Data, mode: Mode granting grants, flags: List[Nothing] )
      ( block: ((Image.Handle & Granting[grants])^) ?=> result )
    :   result =

      if mode.atoms.stdlib.contains(Write) then abort(Oci.Error(Oci.Error.Reason.WriteUnsupported))
      block(using new Image.Handle(Tarfile.read(value.stream).to(List).asInstanceOf[List[bitumen.Tar.Entry]]) with Granting[grants] {})

case class Image
  ( layers:             List[Layer],
    config:             Oci.Config,
    configBytes:        Data,
    configDescriptor:   Descriptor,
    manifest:           Oci.Manifest,
    manifestBytes:      Data,
    manifestDescriptor: Descriptor,
    index:              Index,
    indexBytes:         Data ):

  // The config blob narrowed to whichever form it takes; each is `Unset` for an image of
  // the other kind, so a caller that knows which it built can read the typed document
  // without a match.
  def imageConfig: Optional[Image.Config] = config.only { case config: Image.Config => config }
  def wasmConfig: Optional[WasmConfig] = config.only { case config: WasmConfig => config }

  // Every blob in the image, as `(digest, bytes)` pairs: the config, each layer,
  // and the manifest.
  def blobs: List[(Text, Data)] =
    val layerBlobs = layers.map: layer => (layer.digest, layer.blob)

    List((configDescriptor.digest, configBytes))
    ::: layerBlobs
    ::: List((manifestDescriptor.digest, manifestBytes))

  // The complete image serialised as an OCI image-layout tar (an "oci-archive"):
  // an `oci-layout` marker, the `index.json`, and every blob under
  // `blobs/sha256/`. Suitable for `ctr images import`, `podman load`, or
  // `skopeo copy oci-archive:…`.
  def archive: Tarfile =
    def entry(path: Text, content: Data): Tar.Entry =
      Tar.Entry.File
        ( path  = path.as[Relative on Tar],
          mode  = UnixMode(),
          user  = UnixUser(0),
          group = UnixGroup(0),
          mtime = 0.bits.u32,
          data  = Tar.Body(content) )

    val layoutEntry = entry(t"oci-layout", t"""{"imageLayoutVersion":"1.0.0"}""".in[Data])
    val indexEntry  = entry(t"index.json", indexBytes)

    val blobEntries: List[bitumen.Tar.Entry] = blobs.map: (digest, content) =>
      val hex = digest.s.stripPrefix("sha256:").tt
      entry(t"blobs/sha256/$hex", content)

    Tarfile(List(layoutEntry, indexEntry) ::: blobEntries)
