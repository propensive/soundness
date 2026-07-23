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

import anticipation.*
import bitumen.*
import contingency.*
import distillate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import hypotenuse.*
import prepositional.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*

object Image:
  // Anchored here so `data.open[Image]()` resolves with no import. Opening a filesystem
  // *path* as an image (`path.open[Image]`) lives in the JVM-only source set.
  given dataOpenable: (Tactic[OciError], Tactic[TarError], Tactic[StreamError])
  =>  ( ImageDataOpenable^ ) =
    ImageDataOpenable()

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
      ImageConfig
        ( architecture = architecture,
          os           = os,
          rootfs       = RootFs(t"layers", layers.map(_.diffId)),
          config       = config )

    val configBytes      = render(imageConfig)
    val configType       = media"application/vnd.oci.image.config.v1+json"
    val configDescriptor = descriptorOf(configType, configBytes)

    val manifestType = media"application/vnd.oci.image.manifest.v1+json"

    val manifest =
      Oci.Manifest(2, manifestType, configDescriptor, layers.map(_.descriptor), annotations)

    val manifestBytes      = render(manifest)
    val manifestDescriptor = descriptorOf(manifestType, manifestBytes)

    val indexType  = media"application/vnd.oci.image.index.v1+json"
    val index      = Index(2, indexType, List(manifestDescriptor))
    val indexBytes = render(index)

    Image(layers, imageConfig, configBytes, configDescriptor, manifest, manifestBytes,
        manifestDescriptor, index, indexBytes)

case class Image
  ( layers:             List[Layer],
    imageConfig:        ImageConfig,
    configBytes:        Data,
    configDescriptor:   Descriptor,
    manifest:           Oci.Manifest,
    manifestBytes:      Data,
    manifestDescriptor: Descriptor,
    index:              Index,
    indexBytes:         Data ):

  // Every blob in the image, as `(digest, bytes)` pairs: the config, each layer,
  // and the manifest.
  def blobs: List[(Text, Data)] =
    val layerBlobs = layers.map: layer => (layer.digest, layer.blob)
    (configDescriptor.digest, configBytes) ::
      layerBlobs :::
      List((manifestDescriptor.digest, manifestBytes))

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
          data  = TarBody(content) )

    val layoutEntry = entry(t"oci-layout", t"""{"imageLayoutVersion":"1.0.0"}""".in[Data])
    val indexEntry  = entry(t"index.json", indexBytes)

    val blobEntries = blobs.map: (digest, content) =>
      val hex = digest.s.stripPrefix("sha256:").tt
      entry(t"blobs/sha256/$hex", content)

    Tarfile(layoutEntry :: indexEntry :: blobEntries)
