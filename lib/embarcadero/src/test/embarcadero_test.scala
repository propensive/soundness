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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import soundness.*
import bitumen.fromGzip

import alphabets.hex.lowerCase
import charEncoders.utf8
import jsonPrinters.minimal
import strategies.throwUnsafely

object Tests extends Suite(m"Embarcadero OCI Tests"):
  def run(): Unit =
    def fileEntry(name: Text, content: Text): TarEntry =
      TarEntry.File
       ( path  = name.decode[Relative on Posix],
         mode  = UnixMode(),
         user  = UnixUser(0),
         group = UnixGroup(0),
         mtime = 0.bits.u32,
         data  = LazyList(content.data) )

    def bytesOf(stream: LazyList[Data]): Data = stream.foldLeft(IArray.empty[Byte])(_ ++ _)

    val layerTar = Tar(LazyList(fileEntry(t"hello.txt", t"hello world\n")))
    val layer    = Layer(layerTar)
    val image    = Image(List(layer), config = ContainerConfig(Cmd = List(t"/bin/sh")))

    suite(m"Layer digests"):
      val raw = bytesOf(layerTar.stream[Data])

      test(m"diff_id is the sha256 of the uncompressed tar"):
        layer.diffId
      . assert(_ == t"sha256:${raw.digest[Sha2[256]].serialize[Hex]}")

      test(m"descriptor digest is the sha256 of the gzipped blob"):
        layer.descriptor.digest
      . assert(_ == t"sha256:${layer.blob.digest[Sha2[256]].serialize[Hex]}")

      test(m"compressed digest differs from the diff_id"):
        layer.digest == layer.diffId
      . assert(_ == false)

      test(m"descriptor size matches the compressed blob length"):
        layer.descriptor.size
      . assert(_ == layer.blob.length.toLong)

      test(m"descriptor uses the gzipped-layer media type"):
        layer.descriptor.mediaType
      . assert(_ == media"application/vnd.oci.image.layer.v1.tar+gzip")

      test(m"the blob is a valid gzipped tar round-tripping to the original entry"):
        Tar.fromGzip(LazyList(layer.blob)).map(_.entryName).to(List)
      . assert(_ == List(t"hello.txt"))

    suite(m"Image config"):
      test(m"rootfs diff_ids list the layer diff_id in order"):
        image.imageConfig.rootfs.diff_ids
      . assert(_ == List(layer.diffId))

      test(m"rootfs type is 'layers'"):
        image.imageConfig.rootfs.`type`
      . assert(_ == t"layers")

      test(m"architecture and os default to amd64/linux"):
        (image.imageConfig.architecture, image.imageConfig.os)
      . assert(_ == (t"amd64", t"linux"))

      test(m"config blob JSON uses the snake_case diff_ids key"):
        image.imageConfig.json.show.s.contains("\"diff_ids\"")
      . assert(_ == true)

      test(m"config blob JSON preserves the capitalised Cmd key"):
        image.imageConfig.json.show.s.contains("\"Cmd\"")
      . assert(_ == true)

    suite(m"Manifest"):
      test(m"schemaVersion is 2"):
        image.manifest.schemaVersion
      . assert(_ == 2)

      test(m"manifest references the config descriptor by its media type"):
        image.manifest.config.mediaType
      . assert(_ == media"application/vnd.oci.image.config.v1+json")

      test(m"manifest lists the layer descriptor digest"):
        image.manifest.layers.map(_.digest)
      . assert(_ == List(layer.digest))

      test(m"config descriptor digest matches the config blob bytes"):
        image.configDescriptor.digest
      . assert(_ == t"sha256:${image.configBytes.digest[Sha2[256]].serialize[Hex]}")

      test(m"manifest JSON renders the exact OCI media-type string"):
        image.manifest.json.show.s.contains("\"application/vnd.oci.image.manifest.v1+json\"")
      . assert(_ == true)

      test(m"manifest JSON round-trips through jacinta"):
        image.manifest.json.as[Manifest]
      . assert(_ == image.manifest)

    suite(m"OCI archive"):
      val entries    = Tar.read(image.archive.stream[Data]).to(List)
      val names      = entries.map(_.entryName)
      val layoutData = entries.collect:
        case file: TarEntry.File if file.entryName == t"oci-layout" => bytesOf(file.data)

      test(m"archive contains the oci-layout marker and index.json"):
        (names.contains(t"oci-layout"), names.contains(t"index.json"))
      . assert(_ == (true, true))

      test(m"archive contains one blob per config, layer and manifest"):
        names.count(_.s.startsWith("blobs/sha256/"))
      . assert(_ == 3)

      test(m"the layer blob is stored under its digest path"):
        val hex = layer.digest.s.stripPrefix("sha256:")
        names.map(_.s).contains("blobs/sha256/"+hex)
      . assert(_ == true)

      test(m"oci-layout declares image layout version 1.0.0"):
        layoutData.map(bytes => bytes.to(List))
      . assert(_ == List(t"""{"imageLayoutVersion":"1.0.0"}""".data.to(List)))
