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
package anthology

import java.nio.file as jnf

import scala.util.control as suc

import proscenium.compat.*

import anticipation.*
import contingency.*
import digression.*
import embarcadero.*
import galilei.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import xenophile.*
import zephyrine.*

import sjsLinkages.given

object ociOptions:
  private def oci(edit: OciConfiguration => OciConfiguration): Linker.Option[Artifact.OciImage] =
    Linker.Option(edit)

  def architecture(name: Text): Linker.Option[Artifact.OciImage] = oci(_.copy(architecture = name))
  def os(name: Text): Linker.Option[Artifact.OciImage] = oci(_.copy(os = name))
  def author(name: Text): Linker.Option[Artifact.OciImage] = oci(_.copy(author = name))

  def annotation(key: Text, value: Text): Linker.Option[Artifact.OciImage] =
    oci: config => config.copy(annotations = config.annotations.updated(key, value))

// The sjsir-to-OCI link family: links the compilation as a WASI component (reusing the `Wasi`
// linkage), then wraps that component in a Wasm OCI Artifact and writes it as an `oci-archive`
// tar. Import it where OCI images are linked: `import ociLinkages.given`.
//
// The artifact's `component` metadata — the interfaces the workload imports and exports — is read
// from the world it was linked against, not from the component binary: the world is the
// authoritative statement of the contract, and reading it needs no disassembler.
object ociLinkages:
  // The world a component exports `wasi:http/incoming-handler` from is, by construction, the
  // standard HTTP proxy world; naming it as the artifact's `target` is what lets a host know the
  // component is servable without inspecting its exports one by one.
  private val incomingHandler = t"wasi:http/incoming-handler@0.2.0"
  private val proxy = t"wasi:http/proxy@0.2.0"

  given ociImage(using toolchain: WasiToolchain, world: WitWorld)
  :   (Linkage[Artifact.OciImage] from Universe.Sjsir) =

    // The component link this one wraps. Taken with its `Form` visible, so its configuration is
    // reused rather than restated: an OCI image of a component must be linked exactly as that
    // component would be linked on its own.
    val inner = sjsLinkages.wasiLinkage(world)

    new Linkage[Artifact.OciImage]:
      type Origin = Universe.Sjsir
      private[anthology] type Form = OciConfiguration

      private[anthology] def initial: OciConfiguration = OciConfiguration(inner.initial)

      private[anthology] def link
        ( form:        OciConfiguration,
          compilation: Compilation[Universe.Sjsir],
          entryPoints: List[Linker.EntryPoint],
          out:         Path on Linux )
      :   Path on Linux logs LinkEvent raises LinkError =

        val component = inner.link(form.config, compilation, entryPoints, out / "component")

        val (imports, exports) = interfaces(world)

        try
          val bytes: Data =
            jnf.Files.readAllBytes(jnf.Paths.get(component.encode.s).nn).nn.pipe(Array.unsafeFrozen)

          val image =
            Image.wasm
              ( bytes,
                exports      = exports,
                imports      = imports,
                target       = if exports.has(incomingHandler) then proxy else Unset,
                architecture = form.architecture,
                os           = form.os,
                annotations  = if form.annotations.isEmpty then Unset else form.annotations )

          val archive = out / "image.tar"
          jnf.Files.createDirectories(jnf.Paths.get(out.encode.s))

          jnf.Files.write
            ( jnf.Paths.get(archive.encode.s).nn,
              image.archive.source[Data].memoize.mutable(using Unsafe) )

          archive

        catch case suc.NonFatal(error) =>
          abort(LinkError(LinkError.Reason.Failed(error.stackTrace)))

  // The world's imports and exports, read from the `.wit` sources at the top of the WIT directory.
  // Only that top level is searched: `deps/` holds the packages the world draws on, and none of
  // them declares the world being linked. A world that cannot be found contributes no interfaces
  // rather than failing the link — the component is still valid, only less well described.
  private def interfaces(world: WitWorld): (List[Text], List[Text]) =
    val files = jnf.Paths.get(world.directory.encode.s).nn.toFile.nn.listFiles.nn

    def search(index: Int): Optional[WitDialect.World] =
      if index >= files.length then Unset else
        val file = files(index).nn

        if !file.getName.nn.endsWith(".wit") then search(index + 1) else
          val text = String(jnf.Files.readAllBytes(file.toPath.nn).nn, "UTF-8").tt

          WitDialect.worlds(text).stdlib.get(world.world).optional.or(search(index + 1))

    search(0).lay((Nil, Nil)): found =>
      (found.imports, found.exports)
