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
package ziggurat

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import ethereal.*
import eucalyptus.*
import fulminate.*
import galilei.*, galilei.Platform.pathReadable
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import errorDiagnostics.emptyDiagnostics
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.overwritePreexisting.enabled
import gastronomy.*, providers.javaStdlibProvider
import httpBackends.virtualMachineHttp
import internetAccess.online
import monotonous.*, alphabets.hexLowerCase

import filesystemBackends.virtualMachineFilesystem
import filesystemOptions.dereferenceSymlinks.enabled

// Turns a `Packaging` configuration into a distributable. Each per-platform binary is the
// application JAR appended to a bare reusable runner stub, obtained from `RunnerSource` —
// read from a local directory, or downloaded and SHA-256-verified against the manifest.
// `Native` emits one self-contained binary; `EmbedAll` emits a polyglot script embedding
// every (ETHRCFG-patched) stub plus the JAR once (`Xeq.installer`); `Download` emits a
// polyglot launcher. Burdock remote dependencies remain unimplemented.
object Packager:
  // The embedded JAR payload's label — must match the launcher templates' `get_offset "data"`.
  private val DataName: Text = t"data"

  def pack(config: Packaging)(using WorkingDirectory): Path on Linux raises Packager.Error =
    val appJar: Path on Linux = config.dependencies.absolve match
      case Packaging.Dependencies.FatJar(jar) =>
        jar

      case Packaging.Dependencies.BurdockRemote(_) =>
        abort(Packager.Error(m"Burdock remote dependencies are not yet supported (Stage C)"))

    config.delivery match
      case Packaging.Delivery.Native if config.targets.stdlib.length != 1 =>
        val length: Int = config.targets.stdlib.length

        abort:
          Packager.Error(m"Native delivery requires exactly one target, but $length were given")

      case _ =>
        ()

    mitigate:
      case Http.Error(_, _)        => Packager.Error(m"A runner stub could not be downloaded")
      case Connect.Error(_)         => Packager.Error(m"Could not connect to download a runner stub")
      case Url.Error(_, _, _)      => Packager.Error(m"A runner stub URL is invalid")
      case Assembler.Error(detail) => Packager.Error(detail)
      case Io.Error(_, _, _, _)     => Packager.Error(m"A filesystem error occurred when packaging")
      case Truncation.Error(_)          => Packager.Error(m"A stream error occurred during packaging")
      case Path.Error(_, _)        => Packager.Error(m"A path could not be resolved when packaging")

    . protect:
        val jdk: Boolean = config.java.bundle == Packaging.Bundle.Jdk

        val publicKey: Data =
          val zeros: Data = Array.fill(Assembler.PublicKeyLength)(0.toByte)

          config.signing.lay(zeros): signing =>
            signing.publicKey.lay(zeros): key =>
              val raw: Data = key.read[Data]

              if raw.length != Assembler.PublicKeyLength
              then abort(Packager.Error(m"The signing public key is the wrong size"))

              raw

        // The bare reusable stub bytes for a platform — read from a local directory, or
        // downloaded and verified against the manifest hash.
        def stub(label: Text): Data =
          val name: Text =
            if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

          config.runnerSource.absolve match
            case Packaging.RunnerSource.Local(directory) =>
              val file: Path on Linux = t"$directory/$name".as[Path on Linux]
              file.read[Data]

            case Packaging.RunnerSource.Remote(baseUrl, hashes) =>
              val expected: Text =
                hashes(label).lest(Packager.Error(m"No runner hash given for $label"))

              val base: Text = if baseUrl.ends(t"/") then baseUrl else t"$baseUrl/"
              val runner: Data = mute[Http.Event](t"$base$name".as[HttpUrl].fetch().read[Data])
              val actual: Text = runner.digest[Sha2[256]].serialize[Hex]

              if actual != expected
              then abort(Packager.Error(m"The runner for $label has the wrong SHA-256 ($actual)"))

              runner

        // One self-contained per-platform binary: bare stub, ETHRCFG patched, JAR appended.
        def binary(label: Text, output: Path on Linux): Unit =
          Assembler.assemble
            ( stub(label), appJar, output, label, config.buildId, config.java.minimum,
              config.java.preferred, jdk, publicKey )

        config.delivery match
          case Packaging.Delivery.Native =>
            binary(config.targets.stdlib.head, config.output)
            config.output

          case Packaging.Delivery.EmbedAll =>
            val stubs: List[Payload] = config.targets.map: label =>
              val patched: Data =
                Assembler.patch
                  ( stub(label), config.buildId, config.java.minimum, config.java.preferred, jdk,
                    publicKey )

              Payload(label, patched, gzip = !label.starts(t"windows"))

            val data: Payload = Payload(DataName, appJar.read[Data], gzip = false)
            write(config.output, Xeq.installer((stubs.stdlib :+ data).to(List)))
            config.output

          case Packaging.Delivery.Download =>
            // Online: the JAR is embedded once; the launcher downloads each bare stub from the
            // `Remote` base URL at runtime and appends the embedded JAR. No per-platform binary
            // is built or published here — only the reusable stubs (published independently).
            val entries: List[(Text, Text, Text)] = config.runnerSource.absolve match
              case Packaging.RunnerSource.Local(_) =>
                abort(Packager.Error(m"Download delivery requires a Remote runner source"))

              case Packaging.RunnerSource.Remote(baseUrl, hashes) =>
                val base: Text = if baseUrl.ends(t"/") then baseUrl else t"$baseUrl/"

                config.targets.map: label =>
                  val name: Text =
                    if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

                  val hash: Text =
                    hashes(label).lest(Packager.Error(m"No runner hash given for $label"))

                  (label, t"$base$name", hash)

            val jarData: Data = appJar.read[Data]
            write(config.output, Xeq.onlineLauncher(jarData, entries))
            config.output


  private def write(output: Path on Linux, data: Data)
  :   Unit raises Io.Error raises Truncation.Error =

    output.create[File](CreateFlag.Parents, CreateFlag.Replace): handle ?=>
      handle.write(Chain(data))

    output.executable() = true

  // PackageError → Packager.Error
  case class Error(detail: Message)(using Diagnostics) extends fulminate.Error(detail)
