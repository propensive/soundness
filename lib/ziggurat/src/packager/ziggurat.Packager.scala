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
package ziggurat

import ambience.*
import anticipation.*
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
import httpBackends.virtualMachine
import internetAccess.online
import monotonous.*, alphabets.hexLowerCase

import filesystemBackends.virtualMachine

// Turns a `Packaging` configuration into a distributable. Each per-platform binary is the
// application JAR appended to a bare reusable runner stub, obtained from `RunnerSource` —
// read from a local directory, or downloaded and SHA-256-verified against the manifest.
// `Native` emits one self-contained binary; `EmbedAll` emits a polyglot script embedding
// every (ETHRCFG-patched) stub plus the JAR once (`Xeq.installer`); `Download` emits a
// polyglot launcher. Burdock remote dependencies remain unimplemented.
object Packager:
  // The embedded JAR payload's label — must match the launcher templates' `get_offset "data"`.
  private val DataName: Text = t"data"

  def pack(config: Packaging)(using WorkingDirectory): Path on Linux raises PackageError =
    val appJar: Path on Linux = config.dependencies.absolve match
      case Packaging.Dependencies.FatJar(jar) =>
        jar

      case Packaging.Dependencies.BurdockRemote(_) =>
        abort(PackageError(m"Burdock remote dependencies are not yet supported (Stage C)"))

    config.delivery match
      case Packaging.Delivery.Native if config.targets.length != 1 =>
        val length = config.targets.length
        abort(PackageError(m"Native delivery requires exactly one target, but $length were given"))

      case _ =>
        ()

    mitigate:
      case HttpError(_, _)       => PackageError(m"A runner stub could not be downloaded")
      case ConnectError(_)       => PackageError(m"Could not connect to download a runner stub")
      case UrlError(_, _, _)     => PackageError(m"A runner stub URL is invalid")
      case AssemblyError(detail) => PackageError(detail)
      case IoError(_, _, _, _)   => PackageError(m"A filesystem error occurred during packaging")
      case StreamError(_)        => PackageError(m"A stream error occurred during packaging")
      case PathError(_, _)       => PackageError(m"A path could not be resolved during packaging")

    . protect:
        val jdk: Boolean = config.java.bundle == Packaging.Bundle.Jdk

        val publicKey: Data =
          val zeros: Data = IArray.fill(Assembler.PublicKeyLength)(0.toByte)

          config.signing.lay(zeros): signing =>
            signing.publicKey.lay(zeros): key =>
              val raw: Data = key.read[Data]

              if raw.length != Assembler.PublicKeyLength
              then abort(PackageError(m"The signing public key is the wrong size"))

              raw

        // The bare reusable stub bytes for a platform — read from a local directory, or
        // downloaded and verified against the manifest hash.
        def stub(label: Text): Data =
          val name: Text =
            if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

          config.runnerSource.absolve match
            case Packaging.RunnerSource.Local(directory) =>
              val file: Path on Linux = t"$directory/$name".decode[Path on Linux]
              file.read[Data]

            case Packaging.RunnerSource.Remote(baseUrl, hashes) =>
              val expected: Text =
                hashes.at(label).lest(PackageError(m"No runner hash given for $label"))

              val base: Text = if baseUrl.ends(t"/") then baseUrl else t"$baseUrl/"
              val runner: Data = mute[HttpEvent](t"$base$name".decode[HttpUrl].fetch().read[Data])
              val actual: Text = runner.digest[Sha2[256]].serialize[Hex]

              if actual != expected
              then abort(PackageError(m"The runner for $label has the wrong SHA-256 ($actual)"))

              runner

        // One self-contained per-platform binary: bare stub, ETHRCFG patched, JAR appended.
        def binary(label: Text, output: Path on Linux): Unit =
          Assembler.assemble
            ( stub(label), appJar, output, label, config.buildId, config.java.minimum,
              config.java.preferred, jdk, publicKey )

        config.delivery match
          case Packaging.Delivery.Native =>
            binary(config.targets.head, config.output)
            config.output

          case Packaging.Delivery.EmbedAll =>
            val stubs: List[Payload] = config.targets.map: label =>
              val patched: Data =
                Assembler.patch
                  ( stub(label), config.buildId, config.java.minimum, config.java.preferred, jdk,
                    publicKey )

              Payload(label, patched, gzip = !label.starts(t"windows"))

            val data: Payload = Payload(DataName, appJar.read[Data], gzip = false)
            write(config.output, Xeq.installer(stubs :+ data))
            config.output

          case Packaging.Delivery.Download =>
            // Online: the JAR is embedded once; the launcher downloads each bare stub from the
            // `Remote` base URL at runtime and appends the embedded JAR. No per-platform binary
            // is built or published here — only the reusable stubs (published independently).
            val entries: List[(Text, Text, Text)] = config.runnerSource.absolve match
              case Packaging.RunnerSource.Local(_) =>
                abort(PackageError(m"Download delivery requires a Remote runner source"))

              case Packaging.RunnerSource.Remote(baseUrl, hashes) =>
                val base: Text = if baseUrl.ends(t"/") then baseUrl else t"$baseUrl/"

                config.targets.map: label =>
                  val name: Text =
                    if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

                  val hash: Text =
                    hashes.at(label).lest(PackageError(m"No runner hash given for $label"))

                  (label, t"$base$name", hash)

            val jarData: Data = appJar.read[Data]
            write(config.output, Xeq.onlineLauncher(jarData, entries))
            config.output


  private def write(output: Path on Linux, data: Data)
  :   Unit raises IoError raises StreamError =

    output.create[File]()
    output.open(LazyList(data).writeTo(_))
    output.executable() = true
