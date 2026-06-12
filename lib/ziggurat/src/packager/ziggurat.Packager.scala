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
package ziggurat

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import ethereal.*
import eucalyptus.*
import fulminate.*
import galilei.*
import gastronomy.*, hashProviders.javaStdlibHashing
import gossamer.*
import guillotine.*
import monotonous.*, alphabets.hex.lowerCase
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*

import errorDiagnostics.empty
import internetAccess.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled

// Turns a `Packaging` configuration into a distributable, across all three
// deliveries. With `RunnerSource.LocalResource` the application self-assembles
// each per-platform binary in a subprocess; with `RunnerSource.Remote` the
// runners are downloaded, verified, and assembled in-process via
// `ethereal.Assembler`. Burdock remote dependencies remain unimplemented.
object Packager:
  def pack(config: Packaging)
     ( using working: WorkingDirectory, environment: Environment )
  :   Path on Linux raises PackageError =

    val appJar: Path on Linux = config.dependencies.absolve match
      case Packaging.Dependencies.FatJar(jar) =>
        jar

      case Packaging.Dependencies.BurdockRemote(_) =>
        abort(PackageError(m"Burdock remote dependencies are not yet supported (Stage C)"))

    config.delivery match
      case Packaging.Delivery.Native if config.targets.length != 1 =>
        abort(PackageError(m"Native delivery requires exactly one target, but ${config.targets.length} were given"))

      case _ =>
        ()

    whereas:
      case ExecError(_, _, _)  => PackageError(m"A subprocess failed during packaging")
      case IoError(_, _, _, _) => PackageError(m"A filesystem error occurred during packaging")
      case StreamError(_)      => PackageError(m"A stream error occurred during packaging")
      case PathError(_, _)     => PackageError(m"A path could not be resolved during packaging")

    . mitigate:
        config.delivery match
          case Packaging.Delivery.Native =>
            buildBinary(config, appJar, config.targets.head, config.output)
            config.output

          case Packaging.Delivery.EmbedAll =>
            val dir = config.output.parent.vouch

            val payloads: List[Payload] = config.targets.map: label =>
              val staged: Path on Linux = t"$dir/.xeq-$label".decode[Path on Linux]
              buildBinary(config, appJar, label, staged)
              val bytes: Data = staged.open(_.read[Data])
              staged.delete()
              Payload(label, bytes, gzip = !label.starts(t"windows"))

            write(config.output, Xeq.installer(payloads))
            config.output

          case Packaging.Delivery.Download(baseUrl, version) =>
            val dir = config.output.parent.vouch
            val base: Text = if baseUrl.ends(t"/") then baseUrl else t"$baseUrl/"

            val entries: List[(Text, Text, Text)] = config.targets.map: label =>
              val asset: Text = t"xeq-$label-$version"
              val staged: Path on Linux = t"$dir/$asset".decode[Path on Linux]
              buildBinary(config, appJar, label, staged)
              val hash: Text = staged.open(_.read[Data]).digest[Sha2[256]].serialize[Hex]
              (label, t"$base$asset", hash)

            write(config.output, Xeq.multiDownloader(entries))
            config.output


  // Produces one self-contained per-platform binary, dispatching on the runner
  // source: the app self-assembles in a subprocess (LocalResource), or the
  // runner is downloaded and assembled in-process (Remote).
  private def buildBinary(config: Packaging, appJar: Path on Linux, label: Text, output: Path on Linux)
     ( using working: WorkingDirectory )
  :   Unit raises ExecError raises PackageError =

    config.runnerSource match
      case Packaging.RunnerSource.LocalResource =>
        assembleViaSubprocess(config, appJar, label, output)

      case Packaging.RunnerSource.Remote(baseUrl, hashes) =>
        assembleRemote(config, appJar, label, output, baseUrl, hashes)


  // Drives the application's own self-assembly (ethereal's `cli` build path) in a
  // subprocess: `java -Dbuild.executable=… -Dbuild.target=… … -jar <appJar>`
  // produces one self-contained per-platform binary and exits. `ethereal.name`
  // must be left unset or the build path is not taken.
  private def assembleViaSubprocess
     ( config: Packaging, appJar: Path on Linux, label: Text, output: Path on Linux )
     ( using working: WorkingDirectory )
  :   Unit raises ExecError raises PackageError =

    val bundle: Text = config.java.bundle match
      case Packaging.Bundle.Jre => t"jre"
      case Packaging.Bundle.Jdk => t"jdk"

    val publicKey: List[Text] = config.signing.lay(Nil): signing =>
      signing.publicKey.lay(Nil): key =>
        List(t"-Dethereal.publicKey=$key")

    val arguments: List[Text] =
      List
       ( t"java",
        t"-Dbuild.executable=$output",
        t"-Dbuild.target=$label",
        t"-Dbuild.java.minimum=${config.java.minimum}",
        t"-Dbuild.java.preferred=${config.java.preferred}",
        t"-Dbuild.java.bundle=$bundle",
        t"-Dbuild.id=${config.buildId}" )
      ++ publicKey
      ++ List(t"-jar", appJar.show)

    if mute[ExecEvent](Command(arguments*).exec[Exit]()) != Exit.Ok
    then abort(PackageError(m"Assembling the executable for $label failed"))


  // Downloads `<baseUrl>/runner-<label>[.exe]`, verifies its SHA-256 against
  // `hashes(label)`, then patches the ETHRCFG block and appends the app JAR
  // in-process via `ethereal.Assembler`. All lower-level failures are surfaced
  // as `PackageError`.
  private def assembleRemote
     ( config:  Packaging,
      appJar:  Path on Linux,
      label:   Text,
      output:  Path on Linux,
      baseUrl: Text,
      hashes:  Map[Text, Text] )
     ( using working: WorkingDirectory )
  :   Unit raises PackageError =

    whereas:
      case HttpError(_, _)       => PackageError(m"Failed to download the runner for $label")
      case ConnectError(_)       => PackageError(m"Could not connect to download the runner for $label")
      case AssemblyError(detail) => PackageError(detail)
      case IoError(_, _, _, _)   => PackageError(m"A filesystem error occurred assembling $label")
      case StreamError(_)        => PackageError(m"A stream error occurred assembling $label")
      case UrlError(_, _, _)     => PackageError(m"The runner URL for $label is invalid")

    . mitigate:
        val expected: Text = hashes.at(label).lest(PackageError(m"No runner hash given for $label"))

        val runnerName: Text =
          if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

        val base: Text = if baseUrl.ends(t"/") then baseUrl else t"$baseUrl/"

        val runner: Data =
          mute[HttpEvent](t"$base$runnerName".decode[HttpUrl].fetch().read[Data])

        val actual: Text = runner.digest[Sha2[256]].serialize[Hex]

        if actual != expected then
          abort(PackageError(m"Downloaded runner for $label has SHA-256 $actual, expected $expected"))

        val zeros: Data = IArray.fill(Assembler.PublicKeyLength)(0.toByte)

        val publicKey: Data = config.signing.lay(zeros): signing =>
          signing.publicKey.lay(zeros): key =>
            val raw: Data = key.open(_.stream[Data].read[Data])

            if raw.length != Assembler.PublicKeyLength
            then abort(PackageError(m"The public key for $label is the wrong size"))

            raw

        val jdk: Boolean = config.java.bundle == Packaging.Bundle.Jdk

        Assembler.assemble
         ( runner, appJar, output, label, config.buildId, config.java.minimum,
          config.java.preferred, jdk, publicKey )


  private def write(output: Path on Linux, data: Data)
  :   Unit raises IoError raises StreamError =

    output.create[File]()
    output.open(Stream(data).writeTo(_))
    output.executable() = true
