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
package ethereal

import java.lang as jl

import ambience.*, systems.javaBaseSystem
import anticipation.*
import contingency.*
import aperture.*
import fulminate.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*

import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteOnlyEmpty
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.overwritePreexisting

import filesystemBackends.javaBaseFilesystem

// Apply an upgrade to the running ethereal application. The given `source`
// must yield the bytes of a complete signed runner+JAR binary — exactly
// what `ethereal-sign` produces. The Scala side performs no verification;
// the freshly-spawned launcher's `check_updates` (in `update.rs`) verifies
// the ML-DSA-44 signature against the public key baked into the running
// launcher before swapping anything into place.
//
// On success this function does not return — it spawns a new launcher and
// `System.exit(0)`s the current JVM. The new launcher picks up `.pending`,
// verifies, swaps, and re-execs into the upgraded binary.
//
// If the signature is bad, the new launcher silently deletes `.pending` and
// continues with the existing binary. The caller's exit was still effective.
// There is no synchronous success/failure signal because the verifying
// process is, by design, not the calling one.
object Upgrade:
  inline def apply[source]
    ( source: source )
    ( using environment: Environment,
            system:      System,
            diagnostics: Diagnostics,
            readable:    source is Readable to Data )
  :   Nothing raises Upgrade.Error =

    applyBytes(source.read[Data])


  def applyBytes(bytes: Data)
    ( using environment: Environment,
            system:      System,
            diagnostics: Diagnostics )
  :   Nothing raises Upgrade.Error =

    mitigate:
      case Path.Error(_, _)     => Upgrade.Error(Upgrade.Error.Reason.CannotResolveLauncher)
      case Property.Error(_)    => Upgrade.Error(Upgrade.Error.Reason.CannotResolveLauncher)
      case Io.Error(_, _, _, _) => Upgrade.Error(Upgrade.Error.Reason.CannotWritePending)
      case Name.Error(_, _, _)  => Upgrade.Error(Upgrade.Error.Reason.CannotWritePending)
      case Truncation.Error(_)      => Upgrade.Error(Upgrade.Error.Reason.CannotReadSource)

    . protect:
        val name: Text = System.properties.ethereal.name[Text]()

        val dataHome: Path on Linux =
          if isWindows then Directories.cacheHome[Path on Linux] else Xdg.dataHome[Path on Linux]

        val pendingDir: Path on Linux = dataHome/name
        pendingDir.create[Directory](CreateFlag.Parents, CreateFlag.Replace)
        val pendingPath: Path on Linux = pendingDir/t".pending"

        pendingPath.open[File](Write, OpenFlag.Create): file ?=>
          file.write(Chain(bytes))

        val launcher: Text = System.properties.ethereal.script[Text]()

        try new jl.ProcessBuilder(launcher.s).inheritIO().nn.start()
        catch case _: jl.Throwable =>
          abort(Upgrade.Error(Upgrade.Error.Reason.CannotRespawnLauncher))

        jl.System.exit(0)
        throw new jl.AssertionError("unreachable: System.exit returned")


  private def isWindows(using system: System): Boolean =
    safely(System.properties.os.name[Text]().lower.contains(t"win")).or(false)

  // UpgradeError → Upgrade.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case CannotReadSource      => m"the upgrade source could not be read"
        case CannotWritePending    => m"the .pending file could not be written"
        case CannotResolveLauncher => m"the running launcher's path is not available"
        case CannotRespawnLauncher => m"the launcher could not be relaunched"

    enum Reason(val number: Int) extends Clarification:
      case CannotReadSource      extends Reason(1)
      case CannotWritePending    extends Reason(2)
      case CannotResolveLauncher extends Reason(3)
      case CannotRespawnLauncher extends Reason(4)

  case class Error(reason: Upgrade.Error.Reason)(using Diagnostics)
  extends fulminate.Error(631, reason.number)(m"could not apply the upgrade because $reason")

