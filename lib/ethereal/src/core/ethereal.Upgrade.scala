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
package ethereal

import java.lang as jl

import ambience.*, systems.javaSystem
import anticipation.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*

import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.disabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled

import filesystemBackends.virtualMachine

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
  :   Nothing raises UpgradeError =

    applyBytes(source.read[Data])


  def applyBytes(bytes: Data)
    ( using environment: Environment,
            system:      System,
            diagnostics: Diagnostics )
  :   Nothing raises UpgradeError =

    mitigate:
      case PathError(_, _)     => UpgradeError(UpgradeError.Reason.CannotResolveLauncher)
      case PropertyError(_)    => UpgradeError(UpgradeError.Reason.CannotResolveLauncher)
      case IoError(_, _, _, _) => UpgradeError(UpgradeError.Reason.CannotWritePending)
      case NameError(_, _, _)  => UpgradeError(UpgradeError.Reason.CannotWritePending)
      case StreamError(_)      => UpgradeError(UpgradeError.Reason.CannotReadSource)

    . protect:
        val name: Text = System.properties.ethereal.name[Text]()

        val dataHome: Path on Linux =
          if isWindows then Directories.cacheHome[Path on Linux]
          else Xdg.dataHome[Path on Linux]

        val pendingDir: Path on Linux = dataHome/name
        pendingDir.create[Directory]()
        val pendingPath: Path on Linux = pendingDir/t".pending"

        pendingPath.open: file =>
          LazyList(bytes).writeTo(file)

        val launcher: Text = System.properties.ethereal.script[Text]()

        try new jl.ProcessBuilder(launcher.s).inheritIO().nn.start()
        catch case _: jl.Throwable =>
          abort(UpgradeError(UpgradeError.Reason.CannotRespawnLauncher))

        jl.System.exit(0)
        throw new jl.AssertionError("unreachable: System.exit returned")


  private def isWindows(using system: System): Boolean =
    safely(System.properties.os.name[Text]().lower.contains(t"win")).or(false)
