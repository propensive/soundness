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
package exoskeleton

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import aperture.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*
import virility.*

import filesystemBackends.virtualMachineFilesystem

// Installs a rendered manpage into the XDG man hierarchy, mirroring how `Completions.install`
// places shell-completion scripts. `$XDG_DATA_HOME/man` is on the default manpath of modern
// man-db, so no `MANPATH` mutation is needed. The target directory and filename come from the
// page itself: `man<section>/<title>.<section>`.
object Manpages:
  object InstallResult:
    given communicable: InstallResult is Communicable =
      case Installed(path)        => m"The manpage was installed to $path."
      case AlreadyInstalled(path) => m"A manpage already exists at $path."
      case NoWritableLocation     => m"No writable install location could be found."

  enum InstallResult:
    case Installed(path: Text)
    case AlreadyInstalled(path: Text)
    case NoWritableLocation

    def pathname: Optional[Text] = this.only:
      case Installed(path)        => path
      case AlreadyInstalled(path) => path

  def install(page: Roff, force: Boolean = false)
    ( using erased effectful: Effectful )
    ( using Diagnostics )
  ( using (Io.Event is Loggable)^ )
  ( using Tactic[Install.Error] )
  :   InstallResult =

    mitigate:
      case Io.Error(_, _, _, _) => Install.Error(Install.Error.Reason.Io)
      case Name.Error(_, _, _)  => Install.Error(Install.Error.Reason.Io)
      case Path.Error(_, _)     => Install.Error(Install.Error.Reason.Io)
      case Truncation.Error(_)  => Install.Error(Install.Error.Reason.Io)

    . protect:
        safely:
          val dir: Path on Linux =
            Xdg.dataHome[Path on Linux]/Name[Linux](t"man")/Name[Linux](t"man${page.section}")

          if !dir.existent() then dir.create[Directory](CreateFlag.Parents)

          val path = dir/Name[Linux](t"${page.title}.${page.section}")

          if path.existent() && !force then InstallResult.AlreadyInstalled(path.encode)
          else
            path.write(page.serialize.sysData)
            InstallResult.Installed(path.encode)

        . or(InstallResult.NoWritableLocation)
