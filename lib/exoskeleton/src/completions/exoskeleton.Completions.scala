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
package exoskeleton

import scala.collection.mutable as scm

import ambience.*, environments.java, systems.java
import anticipation.*
import contingency.*
import denominative.*
import digression.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.{where as _, *}
import guillotine.*
import nomenclature.*
import parasite.*
import prepositional.*
import rudiments.*, homeDirectories.system
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled

object Completions:
  case class Tab(arguments: List[Text], focus: Int, cursor: Int, count: Int = 0):
    def next: Tab = copy(count = count + 1)
    def zero: Tab = copy(count = 0)

  private var cache: scm.HashMap[Text, Tab] = scm.HashMap()

  def tab(tty: Text, tab0: Tab): Ordinal =
    cache.at(tty).let { tab => tab.next.unless(tab.zero != tab0) }.or(tab0).tap { cache(tty) = _ }
    . count
    . z

  enum Installation:
    case CommandNotOnPath(script: Text)
    case Shells
          (zsh:  Installation.InstallResult,
           bash: Installation.InstallResult,
           fish: Installation.InstallResult)

    def paths: List[Text] =
      this match
        case CommandNotOnPath(_) => Nil
        case Shells(zsh, bash, fish) => List(zsh, bash, fish).map(_.pathname).compact

  object Installation:
    given communicable: Installation is Communicable =
      case CommandNotOnPath(script) =>
        m"The ${script} command is not on the PATH, so completions scripts cannot be installed."

      case Shells(zsh, bash, fish) =>
        m"$zsh\n\n$bash\n\n$fish"

    object InstallResult:
      given communicable: InstallResult is Communicable =
        case Installed(shell, path) =>
          m"The $shell completion script was installed to $path."

        case AlreadyInstalled(shell, path) =>
          m"A $shell completion script already exists at $path."

        case NoWritableLocation(shell) =>
          m"No writable install location could be found for $shell completions."

        case ShellNotInstalled(shell) =>
          m"The $shell shell is not installed."

    enum InstallResult:
      case Installed(shell: Shell, path: Text)
      case AlreadyInstalled(shell: Shell, path: Text)
      case NoWritableLocation(shell: Shell)
      case ShellNotInstalled(shell: Shell)

      def pathname: Optional[Text] = this.only:
        case Installed(_, path)        => path
        case AlreadyInstalled(_, path) => path

  def ensure(force: Boolean = false)(using Entrypoint, WorkingDirectory, Diagnostics)
  : List[Text] logs CliEvent =

      safely(effectful(install(force))).let(_.paths).or(Nil)


  def install(force: Boolean = false)(using entrypoint: Entrypoint)
       (using WorkingDirectory, Effectful, Diagnostics)
  : Installation raises InstallError logs CliEvent =

      mitigate:
        case PathError(_, _)    => InstallError(InstallError.Reason.Environment)
        case NameError(_, _, _) => InstallError(InstallError.Reason.Environment)
        case ExecError(_, _, _) => InstallError(InstallError.Reason.Environment)
      . within:
          val scriptPath = sh"sh -c 'command -v ${entrypoint.script}'".exec[Text]()
          val command: Text = entrypoint.script

          if !force && safely(scriptPath.decode[Path on Linux]) != entrypoint.executable
          then Installation.CommandNotOnPath(entrypoint.script)
          else
            val zsh: Installation.InstallResult =
              if sh"sh -c 'command -v zsh'".exec[Exit]() != Exit.Ok
              then Installation.InstallResult.ShellNotInstalled(Shell.Zsh)
              else
                val dirNamesCmd = sh"zsh -c 'source ~/.zshrc 2> /dev/null; printf %s, $$fpath'"
                val dirNames = dirNamesCmd.exec[Text]().cut(t",").to(List)

                val dirs =
                  dirNames.filter(_.trim != t"").map: dir =>
                    safely(dir.decode[Path on Linux])
                  . compact

                install(Shell.Zsh, command, Name[Linux](t"_$command"), dirs)

            val bash: Installation.InstallResult =
              if sh"sh -c 'command -v bash'".exec[Exit]() != Exit.Ok
              then Installation.InstallResult.ShellNotInstalled(Shell.Bash)
              else
                install
                 (Shell.Bash,
                  command,
                  Name[Linux](command),
                  List
                   (Xdg.dataDirs[Path on Linux].last/"bash-completion"/"completions",
                    Xdg.dataHome[Path on Linux]/"bash-completion"/"completions"))

            val fish: Installation.InstallResult =
              if sh"sh -c 'command -v fish'".exec[Exit]() != Exit.Ok
              then Installation.InstallResult.ShellNotInstalled(Shell.Fish)
              else install
                    (Shell.Fish,
                     command,
                     Name[Linux](t"$command.fish"),
                     List
                      (Xdg.dataDirs[Path on Linux].last/"fish"/"vendor_completions.d",
                       Xdg.configHome[Path on Linux]/"fish"/"completions"))

            Installation.Shells(zsh, bash, fish)


  def install(shell: Shell, command: Text, scriptName: Name[Linux], dirs: List[Path on Linux])
       (using Effectful, Diagnostics)
  : Installation.InstallResult raises InstallError logs CliEvent =

    mitigate:
      case IoError(_, _, _)   => InstallError(InstallError.Reason.Io)
      case NameError(_, _, _) => InstallError(InstallError.Reason.Io)
      case PathError(_, _)    => InstallError(InstallError.Reason.Io)
      case StreamError(_)     => InstallError(InstallError.Reason.Io)

    . within:
        dirs.where { dir => dir.exists() && dir.writable() }.let: dir =>
          val path = dir/scriptName

          if path.exists()
          then Installation.InstallResult.AlreadyInstalled(shell, path.encode)
          else
            path.open(script(shell, command).sysData.writeTo(_))
            Installation.InstallResult.Installed(shell, path.encode)

        . or(Installation.InstallResult.NoWritableLocation(shell))

  def script(shell: Shell, command: Text): Text = shell match
    case Shell.Zsh =>
      t"""|#compdef $command
          |local -a ln
          |_$command() {
          |  $command '{completions}' zsh "$$CURRENT" "$${#PREFIX}" "$$TTY" \\
          |    -- $$words | while IFS=$$'\\0' read -r -A ln
          |  do
          |    desc=("$${ln[1]}")
          |    compadd -Q "$${(@)ln:1}"
          |  done
          |}
          |_$command
          |return 0
          |""".s.stripMargin.tt

    case Shell.Fish =>
      t"""|function completions
          |  set position (count (commandline --tokenize --cut-at-cursor))
          |  ${command} '{completions}' fish $$position (commandline -C -t) (tty) \\
          |    -- (commandline -o)
          |end
          |complete -f -c $command -a '(completions)'
          |""".s.stripMargin.tt

    case Shell.Bash =>
      t"""|_${command}_complete() {
          |  _init_completion -n = || return
          |  readarray -t COMPREPLY < <(${command} '{completions}' bash $$COMP_CWORD 0 $$(tty) \\
          |    -- $${COMP_WORDS[@]})
          |}
          |complete -F _${command}_complete $command
          |""".s.stripMargin.tt

object CliEvent:
  given execEvent: CliEvent transcribes ExecEvent = CliEvent.Exec(_)

  given communicable: CliEvent is Communicable =
    case Exec(event)          => m"exeution error: $event"
    case Installing(location) => m"installing to $location"

enum CliEvent:
  case Exec(event: ExecEvent)
  case Installing(location: Text)
