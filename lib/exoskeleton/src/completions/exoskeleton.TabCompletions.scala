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
┃    Soundness, version 0.40.0.                                                                    ┃
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

import ambience.*, environments.jre, systemProperties.jre
import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.{where as _, *}
import guillotine.*
import nomenclature.*
import prepositional.*
import rudiments.*, homeDirectories.systemProperties
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

object TabCompletions:
  def install(force: Boolean = false)(using service: ShellContext)
       (using WorkingDirectory, Effectful, Diagnostics)
  : TabCompletionsInstallation raises InstallError logs CliEvent =

      mitigate:
        case PathError(_, _)    => InstallError(InstallError.Reason.Environment)
        case NameError(_, _, _) => InstallError(InstallError.Reason.Environment)
        case ExecError(_, _, _) => InstallError(InstallError.Reason.Environment)
      . within:
          val scriptPath = sh"sh -c 'command -v ${service.scriptName}'".exec[Text]()
          val command: Text = service.scriptName

          if !force && safely(scriptPath.decode[Path on Linux]) != service.script
          then TabCompletionsInstallation.CommandNotOnPath(service.scriptName)
          else
            val zsh: TabCompletionsInstallation.InstallResult =
              if sh"sh -c 'command -v zsh'".exec[Exit]() != Exit.Ok
              then TabCompletionsInstallation.InstallResult.ShellNotInstalled(Shell.Zsh)
              else
                val dirNamesCmd = sh"zsh -c 'source ~/.zshrc 2> /dev/null; printf %s, $$fpath'"
                val dirNames = dirNamesCmd.exec[Text]().cut(t",").to(List)

                val dirs =
                  dirNames.filter(_.trim != t"").map: dir =>
                    safely(dir.decode[Path on Linux])
                  . compact

                install(Shell.Zsh, command, Name[Linux](t"_$command"), dirs)

            val bash: TabCompletionsInstallation.InstallResult =
              if sh"sh -c 'command -v bash'".exec[Exit]() != Exit.Ok
              then TabCompletionsInstallation.InstallResult.ShellNotInstalled(Shell.Bash)
              else
                install
                 (Shell.Bash,
                  command,
                  Name[Linux](command),
                  List
                   (Xdg.dataDirs[Path on Linux].last/"bash-completion"/"completions",
                    Xdg.dataHome[Path on Linux]/"bash-completion"/"completions"))

            val fish: TabCompletionsInstallation.InstallResult =
              if sh"sh -c 'command -v fish'".exec[Exit]() != Exit.Ok
              then TabCompletionsInstallation.InstallResult.ShellNotInstalled(Shell.Fish)
              else install
                    (Shell.Fish,
                     command,
                     Name[Linux](t"$command.fish"),
                     List
                      (Xdg.dataDirs[Path on Linux].last/"fish"/"vendor_completions.d",
                       Xdg.configHome[Path on Linux]/"fish"/"completions"))

            TabCompletionsInstallation.Shells(zsh, bash, fish)


  def install(shell: Shell, command: Text, scriptName: Name[Linux], dirs: List[Path on Linux])
       (using Effectful, Diagnostics)
  : TabCompletionsInstallation.InstallResult raises InstallError logs CliEvent =

    mitigate:
      case IoError(_, _, _)   => InstallError(InstallError.Reason.Io)
      case NameError(_, _, _) => InstallError(InstallError.Reason.Io)
      case PathError(_, _)    => InstallError(InstallError.Reason.Io)
      case StreamError(_)     => InstallError(InstallError.Reason.Io)

    . within:
        dirs.where { dir => dir.exists() && dir.writable() }.let: dir =>
          val path = dir/scriptName

          if path.exists()
          then TabCompletionsInstallation.InstallResult.AlreadyInstalled(shell, path.encode)
          else
            path.open(script(shell, command).sysBytes.writeTo(_))
            TabCompletionsInstallation.InstallResult.Installed(shell, path.encode)

        . or(TabCompletionsInstallation.InstallResult.NoWritableLocation(shell))

  // def messages(shell: Shell, global: Boolean): List[Message] =
  //   if shell == Shell.Zsh && !global
  //   then List(m"""Make sure that your ${t"~/.zshrc"} file contains the following lines:

  //     fpath=(~/.zsh/completion $$fpath)
  //     autoload -U compinit
  //     compinit
  //   """)
  //   else Nil

  def script(shell: Shell, command: Text): Text = shell match
    case Shell.Zsh =>
      t"""|#compdef $command
          |local -a ln
          |_$command() {
          |  $command '{completions}' zsh "$$CURRENT" "$${#PREFIX}" -- $$words | while IFS=$$'\\0' read -r -A ln
          |  do
          |    desc=($${ln[1]})
          |    compadd -Q "$${(@)ln:1}"
          |  done
          |}
          |_$command
          |return 0
          |""".s.stripMargin.tt

    case Shell.Fish =>
      t"""|function completions
          |  set position (count (commandline --tokenize --cut-at-cursor))
          |  ${command} '{completions}' fish $$position (commandline -C -t) -- (commandline -o)
          |end
          |complete -f -c $command -a '(completions)'
          |""".s.stripMargin.tt

    case Shell.Bash =>
      t"""|_${command}_complete() {
          |  output="$$(${command} '{completions}' bash $$COMP_CWORD 0 -- $$COMP_LINE)"
          |  COMPREPLY=($$output)
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
