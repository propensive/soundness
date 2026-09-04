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

import scala.collection.mutable as scm

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import digression.idempotent
import distillate.*
import fulminate.*
import galilei.*, galilei.Platform.pathReadable
import gossamer.*
import guillotine.*
import hieroglyph.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import charDecoders.utf8Decoder
import textSanitizers.skipSanitizer

import filesystemBackends.javaBaseFilesystem

object Completions:
  case class Tab(arguments: List[Text], focus: Int, cursor: Int, count: Int = 0):
    def next: Tab = copy(count = count + 1)
    def zero: Tab = copy(count = 0)

  private val cache: scm.HashMap[Text, Tab] = scm.HashMap()

  def tab(tty: Text, tab0: Tab): Ordinal =
    cache.at(tty).let: tab => tab.next.unless(_ => tab.zero != tab0)
    . or(tab0)
    . tap: value => cache(tty) = value
    . count
    . z

  object Installation:
    given communicable: Installation is Communicable =
      case CommandNotOnPath(script) =>
        m"The ${script} command is not on the PATH, so completions scripts cannot be installed."

      case Shells(zsh, bash, fish, powershell) =>
        m"$zsh\n\n$bash\n\n$fish\n\n$powershell"

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


  def ensure(force: Boolean = false)(using Entrypoint^, WorkingDirectory, Diagnostics)
  ( using (CliEvent is Loggable)^ )
  :   List[Text] =

    if force then safely(effectful(install(force))).let(_.paths).or(Nil)
    else
      // The non-force path is meant to be a fire-and-forget "install if not
      // already installed" check at startup. Each invocation otherwise spawns
      // 5–7 subprocesses (including a `zsh -c 'source ~/.zshrc'`) — measured
      // ~300 ms per call on macOS, dominating the launch time of any
      // daemon-backed CLI that calls this on every invocation. Use
      // `idempotent` so the work runs once per JVM lifetime; subsequent
      // calls are a no-op.
      idempotent(safely(effectful(install())))
      Nil


  def install(force: Boolean = false)(using entrypoint: Entrypoint^)(using erased effectful: Effectful)
    ( using WorkingDirectory, Diagnostics )
  ( using (CliEvent is Loggable)^ )
  ( using Tactic[Install.Error] )
  :   Installation =

    mitigate:
      case Path.Error(_, _)    => Install.Error(Install.Error.Reason.Environment)
      case Name.Error(_, _, _) => Install.Error(Install.Error.Reason.Environment)
      case guillotine.Exec.Error(_, _, _) => Install.Error(Install.Error.Reason.Environment)

    . protect:
        val scriptPath: Optional[Path on Local] =
          scala.caps.unsafe.unsafeAssumeSeparate:
            safely(sh"sh -c 'command -v ${entrypoint.script}'".exec[Path on Local]())

        val command: Text = entrypoint.script

        if !force && scriptPath != entrypoint.executable
        then Installation.CommandNotOnPath(entrypoint.script)
        else
          val zsh: Installation.InstallResult =
            if sh"sh -c 'command -v zsh'".exec[Exit]() != Exit.Ok
            then Installation.InstallResult.ShellNotInstalled(Shell.Zsh)
            else
              val dirNamesCmd = sh"zsh -c 'source ~/.zshrc 2> /dev/null; printf %s, $$fpath'"
              val dirNames = dirNamesCmd.exec[Text]().cut(t",")

              val dirs: List[Path on Linux] =
                dirNames.filter(_.trim != t"").bind: dir =>
                  safely(dir.as[Path on Linux]).lay(Nil: List[Path on Linux])(List(_))

              install(Shell.Zsh, command, Name[Linux](t"_$command"), dirs)

          val bash: Installation.InstallResult =
            if sh"sh -c 'command -v bash'".exec[Exit]() != Exit.Ok
            then Installation.InstallResult.ShellNotInstalled(Shell.Bash)
            else
              install
                ( Shell.Bash,
                  command,
                  Name[Linux](command),
                  List
                    // stdlib bridge: the native `last` is `Optional`, and the installer wants
                    // the last data directory outright — an empty `dataDirs` is a hard error
                    // here, as it always was.
                    ( Xdg.dataDirs[Path on Linux].stdlib.last/"bash-completion"/"completions",
                      Xdg.dataHome[Path on Linux]/"bash-completion"/"completions" ) )

          val fish: Installation.InstallResult =
            if sh"sh -c 'command -v fish'".exec[Exit]() != Exit.Ok
            then Installation.InstallResult.ShellNotInstalled(Shell.Fish)
            else
              install
                ( Shell.Fish,
                  command,
                  Name[Linux](t"$command.fish"),
                  List
                    // stdlib bridge, as for bash above.
                    ( Xdg.dataDirs[Path on Linux].stdlib.last/"fish"/"vendor_completions.d",
                      Xdg.configHome[Path on Linux]/"fish"/"completions" ) )

          val powershell: Installation.InstallResult =
            if sh"sh -c 'command -v pwsh'".exec[Exit]() != Exit.Ok
            then Installation.InstallResult.ShellNotInstalled(Shell.Powershell)
            else safely:
              val profile = scala.caps.unsafe.unsafeAssumeSeparate:
                sh"pwsh -NoProfile -Command 'echo $$PROFILE'".exec[Path on Linux]()
              val marker = t"# $command tab-completions"

              if profile.existent() && profile.read[Text].contains(marker)
              then Installation.InstallResult.AlreadyInstalled(Shell.Powershell, profile.encode)
              else
                Eof(profile).open(Write): handle ?=>
                  handle.write(script(Shell.Powershell, command).sysData)

                Installation.InstallResult.Installed(Shell.Powershell, profile.encode)

            .or(Installation.InstallResult.NoWritableLocation(Shell.Powershell))

          Installation.Shells(zsh, bash, fish, powershell)


  def install(shell: Shell, command: Text, scriptName: Name[Linux], dirs: List[Path on Linux])
    ( using erased effectful: Effectful )
    ( using Diagnostics )
  ( using (CliEvent is Loggable)^ )
  ( using Tactic[Install.Error] )
  :   Installation.InstallResult =

    mitigate:
      case Io.Error(_, _, _, _) => Install.Error(Install.Error.Reason.Io)
      case Name.Error(_, _, _)  => Install.Error(Install.Error.Reason.Io)
      case Path.Error(_, _)     => Install.Error(Install.Error.Reason.Io)
      case Truncation.Error(_)      => Install.Error(Install.Error.Reason.Io)

    . protect:
        dirs.seek { dir => dir.existent() && dir.writable() }.let: dir =>
          val path = dir/scriptName

          if path.existent()
          then Installation.InstallResult.AlreadyInstalled(shell, path.encode)
          else
            path.write(script(shell, command).sysData)
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

    case Shell.Powershell =>
      t"""|# $command tab-completions
          |Register-ArgumentCompleter -Native -CommandName '$command' -ScriptBlock {
          |    param($$wordToComplete, $$commandAst, $$cursorPosition)
          |    & '$command' '{completions}' powershell $$cursorPosition 0 '' `
          |        -- "$$($$commandAst.ToString())" |
          |    ForEach-Object {
          |        $$parts = $$_ -split "`t", 2
          |        $$name = $$parts[0]
          |        $$desc = if ($$parts.Length -gt 1) { $$parts[1] } else { $$name }
          |        [System.Management.Automation.CompletionResult]::new(
          |            $$name, $$name, 'ParameterValue', $$desc)
          |    }
          |}
          |""".s.stripMargin.tt

  enum Installation:
    case CommandNotOnPath(script: Text)

    case Shells
      ( zsh:        Installation.InstallResult,
        bash:       Installation.InstallResult,
        fish:       Installation.InstallResult,
        powershell: Installation.InstallResult )

    def paths: List[Text] =
      this match
        case CommandNotOnPath(_)              => Nil
        case Shells(zsh, bash, fish, pwsh) =>
          List(zsh, bash, fish, pwsh).map(_.pathname).sweep { case text: Text => text }

object CliEvent:
  given execEvent: CliEvent transcribes guillotine.Exec.Event = CliEvent.Exec(_)

  given communicable: CliEvent is Communicable =
    case Exec(event)          => m"execution error: $event"
    case Installing(location) => m"installing to $location"

enum CliEvent:
  case Exec(event: guillotine.Exec.Event)
  case Installing(location: Text)
