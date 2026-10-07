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

import java.lang as jl

import scala.collection.mutable as scm

import ambience.*
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

import charsets.utf8Charset
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

        case Unconfirmed(Shell.Zsh, path) =>
          m"""The zsh completion script was installed to $path, but that directory is not on zsh's
              fpath (or zsh did not answer in time), so zsh will not find it until it is added."""

        case Unconfirmed(Shell.Bash, path) =>
          m"""The bash completion script was installed to $path, but bash does not load
              bash-completion (or bash did not answer in time), so it will not be used until
              bash-completion is installed and enabled."""

        case Unconfirmed(Shell.Fish, path) =>
          m"""The fish completion script was installed to $path, but that directory is not on
              fish's completion path (or fish did not answer in time)."""

        case Unconfirmed(shell, path) =>
          m"The $shell completion script was installed to $path, but $shell may not load it."

    enum InstallResult:
      case Installed(shell: Shell, path: Text)
      case AlreadyInstalled(shell: Shell, path: Text)
      case NoWritableLocation(shell: Shell)
      case ShellNotInstalled(shell: Shell)

      // Written, but the shell could not be shown to load it: the directory is not one the shell
      // searches, the shell lacks the machinery that would (bash without bash-completion), or
      // the shell did not answer in time.
      case Unconfirmed(shell: Shell, path: Text)

      def pathname: Optional[Text] = this.only:
        case Installed(_, path)        => path
        case AlreadyInstalled(_, path) => path
        case Unconfirmed(_, path)      => path


  // The environment and system are the invocation's, not the daemon JVM's, so the XDG
  // directories are those of the client asking for the install (#2034); a `Cli` in scope
  // supplies the `Environment`.
  def ensure(force: Boolean = false)
    ( using Entrypoint^, Environment, System, WorkingDirectory, Diagnostics )
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
    ( using Environment, System, WorkingDirectory, Diagnostics )
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
          import Installation.InstallResult.ShellNotInstalled

          def present(shell: Text): Boolean =
            Command(t"sh", t"-c", t"command -v $shell").exec[Exit]() == Exit.Ok

          // Each shell present is asked at once, so the queries run side by side and a slow or
          // hanging startup file costs the installer at most `queryTimeout` in all.
          val zshQuery = if present(t"zsh") then ask(zshQuestion) else Unset
          val bashQuery = if present(t"bash") then ask(bashQuestion) else Unset
          val fishQuery = if present(t"fish") then ask(fishQuestion) else Unset
          val pwshQuery = if present(t"pwsh") then ask(pwshQuestion) else Unset
          val deadline = jl.System.nanoTime + queryTimeout

          val zsh: Installation.InstallResult =
            zshQuery.lay(ShellNotInstalled(Shell.Zsh)): query =>
              installZsh(command, answer(query, deadline))

          val bash: Installation.InstallResult =
            bashQuery.lay(ShellNotInstalled(Shell.Bash)): query =>
              installBash(command, answer(query, deadline))

          val fish: Installation.InstallResult =
            fishQuery.lay(ShellNotInstalled(Shell.Fish)): query =>
              installFish(command, answer(query, deadline))

          val powershell: Installation.InstallResult =
            pwshQuery.lay(ShellNotInstalled(Shell.Powershell)): query =>
              installPowershell(command, answer(query, deadline))

          Installation.Shells(zsh, bash, fish, powershell)


  // How long the shells, asked together, have to say where they look for completions.
  private val queryTimeout: Long = 5_000_000_000L

  // The line that brackets each part of a shell's answer, so that anything else its startup
  // files print is ignored.
  private[exoskeleton] val marker: Text = t"--exoskeleton--"

  // zsh's `$fpath` is an unexported shell variable that only an interactive shell (one that has
  // read `.zshrc`) knows in full.
  private val zshQuestion: List[Text] =
    List(t"zsh", t"-i", t"-c", t"print -rl -- $marker $$fpath $marker")

  // Whether an interactive bash has loaded bash-completion, which alone reads the per-command
  // files in its completions directories: the loader function of version 2.12 and later, of
  // earlier 2.x versions, or of 1.x.
  private val bashQuestion: List[Text] =
    List
      ( t"bash", t"-i", t"-c",
        t"echo $marker; declare -F _comp_load __load_completion _completion_loader; echo $marker" )

  // fish reads its configuration even for `-c`, so these are the values an interactive fish has.
  private val fishQuestion: List[Text] =
    List
      ( t"fish", t"-c",
        t"printf '%s\\n' $marker $$__fish_config_dir $marker $$fish_complete_path $marker" )

  private val pwshQuestion: List[Text] =
    List
      ( t"pwsh", t"-NoProfile", t"-Command",
        t"Write-Output '$marker'; Write-Output $$PROFILE; Write-Output '$marker'" )

  // The variables a shell consults to find its configuration, passed on from the invocation's
  // environment so that the shell answers for the client that asked rather than for the daemon.
  private val consulted: List[Text] =
    List
      ( t"HOME", t"ZDOTDIR", t"XDG_CONFIG_HOME", t"XDG_DATA_HOME", t"XDG_DATA_DIRS",
        t"BASH_COMPLETION_USER_DIR" )

  // Starts a shell on a question, with its standard input closed and its standard error
  // discarded, so that a startup file which prompts or complains can neither hold it up nor
  // corrupt the answer.
  private[exoskeleton] def ask(question: List[Text])
    ( using Environment, WorkingDirectory, (CliEvent is Loggable)^, Tactic[Exec.Error] )
  :   Job[?, Text]^ =

    val variables = consulted.bind: name =>
      safely(Environment[Text](name)).lay(Nil) { (value: Text) => List(name ++ t"=" ++ value) }

    val prefix = List(t"sh", t"-c", t"exec \"$$@\" </dev/null 2>/dev/null", t"sh", t"env")
    Command((prefix ++ variables ++ question)*).fork[Text]()

  // A shell's answer, as the parts between its marker lines, or `Unset` if it gave none before
  // `deadline` (a `System.nanoTime` value), in which case it is killed.
  private[exoskeleton] def answer(query: Job[?, Text]^, deadline: Long)
    ( using (CliEvent is Loggable)^ )
  :   Optional[List[List[Text]]] =

    import abstractables.nanosecondsAbstractable

    val output: Optional[Text] = safely(query.await((deadline - jl.System.nanoTime).max(0L)))
    if output.absent then query.kill()
    output.let(sections(_))

  // The lines between successive marker lines; lines before the first marker and after the last
  // are discarded.
  private[exoskeleton] def sections(output: Text): Optional[List[List[Text]]] =
    val initial: (List[List[Text]], List[Text], Boolean) = (Nil, Nil, false)

    val (parts, _, _) = output.cut(t"\n").fold(initial):
      case ((parts, current, open), line) =>
        val trimmed = line.trim
        if trimmed == marker then (if open then current.reverse :: parts else parts, Nil, true)
        else if open then (parts, trimmed :: current, open)
        else (parts, current, open)

    if parts.nil then Unset else parts.reverse

  private def path(text: Text): Optional[Path on Linux] = safely(text.as[Path on Linux])

  // Writes `command`'s script for `shell` into `directory`, creating it if necessary, and reports
  // it as `Unconfirmed` unless the shell is known to read it from there.
  private def place
    ( shell: Shell, command: Text, scriptName: Name[Linux], directory: Path on Linux,
      loaded: Boolean )
    ( using erased effectful: Effectful )
    ( using Diagnostics )
  ( using (CliEvent is Loggable)^ )
  ( using Tactic[Install.Error] )
  :   Installation.InstallResult =

    import filesystemOptions.createNonexistentParents

    mitigate:
      case Io.Error(_, _, _, _) => Install.Error(Install.Error.Reason.Io)
      case Name.Error(_, _, _)  => Install.Error(Install.Error.Reason.Io)
      case Path.Error(_, _)     => Install.Error(Install.Error.Reason.Io)
      case Truncation.Error(_)  => Install.Error(Install.Error.Reason.Io)

    . protect:
        if !directory.existent() then directory.create[Directory]()
        val target = directory/scriptName
        val existed = target.existent()
        if !existed then target.write(script(shell, command).sysData)

        if !loaded then Installation.InstallResult.Unconfirmed(shell, target.encode)
        else if existed then Installation.InstallResult.AlreadyInstalled(shell, target.encode)
        else Installation.InstallResult.Installed(shell, target.encode)

  // Into the first writable `fpath` directory, preferring one under the user's home (which their
  // own configuration added) to a system one; failing both, into a user directory that zsh is
  // then told to look in.
  private def installZsh(command: Text, answer: Optional[List[List[Text]]])
    ( using erased effectful: Effectful )
    ( using Environment, System, Diagnostics )
  ( using (CliEvent is Loggable)^ )
  ( using Tactic[Install.Error], Tactic[Path.Error] )
  :   Installation.InstallResult =

    val home = safely(Environment[Text](t"HOME")).or(Directories.homeText)
    val fpath = answer.let(_.prim).or(Nil).bind(path(_).lay(Nil)(List(_)))
    val writable = fpath.filter { dir => dir.existent() && dir.writable() }
    val own = writable.filter(_.encode.starts(t"$home/"))
    val scriptName = unsafely(Name[Linux](t"_$command"))
    val zshFallback: Path on Linux = Xdg.dataHome[Path on Linux]/"zsh"/"site-functions"

    (own ++ writable).prim.lay(place(Shell.Zsh, command, scriptName, zshFallback, false)): dir =>
      place(Shell.Zsh, command, scriptName, dir, true)

  // Into bash-completion's user directory, which is never on a path the user must configure, but
  // which only bash-completion reads.
  private def installBash(command: Text, answer: Optional[List[List[Text]]])
    ( using erased effectful: Effectful )
    ( using Environment, System, Diagnostics )
  ( using (CliEvent is Loggable)^ )
  ( using Tactic[Install.Error], Tactic[Path.Error] )
  :   Installation.InstallResult =

    val base: Path on Linux =
      safely(Environment[Text](t"BASH_COMPLETION_USER_DIR")).let(path(_))
      . or(Xdg.dataHome[Path on Linux]/"bash-completion")

    val loaded = answer.let(_.prim).lay(false)(_.exists(_ != t""))
    place(Shell.Bash, command, unsafely(Name[Linux](command)), base/"completions", loaded)

  // Into the `completions` directory of fish's configuration directory, which fish always
  // searches; the vendor directories are for packagers.
  private def installFish(command: Text, answer: Optional[List[List[Text]]])
    ( using erased effectful: Effectful )
    ( using Environment, System, Diagnostics )
  ( using (CliEvent is Loggable)^ )
  ( using Tactic[Install.Error], Tactic[Path.Error] )
  :   Installation.InstallResult =

    val configuration: Path on Linux =
      answer.let(_.prim).let(_.prim).let(path(_)).or(Xdg.configHome[Path on Linux]/"fish")

    val directory = configuration/"completions"
    val searched = answer.lay(Nil)(List.drop(_, 1)).prim.or(Nil)
    val scriptName = unsafely(Name[Linux](t"$command.fish"))
    place(Shell.Fish, command, scriptName, directory, searched.has(directory.encode))

  // Appended to the profile PowerShell names, unless the script is already there.
  private def installPowershell(command: Text, answer: Optional[List[List[Text]]])
    ( using erased effectful: Effectful )
    ( using Environment, System, WorkingDirectory, Diagnostics )
  ( using (CliEvent is Loggable)^ )
  :   Installation.InstallResult =

    val unwritable = Installation.InstallResult.NoWritableLocation(Shell.Powershell)

    answer.let(_.prim).let(_.prim).lay(unwritable): (location: Text) =>
      safely:
        val profile = location.as[Path on Linux]
        val marker = t"# $command tab-completions"

        if profile.existent() && profile.read[Text].contains(marker)
        then Installation.InstallResult.AlreadyInstalled(Shell.Powershell, profile.encode)
        else
          Eof(profile).open(Write): handle ?=>
            handle.write(script(Shell.Powershell, command).sysData)

          Installation.InstallResult.Installed(Shell.Powershell, profile.encode)

      . or(unwritable)


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
