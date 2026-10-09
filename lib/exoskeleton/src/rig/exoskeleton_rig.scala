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

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import filesystemBackends.javaBaseFilesystem
import probates.cancelProbate

extension (shell: Shell)
  // A pane on a pseudo-terminal, rendered by yossarian, running `shell` as `rig.launch` starts it.
  //
  // Explicit `using` evidence instead of `raises`/`logs` sugar: a context-function result would
  // hide the parameters, which the separation checker rejects.
  def pane(width: Int = 80, height: Int = 24)[result](action: (pane: Pane) ?=> result)
    ( using WorkingDirectory, Environment, Enclave.Tool, Monitor )
    ( using Tactic[Pane.Error], (guillotine.Exec.Event is Loggable)^ )
  :   result =

    val command = rig.launch(shell, terminal = true)

    mitigate:
      case guillotine.Exec.Error(_, _, _) => Pane.Error(Pane.Error.Reason.ExecFailed)

    . protect:
        command.pty(width, height).session: job ?=>
          val display = Pane.Display(width, height)

          // The output is rendered, and the shell's queries answered, from the moment it starts,
          // by two tasks which end with the session: one when the terminal's output ends, the
          // other when `finish` stops yossarian's replies.
          val reader = async(safely(job.stdout().chunks.each(display.render(_))))
          val answers = async(safely(display.answer(job)))

          try
            val pane = Pane.Emulated(shell, display, job)
            rig.ready(using pane)
            action(using pane)
          finally
            display.finish()
            safely(answers.await())

  // A pane in a tmux session, running `shell` exactly as `pane` does, to compare yossarian's
  // rendering with an independent terminal emulator's.
  def tmux(width: Int = 80, height: Int = 24)[result](action: (pane: Pane) ?=> result)
    ( using WorkingDirectory, Environment, Enclave.Tool, Monitor )
    ( using Tactic[Pane.Error], (guillotine.Exec.Event is Loggable)^ )
  :   result =

    val command = rig.launch(shell, terminal = false)

    val tmux =
      Tmux(Uuid().show, summon[WorkingDirectory], summon[Environment], width, height, shell)

    mitigate:
      case guillotine.Exec.Error(_, _, _) => Pane.Error(Pane.Error.Reason.ExecFailed)

    . protect:
        sh"tmux new-session -d -s ${tmux.id} -x $width -y $height ${command.escape}".exec[Unit]()

        try
          rig.ready(using tmux)
          action(using tmux)
        finally safely(sh"tmux kill-session -t ${tmux.id}".exec[Exit]())

object rig:
  // The command which starts `shell` for a test: under the tool's scratch home, with the tool's
  // directory first on the `PATH`, and with a configuration of the rig's own — a prompt of `> `,
  // and the tool's completion script loaded — in place of any the user has. `terminal` sets
  // `TERM`, which tmux sets for itself.
  def launch(shell: Shell, terminal: Boolean)(using tool: Enclave.Tool, environment: Environment)
    ( using Tactic[Pane.Error] )
  :   Command =

    val home = tool.home
    val directory = tool.path.parent.lay(abort(Pane.Error(Pane.Error.Reason.ExecFailed)))(_.encode)
    def quote(text: Text): Text = t"'${text.sub(t"'", t"'\\\\''")}'"

    // Named methods, not lambdas: interpolating inside a lambda passed to a combinator runs the
    // interpolator's implicit search while the combinator's type is still uninstantiated,
    // tripping dotc's `wildApprox` assertion (scala/scala3#24824).
    def prefixed(rest: Text): Text = t"$directory:$rest"
    def assignment(variable: (Text, Text)): Text = t"${variable(0)}=${variable(1)}"

    val path = environment.variable(t"PATH").lay(directory)(prefixed)

    val variables: List[(Text, Text)] =
      val own = List(t"PATH" -> path, t"POWERSHELL_UPDATECHECK" -> t"Off")
      val term = if terminal then List(t"TERM" -> t"xterm-256color") else Nil
      List.concat(List.concat(Enclave.scratch(home).to[List], own), term)

    // Writes a configuration file into `directory`, a path of names under the scratch home.
    def write(directory: List[Text], name: Text, content: Text): Text =
      def unconfigured(problem: Message): Pane.Error =
        Pane.Error(Pane.Error.Reason.Unconfigured(problem.text))

      // Each directory is created in turn, from the scratch home down.
      mitigate:
        case error@Io.Error(_, _, _, _) => unconfigured(error.message)
        case error@Path.Error(_, _)     => unconfigured(error.message)
        case error@Name.Error(_, _, _)  => unconfigured(error.message)

      . protect:
          val parent = directory.fuse(home):
            val child = state/next
            if !child.existent() then child.create[Directory]()
            child

          val file = parent/name

          // Each pane writes its configuration afresh, over whatever an earlier pane wrote.
          locally:
            import filesystemOptions.deleteOnlyEmpty
            if file.existent() then file.delete()

          file.write(content.sysData)
          file.encode

    // The tool's completion script for each shell, generated as `install` would write it, but
    // into the scratch home, where the shell's configuration loads it.
    def script(directory: List[Text], name: Text): Text =
      write(directory, name, Completions.script(shell, tool.command))

    val invocation: List[Text] = shell match
      case Shell.Bash =>
        val completions = List(t".local", t"share", t"bash-completion", t"completions")
        val source = t". ${quote(script(completions, tool.command))}"

        val rc = write(List(t".rig"), t"bashrc", t"""PS1='> '
          |_init_completion() { return 0; }
          |$source
          |bind "set show-all-if-ambiguous on"
          |bind "set show-all-if-unmodified on"
          |""".s.stripMargin.tt)

        List(t"bash", t"--rcfile", rc, t"-i")

      case Shell.Zsh =>
        val functions = List(t".rig", t"functions")
        val fpath = t"fpath+=(${quote(rig.directory(script(functions, t"_${tool.command}")))})"

        val rc = write(List(t".rig", t"zsh"), t".zshrc", t"""PROMPT='> '
          |RPROMPT=''
          |$fpath
          |autoload -Uz compinit
          |compinit -u -d ${quote(t"${home.encode}/.rig/zcompdump")}
          |""".s.stripMargin.tt)

        List(t"env", t"ZDOTDIR=${rig.directory(rc)}", t"zsh", t"-i")

      case Shell.Fish =>
        val completions = List(t".config", t"fish", t"completions")
        val source = t"source ${quote(script(completions, t"${tool.command}.fish"))}"

        write(List(t".config", t"fish"), t"config.fish", t"""set -g fish_greeting ''
          |function fish_prompt; echo -n '> '; end
          |function fish_right_prompt; end
          |$source
          |""".s.stripMargin.tt)

        List(t"fish", t"-i")

      case Shell.Powershell =>
        val profile = write(List(t".rig"), t"profile.ps1", rig.powershell(tool.command))
        List(t"pwsh", t"-NoLogo", t"-NoProfile", t"-NoExit", t"-File", profile)

    val assignments = variables.map(assignment)
    Command(List.concat(t"env" :: assignments, invocation)*)

  // The directory part of a path.
  def directory(path: Text): Text = Text(path.s.substring(0, path.s.lastIndexOf('/').max(0)).nn)

  // Waits until the shell has started and loaded its configuration, then clears the screen, so a
  // test begins with a prompt alone. Every shell but PowerShell is asked to echo a marker, whose
  // appearance shows every line of its configuration has been processed.
  def ready(using pane: Pane)(using Monitor, Tactic[Pane.Error]): Unit =
    val shell = pane.shell.toString.tt
    if !Pane.waitFor(_.starts(t">")) then abort(Pane.Error(Pane.Error.Reason.NotReady(shell)))

    // The marker is short, so that neither it nor the command echoing it wraps in a narrow pane,
    // where no single line would match it; it need only be unlikely to appear by chance.
    if pane.shell != Shell.Powershell then
      val marker = t"RDY${(jl.System.nanoTime % 100000).toString.tt}"
      Pane.enter(t"echo $marker", '\r')

      if !Pane.waitFor(_.trim == marker) then abort(Pane.Error(Pane.Error.Reason.NotReady(shell)))

      Pane.attend(t"C-l")

  // A PowerShell profile giving a prompt of `> `, completing `command` on Tab through its own
  // completion protocol, and defining `_completions`, which prints each completion of a line as
  // `name@@description`.
  def powershell(command: Text): Text =
    s"""using namespace Microsoft.PowerShell
       |function global:prompt { '> ' }
       |try {
       |    Set-PSReadLineKeyHandler -Key Tab -ScriptBlock {
       |        param($$key, $$arg)
       |        $$line = $$null; $$cursor = $$null
       |        [PSConsoleReadLine]::GetBufferState([ref]$$line, [ref]$$cursor)
       |        $$ws = $$cursor
       |        while ($$ws -gt 0 -and $$line[$$ws - 1] -ne ' ') { $$ws-- }
       |        $$w = $$line.Substring($$ws, $$cursor - $$ws)
       |        $$cmpArgs = @('{completions}', 'powershell', "$$cursor", `
       |                      '0', '-', '--', $$line)
       |        $$results = @(& '$command' @cmpArgs 2>$$null |
       |            ForEach-Object { ($$_ -split "`t", 2)[0] })
       |        $$matching = @($$results | Where-Object { $$_.StartsWith($$w) })
       |        if ($$matching.Count -eq 0) { return }
       |        $$lcp = $$matching[0]
       |        for ($$i = 1; $$i -lt $$matching.Count; $$i++) {
       |            $$m = $$matching[$$i]; $$j = 0
       |            while ($$j -lt $$lcp.Length -and $$j -lt $$m.Length `
       |                   -and $$lcp[$$j] -eq $$m[$$j]) { $$j++ }
       |            $$lcp = $$lcp.Substring(0, $$j)
       |        }
       |        if ($$matching.Count -eq 1) {
       |            [PSConsoleReadLine]::Replace($$ws, $$cursor - $$ws, $$lcp + ' ')
       |            [PSConsoleReadLine]::SetCursorPosition($$ws + $$lcp.Length + 1)
       |        } elseif ($$lcp.Length -gt $$w.Length) {
       |            [PSConsoleReadLine]::Replace($$ws, $$cursor - $$ws, $$lcp)
       |            [PSConsoleReadLine]::SetCursorPosition($$ws + $$lcp.Length)
       |        }
       |    }
       |} catch {}
       |function global:_completions {
       |    param($$text)
       |    $$line = '$command ' + $$text
       |    $$cursor = $$line.Length
       |    $$cmpArgs = @('{completions}', 'powershell', "$$cursor", `
       |                  '0', '-', '--', $$line)
       |    & '$command' @cmpArgs 2>$$null | ForEach-Object {
       |        $$p = $$_ -split "`t", 2
       |        $$n = $$p[0].TrimEnd()
       |        if ($$p.Length -gt 1 -and $$p[1] -cne $$n) `
       |        { "$$n@@$$($$p[1])" } else { $$n }
       |    }
       |}
       |""".stripMargin.tt
