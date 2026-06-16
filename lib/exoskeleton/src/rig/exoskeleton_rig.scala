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

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import interfaces.paths.pathOnLinux

extension (shell: Shell)
  def tmux(width: Int = 80, height: Int = 24)[result](action: (tmux: Tmux) ?=> result)
    ( using WorkingDirectory, Enclave.Tool, Monitor, TemporaryDirectory )
  :   result raises TmuxError logs ExecEvent =

    whereas:
      case ExecError(_, _, _)   => TmuxError(TmuxError.Reason.ExecFailed)
      case NumberError(_, _, _) => TmuxError(TmuxError.Reason.SessionDied)
      case IoError(_, _, _, _)  => TmuxError(TmuxError.Reason.ExecFailed)
      case StreamError(_)       => TmuxError(TmuxError.Reason.ExecFailed)

    . mitigate:
        given tmux: Tmux = Tmux(Uuid().show, summon[WorkingDirectory], width, height, shell)

        val path = summon[Enclave.Tool].path.parent.vouch.encode

        var psFile: Optional[Path on Linux] = Unset

        val shellBinary = shell match
          case Shell.Zsh        => t"zsh"
          case Shell.Fish       => t"fish"
          case Shell.Bash       => t"bash"
          case Shell.Powershell => t"pwsh"

        locally:
          import logging.silent

          if sh"which $shellBinary".exec[Exit]() != Exit.Ok
          then abort(TmuxError(TmuxError.Reason.ShellNotInstalled(shellBinary)))

        val shellInvocation = shell match
          case Shell.Zsh        => t"zsh -l"
          case Shell.Fish       => t"fish -l"
          case Shell.Bash       => t"bash -l"

          case Shell.Powershell =>
            val cmd = summon[Enclave.Tool].command

            val psScript =
              s"""using namespace Microsoft.PowerShell
                 |function global:prompt { '> ' }
                 |$$env:PATH = "${path}:" + $$env:PATH
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
                 |        $$results = @(& '$cmd' @cmpArgs 2>$$null |
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
                 |    $$line = '$cmd ' + $$text
                 |    $$cursor = $$line.Length
                 |    $$cmpArgs = @('{completions}', 'powershell', "$$cursor", `
                 |                  '0', '-', '--', $$line)
                 |    & '$cmd' @cmpArgs 2>$$null | ForEach-Object {
                 |        $$p = $$_ -split "`t", 2
                 |        $$n = $$p[0].TrimEnd()
                 |        if ($$p.Length -gt 1 -and $$p[1] -cne $$n) `
                 |        { "$$n@@$$($$p[1])" } else { $$n }
                 |    }
                 |}
                 |""".stripMargin

            import filesystemOptions.writeAccess.enabled
            import filesystemOptions.readAccess.disabled
            import filesystemOptions.createNonexistent.enabled
            import filesystemOptions.createNonexistentParents.disabled
            import filesystemOptions.dereferenceSymlinks.enabled
            import charEncoders.utf8

            val file: Path on Linux = unsafely(temporaryDirectory/t"exoskeleton-${Uuid()}.ps1")
            file.open(psScript.tt.writeTo(_))
            psFile = file
            t"POWERSHELL_UPDATECHECK=Off pwsh -NoLogo -NoExit -File ${file.encode}"

        sh"tmux new-session -d -s ${tmux.id} -x $width -y $height '$shellInvocation'".exec[Unit]()

        Tmux.attend:
          ()

        shell match
          case Shell.Zsh =>
            val command = t"""precmd_functions=() preexec_functions=() PROMPT="> " RPROMPT="""""
            sh"""tmux send-keys -t ${tmux.id} $command C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} "path+=(\"$path\")" C-m""".exec[Unit]()

            sh"""tmux send-keys -t ${tmux.id} "autoload -Uz compinit; compinit -u" C-m"""
            . exec[Unit]()

            // `compinit` walks every completion function and can take seconds
            // under Docker load; without an explicit ready-marker the test
            // would start sending keys (including TAB) before the completion
            // system is initialised, so TAB resolves to nothing.
            sh"""tmux send-keys -t ${tmux.id} 'echo READY-${tmux.id}' C-m""".exec[Unit]()
            var zshReady = false
            var zshAttempts = 0

            while !zshReady && zshAttempts < 666 do
              delay(0.03*Second)
              zshReady = Tmux.screenshot().screen.filter(_.trim == t"READY-${tmux.id}").length > 0
              zshAttempts += 1

            Tmux.attend:
              sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Bash =>
            val cmd = summon[Enclave.Tool].command

            sh"""tmux send-keys -t ${tmux.id} "PS1='> '" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} 'export PATH="$path:$$PATH"' C-m""".exec[Unit]()

            // Stock bash on macOS does not ship with bash-completion 2, which
            // defines `_init_completion` (used by exoskeleton's installed bash
            // completion script) and provides on-demand loading from the XDG
            // bash-completion directories. Stub `_init_completion` as a no-op
            // and source the installed completion script directly so the test
            // doesn't depend on bash-completion 2 being present.
            sh"""tmux send-keys -t ${tmux.id} '_init_completion() { return 0; }' C-m"""
            . exec[Unit]()

            val sourceScript =
              t"""for d in "$$XDG_DATA_HOME" "$$HOME/.local/share" /usr/local/share """+
              t"""/usr/share; do [ -n "$$d" ] && """+
              t"""[ -r "$$d/bash-completion/completions/$cmd" ] && """+
              t""". "$$d/bash-completion/completions/$cmd" && break; done"""

            sh"""tmux send-keys -t ${tmux.id} '$sourceScript' C-m""".exec[Unit]()

            sh"""tmux send-keys -t ${tmux.id} 'bind "set show-all-if-ambiguous on"' C-m"""
            . exec[Unit]()

            sh"""tmux send-keys -t ${tmux.id} 'bind "set show-all-if-unmodified on"' C-m"""
            . exec[Unit]()

            // Send a marker echo and wait for it to appear, so we know every
            // setup command above has been processed before the test starts
            // sending keystrokes.
            sh"""tmux send-keys -t ${tmux.id} 'echo READY-${tmux.id}' C-m""".exec[Unit]()
            var bashReady = false
            var bashAttempts = 0

            while !bashReady && bashAttempts < 666 do
              delay(0.03*Second)
              bashReady = Tmux.screenshot().screen.filter(_.trim == t"READY-${tmux.id}").length > 0
              bashAttempts += 1

            Tmux.attend:
              sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Fish =>
            val cmd = summon[Enclave.Tool].command

            sh"""tmux send-keys -t ${tmux.id} "function fish_prompt; echo -n '> '; end" C-m"""
            . exec[Unit]()

            // Override fish_right_prompt too — the user may have a global
            // override (e.g. git branch indicator) that would otherwise
            // appear at the end of the line and break exact-text assertions.
            sh"""tmux send-keys -t ${tmux.id} "function fish_right_prompt; end" C-m"""
            . exec[Unit]()

            sh"""tmux send-keys -t ${tmux.id} 'fish_add_path --global "$path"' C-m"""
            . exec[Unit]()

            // Fish auto-loads completions from $XDG_CONFIG_HOME/fish/completions/
            // lazily on first completion request, but the loaded script's
            // `complete -c $cmd -a '(completions)'` only takes effect once
            // fish has parsed the file. Source the installed script explicitly
            // so it's registered before the first test key is sent.
            val sourceFish =
              t"""for d in $$XDG_CONFIG_HOME $$HOME/.config; """+
              t"""test -r "$$d/fish/completions/$cmd.fish"; """+
              t"""and source "$$d/fish/completions/$cmd.fish"; and break; end"""

            sh"""tmux send-keys -t ${tmux.id} '$sourceFish' C-m""".exec[Unit]()

            // Fish's login-shell startup (welcome banner + greeting) can still
            // be drawing when the next send-keys arrives, so prior setup
            // commands get echoed into the banner rather than processed. Send
            // a marker echo and wait for it to appear before the test starts.
            sh"""tmux send-keys -t ${tmux.id} 'echo READY-${tmux.id}' C-m""".exec[Unit]()
            var fishReady = false
            var fishAttempts = 0

            while !fishReady && fishAttempts < 666 do
              delay(0.03*Second)
              fishReady = Tmux.screenshot().screen.filter(_.trim == t"READY-${tmux.id}").length > 0
              fishAttempts += 1

            Tmux.attend:
              sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Powershell =>
            var psReady = false
            var psAttempts = 0

            while !psReady && psAttempts < 666 do
              delay(0.03*Second)
              psReady = Tmux.screenshot().screen.filter(_.starts(t">")).length > 0
              psAttempts += 1

        val result = action

        sh"tmux kill-session -t ${tmux.id}".exec[Exit]()

        psFile.let: file =>
          import filesystemOptions.deleteRecursively.disabled
          safely(file.delete())

        result
