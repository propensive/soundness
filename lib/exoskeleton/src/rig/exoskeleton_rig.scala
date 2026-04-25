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

import errorDiagnostics.stackTraces

extension (shell: Shell)
  def tmux(width: Int = 80, height: Int = 24)[result](action: (tmux: Tmux) ?=> result)
    ( using WorkingDirectory, Sandbox.Tool, Monitor )
  :   result raises TmuxError logs ExecEvent =

    mitigate:
      case ExecError(_, _, _) => TmuxError()
      case NumberError(_, _)  => TmuxError()

    . within:
        given tmux: Tmux = Tmux(Uuid().show, summon[WorkingDirectory], width, height, shell)

        val path = summon[Sandbox.Tool].path.parent.vouch.encode

        val shellInvocation = shell match
          case Shell.Zsh        => t"zsh -l"
          case Shell.Fish       => t"fish -l"
          case Shell.Bash       => t"bash -l"
          case Shell.Powershell =>
            val cmd = summon[Sandbox.Tool].command
            sh"${summon[Sandbox.Tool].path} '{admin}' install".exec[Unit]()

            val psScript =
              s"""function global:prompt { '> ' }
                 |$$env:PATH = "${path}:" + $$env:PATH
                 |try {
                 |    Set-PSReadLineKeyHandler -Key Tab -ScriptBlock {
                 |        param($$key, $$arg)
                 |        $$line = $$null; $$cursor = $$null
                 |        [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$$line, [ref]$$cursor)
                 |        $$wordStart = $$cursor
                 |        while ($$wordStart -gt 0 -and $$line[$$wordStart - 1] -ne ' ') { $$wordStart-- }
                 |        $$w = $$line.Substring($$wordStart, $$cursor - $$wordStart)
                 |        $$results = @(& '$cmd' '{completions}' powershell $$cursor 0 '' -- $$line 2>&1 |
                 |            ForEach-Object { ($$_ -split "`t", 2)[0] })
                 |        $$matching = @($$results | Where-Object { $$_.StartsWith($$w) })
                 |        if ($$matching.Count -eq 0) { return }
                 |        $$lcp = $$matching[0]
                 |        for ($$i = 1; $$i -lt $$matching.Count; $$i++) {
                 |            $$m = $$matching[$$i]; $$j = 0
                 |            while ($$j -lt $$lcp.Length -and $$j -lt $$m.Length -and $$lcp[$$j] -eq $$m[$$j]) { $$j++ }
                 |            $$lcp = $$lcp.Substring(0, $$j)
                 |        }
                 |        if ($$matching.Count -eq 1) {
                 |            [Microsoft.PowerShell.PSConsoleReadLine]::Replace($$wordStart, $$cursor - $$wordStart, $$lcp + ' ')
                 |            [Microsoft.PowerShell.PSConsoleReadLine]::SetCursorPosition($$wordStart + $$lcp.Length + 1)
                 |        } elseif ($$lcp.Length -gt $$w.Length) {
                 |            [Microsoft.PowerShell.PSConsoleReadLine]::Replace($$wordStart, $$cursor - $$wordStart, $$lcp)
                 |            [Microsoft.PowerShell.PSConsoleReadLine]::SetCursorPosition($$wordStart + $$lcp.Length)
                 |        }
                 |    }
                 |} catch {}
                 |function global:_completions {
                 |    param($$text)
                 |    $$line = '$cmd ' + $$text
                 |    $$cursor = $$line.Length
                 |    & '$cmd' '{completions}' powershell $$cursor 0 '' -- $$line 2>&1 | ForEach-Object {
                 |        $$p = $$_ -split "`t", 2
                 |        $$n = $$p[0].TrimEnd()
                 |        if ($$p.Length -gt 1 -and $$p[1] -cne $$n) { "$$n@@$$($$p[1])" } else { $$n }
                 |    }
                 |}
                 |""".stripMargin

            val psFile = java.io.File.createTempFile("exoskeleton-", ".ps1", java.io.File("/tmp").nn).nn
            psFile.deleteOnExit()
            val writer = java.io.FileWriter(psFile)
            writer.write(psScript)
            writer.close()
            t"POWERSHELL_UPDATECHECK=Off pwsh -NoLogo -NoExit -File ${psFile.getAbsolutePath.nn.tt}"

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

            Tmux.attend:
              sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Bash =>
            sh"""tmux send-keys -t ${tmux.id} "PS1='> '" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} 'export PATH="$path:$$PATH"' C-m""".exec[Unit]()

            sh"""tmux send-keys -t ${tmux.id} 'bind "set show-all-if-ambiguous on"' C-m"""
            . exec[Unit]()

            sh"""tmux send-keys -t ${tmux.id} 'bind "set show-all-if-unmodified on"' C-m"""
            . exec[Unit]()

            Tmux.attend:
              sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Fish =>
            sh"""tmux send-keys -t ${tmux.id} "function fish_prompt; echo -n '> '; end" C-m"""
            . exec[Unit]()

            sh"""tmux send-keys -t ${tmux.id} 'fish_add_path --global "$path"' C-m"""
            . exec[Unit]()

            Tmux.attend:
              sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Powershell =>
            var psReady = false
            var psAttempts = 0
            while !psReady && psAttempts < 200 do
              delay(0.1*Second)
              psReady = Tmux.screenshot().screen.filter(_.starts(t">")).length > 0
              psAttempts += 1

            Tmux.attend:
              sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

        val result = action

        sh"tmux kill-session -t ${tmux.id}".exec[Exit]()

        result
