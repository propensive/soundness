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
┃    Soundness, version 0.43.0.                                                                    ┃
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
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.overwritePreexisting.disabled

extension (shell: Shell)
  def tmux(width: Int = 80, height: Int = 24)[result](action: (tmux: Tmux) ?=> result)
        (using WorkingDirectory, Sandbox.Tool, Monitor)
  : result raises TmuxError logs ExecEvent =

      mitigate:
        case ExecError(_, _, _) => TmuxError()
        case NumberError(_, _)  => TmuxError()

      . within:
          given tmux: Tmux = Tmux(Uuid().show, summon[WorkingDirectory], width, height, shell)
          val shellPath = shell match
            case Shell.Zsh  => t"zsh"
            case Shell.Fish => t"fish"
            case Shell.Bash => t"bash"

          sh"tmux new-session -d -s ${tmux.id} -x $width -y $height '$shellPath -l'".exec[Unit]()
          Tmux.attend:
            ()

          val path = summon[Sandbox.Tool].path.parent.vouch.encode

          shell match
            case Shell.Zsh  =>
              sh"""tmux send-keys -t ${tmux.id} 'precmd_functions=() preexec_functions=() PROMPT="> " RPROMPT=""' C-m""".exec[Unit]()
              sh"""tmux send-keys -t ${tmux.id} "path+=(\"$path\")" C-m""".exec[Unit]()
              sh"""tmux send-keys -t ${tmux.id} "autoload -Uz compinit; compinit -u" C-m""".exec[Unit]()
              Tmux.attend:
                sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

            case Shell.Bash =>
              sh"""tmux send-keys -t ${tmux.id} "PS1='> '" C-m""".exec[Unit]()
              sh"""tmux send-keys -t ${tmux.id} 'export PATH="$path:$$PATH"' C-m""".exec[Unit]()
              sh"""tmux send-keys -t ${tmux.id} 'bind "set show-all-if-ambiguous on"' C-m""".exec[Unit]()
              sh"""tmux send-keys -t ${tmux.id} 'bind "set show-all-if-unmodified on"' C-m""".exec[Unit]()
              Tmux.attend:
                sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

            case Shell.Fish =>
              sh"""tmux send-keys -t ${tmux.id} "function fish_prompt; echo -n '> '; end" C-m""".exec[Unit]()
              sh"""tmux send-keys -t ${tmux.id} 'fish_add_path --global "$path"' C-m""".exec[Unit]()
              Tmux.attend:
                sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          val result = action

          sh"tmux kill-session -t ${tmux.id}".exec[Exit]()

          result
