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
┃    Soundness, version 0.41.0.                                                                    ┃
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

def enter(keypresses: (Text | Char)*)(using tmux: Tmux): Unit raises TmuxError =
  given WorkingDirectory = tmux.workingDirectory
  import logging.silent
  mitigate:
    case ExecError(_, _, _) => TmuxError()

  . within:
      keypresses.each:
        case text: Text => sh"tmux send-keys -t ${tmux.id} '$text'".exec[Unit]()
        case char: Char => sh"tmux send-keys -t ${tmux.id} '$char'".exec[Unit]()

def screenshot()(using tmux: Tmux)(using WorkingDirectory): Screenshot = unsafely:
  import logging.silent
  val content = IArray.from(sh"tmux capture-pane -pt ${tmux.id}".exec[List[Text]]())
  val x = sh"tmux display-message -pt ${tmux.id} '#{cursor_x}'".exec[Text]().trim.decode[Int].z
  val y = sh"tmux display-message -pt ${tmux.id} '#{cursor_y}'".exec[Text]().trim.decode[Int].z

  Screenshot(content, (tmux.width, tmux.height), (x, y))


case class TmuxError()(using Diagnostics) extends Error(m"can't execute tmux")
case class Tmux(id: Text, workingDirectory: WorkingDirectory, width: Int, height: Int)

case class Screenshot(screen: IArray[Text], size: (Int, Int), cursor: (Ordinal, Ordinal)):
  def apply(): Text = screen.join("\n")
  def cursor(char: Char): Text = ???



def tmux(shell: Shell = Shell.Zsh, width: Int = 80, height: Int = 24)[result]
      (action: Tmux ?=> result)
      (using WorkingDirectory, Tool)
: result raises TmuxError logs ExecEvent =

    mitigate:
      case ExecError(_, _, _) => TmuxError()
      case NumberError(_, _)  => TmuxError()
    . within:
        val tmux = Tmux(Uuid().show, summon[WorkingDirectory], width, height)
        val shellPath = shell match
          case Shell.Zsh  => t"zsh"
          case Shell.Fish => t"fish"
          case Shell.Bash => t"bash"

        sh"tmux new-session -d -s ${tmux.id} -x $width -y $height '$shellPath -l'".exec[Unit]()

        val path = summon[Tool].path.parent.vouch.encode
        Thread.sleep(100)

        shell match
          case Shell.Zsh  =>
            val fpath: Path on Linux = summon[Tool].path.parent.vouch / "zsh"
            unsafely(fpath.make[Directory]())
            sh"""tmux send-keys -t ${tmux.id} "PS1='> '" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} "path+=(\"$path\")" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} "fpath+=(\"$fpath\")" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} "autoload -Uz compinit; compinit" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Bash =>
            sh"""tmux send-keys -t ${tmux.id} "PS1='> '" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} 'export PATH="$path:$$PATH"' C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

          case Shell.Fish => ()
            sh"""tmux send-keys -t ${tmux.id} "function fish_prompt; echo -n '> '; end" C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} 'fish_add_path "$path"' C-m""".exec[Unit]()
            sh"""tmux send-keys -t ${tmux.id} C-l""".exec[Unit]()

        val result = action(using tmux)

        sh"tmux kill-session -t ${tmux.id}".exec[Exit]()

        result
