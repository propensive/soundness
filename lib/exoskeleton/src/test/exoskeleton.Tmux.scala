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
  val content = sh"tmux capture-pane -pt ${tmux.id}".exec[Text]().trim
  val x = sh"tmux display-message -pt ${tmux.id} '#{cursor_x}'".exec[Text]().decode[Int].z
  val y = sh"tmux display-message -pt ${tmux.id} '#{cursor_y}'".exec[Text]().decode[Int].z

  Screenshot(content, (x, y))


case class TmuxError()(using Diagnostics) extends Error(m"can't execute tmux")
case class Tmux(id: Text, workingDirectory: WorkingDirectory)
case class Screenshot(screen: Text, cursor: (Ordinal, Ordinal))

extension (tool: Tool)
  def tmux(width: Int = 80, height: Int = 24)[result](action: Tmux ?=> result)
       (using WorkingDirectory)
  : result raises TmuxError logs ExecEvent =

      mitigate:
        case ExecError(_, _, _) => TmuxError()
        case NumberError(_, _)  => TmuxError()
      . within:
          val tmux = Tmux(Uuid().show, summon[WorkingDirectory])
          sh"tmux new-session -d -s ${tmux.id} -x $width -y $height '/bin/zsh -l'".exec[Unit]()

          sh"tmux send-keys -t ${tmux.id} 'PS1=\"> \"; autoload -Uz compinit; compinit' C-m C-l"
          . exec[Unit]()

          val result = action(using tmux)

          sh"tmux kill-session -t ${tmux.id}".exec[Exit]()

          result
