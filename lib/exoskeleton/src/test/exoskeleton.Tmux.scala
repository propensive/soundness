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

def enter(keypresses: (Text | Char)*)(using session: Tmux.Session): Unit raises TmuxError =
  given WorkingDirectory = session.workingDirectory
  import logging.silent
  mitigate:
    case ExecError(_, _, _) => TmuxError()

  . within:
      keypresses.each:
        case text: Text => sh"tmux send-keys -t ${session.id} '$text'".exec[Unit]()
        case char: Char => sh"tmux send-keys -t ${session.id} '$char'".exec[Unit]()

case class TmuxError()(using Diagnostics) extends Error(m"can't execute tmux")

object Tmux:
  case class Session(id: Text, workingDirectory: WorkingDirectory)
  case class Capture(screen: Text, cursor: (Ordinal, Ordinal))

  def session(width: Int = 80, height: Int = 24)[result](action: Session ?=> result)
       (using WorkingDirectory)
  : Capture raises TmuxError logs ExecEvent =

      mitigate:
        case ExecError(_, _, _) => TmuxError()
        case NumberError(_, _)  => TmuxError()
      . within:
          val session = Session(Uuid().show, summon[WorkingDirectory])
          sh"tmux new-session -d -s ${session.id} -x $width -y $height '/bin/zsh -l'".exec[Unit]()

          sh"tmux send-keys -t ${session.id} 'PS1=\"> \"; autoload -Uz compinit; compinit' C-m C-l"
          . exec[Unit]()

          action(using session)

          val content = sh"tmux capture-pane -pt ${session.id}".exec[Text]().trim
          val x = sh"tmux display-message -pt ${session.id} '#{cursor_x}'".exec[Text]().decode[Int].z
          val y = sh"tmux display-message -pt ${session.id} '#{cursor_y}'".exec[Text]().decode[Int].z

          sh"tmux kill-session -t ${session.id}".exec[Unit]()

          Capture(content, (x, y))
