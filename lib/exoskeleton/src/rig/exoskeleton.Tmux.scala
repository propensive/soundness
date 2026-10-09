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

import soundness.*

import errorDiagnostics.stackTracesDiagnostics

// A pane in a tmux session, an independent terminal emulator, kept so that yossarian's rendering
// can be compared with it. Keys are sent with `send-keys`, which interprets tmux's own key names,
// and the screen is read with `capture-pane`; each is a subprocess, so this backend is slow.
case class Tmux
  ( id:               Text,
    workingDirectory: WorkingDirectory,
    environment:      Environment,
    width:            Int,
    height:           Int,
    shell:            Shell )
extends Pane, Findable:

  def press(keys: Text)(using Tactic[Pane.Error]): Unit =
    given WorkingDirectory = workingDirectory
    given Environment = environment
    import logging.silentLogging

    mitigate:
      case guillotine.Exec.Error(_, _, _) => Pane.Error(Pane.Error.Reason.ExecFailed)

    . protect(sh"tmux send-keys -t $id '$keys'".exec[Unit]())

  // Delivers `SIGWINCH`, and tmux's own reflow, to the application in the pane. Sessions are
  // created detached with an explicit size, which is the case `resize-window` controls.
  def resize(width: Int, height: Int)(using Tactic[Pane.Error]): Unit =
    given WorkingDirectory = workingDirectory
    given Environment = environment
    import logging.silentLogging

    mitigate:
      case guillotine.Exec.Error(_, _, _) => Pane.Error(Pane.Error.Reason.ExecFailed)

    . protect(sh"tmux resize-window -t $id -x $width -y $height".exec[Unit]())

  def screenshot()(using Tactic[Pane.Error]): Screenshot =
    given WorkingDirectory = workingDirectory
    given Environment = environment
    import logging.silentLogging

    mitigate:
      case guillotine.Exec.Error(_, _, _) => Pane.Error(Pane.Error.Reason.SessionDied)
      case Number.Error(_, _, _)          => Pane.Error(Pane.Error.Reason.SessionDied)

    . protect:
        val content = sh"tmux capture-pane -pt $id".exec[List[Text]]().to[Array]
        val x = sh"tmux display-message -pt $id '#{cursor_x}'".exec[Text]().trim.as[Int].z
        val y = sh"tmux display-message -pt $id '#{cursor_y}'".exec[Text]().trim.as[Int].z

        Screenshot(content, (width, height), (x, y))
