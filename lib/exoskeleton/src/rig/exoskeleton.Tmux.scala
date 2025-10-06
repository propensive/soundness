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
┃    Soundness, version 0.42.0.                                                                    ┃
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

object Tmux:
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

  def attend(using tmux: Tmux)[result](block: => result)(using Monitor, WorkingDirectory): result =
    val init = screenshot().screen
    var count = 0
    block.also:
      while init === screenshot().screen && count < 60 do delay(10L) yet (count += 1)


  def completions(text: Text)(using tool: Sandbox.Tool, tmux: Tmux)(using Monitor, WorkingDirectory)
  : Text raises TmuxError =

      enter(tool.command)
      enter(' ')
      enter(text)
      attend(enter('\t'))
      screenshot().screen.filter(!_.starts(t"> ")).join(t"\n").trim

  def progress(text: Text, decorate: Char => Text = char => t"^")
       (using tool: Sandbox.Tool, tmux: Tmux)
       (using Monitor, WorkingDirectory)
  : Text raises TmuxError =

      enter(tool.command)
      enter(' ')
      enter(text)
      attend(enter('\t'))
      screenshot().currentLine(decorate).sub(t"> ${tool.command} ", t"")

case class TmuxError()(using Diagnostics) extends Error(m"can't execute tmux")
case class Tmux(id: Text, workingDirectory: WorkingDirectory, width: Int, height: Int, shell: Shell)

case class Screenshot(screen: IArray[Text], size: (Int, Int), cursor: (Ordinal, Ordinal)):
  def apply(): Text = screen.join("\n")

  def currentLine(decorate: Char => Text): Text =
    val line0 = screen.at(cursor(1)).or(t"")
    val line = line0+t" "*(size(0) - line0.length)

    t"${line.before(cursor(0))}${line.at(cursor(0)).let(decorate).or(t"?")}${line.from(cursor(0))}"
    . trim
