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

import scala.caps

import soundness.*

import errorDiagnostics.stackTracesDiagnostics

object Tmux:
  def enter(keypresses: (Text | Char)*)(using tmux: Tmux): Unit raises Tmux.Error =
    given WorkingDirectory = tmux.workingDirectory

    import logging.silentLogging

    mitigate:
      case guillotine.Exec.Error(_, _, _) => Tmux.Error(Tmux.Error.Reason.ExecFailed)

    . protect:
        keypresses.foreach:
          case text: Text => sh"tmux send-keys -t ${tmux.id} '$text'".exec[Unit]()
          case char: Char => sh"tmux send-keys -t ${tmux.id} '$char'".exec[Unit]()
          case _          => panic(m"unreachable case")

  // Resize the tmux window, delivering a SIGWINCH (and tmux's own reflow) to the
  // application in the pane. Sessions in this rig are created detached with explicit
  // `-x`/`-y`, i.e. manually sized, which is exactly the case `resize-window`
  // controls. Note `Tmux.width`/`height` record the CREATION size; after a resize,
  // read the live size from tmux itself if it matters.
  def resize(width: Int, height: Int)(using tmux: Tmux)(using WorkingDirectory)
  :   Unit raises Tmux.Error =

    import logging.silentLogging

    mitigate:
      case guillotine.Exec.Error(_, _, _) => Tmux.Error(Tmux.Error.Reason.ExecFailed)

    . protect:
        sh"tmux resize-window -t ${tmux.id} -x $width -y $height".exec[Unit]()

  def screenshot()(using tmux: Tmux)(using WorkingDirectory): Screenshot raises Tmux.Error =
    import logging.silentLogging

    mitigate:
      case guillotine.Exec.Error(_, _, _)   => Tmux.Error(Tmux.Error.Reason.SessionDied)
      case Number.Error(_, _, _) => Tmux.Error(Tmux.Error.Reason.SessionDied)

    . protect:
        // `.stdlib`: `Array.from` takes a stdlib `IterableOnce`.
        val content = Array.from(sh"tmux capture-pane -pt ${tmux.id}".exec[List[Text]]().stdlib)
        val cx = sh"tmux display-message -pt ${tmux.id} '#{cursor_x}'".exec[Text]()
        val cy = sh"tmux display-message -pt ${tmux.id} '#{cursor_y}'".exec[Text]()
        val x = cx.trim.as[Int].z
        val y = cy.trim.as[Int].z

        Screenshot(content, (tmux.width, tmux.height), (x, y))

  // Explicit `using` evidence instead of `raises` sugar: a context-function result would
  // hide the parameters, which the separation checker rejects.
  def attend(using tmux: Tmux)[result](block: => result)
    ( using Monitor, WorkingDirectory, Tactic[Tmux.Error] )
  :   result =

    val init = screenshot().screen

    var count = 0

    block.also:
      while init === screenshot().screen && count < 60 do delay(0.01*Second) yet (count += 1)


  def completions(text: Text)(using tool: Enclave.Tool, tmux: Tmux)
    ( using Monitor, WorkingDirectory, Tactic[Tmux.Error] )
  :   Text =

    tmux.shell match
      case Shell.Powershell =>
        enter(t"""_completions "$text"""")
        scala.caps.unsafe.unsafeAssumeSeparate(attend(enter('\r')))
        var count = 0

        while Tmux.screenshot().screen.filter(_ == t">").readable.length == 0 && count < 333 do
          delay(0.03*Second)
          count += 1
        // A named method, not a lambda: interpolating inside a lambda passed to a collection
        // combinator runs the interpolator's implicit search while the combinator's element type
        // is still uninstantiated, tripping dotc's `wildApprox` assertion (scala/scala3#24824).
        def described(line: Text): List[Text] = line.cut(t"@@") match
          case name :: desc :: Nil => List(t"$name  ($desc)")
          case name :: Nil         => List(name)
          case _                   => Nil

        val lines: List[Text] = screenshot().screen.to[List]
        val shown: List[Text] = lines.filter(!_.starts(t">"))
        val trimmed: List[Text] = shown.map(_.trim).filter(_.length > 0)

        trimmed.bind(described).join(t"  ")

      case _ =>
        enter(tool.command)
        enter(' ')
        enter(text)
        scala.caps.unsafe.unsafeAssumeSeparate(attend(enter(Ht)))
        screenshot().screen.filter(!_.starts(t"> ")).readable.toSeq.join(t"\n").trim


  def progress(text: Text, decorate: Char => Text = char => t"^")
    ( using tool: Enclave.Tool, tmux: Tmux )
    ( using Monitor, WorkingDirectory, Tactic[Tmux.Error] )
  :   Text =

    enter(tool.command)
    enter(' ')
    enter(text)

    tmux.shell match
      case Shell.Powershell =>
        delay(0.05*Second)
        val init = screenshot().screen
        enter(Ht)
        var count = 0

        while init === screenshot().screen && count < 150 do delay(0.01*Second) yet (count += 1)

        if init !== screenshot().screen then
          var prev = screenshot().screen
          var stable = 0

          while stable < 3 && count < 200 do
            delay(0.01*Second)
            val current = screenshot().screen

            if current === prev then stable += 1 else
              stable = 0
              prev = current

            count += 1

      case _ =>
        scala.caps.unsafe.unsafeAssumeSeparate(attend(enter(Ht)))

    screenshot().currentLine(decorate).sub(t"> ${tool.command} ", t"")

  // TmuxError → Tmux.Error
  object Error:
    enum Reason(val number: Int):
      case ShellNotInstalled(shell: Text) extends Reason(1)
      case SessionDied                    extends Reason(2)
      case ExecFailed                     extends Reason(3)

    given communicable: Reason is Communicable =
      case Reason.ShellNotInstalled(shell) => m"the shell binary `$shell` is not installed"
      case Reason.SessionDied              => m"the tmux session terminated unexpectedly"
      case Reason.ExecFailed               => m"could not execute tmux"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(271, reason.number)(m"can't drive tmux: $reason")


// A `Tmux` is a *capability*: it identifies a live external tmux session whose lifetime is
// the `tmux` block that creates it (killed after the block). `Exclusive` because a session
// is driven by one test at a time.
case class Tmux(id: Text, workingDirectory: WorkingDirectory, width: Int, height: Int, shell: Shell)
extends Findable, caps.ExclusiveCapability

case class Screenshot(screen: Array[Text]^{}, size: (Int, Int), cursor: (Ordinal, Ordinal)):
  def apply(): Text = screen.readable.toSeq.join(t"\n")

  def currentLine(decorate: Char => Text): Text =
    val line0 = screen.at(cursor(1)).or(t"")
    val line = line0+t" "*(size(0) - line0.length)

    t"${line.before(cursor(0))}${line(cursor(0)).let(decorate).or(t"?")}${line.from(cursor(0))}"
    . trim
