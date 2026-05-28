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
package profanity

import anticipation.*
import gossamer.*
import iridescence.*
import parasite.*
import rudiments.*
import symbolism.*
import turbulence.*
import vacuous.*

object Terminal:
  def reportBackground: Text = t"\e]11;?\e\\"
  def reportSize: Text = t"\e7\e[4095C\e[4095B\e[6n\e8"
  def enableFocus: Text = t"\e[?1004h"
  def disableFocus: Text = t"\e[?1004l"
  def enablePaste: Text = t"\e[?2004h"
  def disablePaste: Text = t"\e[?2004l"

case class Terminal()(using console: Console, monitor: Monitor, codicil: Codicil)
extends Interactivity[TerminalEvent]:

  export console.stdio.{in, out, err}

  val keyboard: Keyboard.Standard = Keyboard.Standard()

  var mode: Optional[Brightness] = Unset
  var rows: Optional[Int] = Unset
  var columns: Optional[Int] = Unset

  def knownColumns: Int = columns.or(80)
  def knownRows: Int = rows.or(80)

  val cap: Termcap = new Termcap:
    def ansi: Boolean = true
    def color: ColorDepth = ColorDepth.TrueColor
    override def width: Int = knownColumns

  given stdio: Stdio = new Stdio:
    val termcap = cap
    val out = console.stdio.out
    val err = console.stdio.err
    val in = console.stdio.in

  val events: Spool[TerminalEvent] = Spool()

  def eventIterator(): Iterator[TerminalEvent] = events.iterator

  console.trap:
    case Signal.Winch =>
      out.print(Terminal.reportSize)
      events.put(Signal.Winch)
      SignalResponse.Accept

    case signal =>
      events.put(signal)
      SignalResponse.Accept

  private def dark(red: Int, green: Int, blue: Int): Boolean =
    (0.299*red + 0.587*green + 0.114*blue) < 32768

  val pumpInput: Task[Unit] = task(t"stdin"):
    keyboard.process(In.stream[Char]).each:
      case resize@TerminalInfo.WindowSize(rows2, columns2) =>
        rows = rows2
        columns = columns2
        events.put(resize)

      case bgColor@TerminalInfo.BgColor(red, green, blue) =>
        mode = if dark(red, green, blue) then Brightness.Dark else Brightness.Light
        events.put(bgColor)

      case other =>
        events.put(other)
