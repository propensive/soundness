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
import contingency.*
import fulminate.*
import parasite.*
import proscenium.*
import turbulence.*

private given realm: Realm = realm"pf"

given stdio: (terminal: Terminal) => Stdio = terminal.stdio


def interactive[result](block: (terminal: Terminal) ?=> result)
  ( using console: Console, monitor: Monitor, codicil: Codicil )
  ( using BracketedPasteMode,
          LuminosityDetection,
          TerminalFocusDetection,
          TerminalSizeDetection )
:   result raises TerminalError =

  given terminal: Terminal = Terminal(console.signals)

  if summon[LuminosityDetection]() then Out.print(Terminal.reportBackground)
  if summon[TerminalFocusDetection]() then Out.print(Terminal.enableFocus)
  if summon[BracketedPasteMode]() then Out.print(Terminal.enablePaste)
  if summon[TerminalSizeDetection]() then Out.print(Terminal.reportSize)

  try
    if console.stdio.platform then
      val processBuilder =
        ProcessBuilder("stty", "intr", "undef", "-echo", "icanon", "raw", "opost")

      processBuilder.inheritIO()
      if processBuilder.start().nn.waitFor() != 0 then abort(TerminalError())
    block(using terminal)

  finally
    terminal.signals.stop()
    terminal.stdio.in.close()
    terminal.events.stop()
    safely(terminal.pumpSignals.attend())
    safely(terminal.pumpInput.await())
    if summon[BracketedPasteMode]() then Out.print(Terminal.disablePaste)
    if summon[TerminalFocusDetection]() then Out.print(Terminal.disableFocus)


package keyboards:
  given raw: Keyboard:
    type Keypress = Char

    def process(stream: Stream[Char]): Stream[Keypress] = stream

  given numeric: Keyboard:
    type Keypress = Int

    def process(stream: Stream[Char]): Stream[Int] = stream.map(_.toInt)

  given standard: (monitor: Monitor, codicil: Codicil) => Keyboard.Standard = Keyboard.Standard()

package terminalOptions:
  given bracketedPasteMode: BracketedPasteMode = () => true
  given luminanceDetection: LuminosityDetection = () => true
  given terminalFocusDetection: TerminalFocusDetection = () => true
  given terminalSizeDetection: TerminalSizeDetection = () => true
