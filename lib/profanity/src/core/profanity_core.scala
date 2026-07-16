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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import ambience.*
import anticipation.*
import contingency.*
import gigantism.*
import gossamer.*
import parasite.*
import turbulence.*

given stdio: (terminal: Terminal) => Stdio = terminal.stdio


def interactive[result](block: (terminal: Terminal) ?=> result)
  ( using console:     Console,
          monitor:     Monitor,
          probate:     Probate,
          environment: Environment )
  ( using features: Every[TerminalFeature] )
:   result raises TerminalError =

  given terminal: Terminal = Terminal()

  // The core session: raw mode, the caller's block, then input/event teardown.
  def session: result =
    try
      if console.stdio.platform then
        val processBuilder =
          ProcessBuilder("stty", "intr", "undef", "-echo", "icanon", "raw", "opost")

        processBuilder.inheritIO()
        if processBuilder.start().nn.waitFor() != 0 then abort(TerminalError())

      block(using terminal)

    finally
      terminal.stdio.in.close()
      terminal.events.stop()
      terminal.pumpInput.attend()

  // Apply every in-scope `TerminalFeature` around the session, nested so that each
  // turns off (in its own try/finally) in the reverse of the order it turned on.
  def applyFeatures(remaining: List[TerminalFeature]): result = remaining match
    case Nil             => session
    case feature :: rest => feature(applyFeatures(rest))

  applyFeatures(features.values)


package keyboards:
  given rawKeyboard: Keyboard:
    type Keypress = Char

    def process(stream: LazyList[Char]): LazyList[Keypress] = stream

  given numericKeyboard: Keyboard:
    type Keypress = Int

    def process(stream: LazyList[Char]): LazyList[Int] = stream.map(_.toInt)

  // Honestly tracked: the keyboard's escape-disambiguation timeout runs `async`, so the
  // instance retains the monitor.
  given standardKeyboard: (monitor: Monitor, probate: Probate) => (Keyboard.Standard^{monitor}) =
    Keyboard.Standard()

// The standard terminal features, each a turn-on/turn-off escape-sequence pair.
// Import the ones a session should enable (or `terminalFeatures.*` for all);
// `interactive` applies every imported feature. The two queries (`backgroundColor`,
// `terminalSize`) have no turn-off, so their `disable` sequence is empty.
package terminalFeatures:
  given bracketedPasteFeature: TerminalFeature = TerminalFeature(t"\e[?2004h", t"\e[?2004l")
  given focusReportingFeature: TerminalFeature = TerminalFeature(t"\e[?1004h", t"\e[?1004l")

  given mouseTrackingFeature: TerminalFeature =
    TerminalFeature(t"\e[?1000h\e[?1006h", t"\e[?1006l\e[?1000l")

  given alternateScreenFeature: TerminalFeature = TerminalFeature(t"\e[?1049h", t"\e[?1049l")
  given kittyKeyboardFeature: TerminalFeature = TerminalFeature(t"\e[>1u", t"\e[<u")
  given backgroundColorFeature: TerminalFeature = TerminalFeature(t"\e]11;?\e\\", t"")
  given terminalSizeFeature: TerminalFeature = TerminalFeature(Terminal.reportSize, t"")


type UnixSignal = Signal

val UnixSignal = Signal
