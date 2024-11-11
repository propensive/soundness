/*
    Profanity, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package profanity

import rudiments.*
import fulminate.*
import contingency.*
import parasite.*
import turbulence.*
import anticipation.*

import language.experimental.captureChecking

given realm: Realm = realm"profanity"

given (using terminal: Terminal) => Stdio as stdio = terminal.stdio

def terminal[ResultType](block: (terminal: Terminal) ?=> ResultType)
   (using context: ProcessContext, monitor: Monitor, codicil: Codicil)
   (using BracketedPasteMode, BackgroundColorDetection, TerminalFocusDetection, TerminalSizeDetection)
        : ResultType raises TerminalError =

  given terminal: Terminal = Terminal(context.signals)

  if summon[BackgroundColorDetection]() then Out.print(Terminal.reportBackground)
  if summon[TerminalFocusDetection]() then Out.print(Terminal.enableFocus)
  if summon[BracketedPasteMode]() then Out.print(Terminal.enablePaste)
  if summon[TerminalSizeDetection]() then Out.print(Terminal.reportSize)

  try
    if context.stdio.platform then
      val processBuilder = ProcessBuilder("stty", "intr", "undef", "-echo", "icanon", "raw", "opost")
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
  given Keyboard as raw:
    type Keypress = Char
    def process(stream: LazyList[Char]): LazyList[Keypress] = stream

  given Keyboard as numeric:
    type Keypress = Int
    def process(stream: LazyList[Char]): LazyList[Int] = stream.map(_.toInt)

  given (using monitor: Monitor, codicil: Codicil) => StandardKeyboard as standard =
    StandardKeyboard()

package terminalOptions:
  given BracketedPasteMode as bracketedPasteMode = () => true
  given BackgroundColorDetection as backgroundColorDetection = () => true
  given TerminalFocusDetection as terminalFocusDetection = () => true
  given TerminalSizeDetection as terminalSizeDetection = () => true
