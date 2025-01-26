/*
    Profanity, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*, durationApi.javaLong
import contingency.*
import gossamer.*
import parasite.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

import language.experimental.captureChecking

object Terminal:
  def reportBackground: Text = t"\e]11;?\e\\"
  def reportSize: Text = t"\e7\e[4095C\e[4095B\e[6n\e8"
  def enableFocus: Text = t"\e[?1004h"
  def disableFocus: Text = t"\e[?1004l"
  def enablePaste: Text = t"\e[?2004h"
  def disablePaste: Text = t"\e[?2004l"

case class Terminal(signals: Spool[Signal])
   (using context: ProcessContext, monitor: Monitor, codicil: Codicil)
extends Interactivity[TerminalEvent]:

  export context.stdio.{in, out, err}

  val keyboard: StandardKeyboard = StandardKeyboard()
  val rows0: Promise[Int] = Promise()
  val columns0: Promise[Int] = Promise()
  var mode: Optional[TerminalMode] = Unset
  var rows: Optional[Int] = Unset
  var columns: Optional[Int] = Unset

  def knownColumns: Int = columns.or(safely(columns0.await(50L))).or(80)
  def knownRows: Int = rows.or(safely(rows0.await(50L))).or(80)

  val cap: Termcap = new Termcap:
    def ansi: Boolean = true
    def color: ColorDepth = ColorDepth.TrueColor
    override def width: Int = knownColumns

  given stdio: Stdio = new Stdio:
    val termcap = cap
    val out = context.stdio.out
    val err = context.stdio.err
    val in = context.stdio.in

  val events: Spool[TerminalEvent] = Spool()
  def eventStream(): Stream[TerminalEvent] = events.stream

  val pumpSignals: Daemon = daemon:
    signals.stream.each:
      case Signal.Winch =>
        out.print(Terminal.reportSize)
        events.put(Signal.Winch)

      case signal =>
        events.put(signal)

  private def dark(red: Int, green: Int, blue: Int): Boolean =
    (0.299*red + 0.587*green + 0.114*blue) < 32768

  val pumpInput: Task[Unit] = task(t"stdin"):
    keyboard.process(In.stream[Char]).each:
      case resize@TerminalInfo.WindowSize(rows2, columns2) =>
        rows = rows2
        rows0.offer(rows2)
        columns = columns2
        columns0.offer(columns2)
        events.put(resize)

      case bgColor@TerminalInfo.BgColor(red, green, blue) =>
        mode = if dark(red, green, blue) then TerminalMode.Dark else TerminalMode.Light
        events.put(bgColor)

      case other =>
        events.put(other)
