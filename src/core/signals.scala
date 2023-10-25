/*
    Profanity, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import gossamer.*
import spectacular.*
import rudiments.*

object Signal:
  given decoder: Decoder[Signal] = text => Signal.valueOf(text.lower.capitalize.s)
  given encoder: Encoder[Signal] = _.shortName

sealed trait TtyEvent

enum TerminalInfo extends TtyEvent:
  case WindowSize(rows: Int, columns: Int)
  case BgColor(red: Int, green: Int, blue: Int)
  case LoseFocus
  case GainFocus
  case Paste(text: Text)

enum Signal extends TtyEvent:
  case Hup, Int, Quit, Ill, Trap, Abrt, Bus, Fpe, Kill, Usr1, Segv, Usr2, Pipe, Alrm, Term, Chld, Cont, Stop,
      Tstp, Ttin, Ttou, Urg, Xcpu, Xfsz, Vtalrm, Prof, Winch, Io, Pwr, Sys
  
  def shortName: Text = this.toString.show.upper
  def name: Text = t"SIG${this.toString.show.upper}"
  def id: Int = if ordinal < 15 then ordinal - 1 else ordinal

  // def sendTo(process: OsProcess)(using WorkingDirectory, Raises[ExecError]): Unit =
  //   sh"kill -${shortName} ${process.pid.value}"()

enum Keypress extends TtyEvent:
  case Printable(char: Char)
  case Function(number: Int)
  case Ctrl(char: Char)
  case EscapeSeq(id: Char, content: Char*)
  case Enter, Escape, Tab, Backspace, Delete, PageUp, PageDown, LeftArrow, RightArrow, UpArrow,
      DownArrow, CtrlLeftArrow, CtrlRightArrow, CtrlUpArrow, CtrlDownArrow, End, Home, Insert
