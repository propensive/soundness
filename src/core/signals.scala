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

sealed trait TerminalEvent

enum TerminalInfo extends TerminalEvent:
  case WindowSize(rows: Int, columns: Int)
  case BgColor(red: Int, green: Int, blue: Int)
  case LoseFocus
  case GainFocus
  case Paste(text: Text)

enum Signal extends TerminalEvent:
  case Hup, Int, Quit, Ill, Trap, Abrt, Bus, Fpe, Kill, Usr1, Segv, Usr2, Pipe, Alrm, Term, Chld, Cont, Stop,
      Tstp, Ttin, Ttou, Urg, Xcpu, Xfsz, Vtalrm, Prof, Winch, Io, Pwr, Sys
  
  def shortName: Text = this.toString.show.upper
  def name: Text = t"SIG${this.toString.show.upper}"
  def id: Int = if ordinal < 15 then ordinal - 1 else ordinal

object Keypress:
  type EditKey = Tab.type | Home.type | End.type | PageUp.type | PageDown.type | Insert.type | Delete.type |
      Enter.type | Backspace.type | Escape.type | Left.type | Right.type | Up.type | Down.type

enum Keypress extends TerminalEvent:
  case Tab, Home, End, PageUp, PageDown, Insert, Delete, Enter, Backspace, Escape, Left, Right, Up, Down
  case CharKey(char: Char)
  case FunctionKey(number: Int)
  case Control(char: Char)
  case EscapeSeq(id: Char, content: Char*)
  case Shift(keypress: Keypress.EditKey | FunctionKey)
  case Alt(keypress: Shift | Keypress.EditKey | FunctionKey)
  case Ctrl(keypress: Alt | Shift | Keypress.EditKey | FunctionKey)
  case Meta(keypress: Ctrl | Alt | Shift | Keypress.EditKey | FunctionKey)


