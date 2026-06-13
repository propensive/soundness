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
import distillate.*
import gossamer.*
import prepositional.*
import spectacular.*


sealed trait TerminalEvent


enum TerminalInfo extends TerminalEvent:
  case WindowSize(rows: Int, columns: Int)
  case BgColor(red: Int, green: Int, blue: Int)
  case LoseFocus
  case GainFocus
  case Paste(text: Text)

  // A synthetic event an application can put onto the terminal's event spool to
  // wake the event loop and request a repaint — e.g. after a background task has
  // changed the layout.
  case Redraw


object Signal:
  given decoder: Signal is Decodable in Text = text => Signal.valueOf(text.lower.capitalize.s)
  given encodable: Signal is Encodable in Text = _.shortName
  given showable: Signal is Showable = _.shortName


enum Signal extends TerminalEvent:
  case Hup, Int, Quit, Ill, Trap, Abrt, Bus, Fpe, Kill, Usr1, Segv, Usr2, Pipe, Alrm, Term, Chld,
    Cont, Stop, Tstp, Ttin, Ttou, Urg, Xcpu, Xfsz, Vtalrm, Prof, Winch, Io, Pwr, Sys

  def shortName: Text = this.toString.show.upper
  def name: Text = t"SIG${this.toString.show.upper}"
  def id: Int = if ordinal < 15 then ordinal + 1 else ordinal + 2


object WindowsSignal:
  given decoder: WindowsSignal is Decodable in Text =
    text => WindowsSignal.valueOf(text.lower.capitalize.s)

  given encodable: WindowsSignal is Encodable in Text = _.shortName
  given showable: WindowsSignal is Showable = _.shortName


enum WindowsSignal extends TerminalEvent:
  case CtrlC, CtrlBreak, Close, Logoff, Shutdown

  def shortName: Text = this match
    case CtrlC     => t"CTRL_C"
    case CtrlBreak => t"CTRL_BREAK"
    case Close     => t"CLOSE"
    case Logoff    => t"LOGOFF"
    case Shutdown  => t"SHUTDOWN"


object CtrlChar:
  def unapply(code: Char)
  :   ( Option
          [ 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O'
            | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\' | ']'
            | '^'
            | '_' | '@' ] ) =

      (code + 64).toChar match
        case char: ('@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L'
                    | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y'
                    | 'Z' | '[' | '\\' | ']' | '^' | '_' | '@') =>
          Some(char)

        case _ =>
          None


object Keypress:
  type EditKey = Tab.type | Home.type | End.type | PageUp.type | PageDown.type | Insert.type |
    Delete.type | Enter.type | Backspace.type | Escape.type | Left.type | Right.type | Up.type |
    Down.type

  // Renders a keypress with a Unicode symbol in square brackets for each special
  // key (and modifier), joining a modifier to the key it modifies with `+`; an
  // ordinary character is shown as itself. E.g. `[⇧]+[↵]`, `[⌃]+C`, `[⌥]+[→]`.
  private def render(keypress: Keypress): Text =
    def key(symbol: Text): Text = t"[$symbol]"

    keypress match
      case Shift(inner) => t"${key(t"⇧")}+${render(inner)}"
      case Alt(inner)   => t"${key(t"⌥")}+${render(inner)}"
      case Meta(inner)  => t"${key(t"⌘")}+${render(inner)}"

      case Ctrl(inner) => inner match
        case char: Char      => t"${key(t"⌃")}+${key(char.show)}"
        case other: Keypress => t"${key(t"⌃")}+${render(other)}"

      case CharKey(' ')      => key(t"␣")
      case CharKey(char)     => key(char.show)
      case FunctionKey(n)    => key(t"F${n.show}")
      case EscapeSeq(id, _*) => key(t"⎋${id.show}")

      case Tab       => key(t"⇥")
      case Enter     => key(t"↵")
      case Backspace => key(t"⌫")
      case Delete    => key(t"⌦")
      case Escape    => key(t"⎋")
      case Up        => key(t"↑")
      case Down      => key(t"↓")
      case Left      => key(t"←")
      case Right     => key(t"→")
      case Home      => key(t"↖")
      case End       => key(t"↘")
      case PageUp    => key(t"⇞")
      case PageDown  => key(t"⇟")
      case Insert    => key(t"⎀")

  given showable: Keypress is Showable = render(_)


enum Keypress extends TerminalEvent:
  case Tab, Home, End, PageUp, PageDown, Insert, Delete, Enter, Backspace, Escape, Left, Right, Up,
    Down

  case CharKey(char: Char)
  case FunctionKey(number: Int)
  case EscapeSeq(id: Char, content: Char*)
  case Shift(keypress: Keypress.EditKey | FunctionKey)
  case Alt(keypress: Shift | Keypress.EditKey | FunctionKey)

  case
    Ctrl
      ( keypress: Alt | Shift | Keypress.EditKey | FunctionKey | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' |
          'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' |
          'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\' | ']' | '^' | '_' | '@' )

  case Meta(keypress: Ctrl | Alt | Shift | Keypress.EditKey | FunctionKey)
