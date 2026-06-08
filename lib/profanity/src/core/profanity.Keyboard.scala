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
import distillate.*
import gossamer.*
import hypotenuse.*
import parasite.*
import quantitative.*
import spectacular.*
import symbolism.*
import vacuous.*

object Keyboard:
  import Keypress.*


  // A legacy modifier parameter is `1 + bitmask`, encoded as a single digit.
  def modified(code: Char, keypress: EditKey | FunctionKey)
  :   EditKey | FunctionKey | Shift | Alt | Ctrl | Meta =

    applyModifiers(code - '1', keypress)

  // Wraps `keypress` in the modifier keys named by `bitmask` (shift=1, alt=2,
  // ctrl=4, super/meta=8), the kitty-protocol modifier value minus one.
  def applyModifiers(n: Int, keypress: EditKey | FunctionKey)
  :   EditKey | FunctionKey | Shift | Alt | Ctrl | Meta =

    val shift: EditKey | FunctionKey | Shift = if (n&1) == 1 then Shift(keypress) else keypress
    val alt: EditKey | FunctionKey | Shift | Alt = if (n&2) == 2 then Alt(shift) else shift
    val ctrl: EditKey | FunctionKey | Shift | Alt | Ctrl = if (n&4) == 4 then Ctrl(alt) else alt

    if (n&8) == 8 then Meta(ctrl) else ctrl

  // Decodes a kitty keyboard-protocol CSI-u parameter list — `<codepoint>[:<shifted>
  // [:<base>]][;<modifiers>[:<event>]][;<text>]`, without the trailing `u` — into a
  // keypress. The codepoint is a Unicode value, with the C0 codes 13/9/27/127 for
  // Enter/Tab/Escape/Backspace; the modifier field is `1 + bitmask`.
  def csiu(params: List[Char]): profanity.Keypress =
    val fields: List[Text] = params.map(_.show).join.cut(t";").to(List)

    def number(index: Int, default: Int): Int =
      safely(Integer.parseInt(fields(index).cut(t":").head.s)).or(default)

    val bitmask: Int = (number(1, 1) - 1).max(0)

    def key(value: EditKey | FunctionKey): profanity.Keypress =
      if bitmask == 0 then value else applyModifiers(bitmask, value)

    number(0, 0) match
      case 13      => key(Enter)
      case 9       => key(Tab)
      case 27      => key(Escape)
      case 8 | 127 => key(Backspace)

      case codepoint =>
        val char = codepoint.toChar

        if (bitmask&4) == 4 then ctrlChar(char)
        else if (bitmask&1) == 1 then CharKey(char.toUpper)
        else CharKey(char)

  // The `Ctrl`-modified form of a character, or the plain character if it has no
  // control form.
  def ctrlChar(char: Char): profanity.Keypress = char.toUpper match
    case c: ( '@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
              | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '['
              | '\\' | ']' | '^' | '_' ) =>
      Ctrl(c)

    case _ =>
      CharKey(char)


  def navigation(code: Char): Keypress.EditKey = code match
    case 'A' => Keypress.Up
    case 'B' => Keypress.Down
    case 'C' => Keypress.Right
    case 'D' => Keypress.Left
    case 'F' => Keypress.End
    case 'H' => Keypress.Home
    case _   => Keypress.Escape

  def vt(code: Char): Keypress.EditKey = code match
    case '1' | '7' => Keypress.Home
    case '2'       => Keypress.Insert
    case '3'       => Keypress.Delete
    case '4' | '8' => Keypress.End
    case '5'       => Keypress.PageUp
    case '6'       => Keypress.PageDown
    case _         => Keypress.Escape

  class Standard()(using Monitor, Codicil) extends Keyboard:
    type Keypress = profanity.Keypress | TerminalInfo

    def process(stream: Stream[Char]): Stream[Keypress] = stream match
      case '\u001b' #:: rest =>
        safely(async(rest.head).await(30.0*Milli(Second))) match
          case Unset => Keypress.Escape #:: process(rest)

          case _ => rest match
            case 'O' #:: key #:: rest => Keypress.FunctionKey(key.toInt - 79) #:: process(rest)

            case '[' #:: rest => rest match
              case (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H')) #:: rest =>
                Keyboard.navigation(code) #:: process(rest)

              case code #:: '~' #:: rest if '1' <= code <= '9' =>
                Keyboard.vt(code) #:: process(rest)

              case code #:: ';' #:: modifiers #:: '~' #:: rest if '1' <= code <= '9' =>
                Keyboard.modified(modifiers, Keyboard.vt(code)) #:: process(rest)

              case
                ( '1' #:: ';' #:: modifiers #:: (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H'))
                  #:: rest ) =>

                Keyboard.modified(modifiers, Keyboard.navigation(code)) #:: process(rest)

              case '2' #:: '0' #:: '0' #:: '~' #:: tail =>
                val size = tail.indexOfSlice(List('\u001b', '[', '2', '0', '1', '~'))
                val content = tail.take(size).map(_.show).join
                TerminalInfo.Paste(content) #:: process(tail.drop(size + 6))

              case other =>
                val sequence = other.takeWhile(!_.isLetter)

                other.drop(sequence.length) match
                  // CSI-u (kitty keyboard protocol): a key codepoint with optional
                  // sub-keys and modifiers, terminated by `u`.
                  case 'u' #:: tail =>
                    Keyboard.csiu(sequence.to(List)) #:: process(tail)

                  case 'R' #:: tail => sequence.map(_.show).join.cut(';').to(List) match
                    case List(As[Int](rows), As[Int](cols)) =>
                      TerminalInfo.WindowSize(rows, cols) #:: process(tail)

                    case _ =>
                      process(tail)

                  case 'O' #:: tail =>
                    TerminalInfo.LoseFocus #:: process(tail)

                  case 'I' #:: tail =>
                    TerminalInfo.GainFocus #:: process(tail)

                  case char #:: tail =>
                    Keypress.EscapeSeq(char, sequence*) #:: process(tail)

                  case _ =>
                    Stream()

            case ']' #:: '1' #:: '1' #:: ';' #:: 'r' #:: 'g' #:: 'b' #:: ':' #:: rest =>
              val content = rest.takeWhile(_ != '\u001b').mkString.tt
              val continuation = rest.drop(content.length + 2)

              content.cut(t"/").to(List) match
                case List(red, green, blue) =>
                  def decimal(hex: Text): Int = Integer.parseInt(hex.s, 16)

                  TerminalInfo.BgColor(decimal(red), decimal(green), decimal(blue))
                  #:: process(continuation)

                case _ =>
                  process(continuation)

            case rest =>
              process(rest)

      case ('\b' | '\u007f') #:: rest =>
        Keypress.Backspace #:: process(rest)

      case '\u0009' #:: rest =>
        Keypress.Tab #:: process(rest)

      case ('\u000a' | '\u000d') #:: rest =>
        Keypress.Enter #:: process(rest)

      case CtrlChar(char) #:: rest =>
        Keypress.Ctrl(char) #:: process(rest)

      case other #:: rest =>
        Keypress.CharKey(other) #:: process(rest)

      case _ =>
        Stream()

trait Keyboard:
  type Keypress

  def process(stream: Stream[Char]): Stream[Keypress]
