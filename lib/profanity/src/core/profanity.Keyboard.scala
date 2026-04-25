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
import proscenium.*
import spectacular.*
import vacuous.*

import abstractables.durationIsAbstractable

object Keyboard:
  import Keypress.*


  def modified(code: Char, keypress: EditKey | FunctionKey)
  :   EditKey | FunctionKey | Shift | Alt | Ctrl | Meta =

    val n = code - '1'
    val shift: EditKey | FunctionKey | Shift = if (n&1) == 1 then Shift(keypress) else keypress
    val alt: EditKey | FunctionKey | Shift | Alt = if (n&2) == 2 then Alt(shift) else shift
    val ctrl: EditKey | FunctionKey | Shift | Alt | Ctrl = if (n&4) == 4 then Ctrl(alt) else alt

    if (n&8) == 8 then Meta(ctrl) else ctrl


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
        safely(async(rest.head).await(30L)) match
          case Unset => Keypress.Escape #:: process(rest)
          case _ => rest match
            case 'O' #:: key #:: rest => Keypress.FunctionKey(key.toInt - 79) #:: process(rest)
            case '[' #:: rest        => rest match
              case (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H')) #:: rest =>
                Keyboard.navigation(code) #:: process(rest)

              case code #:: '~' #:: rest if '1' <= code <= '9' =>
                Keyboard.vt(code) #:: process(rest)

              case code #:: ';' #:: modifiers #:: '~' #:: rest if '1' <= code <= '9' =>
                Keyboard.modified(modifiers, Keyboard.vt(code)) #:: process(rest)

              case '1' #:: ';' #:: modifiers #:: (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H'))
                  #:: rest =>
                Keyboard.modified(modifiers, Keyboard.navigation(code)) #:: process(rest)

              case '2' #:: '0' #:: '0' #:: '~' #:: tail =>
                val size = tail.indexOfSlice(List('\u001b', '[', '2', '0', '1', '~'))
                val content = tail.take(size).map(_.show).join
                TerminalInfo.Paste(content) #:: process(tail.drop(size + 6))

              case other =>
                val sequence = other.takeWhile(!_.isLetter)

                other.drop(sequence.length) match
                  case 'R' #:: tail => sequence.map(_.show).join.cut(';').to(List) match
                    case List(As[Int](rows), As[Int](cols)) =>
                      TerminalInfo.WindowSize(rows, cols) #:: process(tail)

                    case _ =>
                      TerminalInfo.WindowSize(20, 30) #:: process(tail)

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
