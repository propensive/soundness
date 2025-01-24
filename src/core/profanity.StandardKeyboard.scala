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
import hypotenuse.*
import parasite.*
import rudiments.*
import spectacular.*
import vacuous.*

class StandardKeyboard()(using Monitor, Codicil) extends Keyboard:
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
              case List(Hex(red), Hex(green), Hex(blue)) =>
                TerminalInfo.BgColor(red, green, blue) #:: process(continuation)

              case _ =>
                process(continuation)

          case rest =>
            process(rest)

    case ('\b' | '\u007f') #:: rest     => Keypress.Backspace #:: process(rest)
    case '\u0009' #:: rest              => Keypress.Tab #:: process(rest)
    case ('\u000a' | '\u000d') #:: rest => Keypress.Enter #:: process(rest)
    case CtrlChar(char) #:: rest        => Keypress.Ctrl(char) #:: process(rest)
    case other #:: rest                 => Keypress.CharKey(other) #:: process(rest)
    case _                              => Stream()
