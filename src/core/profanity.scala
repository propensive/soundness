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

import rudiments.*
import fulminate.*
import gossamer.*
import spectacular.*
import perforate.*
import parasite.*
import turbulence.*
import anticipation.*, timeApi.long

trait Keyboard:
  type Keypress
  def process(stream: LazyList[Char]): LazyList[Keypress]

class StandardKeyboard()(using Monitor) extends Keyboard:
  type Keypress = profanity.Keypress

  def process(stream: LazyList[Char]): LazyList[Keypress] = stream match
    case '\u001b' #:: rest                    =>
      safely(Async(rest.head).await(30L)) match
        case Unset => Keypress.Escape #:: process(rest)
        case _ => rest match
          case 'O' #:: fn #:: rest                  => Keypress.Function(fn.toInt - 79) #:: process(rest)
          case '[' #:: rest                         => rest match
            case '3' #:: '~' #:: rest                 => Keypress.Delete #:: process(rest)
            case '2' #:: '~' #:: rest                 => Keypress.Insert #:: process(rest)
            case 'F' #:: rest                         => Keypress.End #:: process(rest)
            case 'H' #:: rest                         => Keypress.Home #:: process(rest)
            case '5' #:: '~' #:: rest                 => Keypress.PageUp #:: process(rest)
            case '6' #:: '~' #:: rest                 => Keypress.PageDown #:: process(rest)
            case 'A' #:: rest                         => Keypress.UpArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'A' #:: rest => Keypress.CtrlUpArrow #:: process(rest)
            case 'B' #:: rest                         => Keypress.DownArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'B' #:: rest => Keypress.CtrlDownArrow #:: process(rest)
            case 'C' #:: rest                         => Keypress.RightArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'C' #:: rest => Keypress.CtrlRightArrow #:: process(rest)
            case 'D' #:: rest                         => Keypress.LeftArrow #:: process(rest)
            case '1' #:: ';' #:: '5' #:: 'D' #:: rest => Keypress.CtrlLeftArrow #:: process(rest)

            case other =>
              val sequence = other.takeWhile(!_.isLetter)
              val rest = other.drop(sequence.length)
              def continue = process(rest.tail)

              rest.head match
                case 'R'  => sequence.map(_.show).join.cut(';') match
                  case List(As[Int](rows), As[Int](cols)) => Keypress.Resize(rows, cols) #:: continue
                  case _                                  => Keypress.Resize(20, 30) #:: continue
                case char => Keypress.EscapeSeq(char, sequence*) #:: continue

          case ']' #:: '1' #:: '1' #:: ';' #:: 'r' #:: 'g' #:: 'b' #:: ':' #:: rest =>
            val content = rest.takeWhile(_ != '\u001b').mkString.tt
            val continuation = rest.drop(content.length + 2)

            content.cut(t"/") match
              case List(Hex(red), Hex(green), Hex(blue)) =>
                Keypress.BgColor(red, green, blue) #:: process(continuation)
              case _ =>
                process(continuation)

          case rest =>
            process(rest)

    case ('\b' | '\u007f') #:: rest           => Keypress.Backspace #:: process(rest)
    case '\u0009' #:: rest                    => Keypress.Tab #:: process(rest)
    case ('\u000a' | '\u000d') #:: rest       => Keypress.Enter #:: process(rest)
    case char #:: rest if char < 32           => Keypress.Ctrl(char) #:: process(rest)
    case other #:: rest                       => Keypress.Printable(other) #:: process(rest)
    case _                                    => LazyList()

case class TerminalError(ttyMsg: Text) extends Error(msg"STDIN is not attached to a TTY: $ttyMsg")

object Terminal:
  def reportBackground: Text = t"\e]11;?\e\\"
  def reportSize: Text = t"\e[s\e[4095C\e[4095B\e[6n\e[u"

object Keyboard:
  given raw: Keyboard with
    type Keypress = Char
     def process(stream: LazyList[Char]): LazyList[Keypress] = stream

  given numeric: Keyboard with
    type Keypress = Int
     def process(stream: LazyList[Char]): LazyList[Int] = stream.map(_.toInt)

  given standard(using Monitor): Keyboard { type Keypress = profanity.Keypress } = StandardKeyboard()

enum TerminalMode:
  case Dark, Light

case class Terminal(input: LazyList[Char], signals: LazyList[Signal])(using stdio: Stdio, monitor: Monitor)
extends Stdio:

  export stdio.{putErrBytes, putErrText, putOutBytes, putOutText}

  val keyboard = StandardKeyboard()

  var mode: Maybe[TerminalMode] = Unset
  var rows: Int = 0
  var columns: Int = 0

  val signalHandler = Async:
    signals.foreach:
      case Signal.Winch => Io.print(Terminal.reportSize)
      case _            => ()

  def events: LazyList[TtyEvent] = keyboard.process(input).multiplexWith(signals).map:
    case resize@Keypress.Resize(rows2, columns2) =>
      rows = rows2
      columns = columns2
      resize

    case other =>
      other

def terminal
    [ResultType]
    (input: LazyList[Char], signals: LazyList[Signal] = LazyList())
    (block: Terminal ?=> ResultType)
    (using Stdio, Monitor)
    : ResultType =

  val term = Terminal(input, signals)
  Io.print(Terminal.reportBackground)
  Io.print(Terminal.reportSize)
  block(using term)

inline def tty(using inline tty: Terminal): Terminal = tty