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

object Keyboard:
  def modified(code: Char, keypress: Keypress): Keypress =
    def recur(n: Int, modifiers: List[Keypress => Keypress], keypress: Keypress): Keypress = modifiers match
      case Nil => keypress
      
      case modifier :: tail =>
        recur(n*2, tail, if (n & 8) == 8 then modifier(keypress) else keypress)

    recur(code - '1', List(Keypress.Meta(_), Keypress.Ctrl(_), Keypress.Alt(_), Keypress.Shift(_)), keypress)

  def navigation(code: Char): Keypress = code match
    case 'A' => Keypress.UpArrow
    case 'B' => Keypress.DownArrow
    case 'C' => Keypress.RightArrow
    case 'D' => Keypress.LeftArrow
    case 'F' => Keypress.End
    case 'H' => Keypress.Home

  def vt(code: Char): Keypress = code match
    case '1' | '7' => Keypress.Home
    case '2'       => Keypress.Insert
    case '3'       => Keypress.Delete
    case '4' | '8' => Keypress.End
    case '5'       => Keypress.PageUp
    case '6'       => Keypress.PageDown

class StandardKeyboard()(using Monitor) extends Keyboard:
  type Keypress = profanity.Keypress | TerminalInfo

  def process(stream: LazyList[Char]): LazyList[Keypress] = stream match
    case '\u001b' #:: rest                    =>
      safely(Async(rest.head).await(30L)) match
        case Unset => Keypress.Escape #:: process(rest)
        case _ => rest match
          case 'O' #:: fn #:: rest => Keypress.FunctionKey(fn.toInt - 79) #:: process(rest)
          case '[' #:: rest        => rest match
            case code #:: '~' #:: rest if '1' <= code <= '9' =>
              Keyboard.vt(code) #:: process(rest)
            case code #:: ';' #:: modifiers #:: '~' #:: rest if '1' <= code <= '9' =>
              Keyboard.modified(modifiers, Keyboard.vt(code)) #:: process(rest)
            
            case '1' #:: ';' #:: modifiers #:: (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H')) #:: rest =>
              Keyboard.modified(modifiers, Keyboard.navigation(code)) #:: process(rest)
              
            case code #:: rest if 'A' <= code <= 'H' =>
              Keyboard.navigation(code) #:: process(rest)
            
            case '2' #:: '0' #:: '0' #:: '~' #:: tail =>
              val size = tail.indexOfSlice(List('\u001b', '[', '2', '0', '1', '~'))
              val content = tail.take(size).map(_.show).join
              TerminalInfo.Paste(content) #:: process(tail.drop(size + 6))

            case other =>
              val sequence = other.takeWhile(!_.isLetter)

              other.drop(sequence.length) match
                case 'R' #:: tail => sequence.map(_.show).join.cut(';') match
                  case List(As[Int](rows), As[Int](cols)) => TerminalInfo.WindowSize(rows, cols) #:: process(tail)
                  case _                                  => TerminalInfo.WindowSize(20, 30) #:: process(tail)

                case 'O' #:: tail => TerminalInfo.LoseFocus #:: process(tail)
                case 'I' #:: tail => TerminalInfo.GainFocus #:: process(tail)

                case char #:: tail =>
                  Keypress.EscapeSeq(char, sequence*) #:: process(tail)
                
                case _ =>
                  LazyList()

          case ']' #:: '1' #:: '1' #:: ';' #:: 'r' #:: 'g' #:: 'b' #:: ':' #:: rest =>
            val content = rest.takeWhile(_ != '\u001b').mkString.tt
            val continuation = rest.drop(content.length + 2)

            content.cut(t"/") match
              case List(Hex(red), Hex(green), Hex(blue)) =>
                TerminalInfo.BgColor(red, green, blue) #:: process(continuation)
              case _ =>
                process(continuation)

          case rest =>
            process(rest)

    case ('\b' | '\u007f') #:: rest           => Keypress.Backspace #:: process(rest)
    case '\u0009' #:: rest                    => Keypress.Tab #:: process(rest)
    case ('\u000a' | '\u000d') #:: rest       => Keypress.Enter #:: process(rest)
    case char #:: rest if char < 32           => Keypress.Control((char + 64).toChar) #:: process(rest)
    case other #:: rest                       => Keypress.CharKey(other) #:: process(rest)
    case _                                    => LazyList()

case class TerminalError(ttyMsg: Text) extends Error(msg"STDIN is not attached to a TTY: $ttyMsg")

object Terminal:
  def reportBackground: Text = t"\e]11;?\e\\"
  def reportSize: Text = t"\e[s\e[4095C\e[4095B\e[6n\e[u"
  def enableFocus: Text = t"\e[?1004h"
  def disableFocus: Text = t"\e[?1004l"
  def enablePaste: Text = t"\e[?2004h"
  def disablePaste: Text = t"\e[?2004l"

package keyboards:
  given raw: Keyboard with
    type Keypress = Char
     def process(stream: LazyList[Char]): LazyList[Keypress] = stream

  given numeric: Keyboard with
    type Keypress = Int
     def process(stream: LazyList[Char]): LazyList[Int] = stream.map(_.toInt)

  given standard(using Monitor): Keyboard { type Keypress = profanity.Keypress | TerminalInfo } =
    StandardKeyboard()

enum TerminalMode:
  case Dark, Light

case class Terminal(signals: LazyList[Signal])(using context: ProcessContext, monitor: Monitor)
extends Stdio:
  export context.stdio.{in, out, err}
  given stdio: Stdio = context.stdio

  val keyboard = StandardKeyboard()
  var mode: Maybe[TerminalMode] = Unset
  var rows: Maybe[Int] = Unset
  var columns: Maybe[Int] = Unset

  val signalHandler = Async:
    signals.foreach:
      case Signal.Winch => print(Terminal.reportSize)
      case _            => ()

  def events: LazyList[TerminalEvent] = keyboard.process(In.stream[Char]).multiplexWith(signals).map:
    case resize@TerminalInfo.WindowSize(rows2, columns2) =>
      rows = rows2
      columns = columns2
      resize
    
    case bgColor@TerminalInfo.BgColor(red, green, blue) =>
      mode = if (0.299*red + 0.587*green + 0.114*blue) > 32768 then TerminalMode.Light else TerminalMode.Dark
      bgColor

    case other =>
      other

package terminalOptions:
  given bracketedPasteMode: BracketedPasteMode = () => true
  given backgroundColorDetection: BackgroundColorDetection = () => true
  given terminalFocusDetection: TerminalFocusDetection = () => true
  given terminalSizeDetection: TerminalSizeDetection = () => true

object ProcessContext:
  def apply(stdio: Stdio, signals: LazyList[Signal] = LazyList()): ProcessContext =
    inline def initStdio: Stdio = stdio
    inline def initSignals: LazyList[Signal] = signals
    
    new ProcessContext:
      val stdio: Stdio = initStdio
      def signals: LazyList[Signal] = initSignals


trait ProcessContext:
  val stdio: Stdio
  def signals: LazyList[Signal]

object BracketedPasteMode:
  given default: BracketedPasteMode = () => false

trait BracketedPasteMode:
  def apply(): Boolean

object BackgroundColorDetection:
  given default: BackgroundColorDetection = () => false

trait BackgroundColorDetection:
  def apply(): Boolean

object TerminalFocusDetection:
  given default: TerminalFocusDetection = () => false

trait TerminalFocusDetection:
  def apply(): Boolean

object TerminalSizeDetection:
  given default: TerminalSizeDetection = () => false

trait TerminalSizeDetection:
  def apply(): Boolean

def terminal
    [ResultType]
    (block: Terminal ?=> ResultType)
    (using context: ProcessContext, monitor: Monitor)
    (using BracketedPasteMode, BackgroundColorDetection, TerminalFocusDetection, TerminalSizeDetection)
    : ResultType =
  given term: Terminal = Terminal(context.signals)
  if summon[BackgroundColorDetection]() then Out.print(Terminal.reportBackground)
  if summon[TerminalFocusDetection]() then Out.print(Terminal.enableFocus)
  if summon[BracketedPasteMode]() then Out.print(Terminal.enablePaste)
  if summon[TerminalSizeDetection]() then Out.print(Terminal.reportSize)
  
  try block(using term) finally
    if summon[BracketedPasteMode]() then Out.print(Terminal.disablePaste)
    if summon[TerminalFocusDetection]() then Out.print(Terminal.disableFocus)


inline def tty(using inline tty: Terminal): Terminal = tty
