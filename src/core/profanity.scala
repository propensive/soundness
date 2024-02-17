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
import vacuous.*
import hypotenuse.*
import fulminate.*
import gossamer.*
import spectacular.*
import contingency.*
import parasite.*
import turbulence.*
import anticipation.*, timeInterfaces.long

import language.experimental.captureChecking

trait Keyboard:
  type Keypress
  def process(stream: LazyList[Char]): LazyList[Keypress]

object Keyboard:
  import Keypress.*

  def modified(code: Char, keypress: EditKey | FunctionKey): EditKey | FunctionKey | Shift | Alt | Ctrl | Meta =
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

class StandardKeyboard()(using Monitor) extends Keyboard:
  type Keypress = profanity.Keypress | TerminalInfo

  def process(stream: LazyList[Char]): LazyList[Keypress] = stream match
    case '\u001b' #:: rest                    =>
      safely(Async(rest.head).await(30L)) match
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
            
            case '1' #:: ';' #:: modifiers #:: (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H')) #:: rest =>
              Keyboard.modified(modifiers, Keyboard.navigation(code)) #:: process(rest)
              
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

  given standard(using monitor: Monitor): StandardKeyboard^{monitor} = StandardKeyboard()

enum TerminalMode:
  case Dark, Light

case class Terminal(signals: LazyList[Signal])(using context: ProcessContext, monitor: Monitor)
extends Stdio:
  export context.stdio.{in, out, err}
  given stdio: Stdio = context.stdio

  val keyboard: StandardKeyboard^{monitor} = StandardKeyboard()
  val initRows: Promise[Int] = Promise()
  val initColumns: Promise[Int] = Promise()
  var mode: Optional[TerminalMode] = Unset
  var rows: Optional[Int] = Unset
  var columns: Optional[Int] = Unset

  def knownColumns: Int = safely(initColumns.await(50L)).or(80)

  private val signalHandler: Async[Unit] = Async:
    signals.each:
      case Signal.Winch => print(Terminal.reportSize)
      case _            => ()

  def events: LazyList[TerminalEvent]^{monitor} = LazyList.defer:
    keyboard.process(In.stream[Char]).multiplexWith(signals).map:
      case resize@TerminalInfo.WindowSize(rows2, columns2) =>
        rows = rows2
        initRows.offer(rows2)
        columns = columns2
        initColumns.offer(columns2)
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

inline def terminal: Terminal = compiletime.summonInline[Terminal]

def terminal
    [ResultType]
    (block: Terminal ?=> ResultType)
    (using context: ProcessContext, monitor: Monitor)
    (using BracketedPasteMode, BackgroundColorDetection, TerminalFocusDetection, TerminalSizeDetection)
    : ResultType =
  given terminal: Terminal = Terminal(context.signals)
  if summon[BackgroundColorDetection]() then Out.print(Terminal.reportBackground)
  if summon[TerminalFocusDetection]() then Out.print(Terminal.enableFocus)
  if summon[BracketedPasteMode]() then Out.print(Terminal.enablePaste)
  if summon[TerminalSizeDetection]() then Out.print(Terminal.reportSize)
  
  try block(using terminal) finally
    if summon[BracketedPasteMode]() then Out.print(Terminal.disablePaste)
    if summon[TerminalFocusDetection]() then Out.print(Terminal.disableFocus)

