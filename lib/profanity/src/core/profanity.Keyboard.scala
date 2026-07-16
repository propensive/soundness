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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import turbulence.*
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
    case c: ( '@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' |
      'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' |
      '\\' | ']' | '^' | '_' ) =>
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

  // Whether a just-read ESC begins an escape sequence: `true` when another
  // character follows closely enough to belong to one. On a live terminal,
  // already-buffered input decides INSTANTLY, with no timing heuristic at all
  // — a terminal emits a whole sequence in a single write, so its bytes
  // arrive together — and only a genuinely quiet line waits, up to the
  // (short) deadline, before a bare ESC reports as the Escape key. The
  // deadline exists solely for a sequence whose bytes straddle a packet
  // boundary (a fragmented SSH write), so it can be far shorter than the
  // 30ms every bare Escape formerly cost.
  trait Lookahead:
    def sequenceFollows(rest: LazyList[Char]): Boolean

  object Lookahead:
    // For pre-materialized input (tests, replays): a non-empty tail follows.
    // Not for live input — emptiness would block on an unforced tail.
    given immediate: Lookahead = !_.isEmpty

    def tty(stdio: Stdio, deadline: Int = 10): Lookahead = rest =>
      def wait(remaining: Int): Boolean =
        stdio.ready() || remaining > 0 && { Thread.sleep(1); wait(remaining - 1) }

      wait(deadline)

  class Standard()(using lookahead: Lookahead) extends Keyboard:
    type Keypress = profanity.Keypress | TerminalInfo

    def process(stream: LazyList[Char]): LazyList[Keypress] = stream match
      case '\u001b' #:: rest =>
        if !lookahead.sequenceFollows(rest) then Keypress.Escape #:: process(rest)
        else
          rest match
            case 'O' #:: key #:: rest => Keypress.FunctionKey(key.toInt - 79) #:: process(rest)

            case '[' #:: rest => rest match
              case (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H')) #:: rest =>
                Keyboard.navigation(code) #:: process(rest)

              case code #:: '~' #:: rest if '1' <= code <= '9' =>
                Keyboard.vt(code) #:: process(rest)

              case code #:: ';' #:: modifiers #:: '~' #:: rest if '1' <= code <= '9' =>
                Keyboard.modified(modifiers, Keyboard.vt(code)) #:: process(rest)

              case
                ( '1' #:: ';' #:: modifiers #:: (code@('A' | 'B' | 'C' | 'D' | 'F' | 'H')) #::
                  rest ) =>

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

                  case 'R' #:: tail =>
                    // Explicit decoding rather than the `As[Int]` extractor: in a nested
                    // pattern the extractor's evidence is summoned against a skolem-typed
                    // scrutinee, which fails to unify under capture checking.
                    val fields: List[Text] = sequence.map(_.show).join.cut(';').to(List)

                    val size: Optional[TerminalInfo.WindowSize] = fields match
                      case List(rows, cols) =>
                        safely(TerminalInfo.WindowSize(rows.as[Int], cols.as[Int]))

                      case _ =>
                        Unset

                    size.lay(process(tail))(_ #:: process(tail))

                  case 'O' #:: tail =>
                    TerminalInfo.LoseFocus #:: process(tail)

                  case 'I' #:: tail =>
                    TerminalInfo.GainFocus #:: process(tail)

                  case char #:: tail =>
                    Keypress.EscapeSeq(char, sequence*) #:: process(tail)

                  case _ =>
                    LazyList()

            case ']' #:: '1' #:: '1' #:: ';' #:: 'r' #:: 'g' #:: 'b' #:: ':' #:: rest =>
              val content = rest.takeWhile(_ != '\u001b').mkString.tt
              val continuation = rest.drop(content.length + 2)

              content.cut(t"/").to(List) match
                case List(red, green, blue) =>
                  def decimal(hex: Text): Int = Integer.parseInt(hex.s, 16)

                  TerminalInfo.BgColor(decimal(red), decimal(green), decimal(blue)) #::
                    process(continuation)

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
        LazyList()

trait Keyboard:
  type Keypress

  def process(stream: LazyList[Char]): LazyList[Keypress]
