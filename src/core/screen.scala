/*
    Yossarian, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package yossarian

import anticipation.*
import gossamer.*
import fulminate.*
import spectacular.*
import perforate.*
import rudiments.*
import turbulence.*
import kaleidoscope.*
import iridescence.*

import scala.compiletime.*

case class PtyState
    (cursor: Int = 0, savedCursor: Int = 0, style: Style = Style(), focusDetectionMode: Boolean = false,
        focus: Boolean = true, bracketedPasteMode: Boolean = false, hideCursor: Boolean = false,
        title: Text = t"", link: Text = t"")

object Pty:
  def apply(width: Int, height: Int): Pty = Pty(ScreenBuffer(width, height), PtyState(), Funnel())

  def stream(pty: Pty, in: LazyList[Text]): LazyList[Pty] raises PtyEscapeError = in match
    case head #:: tail =>
      val pty2 = pty.consume(head)
      pty2 #:: stream(pty2, tail)
    
    case _ => LazyList()

object PtyEscapeError:

  object Reason:
    given communicable: Communicable[Reason] =
      case BadSgrParameters(ns)         => msg"${ns} is not a valid SGR parameter sequence"
      case BadCsiParameter(n, command)  => msg"$n is not a valid CSI parameter for the $command command"
      case NonintegerSgrParameter(text) => msg"$text is not a numerical SGR parameter"
      case BadColor(n)                  => msg"$n is not a valid color number"
      case BadOscParameter(parameter)   => msg"$parameter is not a recognized OSC parameter"
      case BadCsiCommand(param, char)   => msg"$char (with parameter $param) is not a valid CSI command"
      case BadCsiEscape(char)           => msg"$char is not valid in a CSI escape sequence"
      case BadFeEscape(char)            => msg"$char is not a valid Fe escape"

  enum Reason:
    case BadSgrParameters(n: Text)
    case BadCsiParameter(n: Int, command: Text)
    case NonintegerSgrParameter(text: Text)
    case BadColor(n: Int)
    case BadOscParameter(parameter: Text)
    case BadCsiCommand(param: Text, char: Char)
    case BadCsiEscape(char: Char)
    case BadFeEscape(char: Char)

import PtyEscapeError.Reason, Reason.*

case class PtyEscapeError(reason: Reason)
extends Error(msg"an ANSI escape code could not be handled because $reason")

case class Pty(buffer: ScreenBuffer, state0: PtyState, output: Funnel[Text]):
  def stream: LazyList[Text] = output.stream

  def consume(input: Text): Pty raises PtyEscapeError =
    val escBuffer = StringBuilder()
    val buffer2: ScreenBuffer = buffer.copy()
    
    object cursor:
      private var index: Int = state0.cursor
      def apply(): Int = index
      def update(value: Int): Unit = index = value
      def x: Int = index%buffer2.width
      def y: Int = index/buffer2.width
      def x_=(x2: Int): Unit = index = y*buffer2.width + x2.min(buffer2.width - 1).max(0)
      def y_=(y2: Int): Unit = index = y2.min(buffer2.height - 1).max(0)*buffer2.width + x
      
    var style = state0.style
    var state = state0
    var link = state0.link

    enum Context:
      case Normal, Escape, Csi, Csi2, Osc, Osc2

    import Context.{Normal, Escape, Csi, Csi2, Osc, Osc2}

    object SgrParam:
      def unapply[DefaultType <: Int: ValueOf](params: Text): Some[Int] = params match
        case t""          => Some(valueOf[DefaultType])
        case As[Int](int) => Some(int)
        case text         => raise(PtyEscapeError(NonintegerSgrParameter(text)))(Some(valueOf[DefaultType]))

    object SgrParams:
      def unapplySeq(params: Text): Some[List[Int]] = Some(params.cut(t";").flatMap(SgrParam.unapply[0](_)))

    def wipe(cursor: Int): Unit = buffer2.set(cursor, ' ', style, link)
    
    def set(x: Int, y: Int, char: Char, style: Style = style, link: Text = link): Unit =
      buffer2.set(x, y, char, style, link)
    
    def cuu(n: Int): Unit = cursor.x = cursor.y - n
    def cud(n: Int): Unit = cursor.x = cursor.y + n
    def cuf(n: Int): Unit = cursor() = (cursor() + n).min(buffer2.width*buffer2.height - 1)
    def cub(n: Int): Unit = cursor() = (cursor() - n).max(0)
    
    def cnl(n: Int): Unit =
      cursor.x = 0
      cursor.y = cursor.y + n
    
    def cpl(n: Int): Unit =
      cursor.x = 0
      cursor.y = cursor.y - n
    
    def cha(n: Int): Unit = cursor.x = n - 1
    
    def cup(n: Int, m: Int): Unit =
      cursor.x = n - 1
      cursor.y = m - 1
    
    def ed(n: Int): Unit = n match
      case 0 => for i <- cursor() until buffer2.capacity do wipe(i)
      case 1 => for i <- 0 until cursor() do wipe(i)
      
      case 2 | 3 =>
        for i <- 0 until buffer2.capacity do wipe(i)
        cursor() = 0
      
      case n =>
        raise(PtyEscapeError(BadCsiParameter(n, t"ED")))(())
    
    def el(n: Int): Unit = n match
      case 0 => for x <- cursor.x until buffer2.width do set(x, cursor.y, ' ')
      case 1 => for x <- 0 to cursor.x do set(x, cursor.y, ' ')
      case 2 => for x <- 0 until buffer2.width do set(x, cursor.y, ' ')
      case n => raise(PtyEscapeError(BadCsiParameter(n, t"EL")))(())

    def title(text: Text): Unit = state = state.copy(title = text)
    def setLink(text: Text): Unit = link = text

    def su(n: Int): Unit = buffer2.scroll(n)
    def sd(n: Int): Unit = buffer2.scroll(-n)
    def hvp(n: Int, m: Int): Unit = cup(n, m)
    def dsr(): Unit = output.put(t"\e[${cursor.x + 1};${cursor.y + 1}s")
    def dectcem(state: Boolean): Unit = ()
    def scp(): Unit = state = state.copy(savedCursor = cursor())
    def rcp(): Unit = cursor() = state.savedCursor
    def detectFocus(boolean: Boolean): Unit = state.copy(focusDetectionMode = boolean)
    def focus(boolean: Boolean): Unit = state.copy(focus = boolean)
    def bcp(boolean: Boolean): Unit = state.copy(bracketedPasteMode = boolean)
    
    def osc(command: Text): Unit = command match
      case r"8;;$text(.*)" => setLink(text)
      case r"0;$text(.*)"  => title(text)
      case parameter       => raise(PtyEscapeError(BadOscParameter(parameter)))(())

    import Style.{Bit, Foreground, Background}, Bit.*

    def sgr(params: List[Int]): Unit = params match
      case Nil                            => ()
      case 38 :: 5 :: n :: tail           => style = Foreground(style) = color8(n)
      case 38 :: 2 :: r :: g :: b :: tail => style = Foreground(style) = Rgb24(r, g, b)
      case 48 :: 5 :: n :: tail           => style = Background(style) = color8(n)
      case 48 :: 2 :: r :: g :: b :: tail => style = Background(style) = Rgb24(r, g, b)
      case head :: tail                   =>
        style = head match
          case 0                             => Style()
          case 1                             => Bold(style) = true
          case 2                             => Faint(style) = true
          case 3                             => Italic(style) = true
          case 4                             => Underline(style) = true
          case 5                             => Blink(style) = true
          case 6                             => style
          case 7                             => Reverse(style) = true
          case 8                             => Conceal(style) = true
          case 9                             => Strike(style) = true
          case n if 10 <= n <= 19            => style
          case 20                            => style
          case 21                            => Bold(style) = false
          case 22                            => Faint(Bold(style) = false) = false
          case 23                            => Italic(style) = false
          case 24                            => Underline(style) = false
          case 25                            => Blink(style) = false
          case 26                            => style
          case 27                            => Reverse(style) = false
          case 28                            => Conceal(style) = false
          case 29                            => Strike(style) = false
          case n if 30 <= n <= 37            => Foreground(style) = palette(n - 30)
          case n if 40 <= n <= 47            => Background(style) = palette(n - 40)
          case n if 90 <= n <= 97            => Foreground(style) = palette(n - 82)
          case n if 100 <= n <= 107          => Background(style) = palette(n - 92)
          
          case _ =>
            raise(PtyEscapeError(BadSgrParameters(params.map(_.show).join(t";"))))(style)
        
        sgr(tail)
    
    def csi(params: Text, char: Char): Unit = (params, char) match
      case (SgrParams(params*), 'm') => sgr(params.to(List))
      case (SgrParam[1](n),     'A') => cuu(n)
      case (SgrParam[1](n),     'B') => cud(n)
      case (SgrParam[1](n),     'C') => cuf(n)
      case (SgrParam[1](n),     'D') => cub(n)
      case (SgrParam[1](n),     'E') => cnl(n)
      case (SgrParam[1](n),     'F') => cpl(n)
      case (SgrParam[1](n),     'G') => cha(n)
      case (SgrParams(n, m),    'H') => cup(n, m)
      case (SgrParam[0](n),     'J') => ed(n)
      case (SgrParam[0](n),     'K') => el(n)
      case (SgrParam[1](n),     'S') => su(n)
      case (SgrParam[1](n),     'T') => sd(n)
      case (SgrParams(n, m),    'f') => hvp(n, m)
      case (SgrParam[0](6),     'n') => dsr()
      case (SgrParam[0](6),     's') => scp()
      case (t"?25",             'h') => dectcem(true)
      case (t"?25",             'l') => dectcem(false)
      case (t"?1004",           'h') => detectFocus(true)
      case (t"?1004",           'l') => detectFocus(false)
      case (t"?2004",           'h') => bcp(true)
      case (t"?2004",           'l') => bcp(false)
      case (t"",                'I') => focus(true)
      case (t"",                'O') => focus(false)
      case (param,             char) => raise(PtyEscapeError(BadCsiCommand(param, char)))(())

    // Uses the Ubuntu color palette
    def palette(n: Int): Rgb24 = n match
      case 0  => Rgb24(  1,   1,   1)
      case 1  => Rgb24(222,  56,  43)
      case 2  => Rgb24( 57, 181,  74)
      case 3  => Rgb24(255, 199,   6)
      case 4  => Rgb24(  0, 111, 184)
      case 5  => Rgb24(118,  38, 113)
      case 6  => Rgb24( 44, 181, 233)
      case 7  => Rgb24(204, 204, 204)
      case 8  => Rgb24(  1,   1,   1)
      case 9  => Rgb24(255,   0,   0)
      case 10 => Rgb24(  0, 255,   0)
      case 11 => Rgb24(255, 255,   0)
      case 12 => Rgb24(  0,   0, 255)
      case 13 => Rgb24(255,   0, 255)
      case 14 => Rgb24(  0, 255, 255)
      case 15 => Rgb24(255, 255, 255)
      case _  => throw Mistake(msg"tried to access non-existent palette color")

    def color8(n: Int): Rgb24 = n match
      case n if 0 <= n <= 15 =>
        palette(n)
      
      case n if 232 <= n <= 255 =>
        val gray = n - 232
        val gray2 = 7 + gray*10 + gray/2
        Rgb24(gray2, gray2, gray2)

      case n if 16 <= n <= 231 =>
        val r = (n - 16)/36
        val g = (n - 16 - 36*r)/6
        val b = n - 16 - 36*r - 6*g
        Rgb24(r*42 + r/2, g*42 + g/2, b*42 + b/2)
      
      case n =>
        raise(PtyEscapeError(BadColor(n)))(Rgb24(127, 127, 127))

    def recur(index: Int, context: Context): Pty =
      inline def proceed(context: Context): Pty =
        recur(index + 1, context)
      
      inline def bs(): Pty =
        set(cursor.x, cursor.y, ' ', buffer2.style(cursor.x, cursor.y))
        cursor() -= 1
        if cursor() < 0 then cursor() = 0
        proceed(Normal)
      
      inline def lf(): Pty =
        cursor.x = 0
        ff()
      
      inline def ff(): Pty =
        if cursor.y < buffer2.height - 1 then cursor.y = cursor.y + 1 else su(1)
        proceed(Normal)
      
      inline def cr(): Pty =
        cursor.x = 0
        proceed(Normal)
      
      inline def put(char: Char): Pty =
        set(cursor.x, cursor.y, char)
        cursor() += 1
        if cursor() == buffer2.capacity then
          buffer2.scroll(1)
          cursor() -= buffer2.width

        proceed(Normal)

      if index >= input.length
      then Pty(buffer2, state.copy(cursor = cursor(), style = style, link = link), output = output)
      else
       val current: Char = unsafely(input(index))
       context match
        case Normal => (current: @switch) match
          case '\u0000' => proceed(Normal) // nul()
          case '\u0001' => proceed(Normal) // soh()
          case '\u0002' => proceed(Normal) // stx()
          case '\u0003' => proceed(Normal) // etx()
          case '\u0004' => proceed(Normal) // eot()
          case '\u0005' => proceed(Normal) // enq()
          case '\u0006' => proceed(Normal) // ack()
          case '\u0007' => proceed(Normal) // bel()
          case '\u0008' => bs()
          case '\u0009' => proceed(Normal) // ht()
          case '\u000a' => lf()
          case '\u000b' => proceed(Normal) // vt()
          case '\u000c' => ff()
          case '\u000d' => cr()
          case '\u000e' => proceed(Normal) // so()
          case '\u000f' => proceed(Normal) // si()
          case '\u0010' => proceed(Normal) // dle()
          case '\u0011' => proceed(Normal) // dc1()
          case '\u0012' => proceed(Normal) // dc2()
          case '\u0013' => proceed(Normal) // dc3()
          case '\u0014' => proceed(Normal) // dc4()
          case '\u0015' => proceed(Normal) // nak()
          case '\u0016' => proceed(Normal) // syn()
          case '\u0017' => proceed(Normal) // etb()
          case '\u0018' => proceed(Normal) // can()
          case '\u0019' => proceed(Normal) // em()
          case '\u001a' => proceed(Normal) // sub()
          case '\u001b' => proceed(Escape)
          case '\u001c' => proceed(Normal) // fs()
          case '\u001d' => proceed(Normal) // gs()
          case '\u001e' => proceed(Normal) // rs()
          case '\u001f' => proceed(Normal) // us()
          case ch       => put(ch)
        
        case Escape => current match
          case '['                                      => recur(index + 1, Csi)
          case ']'                                      => recur(index + 1, Osc)
          case 'N' | 'O' | 'P' | '\\' | 'X' | '^' | '_' => proceed(Normal)
          
          case char =>
            raise(PtyEscapeError(BadFeEscape(char)))(())
            proceed(Normal)
        
        case Osc => current match
          case '\u0007' =>
            osc(escBuffer.text.tap(escBuffer.clear().waive))
            proceed(Normal)
          
          case '\u0027' =>
            proceed(Osc2)
          
          case char =>
            escBuffer.append(char)
            proceed(Osc)
        
        case Osc2 => current match
          case '\\' =>
            osc(escBuffer.text.tap(escBuffer.clear().waive))
            proceed(Normal)
          
          case char =>
            escBuffer.append(char)
            proceed(Normal)
        
        case Csi => current match
          case char if '\u0000' <= char <= '\u001f' =>
            raise(PtyEscapeError(BadCsiEscape(char)))(())
            proceed(Csi)

          case char if '\u0020' <= char <= '\u002f' =>
            escBuffer.append(char)
            proceed(Csi2)
          
          case char if '\u0030' <= char <= '\u003f' =>
            escBuffer.append(char)
            proceed(Csi)
          
          case char if '\u0040' <= char <= '\u007e' =>
            csi(escBuffer.text.tap(escBuffer.clear().waive), char)
            proceed(Normal)
          
          case char =>
            raise(PtyEscapeError(BadCsiEscape(char)))(())
            proceed(Normal)

        case Csi2 => current match
          case char if '\u0020' <= char <= '\u002f' =>
            escBuffer.append(char)
            proceed(Csi2)
          
          case char if '\u0040' <= char <= '\u007e' =>
            csi(escBuffer.text.tap(escBuffer.clear().waive), char)
            proceed(Normal)
          
          case char =>
            raise(PtyEscapeError(BadCsiEscape(char)))(())
            proceed(Normal)

    recur(0, Normal)