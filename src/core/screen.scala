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
import perforate.*
import rudiments.*
import turbulence.*
import kaleidoscope.*
import iridescence.*

import scala.compiletime.*

case class PtyState
    (cursor: Int = 0, savedCursor: Int = 0, style: Style = Style(), focusDetectionMode: Boolean = false,
        focus: Boolean = true, bracketedPasteMode: Boolean = false, hideCursor: Boolean = false,
        title: Text = t"")

object Pty:
  def apply(width: Int, height: Int): Pty = Pty(ScreenBuffer(width, height), PtyState(), Funnel())

  def stream(pty: Pty, in: LazyList[Text]): LazyList[Pty] raises PtyEscapeError = in match
    case head #:: tail =>
      val pty2 = pty.consume(head)
      pty2 #:: stream(pty2, tail)
    
    case _ => LazyList()

case class PtyEscapeError() extends Error(msg"an ANSI escape code was not handled")

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

    enum Context:
      case Normal, Escape, Csi, Csi2, Osc, Osc2

    import Context.{Normal, Escape, Csi, Csi2, Osc, Osc2}

    object IntParam:
      def unapply[DefaultType <: Int: ValueOf](params: Text): Some[Int] = params match
        case t""          => Some(valueOf[DefaultType])
        case As[Int](int) => Some(int)
        case other        => raise(PtyEscapeError())(Some(valueOf[DefaultType]))

    object IntParams:
      def unapplySeq(params: Text): Some[List[Int]] = Some(params.cut(t";").flatMap(IntParam.unapply[0](_)))

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
      case 0 => for i <- cursor() until buffer2.capacity do buffer2.set(i, ' ', style)
      case 1 => for i <- 0 until cursor() do buffer2.set(i, ' ', style)
      
      case 2 | 3 =>
        for i <- 0 until buffer2.capacity do buffer2.set(i, ' ', style)
        cursor() = 0
      
      case _ =>
        raise(PtyEscapeError())(())
    
    def el(n: Int): Unit = n match
      case 0 => for x <- cursor.x until buffer2.width do buffer2.set(x, cursor.y, ' ', style)
      case 1 => for x <- 0 to cursor.x do buffer2.set(x, cursor.y, ' ', style)
      case 2 => for x <- 0 until buffer2.width do buffer2.set(x, cursor.y, ' ', style)
      case _ => raise(PtyEscapeError())(())

    def title(text: Text): Unit = state = state.copy(title = text)
    def link(text: Text): Unit = ()

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
      case r"8;;$text(.*)" => link(text)
      case r"0;$text(.*)"  => title(text)
      case _               => raise(PtyEscapeError())(())

    def sgr(params: List[Int]): Unit = params match
      case Nil =>
      case 38 :: 5 :: n :: tail           => style = style.foreground = color8(n)
      case 38 :: 2 :: r :: g :: b :: tail => style = style.foreground = Rgb24(r, g, b)
      case 48 :: 5 :: n :: tail           => style = style.background = color8(n)
      case 48 :: 2 :: r :: g :: b :: tail => style = style.background = Rgb24(r, g, b)
      case head :: tail                   => head match
        case 0                                => style = Style(); sgr(tail)
        case 1                                => style = style.bold = true; sgr(tail)
        case 2                                => style = style.faint = true; sgr(tail)
        case 3                                => style = style.italic = true; sgr(tail)
        case 4                                => style = style.underline = true; sgr(tail)
        case 5                                => style = style.blink = true; sgr(tail)
        case 6                                => sgr(tail)
        case 7                                => style = style.reverse = true; sgr(tail)
        case 8                                => style = style.conceal = true; sgr(tail)
        case 9                                => style = style.strike = true; sgr(tail)
        case n if 10 <= n <= 19               => sgr(tail)
        case 20                               => sgr(tail)
        case 21                               => style = style.bold = false; sgr(tail)
        case 22                               => style = style.bold = false; style = style.faint = false; sgr(tail)
        case 23                               => style = style.italic = false; sgr(tail)
        case 24                               => style = style.underline = false; sgr(tail)
        case 25                               => style = style.blink = false; sgr(tail)
        case 26                               => sgr(tail)
        case 27                               => style = style.reverse = false; sgr(tail)
        case 28                               => style = style.conceal = false; sgr(tail)
        case 29                               => style = style.strike = false; sgr(tail)
        case n if 30 <= n <= 37               => style = style.foreground = palette(n - 30); sgr(tail)
        case n if 40 <= n <= 47               => style = style.background = palette(n - 40); sgr(tail)
        case n if 90 <= n <= 97               => style = style.foreground = palette(n - 82); sgr(tail)
        case n if 100 <= n <= 107             => style = style.background = palette(n - 92); sgr(tail)
        case _                                => raise(PtyEscapeError())(())
    
    def csi(params: Text, char: Char): Unit = (params, char) match
      case (IntParams(params*), 'm') => sgr(params.to(List))
      case (IntParam[1](n),     'A') => cuu(n)
      case (IntParam[1](n),     'B') => cud(n)
      case (IntParam[1](n),     'C') => cuf(n)
      case (IntParam[1](n),     'D') => cub(n)
      case (IntParam[1](n),     'E') => cnl(n)
      case (IntParam[1](n),     'F') => cpl(n)
      case (IntParam[1](n),     'G') => cha(n)
      case (IntParams(n, m),    'H') => cup(n, m)
      case (IntParam[0](n),     'J') => ed(n)
      case (IntParam[0](n),     'K') => el(n)
      case (IntParam[1](n),     'S') => su(n)
      case (IntParam[1](n),     'T') => sd(n)
      case (IntParams(n, m),    'f') => hvp(n, m)
      case (IntParam[0](6),     'n') => dsr()
      case (IntParam[0](6),     's') => scp()
      case (t"?25",             'h') => dectcem(true)
      case (t"?25",             'l') => dectcem(false)
      case (t"?1004",           'h') => detectFocus(true)
      case (t"?1004",           'l') => detectFocus(false)
      case (t"?2004",           'h') => bcp(true)
      case (t"?2004",           'l') => bcp(false)
      case (t"",                'I') => focus(true)
      case (t"",                'O') => focus(false)
      case _                         => raise(PtyEscapeError())(())

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
      case _  => raise(PtyEscapeError())(Rgb24(127, 127, 127))

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
      
      case _ =>
        raise(PtyEscapeError())(Rgb24(127, 127, 127))

    def recur(index: Int, context: Context): Pty =
      inline def proceed(context: Context): Pty =
        recur(index + 1, context)
      
      inline def bs(): Pty =
        buffer2.set(cursor.x, cursor.y, ' ', buffer2.style(cursor.x, cursor.y))
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
        buffer2.set(cursor.x, cursor.y, char, style)
        cursor() += 1
        if cursor() == buffer2.capacity then
          buffer2.scroll(1)
          cursor() -= buffer2.width

        proceed(Normal)

      if index >= input.length then Pty(buffer2, state.copy(cursor = cursor(), style = style), output = output)
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
          
          case _ =>
            raise(PtyEscapeError())(())
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
          case ch if '\u0000' <= ch <= '\u001f' =>
            raise(PtyEscapeError())(())
            proceed(Csi)

          case ch if '\u0020' <= ch <= '\u002f' =>
            escBuffer.append(ch)
            proceed(Csi2)
          
          case ch if '\u0030' <= ch <= '\u003f' =>
            escBuffer.append(ch)
            proceed(Csi)
          
          case ch if '\u0040' <= ch <= '\u007e' =>
            csi(escBuffer.text.tap(escBuffer.clear().waive), ch)
            proceed(Normal)
          
          case ch =>
            raise(PtyEscapeError())(())
            proceed(Normal)

        case Csi2 => current match
          case ch if '\u0020' <= ch <= '\u002f' =>
            escBuffer.append(ch)
            proceed(Csi2)
          
          case ch if '\u0000' <= ch <= '\u003f' =>
            raise(PtyEscapeError())(())
            proceed(Csi)

          case ch if '\u0040' <= ch <= '\u007e' =>
            csi(escBuffer.text.tap(escBuffer.clear().waive), ch)
            proceed(Normal)
          
          case ch =>
            raise(PtyEscapeError())(())
            proceed(Normal)

    recur(0, Normal)