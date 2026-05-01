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
package yossarian

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import kaleidoscope.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*

import PtyEscapeError.Reason, Reason.*

object Pty:
  def apply(width: Int, height: Int): Pty = Pty(Screen(width, height), PtyState(), Spool())

  def stream(pty: Pty, in: Stream[Text]): Stream[Pty] raises PtyEscapeError = in match
    case head #:: tail =>
      val pty2 = pty.consume(head)
      pty2 #:: stream(pty2, tail)

    case _ =>
      Stream()

case class Pty(buffer: Screen, state0: PtyState, output: Spool[Text]):
  def stream: Stream[Text] = output.stream

  def consume(input: Text): Pty raises PtyEscapeError =
    val escBuffer = StringBuilder()
    val buffer2: Screen = buffer.copy()

    object cursor:
      private var index: Ordinal = state0.cursor

      def apply(): Ordinal = index
      def update(value: Ordinal): Unit = index = value

      def x: Ordinal = (index.n0%buffer2.width).z
      def y: Ordinal = (index.n0/buffer2.width).z

      def x_=(x2: Ordinal): Unit =
        val clamped: Ordinal =
          if x2 < Prim then Prim
          else if x2 >= buffer2.width.z then (buffer2.width - 1).z
          else x2

        index = (y.n0*buffer2.width + clamped.n0).z

      def y_=(y2: Ordinal): Unit =
        val clamped: Ordinal =
          if y2 < Prim then Prim
          else if y2 >= buffer2.height.z then (buffer2.height - 1).z
          else y2

        index = (clamped.n0*buffer2.width + x.n0).z

    var style = state0.style
    var state = state0
    var link = state0.link

    enum Context:
      case Normal, Escape, Csi, Csi2, Osc, Osc2

    import Context.{Normal, Escape, Csi, Csi2, Osc, Osc2}

    def wipe(cursor: Ordinal): Unit = buffer2.set(cursor, ' ', style, link)

    def set(x: Ordinal, y: Ordinal, char: Char, style: Style = style, link: Text = link): Unit =
      buffer2.set(x, y, char, style, link)

    def cuu(n: Int): Unit = cursor.y = cursor.y - n
    def cud(n: Int): Unit = cursor.y = cursor.y + n
    def cuf(n: Int): Unit = cursor.x = cursor.x + n
    def cub(n: Int): Unit = cursor.x = cursor.x - n

    def cnl(n: Int): Unit =
      cursor.x = Prim
      cursor.y = cursor.y + n

    def cpl(n: Int): Unit =
      cursor.x = Prim
      cursor.y = cursor.y - n

    def cha(col: Ordinal): Unit = cursor.x = col

    def cup(row: Ordinal, col: Ordinal): Unit =
      cursor.y = row
      cursor.x = col

    def ed(n: Int): Unit = n match
      case 0 => for i <- cursor().n0 until buffer2.capacity do wipe(i.z)
      case 1 => for i <- 0 until cursor().n0 do wipe(i.z)

      case 2 | 3 =>
        for i <- 0 until buffer2.capacity do wipe(i.z)
        cursor() = Prim

      case n =>
        raise(PtyEscapeError(BadCsiParameter(n, t"ED")))

    def el(n: Int): Unit = n match
      case 0 => for x <- cursor.x.n0 until buffer2.width do set(x.z, cursor.y, ' ')
      case 1 => for x <- 0 to cursor.x.n0 do set(x.z, cursor.y, ' ')
      case 2 => for x <- 0 until buffer2.width do set(x.z, cursor.y, ' ')
      case n => raise(PtyEscapeError(BadCsiParameter(n, t"EL")))

    def title(text: Text): Unit = state = state.copy(title = text)
    def setLink(text: Text): Unit = link = text
    def su(n: Int): Unit = buffer2.scroll(n)
    def sd(n: Int): Unit = buffer2.scroll(-n)
    def hvp(row: Ordinal, col: Ordinal): Unit = cup(row, col)
    def dsr(): Unit = output.put(t"\e[${cursor.y.n1};${cursor.x.n1}R")
    def dectcem(visible: Boolean): Unit = state = state.copy(hideCursor = !visible)
    def scp(): Unit = state = state.copy(savedCursor = cursor())
    def rcp(): Unit = cursor() = state.savedCursor
    def detectFocus(value: Boolean): Unit = state = state.copy(focusDetectionMode = value)
    def focus(value: Boolean): Unit = state = state.copy(focus = value)
    def bcp(value: Boolean): Unit = state = state.copy(bracketedPasteMode = value)

    def osc(command: Text): Unit = command match
      case r"8;([^;]*);$text(.*)" => setLink(text)
      case r"0;$text(.*)"         => title(text)
      case parameter              => raise(PtyEscapeError(BadOscParameter(parameter)))

    import Style.{Bit, Foreground, Background}, Bit.*

    def sgr(params: List[Int]): Unit = params match
      case Nil                            => ()
      case 38 :: 5 :: n :: tail           => style = Foreground(style) = color8(n); sgr(tail)
      case 38 :: 2 :: r :: g :: b :: tail => style = Foreground(style) = Chroma(r, g, b); sgr(tail)
      case 48 :: 5 :: n :: tail           => style = Background(style) = color8(n); sgr(tail)
      case 48 :: 2 :: r :: g :: b :: tail => style = Background(style) = Chroma(r, g, b); sgr(tail)

      case head :: tail =>
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
          case 39                            => Foreground(style) = Chroma(255, 255, 255)
          case n if 40 <= n <= 47            => Background(style) = palette(n - 40)
          case 49                            => Background(style) = Chroma(0, 0, 0)
          case n if 90 <= n <= 97            => Foreground(style) = palette(n - 82)
          case n if 100 <= n <= 107          => Background(style) = palette(n - 92)

          case _ =>
            raise(PtyEscapeError(BadSgrParameters(params.map(_.show).join(t";")))) yet style

        sgr(tail)

    def parseInt(text: Text, default: Int): Int =
      if text.s.isEmpty then default
      else text.s.toIntOption.getOrElse:
        raise(PtyEscapeError(NonintegerSgrParameter(text))) yet default

    def parseInts(text: Text): List[Int] =
      if text.s.isEmpty then Nil
      else text.cut(t";").to(List).map(parseInt(_, 0))

    def parsePair(text: Text, default: Int): (Int, Int) =
      if text.s.isEmpty then (default, default) else
        val parts = text.cut(t";").to(List)
        val first = if parts.length >= 1 then parseInt(parts(0), default) else default
        val second = if parts.length >= 2 then parseInt(parts(1), default) else default
        (first, second)

    def privateMode(params: Text, char: Char): Unit = (params, char) match
      case (t"?25",   'h') => dectcem(true)
      case (t"?25",   'l') => dectcem(false)
      case (t"?1004", 'h') => detectFocus(true)
      case (t"?1004", 'l') => detectFocus(false)
      case (t"?2004", 'h') => bcp(true)
      case (t"?2004", 'l') => bcp(false)
      case (_, 'h' | 'l')  => () // unknown DEC private modes are silently ignored
      case _               => raise(PtyEscapeError(BadCsiCommand(params, char)))

    def csi(params: Text, char: Char): Unit =
      if params.s.startsWith("?") then privateMode(params, char) else char match
        case 'm' => sgr(parseInts(params))
        case 'A' => cuu(parseInt(params, 1))
        case 'B' => cud(parseInt(params, 1))
        case 'C' => cuf(parseInt(params, 1))
        case 'D' => cub(parseInt(params, 1))
        case 'E' => cnl(parseInt(params, 1))
        case 'F' => cpl(parseInt(params, 1))
        case 'G' => cha(parseInt(params, 1).u)
        case 'J' => ed(parseInt(params, 0))
        case 'K' => el(parseInt(params, 0))
        case 'S' => su(parseInt(params, 1))
        case 'T' => sd(parseInt(params, 1))

        case 'H' => val (r, c) = parsePair(params, 1); cup(r.u, c.u)
        case 'f' => val (r, c) = parsePair(params, 1); hvp(r.u, c.u)

        case 'n' if parseInt(params, 0) == 6                     => dsr()
        case 's' if params.s.isEmpty || parseInt(params, 0) == 6 => scp()
        case 'u' if params.s.isEmpty                             => rcp()
        case 'I' if params.s.isEmpty                             => focus(true)
        case 'O' if params.s.isEmpty                             => focus(false)

        case _ => raise(PtyEscapeError(BadCsiCommand(params, char)))

    // Uses the Ubuntu color palette
    def palette(n: Int): Chroma = n match
      case 0  => Chroma(001, 001, 001)
      case 1  => Chroma(222, 056, 043)
      case 2  => Chroma(057, 181, 074)
      case 3  => Chroma(255, 199, 006)
      case 4  => Chroma(000, 111, 184)
      case 5  => Chroma(118, 038, 113)
      case 6  => Chroma(044, 181, 233)
      case 7  => Chroma(204, 204, 204)
      case 8  => Chroma(085, 085, 085)
      case 9  => Chroma(255, 000, 000)
      case 10 => Chroma(000, 255, 000)
      case 11 => Chroma(255, 255, 000)
      case 12 => Chroma(000, 000, 255)
      case 13 => Chroma(255, 000, 255)
      case 14 => Chroma(000, 255, 255)
      case 15 => Chroma(255, 255, 255)
      case _  => panic(m"tried to access non-existent palette color")

    def color8(n: Int): Chroma = n match
      case n if 0 <= n <= 15 => palette(n)

      case n if 232 <= n <= 255 =>
        val gray = n - 232
        val gray2 = 7 + gray*10 + gray/2
        Chroma(gray2, gray2, gray2)

      case n if 16 <= n <= 231 =>
        val r = (n - 16)/36
        val g = (n - 16 - 36*r)/6
        val b = n - 16 - 36*r - 6*g
        Chroma(r*42 + r/2, g*42 + g/2, b*42 + b/2)

      case n =>
        raise(PtyEscapeError(BadColor(n))) yet Chroma(127, 127, 127)

    def recur(index: Int, context: Context): Pty =
      inline def proceed(context: Context): Pty =
        recur(index + 1, context)

      inline def bs(): Pty =
        cursor.x = cursor.x - 1
        proceed(Normal)

      inline def lf(): Pty =
        cursor.x = Prim
        ff()

      inline def ff(): Pty =
        if cursor.y.n0 < buffer2.height - 1 then cursor.y = cursor.y + 1 else su(1)
        proceed(Normal)

      inline def cr(): Pty =
        cursor.x = Prim
        proceed(Normal)

      inline def put(char: Char): Pty =
        set(cursor.x, cursor.y, char)
        cursor() += 1
        if cursor().n0 == buffer2.capacity then
          buffer2.scroll(1)
          cursor() -= buffer2.width

        proceed(Normal)

      if index >= input.length
      then Pty(buffer2, state.copy(cursor = cursor(), style = style, link = link), output = output)
      else
        val current: Char = unsafely(input.s.charAt(index))

        context match
          case Normal =>
            (current: @switch) match
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

          case Escape =>
            current match
              case '['                                      => recur(index + 1, Csi)
              case ']'                                      => recur(index + 1, Osc)
              case 'N' | 'O' | 'P' | '\\' | 'X' | '^' | '_' => proceed(Normal)

              case char =>
                raise(PtyEscapeError(BadFeEscape(char)))
                proceed(Normal)

          case Osc =>
            current match
              case '\u0007' =>
                osc(escBuffer.text.also(escBuffer.clear()))
                proceed(Normal)

              case '\u001b' =>
                proceed(Osc2)

              case char =>
                escBuffer.append(char)
                proceed(Osc)

          case Osc2 =>
            current match
              case '\\' =>
                osc(escBuffer.text.also(escBuffer.clear()))
                proceed(Normal)

              case char =>
                escBuffer.append('\u001b')
                escBuffer.append(char)
                proceed(Osc)

          case Csi =>
            current match
              case char if '\u0000' <= char <= '\u001f' =>
                raise(PtyEscapeError(BadCsiEscape(char)))
                proceed(Csi)

              case char if '\u0020' <= char <= '\u002f' =>
                escBuffer.append(char)
                proceed(Csi2)

              case char if '\u0030' <= char <= '\u003f' =>
                escBuffer.append(char)
                proceed(Csi)

              case char if '\u0040' <= char <= '\u007e' =>
                csi(escBuffer.text.also(escBuffer.clear()), char)
                proceed(Normal)

              case char =>
                raise(PtyEscapeError(BadCsiEscape(char)))
                proceed(Normal)

          case Csi2 =>
            current match
              case char if '\u0020' <= char <= '\u002f' =>
                escBuffer.append(char)
                proceed(Csi2)

              case char if '\u0040' <= char <= '\u007e' =>
                csi(escBuffer.text.also(escBuffer.clear()), char)
                proceed(Normal)

              case char =>
                raise(PtyEscapeError(BadCsiEscape(char)))
                proceed(Normal)

    recur(0, Normal)
