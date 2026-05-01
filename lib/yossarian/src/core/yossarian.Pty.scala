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
  def apply(width: Int, height: Int): Pty =
    Pty(Screen(width, height), PtyState(scrollBottom = (height - 1).z), Spool())

  def stream(pty: Pty, in: Stream[Text]): Stream[Pty] raises PtyEscapeError = in match
    case head #:: tail =>
      val pty2 = pty.consume(head)
      pty2 #:: stream(pty2, tail)

    case _ =>
      Stream()

case class Pty(buffer: Screen, state: PtyState, output: Spool[Text]):
  def stream: Stream[Text] = output.stream

  def title: Text = state.title
  def cursor: Ordinal = state.cursor
  def cursorVisible: Boolean = !state.hideCursor

  def consume(input: Text): Pty raises PtyEscapeError =
    val escBuffer = StringBuilder()
    val buffer2: Screen = buffer.copy()

    var pendingWrap: Boolean = state.pendingWrap
    var lastChar: Char = ' '

    object cursor:
      private var index: Ordinal = state.cursor

      def apply(): Ordinal = index

      def update(value: Ordinal): Unit =
        pendingWrap = false
        index = value

      def x: Ordinal = (index.n0%buffer2.width).z
      def y: Ordinal = (index.n0/buffer2.width).z

      def x_=(x2: Ordinal): Unit =
        pendingWrap = false
        val clamped: Ordinal =
          if x2 < Prim then Prim
          else if x2 >= buffer2.width.z then (buffer2.width - 1).z
          else x2

        index = (y.n0*buffer2.width + clamped.n0).z

      def y_=(y2: Ordinal): Unit =
        pendingWrap = false
        val clamped: Ordinal =
          if y2 < Prim then Prim
          else if y2 >= buffer2.height.z then (buffer2.height - 1).z
          else y2

        index = (clamped.n0*buffer2.width + x.n0).z

      // Internal advancement that does NOT clear pendingWrap (used by put).
      def setIndex(value: Ordinal): Unit = index = value

    var style = state.style
    var state2 = state
    var link = state.link
    var scrollTop: Ordinal = state.scrollTop
    var scrollBottom: Ordinal = state.scrollBottom

    enum Context:
      case Normal, Escape, Csi, Csi2, Osc, Osc2, EatString, EatString2

    import Context.{Normal, Escape, Csi, Csi2, Osc, Osc2, EatString, EatString2}

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

    def title(text: Text): Unit = state2 = state2.copy(title = text)
    def setLink(text: Text): Unit = link = text
    def su(n: Int): Unit = buffer2.scroll(n, scrollTop.n0, scrollBottom.n0)
    def sd(n: Int): Unit = buffer2.scroll(-n, scrollTop.n0, scrollBottom.n0)

    def decstbm(top: Ordinal, bottom: Ordinal): Unit =
      val t = if top < Prim then Prim else top
      val b = if bottom >= buffer2.height.z then (buffer2.height - 1).z else bottom
      if t < b then
        scrollTop = t
        scrollBottom = b
        cursor() = (t.n0*buffer2.width).z

    def ind(): Unit =
      if cursor.y == scrollBottom then su(1)
      else if cursor.y.n0 < buffer2.height - 1 then cursor.y = cursor.y + 1

    def ri(): Unit =
      if cursor.y == scrollTop then sd(1)
      else if cursor.y > Prim then cursor.y = cursor.y - 1

    def nel(): Unit =
      cursor.x = Prim
      ind()

    def hvp(row: Ordinal, col: Ordinal): Unit = cup(row, col)
    def dsr(): Unit = output.put(t"\e[${cursor.y.n1};${cursor.x.n1}R")
    // Primary DA: claim VT102 with advanced video option. The response
    // shape `\e[?1;2c` is what xterm and most modern terminals reply.
    def primaryDa(): Unit = output.put(t"\e[?1;2c")
    // Secondary DA: terminal type 0 (VT100), firmware version 0, hardware 0.
    def secondaryDa(): Unit = output.put(t"\e[>0;0;0c")
    def dectcem(visible: Boolean): Unit = state2 = state2.copy(hideCursor = !visible)
    def scp(): Unit = state2 = state2.copy(savedCursor = cursor())
    def rcp(): Unit = cursor() = state2.savedCursor
    def detectFocus(value: Boolean): Unit = state2 = state2.copy(focusDetectionMode = value)
    def focus(value: Boolean): Unit = state2 = state2.copy(focus = value)
    def bcp(value: Boolean): Unit = state2 = state2.copy(bracketedPasteMode = value)

    def writeChar(char: Char): Unit =
      if pendingWrap then
        cursor.x = Prim
        ind()
      set(cursor.x, cursor.y, char)
      lastChar = char
      if cursor.x.n0 == buffer2.width - 1
      then pendingWrap = true
      else
        cursor.x = cursor.x + 1

    def rep(n: Int): Unit =
      val count = if n == 0 then 1 else n
      var i = 0
      while i < count do
        writeChar(lastChar)
        i += 1

    def ht(): Unit =
      val nextStop = ((cursor.x.n0/8 + 1)*8).z
      cursor.x = if nextStop >= buffer2.width.z then (buffer2.width - 1).z else nextStop

    def ich(n: Int): Unit =
      val row = cursor.y.n0
      val col = cursor.x.n0
      val width = buffer2.width
      val shift = n.min(width - col)
      var i = width - 1
      while i >= col + shift do
        buffer2.set(i.z, row.z, buffer2.char((i - shift).z, row.z),
            buffer2.style((i - shift).z, row.z), buffer2.link((i - shift).z, row.z))
        i -= 1
      var j = col
      while j < col + shift do
        buffer2.set(j.z, row.z, ' ', style, link)
        j += 1

    def dch(n: Int): Unit =
      val row = cursor.y.n0
      val col = cursor.x.n0
      val width = buffer2.width
      val shift = n.min(width - col)
      var i = col
      while i < width - shift do
        buffer2.set(i.z, row.z, buffer2.char((i + shift).z, row.z),
            buffer2.style((i + shift).z, row.z), buffer2.link((i + shift).z, row.z))
        i += 1
      var j = width - shift
      while j < width do
        buffer2.set(j.z, row.z, ' ', style, link)
        j += 1

    def ech(n: Int): Unit =
      val row = cursor.y.n0
      val col = cursor.x.n0
      val end = (col + n).min(buffer2.width)
      var i = col
      while i < end do
        buffer2.set(i.z, row.z, ' ', style, link)
        i += 1

    def il(n: Int): Unit =
      val top = cursor.y.n0
      // IL only operates if cursor is inside the scroll region. The bottom
      // limit is the scroll region's bottom (not the screen bottom).
      if top < scrollTop.n0 || top > scrollBottom.n0 then ()
      else
        val width = buffer2.width
        val bottom = scrollBottom.n0
        val shift = n.min(bottom - top + 1)
        var r = bottom
        while r >= top + shift do
          var c = 0
          while c < width do
            buffer2.set(c.z, r.z, buffer2.char(c.z, (r - shift).z),
                buffer2.style(c.z, (r - shift).z), buffer2.link(c.z, (r - shift).z))
            c += 1
          r -= 1
        var rr = top
        while rr < top + shift do
          var c = 0
          while c < width do
            buffer2.set(c.z, rr.z, ' ', style, link)
            c += 1
          rr += 1

    def dl(n: Int): Unit =
      val top = cursor.y.n0
      if top < scrollTop.n0 || top > scrollBottom.n0 then ()
      else
        val width = buffer2.width
        val bottom = scrollBottom.n0
        val shift = n.min(bottom - top + 1)
        var r = top
        while r <= bottom - shift do
          var c = 0
          while c < width do
            buffer2.set(c.z, r.z, buffer2.char(c.z, (r + shift).z),
                buffer2.style(c.z, (r + shift).z), buffer2.link(c.z, (r + shift).z))
            c += 1
          r += 1
        var rr = bottom - shift + 1
        while rr <= bottom do
          var c = 0
          while c < width do
            buffer2.set(c.z, rr.z, ' ', style, link)
            c += 1
          rr += 1

    def decsc(): Unit =
      state2 = state2.copy(savedCursor = cursor(), savedStyle = style, savedLink = link)

    def decrc(): Unit =
      cursor() = state2.savedCursor
      style = state2.savedStyle
      link = state2.savedLink

    def ris(): Unit =
      style = Style()
      link = t""
      scrollTop = Prim
      scrollBottom = (buffer2.height - 1).z
      for i <- 0 until buffer2.capacity do wipe(i.z)
      cursor() = Prim
      state2 = PtyState(scrollBottom = scrollBottom)

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
        case '@' => ich(parseInt(params, 1))
        case 'P' => dch(parseInt(params, 1))
        case 'L' => il(parseInt(params, 1))
        case 'M' => dl(parseInt(params, 1))
        case 'X' => ech(parseInt(params, 1))
        case 'b' => rep(parseInt(params, 1))

        case 'H' => val (r, c) = parsePair(params, 1); cup(r.u, c.u)
        case 'f' => val (r, c) = parsePair(params, 1); hvp(r.u, c.u)

        case 'r' =>
          if params.s.isEmpty then decstbm(Prim, (buffer2.height - 1).z)
          else
            val (top, bot) = parsePair(params, 1)
            decstbm(top.u, bot.u)

        case 'n' if parseInt(params, 0) == 6                     => dsr()
        case 's' if params.s.isEmpty || parseInt(params, 0) == 6 => scp()
        case 'u' if params.s.isEmpty                             => rcp()
        case 'I' if params.s.isEmpty                             => focus(true)
        case 'O' if params.s.isEmpty                             => focus(false)
        case 'c' if params.s.isEmpty || params.s == "0"          => primaryDa()
        case 'c' if params.s.startsWith(">")                     => secondaryDa()

        case _ => raise(PtyEscapeError(BadCsiCommand(params, char)))

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
        ind()
        proceed(Normal)

      inline def ff(): Pty =
        ind()
        proceed(Normal)

      inline def cr(): Pty =
        cursor.x = Prim
        proceed(Normal)

      inline def put(char: Char): Pty =
        writeChar(char)
        proceed(Normal)

      if index >= input.length
      then Pty(buffer2,
          state2.copy(cursor = cursor(), style = style, link = link, scrollTop = scrollTop,
              scrollBottom = scrollBottom, pendingWrap = pendingWrap),
          output = output)
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
              case '\u0009' => ht(); proceed(Normal)
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
              case '['                   => recur(index + 1, Csi)
              case ']'                   => recur(index + 1, Osc)
              case 'P' | 'X' | '^' | '_' => recur(index + 1, EatString)
              case '7'                   => decsc(); proceed(Normal)
              case '8'                   => decrc(); proceed(Normal)
              case 'c'                   => ris(); proceed(Normal)
              case 'D'                   => ind(); proceed(Normal)
              case 'E'                   => nel(); proceed(Normal)
              case 'M'                   => ri(); proceed(Normal)
              case 'N' | 'O'             => proceed(Normal) // SS2 / SS3 — charset not supported
              case '\\'                  => proceed(Normal) // bare ST is ignored

              case char =>
                raise(PtyEscapeError(BadFeEscape(char)))
                proceed(Normal)

          case EatString =>
            current match
              case '\u0007'                          => proceed(Normal) // BEL ends the string
              case '\u001b'                          => proceed(EatString2)
              case _                                 => proceed(EatString)

          case EatString2 =>
            current match
              case '\\' => proceed(Normal)
              case _    => proceed(EatString)

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
