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
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package yossarian

import soundness.*

import strategies.throwUnsafely

object Tests extends Suite(m"Yossarian Tests"):
  def run(): Unit =
    // ESC and BEL bytes assembled at run time so the source file stays free of stray
    // control characters that would otherwise terminate OSC sequences silently.
    val Esc: Text = Text(0x1b.toChar.toString)
    val Bel: Text = Text(0x07.toChar.toString)

    def fresh: Pty = Pty(10, 4)

    def cell(pty: Pty, x: Ordinal, y: Ordinal): Char = pty.buffer.char(x, y)

    def row(pty: Pty, y: Ordinal): Text =
      Text(String((0 until pty.buffer.width).map(i => pty.buffer.char(i.z, y)).toArray))

    def drainOutput(pty: Pty): Text =
      pty.output.stop()
      pty.stream.to(List).map(_.s).mkString.tt

    suite(m"Plain text"):
      test(m"writing text places characters in cells"):
        row(fresh.consume(t"hi"), Prim)
      . assert(_ == t"hi        ")

      test(m"cursor advances after writing"):
        fresh.consume(t"hi").state.cursor
      . assert(_ == Ter)

      test(m"writing past row width wraps to next row"):
        val pty = fresh.consume(t"0123456789X")
        (cell(pty, Prim, Sec), cell(pty, 9.z, Prim))
      . assert(_ == ('X', '9'))

    suite(m"Carriage return and line feed"):
      test(m"CR returns to column 0 of same row"):
        val pty = fresh.consume(t"abc\rX")
        row(pty, Prim)
      . assert(_ == t"Xbc       ")

      test(m"LF advances to next row at column 0"):
        val pty = fresh.consume(t"ab\nc")
        (row(pty, Prim), row(pty, Sec))
      . assert(_ == (t"ab        ", t"c         "))

    suite(m"Backspace"):
      test(m"BS moves cursor left without erasing"):
        val pty = fresh.consume(t"abc\b")
        (row(pty, Prim), pty.state.cursor)
      . assert(_ == (t"abc       ", Ter))

      test(m"BS+SP+BS erases the previous character"):
        val pty = fresh.consume(t"abc\b \b")
        (row(pty, Prim), pty.state.cursor)
      . assert(_ == (t"ab        ", Ter))

      test(m"BS at column 0 stays at column 0"):
        val pty = fresh.consume(t"\b")
        pty.state.cursor
      . assert(_ == Prim)

    suite(m"Cursor positioning"):
      test(m"CUP places cursor at row 3 col 5 (1-indexed)"):
        val pty = fresh.consume(t"$Esc[3;5HX")
        cell(pty, Quin, Ter)
      . assert(_ == 'X')

      test(m"CUP with no parameters homes the cursor"):
        val pty = fresh.consume(t"abc${Esc}[HZ")
        cell(pty, Prim, Prim)
      . assert(_ == 'Z')

      test(m"CUU moves cursor up two rows"):
        val pty = fresh.consume(t"$Esc[4;5H$Esc[2AX")
        cell(pty, Quin, Sec)
      . assert(_ == 'X')

      test(m"CUD moves cursor down one row"):
        val pty = fresh.consume(t"$Esc[1;1H$Esc[1BX")
        cell(pty, Prim, Sec)
      . assert(_ == 'X')

      test(m"CUF moves cursor right within row"):
        val pty = fresh.consume(t"$Esc[1;1H$Esc[3CX")
        cell(pty, Quat, Prim)
      . assert(_ == 'X')

      test(m"CUF clamps at end of row, not into next row"):
        val pty = fresh.consume(t"$Esc[1;1H$Esc[20CX")
        cell(pty, 9.z, Prim)
      . assert(_ == 'X')

      test(m"CUB moves cursor left within row"):
        val pty = fresh.consume(t"$Esc[1;5H$Esc[2DX")
        cell(pty, Ter, Prim)
      . assert(_ == 'X')

    suite(m"Erase"):
      test(m"ED 2 clears the entire screen"):
        val pty = fresh.consume(t"abc\ndef$Esc[2J")
        (row(pty, Prim), row(pty, Sec))
      . assert(_ == (t"          ", t"          "))

      test(m"EL 2 clears the current line"):
        val pty = fresh.consume(t"abcdef$Esc[1;1H$Esc[2K")
        row(pty, Prim)
      . assert(_ == t"          ")

    suite(m"SGR"):
      test(m"SGR 1 enables bold"):
        val pty = fresh.consume(t"$Esc[1mX")
        pty.buffer.style(Prim, Prim).bold
      . assert(_ == true)

      test(m"SGR 0 resets bold"):
        val pty = fresh.consume(t"$Esc[1mX$Esc[0mY")
        (pty.buffer.style(Prim, Prim).bold, pty.buffer.style(Sec, Prim).bold)
      . assert(_ == (true, false))

      test(m"SGR 38;2;R;G;B sets a 24-bit foreground"):
        val pty = fresh.consume(t"$Esc[38;2;100;150;200mX")
        pty.buffer.style(Prim, Prim).foreground
      . assert(_ == Chroma(100, 150, 200))

      test(m"SGR 38;5;N;1m applies bold after extended colour"):
        val pty = fresh.consume(t"$Esc[38;5;201;1mX")
        pty.buffer.style(Prim, Prim).bold
      . assert(_ == true)

    suite(m"OSC"):
      test(m"OSC 0 sets the window title (BEL terminator)"):
        val pty = fresh.consume(t"$Esc]0;My Title$Bel")
        pty.state.title
      . assert(_ == t"My Title")

      test(m"OSC 0 sets the window title (ST terminator)"):
        val pty = fresh.consume(t"$Esc]0;My Title$Esc\\")
        pty.state.title
      . assert(_ == t"My Title")

      test(m"OSC 8 with empty params sets a hyperlink"):
        val pty = fresh.consume(t"$Esc]8;;https://example.com${Bel}X")
        pty.buffer.link(Prim, Prim)
      . assert(_ == t"https://example.com")

      test(m"OSC 8 with params field sets a hyperlink"):
        val pty = fresh.consume(t"$Esc]8;id=123;https://example.com${Bel}X")
        pty.buffer.link(Prim, Prim)
      . assert(_ == t"https://example.com")

    suite(m"DEC private modes"):
      test(m"unknown private mode is silently ignored"):
        fresh.consume(t"$Esc[?1049hX").buffer.char(Prim, Prim)
      . assert(_ == 'X')

      test(m"DECTCEM ?25 l sets hideCursor in state"):
        fresh.consume(t"$Esc[?25l").state.hideCursor
      . assert(_ == true)

      test(m"DECTCEM ?25 h clears hideCursor in state"):
        fresh.consume(t"$Esc[?25l$Esc[?25h").state.hideCursor
      . assert(_ == false)

    suite(m"Output channel"):
      test(m"DSR responds with row;col R"):
        drainOutput(fresh.consume(t"$Esc[3;5H$Esc[6n"))
      . assert(_ == t"$Esc[3;5R")

    suite(m"Palette"):
      test(m"bright black (palette 8) is distinct from black"):
        val black = fresh.consume(t"$Esc[30mA").buffer.style(Prim, Prim).foreground
        val brightBlack = fresh.consume(t"$Esc[90mA").buffer.style(Prim, Prim).foreground
        black != brightBlack
      . assert(_ == true)

    suite(m"Tab"):
      test(m"tab from column 0 advances to column 8"):
        fresh.consume(t"\tX").state.cursor
      . assert(_ == 9.z)

      test(m"tab from column 7 still advances to column 8"):
        Pty(20, 4).consume(t"abcdefg\tX").state.cursor
      . assert(_ == 9.z)

      test(m"tab past last tab stop clamps to last column"):
        fresh.consume(t"abcdefghi\t").state.cursor
      . assert(_ == 9.z)

    suite(m"DECSC / DECRC"):
      test(m"ESC 8 restores cursor saved by ESC 7"):
        fresh.consume(t"$Esc[3;5H${Esc}7$Esc[1;1H${Esc}8X").state.cursor
      . assert(_ == (2*10 + 5).z)

      test(m"ESC 8 restores style saved by ESC 7"):
        val pty = fresh.consume(t"$Esc[1m${Esc}7$Esc[0m${Esc}8X")
        pty.buffer.style(Prim, Prim).bold
      . assert(_ == true)

    suite(m"RIS"):
      test(m"ESC c clears screen, homes cursor, and resets style"):
        val pty = fresh.consume(t"$Esc[1;31mhello${Esc}c")
        (row(pty, Prim), pty.state.cursor, pty.buffer.style(Prim, Prim).bold)
      . assert(_ == (t"          ", Prim, false))

    suite(m"String-discard sequences"):
      test(m"DCS string is silently absorbed up to ST"):
        fresh.consume(t"${Esc}Psome data$Esc\\X").buffer.char(Prim, Prim)
      . assert(_ == 'X')

      test(m"APC string is silently absorbed up to BEL"):
        fresh.consume(t"${Esc}_app data${Bel}X").buffer.char(Prim, Prim)
      . assert(_ == 'X')

    suite(m"Top-level accessors"):
      test(m"pty.title forwards to state.title"):
        fresh.consume(t"$Esc]0;My Window$Bel").title
      . assert(_ == t"My Window")

      test(m"pty.cursor forwards to state.cursor"):
        fresh.consume(t"hi").cursor
      . assert(_ == Ter)

      test(m"pty.cursorVisible reflects DECTCEM"):
        (fresh.cursorVisible, fresh.consume(t"$Esc[?25l").cursorVisible)
      . assert(_ == (true, false))
