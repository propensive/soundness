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

    def cell(pty: Pty, x: Int, y: Int): Char = pty.buffer.char(x, y)

    def row(pty: Pty, y: Int): Text =
      Text(String((0 until pty.buffer.width).map(pty.buffer.char(_, y)).toArray))

    def drainOutput(pty: Pty): Text =
      pty.output.stop()
      pty.stream.to(List).map(_.s).mkString.tt

    suite(m"Plain text"):
      test(m"writing text places characters in cells"):
        row(fresh.consume(t"hi"), 0)
      . assert(_ == t"hi        ")

      test(m"cursor advances after writing"):
        fresh.consume(t"hi").state0.cursor
      . assert(_ == 2)

      test(m"writing past row width wraps to next row"):
        val pty = fresh.consume(t"0123456789X")
        (cell(pty, 0, 1), cell(pty, 9, 0))
      . assert(_ == ('X', '9'))

    suite(m"Carriage return and line feed"):
      test(m"CR returns to column 0 of same row"):
        val pty = fresh.consume(t"abc\rX")
        row(pty, 0)
      . assert(_ == t"Xbc       ")

      test(m"LF advances to next row at column 0"):
        val pty = fresh.consume(t"ab\nc")
        (row(pty, 0), row(pty, 1))
      . assert(_ == (t"ab        ", t"c         "))

    suite(m"Backspace"):
      test(m"BS moves cursor left without erasing"):
        val pty = fresh.consume(t"abc\b")
        (row(pty, 0), pty.state0.cursor)
      . assert(_ == (t"abc       ", 2))

      test(m"BS+SP+BS erases the previous character"):
        val pty = fresh.consume(t"abc\b \b")
        (row(pty, 0), pty.state0.cursor)
      . assert(_ == (t"ab        ", 2))

      test(m"BS at column 0 stays at column 0"):
        val pty = fresh.consume(t"\b")
        pty.state0.cursor
      . assert(_ == 0)

    suite(m"Cursor positioning"):
      test(m"CUP places cursor at row 3 col 5 (1-indexed)"):
        val pty = fresh.consume(t"$Esc[3;5HX")
        cell(pty, 4, 2)
      . assert(_ == 'X')

      test(m"CUP with no parameters homes the cursor"):
        val pty = fresh.consume(t"abc${Esc}[HZ")
        cell(pty, 0, 0)
      . assert(_ == 'Z')

      test(m"CUU moves cursor up two rows"):
        val pty = fresh.consume(t"$Esc[4;5H$Esc[2AX")
        cell(pty, 4, 1)
      . assert(_ == 'X')

      test(m"CUD moves cursor down one row"):
        val pty = fresh.consume(t"$Esc[1;1H$Esc[1BX")
        cell(pty, 0, 1)
      . assert(_ == 'X')

      test(m"CUF moves cursor right within row"):
        val pty = fresh.consume(t"$Esc[1;1H$Esc[3CX")
        cell(pty, 3, 0)
      . assert(_ == 'X')

      test(m"CUF clamps at end of row, not into next row"):
        val pty = fresh.consume(t"$Esc[1;1H$Esc[20CX")
        cell(pty, 9, 0)
      . assert(_ == 'X')

      test(m"CUB moves cursor left within row"):
        val pty = fresh.consume(t"$Esc[1;5H$Esc[2DX")
        cell(pty, 2, 0)
      . assert(_ == 'X')

    suite(m"Erase"):
      test(m"ED 2 clears the entire screen"):
        val pty = fresh.consume(t"abc\ndef$Esc[2J")
        (row(pty, 0), row(pty, 1))
      . assert(_ == (t"          ", t"          "))

      test(m"EL 2 clears the current line"):
        val pty = fresh.consume(t"abcdef$Esc[1;1H$Esc[2K")
        row(pty, 0)
      . assert(_ == t"          ")

    suite(m"SGR"):
      test(m"SGR 1 enables bold"):
        val pty = fresh.consume(t"$Esc[1mX")
        pty.buffer.style(0, 0).bold
      . assert(_ == true)

      test(m"SGR 0 resets bold"):
        val pty = fresh.consume(t"$Esc[1mX$Esc[0mY")
        (pty.buffer.style(0, 0).bold, pty.buffer.style(1, 0).bold)
      . assert(_ == (true, false))

      test(m"SGR 38;2;R;G;B sets a 24-bit foreground"):
        val pty = fresh.consume(t"$Esc[38;2;100;150;200mX")
        pty.buffer.style(0, 0).foreground
      . assert(_ == Chroma(100, 150, 200))

      test(m"SGR 38;5;N;1m applies bold after extended colour"):
        val pty = fresh.consume(t"$Esc[38;5;201;1mX")
        pty.buffer.style(0, 0).bold
      . assert(_ == true)

    suite(m"OSC"):
      test(m"OSC 0 sets the window title (BEL terminator)"):
        val pty = fresh.consume(t"$Esc]0;My Title$Bel")
        pty.state0.title
      . assert(_ == t"My Title")

      test(m"OSC 0 sets the window title (ST terminator)"):
        val pty = fresh.consume(t"$Esc]0;My Title$Esc\\")
        pty.state0.title
      . assert(_ == t"My Title")

      test(m"OSC 8 with empty params sets a hyperlink"):
        val pty = fresh.consume(t"$Esc]8;;https://example.com${Bel}X")
        pty.buffer.link(0, 0)
      . assert(_ == t"https://example.com")

      test(m"OSC 8 with params field sets a hyperlink"):
        val pty = fresh.consume(t"$Esc]8;id=123;https://example.com${Bel}X")
        pty.buffer.link(0, 0)
      . assert(_ == t"https://example.com")

    suite(m"DEC private modes"):
      test(m"unknown private mode is silently ignored"):
        fresh.consume(t"$Esc[?1049hX").buffer.char(0, 0)
      . assert(_ == 'X')

      test(m"DECTCEM ?25 l sets hideCursor in state"):
        fresh.consume(t"$Esc[?25l").state0.hideCursor
      . assert(_ == true)

      test(m"DECTCEM ?25 h clears hideCursor in state"):
        fresh.consume(t"$Esc[?25l$Esc[?25h").state0.hideCursor
      . assert(_ == false)

    suite(m"Output channel"):
      test(m"DSR responds with row;col R"):
        drainOutput(fresh.consume(t"$Esc[3;5H$Esc[6n"))
      . assert(_ == t"$Esc[3;5R")

    suite(m"Palette"):
      test(m"bright black (palette 8) is distinct from black"):
        val black = fresh.consume(t"$Esc[30mA").buffer.style(0, 0).foreground
        val brightBlack = fresh.consume(t"$Esc[90mA").buffer.style(0, 0).foreground
        black != brightBlack
      . assert(_ == true)
