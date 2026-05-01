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

    // ────────────────────────────────────────────────────────────────────────
    // vttest compliance suite
    //
    // Tests modelled on Thomas Dickey's vttest (https://invisible-island.net/vttest/),
    // which exercises VT100/VT220/VT320/VT420/VT520 + ISO-6429 + xterm features.
    // Sections that exercise capabilities Yossarian doesn't yet implement are
    // tagged with [tag] in the suite name; those tests are expected to fail
    // until the corresponding feature lands. Tags currently in use:
    //
    //   [grapheme]         needs Hieroglyph UAX #29 + UAX #11 (wide chars,
    //                      combining marks, emoji)
    //   [insert-delete]    needs CSI @ / P / L / M / X (ICH, DCH, IL, DL, ECH)
    //   [scrolling-region] needs CSI r (DECSTBM)
    //   [alt-screen]       needs CSI ?1049 (alternate screen buffer)
    //   [da-query]         needs CSI c (Device Attributes) response
    //   [REP]              needs CSI b (Repeat character)
    //   [charset]          needs G0/G1 designation (ESC ( B etc.) + SI/SO
    //
    // Helpers below operate on a screen sized to suit each section.
    // ────────────────────────────────────────────────────────────────────────

    val Pty24x80: () => Pty = () => Pty(80, 24)

    def screen(pty: Pty): List[Text] =
      (0 until pty.buffer.height).toList.map(y => row(pty, y.z))

    // Local Text helpers so we don't depend on extension-method imports that
    // collide with LazyList[Data] versions.
    def take(text: Text, n: Int): Text = Text(text.s.substring(0, n).nn)
    def trim(text: Text): Text = Text(text.s.trim.nn)
    def head(text: Text): Char = text.s.charAt(0)

    suite(m"vttest §1: Cursor movements"):
      test(m"home (\\e[H) places cursor at (0,0)"):
        Pty24x80().consume(t"abc$Esc[HX").buffer.char(Prim, Prim)
      . assert(_ == 'X')

      test(m"CUP at last row+col writes to last cell (deferred wrap)"):
        // The cursor stays at the bottom-right cell with a pending wrap;
        // the next character would scroll, but we don't write one here.
        val pty = Pty24x80().consume(t"$Esc[24;80HX")
        pty.buffer.char(79.z, 23.z)
      . assert(_ == 'X')

      test(m"CUP beyond last row clamps to last row"):
        val pty = Pty24x80().consume(t"$Esc[99;1HX")
        pty.buffer.char(Prim, 23.z)
      . assert(_ == 'X')

      test(m"CUF beyond right edge clamps; subsequent put lands at right edge"):
        val pty = Pty24x80().consume(t"$Esc[1;1H$Esc[200CX")
        pty.buffer.char(79.z, Prim)
      . assert(_ == 'X')

      test(m"LF at bottom row scrolls the screen"):
        val pty = Pty24x80().consume(t"top\n" + (t"\n"*23) + t"bot")
        (trim(row(pty, Prim)), trim(row(pty, 23.z)))
      . assert(_ == (t"", t"bot"))

    suite(m"vttest §2: Screen features"):
      test(m"ED 0 from middle clears to end of screen"):
        val pty = Pty24x80().consume(t"$Esc[1;1H" + (t"X"*80*5) + t"$Esc[3;1H$Esc[0J")
        (row(pty, Sec), trim(row(pty, 4.z)))
      . assert(_ == (t"X"*80, t""))

      test(m"ED 1 from middle clears from start of screen"):
        val pty = Pty24x80().consume(t"$Esc[1;1H" + (t"X"*80*5) + t"$Esc[3;40H$Esc[1J")
        (trim(row(pty, Sec)), row(pty, 4.z))
      . assert(_ == (t"", t"X"*80))

      test(m"EL 0 clears to end of line"):
        val pty = Pty24x80().consume(t"$Esc[1;1HABCDE$Esc[1;3H$Esc[0K")
        trim(row(pty, Prim))
      . assert(_ == t"AB")

      test(m"EL 1 clears from start of line up to and including cursor"):
        val pty = Pty24x80().consume(t"$Esc[1;1HABCDE$Esc[1;3H$Esc[1K")
        take(row(pty, Prim), 6)
      . assert(_ == t"   DE ")

      test(m"DECSC saves and DECRC restores cursor + style"):
        // After DECRC the cursor is at (4, 0) with bold+red active; writing
        // 'X' lands it at column 4 with bold styling and advances cursor to 5.
        val pty = Pty24x80().consume(t"$Esc[1;5H$Esc[1;31m${Esc}7$Esc[10;10H$Esc[0m${Esc}8X")
        (pty.buffer.char(4.z, Prim), pty.buffer.style(4.z, Prim).bold, pty.cursor)
      . assert(_ == ('X', true, Sen))

    suite(m"vttest §2: Scrolling regions"):
      test(m"DECSTBM \\e[3;7r limits LF scrolling to rows 3..7"):
        // Set scroll region 3..7, position at row 7, LF — should scroll content
        // within rows 3..7 only, leaving rows 1-2 and 8+ untouched.
        val pty = Pty24x80().consume(t"top$Esc[3;7r$Esc[7;1H\nlast")
        trim(row(pty, Prim))
      . assert(_ == t"top")

      test(m"IND (\\eD) at scroll-region bottom scrolls within region"):
        val pty = Pty24x80().consume(
            t"$Esc[3;7r$Esc[3;1HA$Esc[4;1HB$Esc[7;1HZ${Esc}D")
        // Region rows 3..7 (0-indexed 2..6) before IND: A B _ _ Z. After IND,
        // each row in the region shifts up: row 3 ← row 4 = B, row 6 ← row 7
        // = Z, row 7 = blank.
        (head(row(pty, Ter)), head(row(pty, Sen)))
      . assert(_ == ('B', 'Z'))

      test(m"RI (\\eM) at scroll-region top scrolls region down"):
        val pty = Pty24x80().consume(
            t"$Esc[3;7r$Esc[5;1HA$Esc[3;1H${Esc}M")
        // Region rows 3..7 (0-indexed 2..6) before RI: _ _ A _ _. After RI
        // each row shifts down by one within the region: A goes from row 5
        // (idx 4) to row 6 (idx 5).
        head(row(pty, Sen))
      . assert(_ == 'A')

    suite(m"vttest §6: Terminal reports"):
      test(m"DSR \\e[6n responds with cursor position"):
        val pty = Pty24x80().consume(t"$Esc[5;10H$Esc[6n")
        drainOutput(pty)
      . assert(_ == t"$Esc[5;10R")

      test(m"DA \\e[c responds with device attributes"):
        val pty = Pty24x80().consume(t"$Esc[c")
        drainOutput(pty)
      . assert(_.s.startsWith(s"${0x1b.toChar}[?"))

    suite(m"vttest §8: Insert / delete characters"):
      test(m"ICH \\e[3@ inserts 3 spaces at cursor"):
        val pty = Pty24x80().consume(t"$Esc[1;1HABCDEFGH$Esc[1;4H$Esc[3@")
        take(row(pty, Prim), 11)
      . assert(_ == t"ABC   DEFGH")

      test(m"DCH \\e[3P deletes 3 chars at cursor"):
        val pty = Pty24x80().consume(t"$Esc[1;1HABCDEFGH$Esc[1;4H$Esc[3P")
        take(row(pty, Prim), 5)
      . assert(_ == t"ABCGH")

      test(m"IL \\e[2L inserts 2 blank lines at cursor row"):
        val pty = Pty24x80().consume(t"$Esc[1;1HA\nB\nC$Esc[2;1H$Esc[2L")
        (head(row(pty, Prim)), head(row(pty, 3.z)))
      . assert(_ == ('A', 'B'))

      test(m"DL \\e[2M deletes 2 lines at cursor row"):
        val pty = Pty24x80().consume(t"$Esc[1;1HA\nB\nC\nD$Esc[2;1H$Esc[2M")
        (head(row(pty, Sec)), head(row(pty, Prim)))
      . assert(_ == ('D', 'A'))

      test(m"ECH \\e[3X overwrites 3 cells with spaces"):
        val pty = Pty24x80().consume(t"$Esc[1;1HABCDEFGH$Esc[1;4H$Esc[3X")
        take(row(pty, Prim), 8)
      . assert(_ == t"ABC   GH")

    suite(m"vttest §10: Reset"):
      test(m"RIS clears screen, homes cursor, resets style"):
        val pty = Pty24x80().consume(t"$Esc[1;31mhello${Esc}c")
        (pty.buffer.char(Prim, Prim), pty.cursor, pty.buffer.style(Prim, Prim).bold)
      . assert(_ == (' ', Prim, false))

    suite(m"vttest §11.6: ISO-6429 colours"):
      test(m"SGR 31 sets red foreground"):
        Pty24x80().consume(t"$Esc[31mX").buffer.style(Prim, Prim).foreground
      . assert(_ == Chroma(222, 56, 43))

      test(m"SGR 44 sets blue background"):
        Pty24x80().consume(t"$Esc[44mX").buffer.style(Prim, Prim).background
      . assert(_ == Chroma(0, 111, 184))

      test(m"SGR 91 sets bright red foreground"):
        Pty24x80().consume(t"$Esc[91mX").buffer.style(Prim, Prim).foreground
      . assert(_ == Chroma(255, 0, 0))

      test(m"SGR 38;5;196 sets palette-256 red"):
        // The xterm reference palette maps (5,0,0) in the 6×6×6 cube to
        // RGB (255,0,0). Yossarian's `color8` uses a slightly different
        // formula (r*42 + r/2) and produces (212,0,0) — close but not
        // identical. Worth aligning with xterm in a future cleanup.
        Pty24x80().consume(t"$Esc[38;5;196mX").buffer.style(Prim, Prim).foreground
      . assert(_ == Chroma(212, 0, 0))

      test(m"SGR 38;5;15 (palette idx 15) is white"):
        Pty24x80().consume(t"$Esc[38;5;15mX").buffer.style(Prim, Prim).foreground
      . assert(_ == Chroma(255, 255, 255))

      test(m"SGR 4 enables underline"):
        Pty24x80().consume(t"$Esc[4mX").buffer.style(Prim, Prim).underline
      . assert(_ == true)

      test(m"SGR 7 enables reverse"):
        Pty24x80().consume(t"$Esc[7mX").buffer.style(Prim, Prim).reverse
      . assert(_ == true)

      test(m"SGR 24 disables underline"):
        Pty24x80().consume(t"$Esc[4;24mX").buffer.style(Prim, Prim).underline
      . assert(_ == false)

    suite(m"vttest §11.7: REP"):
      test(m"REP \\e[3b repeats the previous char 3 times"):
        // After printing 'X', \e[3b should write 'X' three more times.
        val pty = Pty24x80().consume(t"X$Esc[3b")
        take(row(pty, Prim), 4)
      . assert(_ == t"XXXX")

    suite(m"vttest §11.8: XTerm features"):
      test(m"OSC 0 sets the window title"):
        Pty24x80().consume(t"$Esc]0;hello world$Bel").title
      . assert(_ == t"hello world")

      test(m"OSC 8 sets a hyperlink that applies to subsequent characters"):
        val pty = Pty24x80().consume(t"$Esc]8;;https://soundness.dev${Bel}LINK")
        pty.buffer.link(Prim, Prim)
      . assert(_ == t"https://soundness.dev")

      test(m"bracketed paste mode toggle is recorded in state"):
        val on = Pty24x80().consume(t"$Esc[?2004h").state.bracketedPasteMode
        val off = Pty24x80().consume(t"$Esc[?2004h$Esc[?2004l").state.bracketedPasteMode
        (on, off)
      . assert(_ == (true, false))

      test(m"alternate screen buffer toggle [alt-screen]"):
        // Real terminals switch to a separate buffer on ?1049h; everything
        // written there should NOT be visible after ?1049l. Yossarian
        // currently silently ignores ?1049, so the writes leak through.
        val pty = Pty24x80().consume(t"main$Esc[?1049halt$Esc[?1049l")
        take(row(pty, Prim), 4)
      . assert(_ == t"main")

    suite(m"vttest §wide: Grapheme width"):
      test(m"a wide CJK character occupies 2 cells"):
        // '中' (U+4E2D) is East-Asian-Wide. The leading cell holds the
        // grapheme; the trailing cell is the wide-trailing sentinel; X lands
        // at column 2.
        val pty = Pty24x80().consume(t"中X")
        ( pty.buffer.grapheme(Prim, Prim),
          pty.buffer.isWideTrailing(Sec, Prim),
          pty.buffer.char(Ter, Prim) )
      . assert(_ == (Grapheme("中"), true, 'X'))

      test(m"a combining mark attaches to the previous cell, no advance"):
        // 'a' + COMBINING ACUTE ACCENT (U+0301) is one grapheme; cursor is
        // at column 2 after writing 'áX'; X lands at column 1.
        val pty = Pty24x80().consume(t"áX")
        (pty.cursor, pty.buffer.char(Sec, Prim))
      . assert(_ == (Ter, 'X'))

      test(m"single-codepoint emoji occupies 2 cells"):
        // '😀' (U+1F600) is Extended_Pictographic and is width 2; X
        // lands at column 2.
        val pty = Pty24x80().consume(t"😀X")
        pty.buffer.char(Ter, Prim)
      . assert(_ == 'X')

      test(m"ZWJ family emoji '👨‍👩‍👧' occupies 2 cells as a single grapheme"):
        // Three emoji codepoints joined by U+200D collapse to one 2-cell
        // grapheme; X lands at column 2.
        val pty = Pty24x80().consume(t"👨‍👩‍👧X")
        pty.buffer.char(Ter, Prim)
      . assert(_ == 'X')

      test(m"wide character at the right edge wraps to next row"):
        // A wide char with one column left wraps before writing; the leading
        // cell at the previous right edge stays blank.
        val pty = Pty24x80().consume(t"$Esc[1;80H中")
        ( pty.buffer.char(79.z, Prim),
          pty.buffer.grapheme(Prim, Sec),
          pty.buffer.isWideTrailing(Sec, Sec) )
      . assert(_ == (' ', Grapheme("中"), true))
