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
package ultimatum

import scala.collection.immutable.IndexedSeq

import java.io as ji

import soundness.*

import proscenium.compat.*

object Tests extends Suite(m"Ultimatum Tests"):
  def run(): Unit =
    suite(m"TerminalCanvas"):
      // Capture everything a surface writes into an in-memory buffer.
      def captured(block: Stdio ?=> Unit): Text =
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)
        block
        String(bytes.toByteArray.nn, "UTF-8").tt

      test(m"move emits an absolute CSI cursor-position sequence"):
        captured: stdio ?=>
          TerminalCanvas(80, 24).move(10.z, 5.z)
      . assert(_ == t"\e[6;11H")

      test(m"move then put places text at the position"):
        captured: stdio ?=>
          val surface = TerminalCanvas(80, 24)
          surface.move(10.z, 5.z)
          surface.put(t"X")
      . assert(_ == t"\e[6;11HX")

      test(m"clear erases the whole display"):
        captured(TerminalCanvas(80, 24).clear())
      . assert(_ == t"\e[2J")

      test(m"hiding the cursor emits the DECTCEM reset"):
        captured(TerminalCanvas(80, 24).cursor(false))
      . assert(_ == t"\e[?25l")

    suite(m"FlowExtent"):
      // A standalone extent over a muted parent; mutation tests never flush, so
      // the parent surface is unused.
      def extent(width: Int, height: Int): FlowExtent =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        FlowExtent(TerminalCanvas(width, height), Rect(0, 0, width, height))

      test(m"text wraps at the rectangle's width"):
        val flow = extent(3, 2)
        flow.put(t"abcdef")
        flow.render
      . assert(_ == t"abc\ndef")

      test(m"a wide (CJK) grapheme occupies two cells"):
        val flow = extent(4, 1)
        flow.put(t"a中b")
        flow.render
      . assert(_ == t"a中b")

      test(m"a wide grapheme wraps when it would straddle the right edge"):
        val flow = extent(3, 2)
        flow.put(t"ab中")
        flow.render
      . assert(_ == t"ab \n中 ")

      test(m"a newline moves to the start of the next row"):
        val flow = extent(5, 3)
        flow.put(t"ab\ncd")
        flow.render
      . assert(_ == t"ab   \ncd   \n     ")

      test(m"the grid scrolls up when the last row overflows"):
        val flow = extent(3, 2)
        flow.put(t"abcdefghi")
        flow.render
      . assert(_ == t"def\nghi")

      test(m"clear blanks the whole grid"):
        val flow = extent(3, 2)
        flow.put(t"abcdef")
        flow.clear()
        flow.render
      . assert(_ == t"   \n   ")

      test(m"Out output through the extent (an Stdio) flows into the grid"):
        val flow = extent(5, 1)
        given Stdio = flow
        Out.print(t"hi")
        flow.render
      . assert(_ == t"hi   ")

      test(m"flush paints the grid onto the parent at the rect's offset"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)
        val flow = FlowExtent(TerminalCanvas(80, 24), Rect(2, 1, 3, 1))
        flow.put(t"xy")
        flow.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[2;3Hxy ")

    suite(m"Layout solver"):
      def cell(sizing: Sizing): Frame = Frame.Cell(sizing)
      def file(children: Frame*): Frame = Frame.Split(Sizing(), Axis.File, children.to(List))
      def rank(children: Frame*): Frame = Frame.Split(Sizing(), Axis.Rank, children.to(List))

      test(m"fractions divide the axis proportionally"):
        val frame = file(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 90, 10)).cells
      . assert(_ == List(Rect(0, 0, 20, 10), Rect(20, 0, 30, 10), Rect(50, 0, 40, 10)))

      test(m"largest-remainder rounding fills the axis exactly"):
        val frame = file(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 100, 1)).cells.map(_.width)
      . assert(_ == List(22, 33, 45))

      test(m"the rounded sizes always sum to the available space"):
        val frame = file(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 100, 1)).cells.map(_.width).fold(0)(_ + _)
      . assert(_ == 100)

      test(m"a child whose minimum exceeds its share is fixed at the minimum"):
        val frame = file(cell(Sizing(1.0, minWidth = 8)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(8, 2))

      test(m"a child whose maximum is below its share is capped at the maximum"):
        val frame = file(cell(Sizing(1.0, maxWidth = 3)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(3, 7))

      test(m"a container's minimum is forced up to the sum of its children's"):
        val frame = file(cell(Sizing(1.0, minWidth = 5)), cell(Sizing(1.0, minWidth = 5)))
        frame.measure(Axis.File)
      . assert(_ == Limits(10, Unset))

      test(m"file children fill the cross axis (full height)"):
        val frame = file(cell(Sizing(1.0)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 8, 4)).cells
      . assert(_ == List(Rect(0, 0, 4, 4), Rect(4, 0, 4, 4)))

      test(m"rank splits distribute height and fill width"):
        val frame = rank(cell(Sizing(1.0)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 8, 4)).cells
      . assert(_ == List(Rect(0, 0, 8, 2), Rect(0, 2, 8, 2)))

      test(m"nested ranks within files place rectangles correctly"):
        val frame = file(cell(Sizing(1.0)), rank(cell(Sizing(1.0)), cell(Sizing(1.0))))
        frame.arrange(Rect(0, 0, 10, 4)).cells
      . assert(_ == List(Rect(0, 0, 5, 4), Rect(5, 0, 5, 2), Rect(5, 2, 5, 2)))

    suite(m"layout / panel DSL"):
      test(m"side-by-side panels paint at their own offsets, no bleed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)

        paint(TerminalCanvas(4, 1), file(panel()(Out.print(t"AA")), panel()(Out.print(t"BB"))))

        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1;1HAA\e[1;3HBB")

      test(m"a panel's output wraps and scrolls within its own rectangle, no bleed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)

        // "HELLO" in a 2x1 panel wraps and scrolls until only "O" remains; the
        // sibling panel's "X" is unaffected, so neither bleeds past column 2.
        paint(TerminalCanvas(4, 1), file(panel()(Out.print(t"HELLO")), panel()(Out.print(t"X"))))

        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1;1HO \e[1;3HX ")

      test(m"a fixed-minimum panel squeezes its fractional sibling"):
        val frame = file(panel(minWidth = 8)(()), panel()(())).frame
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(8, 2))

    suite(m"Focus and reactive layout"):
      test(m"typing into an editor field updates its value"):
        val field = EditorField()
        field.handle(Keypress.CharKey('h'))
        field.handle(Keypress.CharKey('i'))
        field.value
      . assert(_ == t"hi")

      // The demo's compose box is multiline; the Up/Down arrows must move the
      // cursor between lines (in single-line mode they are inert).
      test(m"a multiline editor field moves its cursor up a line on Up"):
        val field = EditorField(LineEditor(t"ab\ncd", mode = LineEditor.Mode.Multiline(_ => false)))
        field.handle(Keypress.Up)
        field.handle(Keypress.CharKey('X'))
        field.value
      . assert(_ == t"abX\ncd")

      test(m"a multiline editor field moves its cursor down a line on Down"):
        val field = EditorField(LineEditor(t"ab\ncd", 0,
            mode = LineEditor.Mode.Multiline(_ => false)))
        field.handle(Keypress.Down)
        field.handle(Keypress.CharKey('X'))
        field.value
      . assert(_ == t"ab\nXcd")

      test(m"an editor field's intrinsic height grows when its text wraps"):
        EditorField(LineEditor(t"aaaaa")).measure(3)
      . assert(_ == (0, 2))

      test(m"a single-line editor field needs one row"):
        EditorField(LineEditor(t"hello")).measure(80)
      . assert(_ == (0, 1))

      test(m"a menu field moves its selection on Down"):
        val field = MenuField(SelectMenu(List(t"a", t"b", t"c"), t"a"))
        field.handle(Keypress.Down)
        field.value
      . assert(_ == t"b")

      test(m"a moved or resized cell is dirty"):
        val before = IndexedSeq(Rect(0, 0, 10, 1), Rect(0, 1, 10, 1))
        val after  = IndexedSeq(Rect(0, 0, 10, 2), Rect(0, 2, 10, 1))
        dirtyCells(before, after, Set())
      . assert(_ == Set(0, 1))

      test(m"an unchanged cell is not dirty"):
        dirtyCells(IndexedSeq(Rect(0, 0, 10, 1)), IndexedSeq(Rect(0, 0, 10, 1)), Set())
      . assert(_ == Set())

      test(m"a content-changed cell is dirty though its rectangle is unchanged"):
        val rects = IndexedSeq(Rect(0, 0, 10, 1), Rect(0, 1, 10, 1))
        dirtyCells(rects, rects, Set(1))
      . assert(_ == Set(1))

      // Drive the whole interactive loop into an in-memory grid: type into the
      // first editor, TAB to the second, type into it, then exit, and read back
      // the composed screen.
      test(m"TAB moves focus so typing lands in the right panel"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalCanvas(10, 4), Rect(0, 0, 10, 4))

        val events = List
         ( Keypress.CharKey('h'), Keypress.CharKey('i'),
           Keypress.Tab,
           Keypress.CharKey('y'), Keypress.CharKey('o'),
           Keypress.Escape )

        Form(root, Mode.Fullscreen, rank(editor(), editor())).run(events.iterator)
        root.render
      . assert(_ == t"hi        \n          \nyo        \n          ")

      // Typing 21 characters into the top editor wraps it onto three rows, raising
      // its panel's minimum height; the solver re-tiles and the bottom editor is
      // pushed from row 2 down to row 3.
      test(m"a growing editor re-tiles and pushes its sibling down"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalCanvas(10, 4), Rect(0, 0, 10, 4))
        val events = List.fill(21)(Keypress.CharKey('a')) ++ List(Keypress.Escape)
        Form(root, Mode.Fullscreen, rank(editor(), editor())).run(events.iterator)
        root.render
      . assert(_ == t"aaaaaaaaaa\naaaaaaaaaa\na         \n          ")

      // A terminal resize surfaces as a WindowSize event (the SIGWINCH handler in
      // profanity's Terminal queries the new size); the layout re-tiles to it and
      // the rows freed by shrinking are cleared. The custom iterator shrinks the
      // root just before yielding the event, mimicking the live size update.
      test(m"a WindowSize event re-tiles to the new terminal size"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = ResizableRoot(10, 4)

        val resize = new Iterator[TerminalEvent]:
          private var pending = true
          def hasNext = pending

          def next() =
            pending = false
            root.resize(10, 2)
            TerminalInfo.WindowSize(2, 10)

        Form(root, Mode.Fullscreen, rank(panel()(Out.print(t"A")), panel()(Out.print(t"B")))).run(resize)
        root.render
      . assert(_ == t"A         \nB         \n          \n          ")

    suite(m"InlineRoot present (inline mode)"):
      def capturing(): (ji.ByteArrayOutputStream, Stdio) =
        val bytes = ji.ByteArrayOutputStream()
        (bytes, Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap))

      test(m"a styled cell emits its colour as SGR"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.xtermTrueColorTermcap)
        val root = InlineRoot(3, 4)
        root.reframe(3, 1)
        root.move(Prim, Prim)
        root.put(e"$Bold(hi)")
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_.contains(t"[1m"))

      // The block (2 rows) is docked to the bottom of the 4-row terminal: it scrolls
      // 2 rows in (`\n\n`) to reserve space, then draws each row at an absolute screen
      // position (rows 3 and 4) and parks the caret absolutely.
      test(m"a first inline render docks the block to the bottom rows"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2)
        root.move(Prim, Prim)
        root.put(t"hi")
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[9999B\r\n\n\e[3;1H\e[2Khi\r\n\e[2K\r\e[3;1H\e[?25h")

      test(m"a growing block scrolls one more row into the dock"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 1); root.move(Prim, Prim); root.put(t"a"); root.flush()
        bytes.reset()
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[9999B\r\n\e[3;1H\e[2Kab\r\n\e[2Kcd\r\e[3;1H\e[?25h")

      // The block bottom-docks: shrinking moves it down (row 4) and clears the row it
      // vacated above (row 3).
      test(m"a shrinking block clears the row it vacated above"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.reframe(3, 1); root.move(Prim, Prim); root.put(t"ef"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[4;1H\e[2Kef\r\e[3;1H\e[2K\e[4;1H\e[?25h")

      test(m"the caret is placed at its absolute screen cell"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2)
        root.move(Prim, Prim)
        root.put(t"ab\ncd")
        root.showCaret(Sec, Sec)
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[9999B\r\n\n\e[3;1H\e[2Kab\r\n\e[2Kcd\r\e[4;2H\e[?25h")

      test(m"finish drops the cursor onto a fresh line below the block"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.finish()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[4;1H\r\n\e[?25h")

      // The first resize switches to top-anchoring: the present clears the whole
      // screen (`\e[1;1H\e[0J`) and repaints the block pinned to rows 1..h, so a taller
      // terminal can't strand a ghost above it.
      test(m"the first resize top-anchors the block, clearing the screen"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.invalidate()
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H\e[0J\e[1;1H\e[2Kab\r\n\e[2Kcd\r\e[1;1H\e[?25h")

      // With `bottomDocked`, a resize never switches to top-anchoring: the block stays
      // docked and its rows are cleared at the bottom (row 3), not the top-left corner.
      test(m"bottomDocked keeps the block docked across a resize"):
        import inlineAnchoring.bottomDocked
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.invalidate()
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[3;1H\e[0J\e[3;1H\e[2Kab\r\n\e[2Kcd\r\e[3;1H\e[?25h")

      // With `topAnchored`, the very first frame is pinned to rows 1..h (no bottom dock,
      // no scroll into scrollback).
      test(m"topAnchored pins the first frame to the top rows"):
        import inlineAnchoring.topAnchored
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H\e[2Kab\r\n\e[2Kcd\r\e[1;1H\e[?25h")

      // With `fullscreen`, the first present enters the alternate screen buffer.
      test(m"fullscreen enters the alternate screen on the first present"):
        import inlineAnchoring.fullscreen
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?1049h\e[?25l\e[1;1H\e[2Kab\r\n\e[2Kcd\r\e[1;1H\e[?25h")

      // ...and leaves it again on finish, restoring the pre-session screen.
      test(m"fullscreen leaves the alternate screen on finish"):
        import inlineAnchoring.fullscreen
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.finish()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[3;1H\r\n\e[?25h\e[?1049l")

      // With `keepTop`, a shrink holds the top row and clears below (row 4), rather than
      // re-docking down and clearing the row it vacated above.
      test(m"keepTop clears below the block on a shrink, holding the top"):
        import inlineShrink.keepTop
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.reframe(3, 1); root.move(Prim, Prim); root.put(t"ef"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[3;1H\e[2Kef\r\n\e[2K\e[3;1H\e[?25h")

      // With `clampToScreen`, a growing block grows upward in place; it never scrolls the
      // screen into scrollback (no `\e[9999B`).
      test(m"clampToScreen grows without scrolling into scrollback"):
        import inlineGrowth.clampToScreen
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 1); root.move(Prim, Prim); root.put(t"a"); root.flush()
        bytes.reset()
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[3;1H\e[2Kab\r\n\e[2Kcd\r\e[3;1H\e[?25h")

    // A geometry-stable re-present diffs against the snapshot of what the last present
    // drew, overprinting only the damaged cells; an identical frame emits nothing.
    suite(m"InlineRoot diff present"):
      def capturing(): (ji.ByteArrayOutputStream, Stdio) =
        val bytes = ji.ByteArrayOutputStream()
        (bytes, Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap))

      // Present `first`, then re-present `second` at the same geometry and capture
      // only the second frame's bytes.
      def represent(width: Int, height: Int, first: Text, second: Text)
        ( using Stdio, ji.ByteArrayOutputStream )
      :   Text =

        val root = InlineRoot(width, 4)
        root.reframe(width, height); root.move(Prim, Prim); root.put(first); root.flush()
        summon[ji.ByteArrayOutputStream].reset()
        root.reframe(width, height); root.move(Prim, Prim); root.put(second); root.flush()
        String(summon[ji.ByteArrayOutputStream].toByteArray.nn, "UTF-8").tt

      test(m"an identical re-present emits nothing at all"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        given ji.ByteArrayOutputStream = bytes
        represent(3, 2, t"ab\ncd", t"ab\ncd")
      . assert(_ == t"")

      test(m"a single changed cell emits one absolutely-addressed grapheme"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        given ji.ByteArrayOutputStream = bytes
        represent(3, 2, t"ab\ncd", t"ab\ncD")
      . assert(_ == t"\e[?25l\e[4;2HD\e[3;1H\e[?25h")

      test(m"adjacent changed cells coalesce into one run"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        given ji.ByteArrayOutputStream = bytes
        represent(3, 2, t"ab\ncd", t"ab\nxy")
      . assert(_ == t"\e[?25l\e[4;1Hxy\e[3;1H\e[?25h")

      test(m"disjoint changed cells are addressed as separate runs"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        given ji.ByteArrayOutputStream = bytes
        represent(3, 1, t"abc", t"xbz")
      . assert(_ == t"\e[?25l\e[4;1Hx\e[4;3Hz\e[4;1H\e[?25h")

      // A wide (CJK) glyph occupies two cells (its trailing sentinel carries the same
      // style), so damage always covers the whole glyph, in both directions.
      test(m"a wide glyph replacing narrow cells re-emits both cells"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        given ji.ByteArrayOutputStream = bytes
        represent(3, 1, t"abc", t"a中")
      . assert(_ == t"\e[?25l\e[4;2H中\e[4;1H\e[?25h")

      test(m"narrow cells replacing a wide glyph re-emit both cells"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        given ji.ByteArrayOutputStream = bytes
        represent(3, 1, t"a中", t"abc")
      . assert(_ == t"\e[?25l\e[4;2Hbc\e[4;1H\e[?25h")

      test(m"an unchanged wide glyph beside a changed cell is not re-emitted"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        given ji.ByteArrayOutputStream = bytes
        represent(4, 1, t"中ab", t"中ax")
      . assert(_ == t"\e[?25l\e[4;4Hx\e[4;1H\e[?25h")

      // A style-only change is damage too, and the run renders its SGR self-contained
      // (ending with a reset, so nothing bleeds beyond the patch).
      test(m"a style-only change re-emits the cell as SGR within the run"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio =
          Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.xtermTrueColorTermcap)
        val root = InlineRoot(3, 4)
        root.reframe(3, 1); root.move(Prim, Prim); root.put(t"ab"); root.flush()
        bytes.reset()
        root.reframe(3, 1); root.move(Prim, Prim); root.put(e"$Bold(a)b"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert: emitted =>
          emitted.contains(t"\e[4;1H") && emitted.contains(t"\e[1m") && emitted.contains(t"\e[0m")
            && !emitted.contains(t"\e[2K")

      test(m"a caret-only move emits just the caret placement"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd")
        root.showCaret(Sec, Prim)
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[3;2H\e[?25h")

    // The buffered fullscreen root: panels composite into its grid, and `flush`
    // presents each frame as one write, diffed against the previous present.
    // A resize present recovers the block's position from the anchor reply (where the
    // terminal moved the parked cursor during its reflow), clearing exactly from the
    // residue's top instead of wiping the screen; an anchor that cannot be reconciled
    // falls back to the historic behaviour.
    suite(m"InlineRoot reflow recovery"):
      def capturing(): (ji.ByteArrayOutputStream, Stdio) =
        val bytes = ji.ByteArrayOutputStream()
        (bytes, Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap))

      test(m"a shrink with an anchor clears from the recovered row and re-docks"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 6
        val root = new InlineRoot(() => w, () => 4)
        root.reframe(6, 2); root.move(Prim, Prim); root.put(t"abcdef\nhi"); root.flush()
        bytes.reset()
        w = 4
        root.invalidate()
        root.anchor(2, 1)
        root.reframe(4, 2); root.move(Prim, Prim); root.put(t"abcd\nhi"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[2;1H\e[0J\e[3;1H\e[2Kabcd\r\n\e[2Khi\r\e[3;1H\e[?25h")

      // The park sat past the wrap point of its own row ("abcdef" at width 4 wraps
      // after "abcd"), so the two models predict different anchor columns: a reply in
      // column 2 can only be the reflow model, whose sub-row offset is honoured.
      test(m"the anchor column selects the reflow model"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 6
        val root = new InlineRoot(() => w, () => 4)
        root.reframe(6, 2); root.move(Prim, Prim); root.put(t"abcdef\nhi")
        root.showCaret(5.z, Prim)
        root.flush()
        bytes.reset()
        w = 4
        root.invalidate()
        root.anchor(3, 2)
        root.reframe(4, 2); root.move(Prim, Prim); root.put(t"abcd\nhi"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[2;1H\e[0J\e[3;1H\e[2Kabcd\r\n\e[2Khi\r\e[3;4H\e[?25h")

      // A reply in column 4 (the old column, clamped) can only be the truncate model:
      // the cursor's row did not move, so the residue still starts at the old top.
      test(m"the anchor column selects the truncate model"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 6
        val root = new InlineRoot(() => w, () => 4)
        root.reframe(6, 2); root.move(Prim, Prim); root.put(t"abcdef\nhi")
        root.showCaret(5.z, Prim)
        root.flush()
        bytes.reset()
        w = 4
        root.invalidate()
        root.anchor(3, 4)
        root.reframe(4, 2); root.move(Prim, Prim); root.put(t"abcd\nhi"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[3;1H\e[0J\e[3;1H\e[2Kabcd\r\n\e[2Khi\r\e[3;4H\e[?25h")

      // A reply matching neither model's column is unmodellable: fall back to the
      // historic flip-and-clear.
      test(m"an unmatchable anchor column falls back to the full clear"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 6
        val root = new InlineRoot(() => w, () => 4)
        root.reframe(6, 2); root.move(Prim, Prim); root.put(t"abcdef\nhi")
        root.showCaret(5.z, Prim)
        root.flush()
        bytes.reset()
        w = 4
        root.invalidate()
        root.anchor(3, 3)
        root.reframe(4, 2); root.move(Prim, Prim); root.put(t"abcd\nhi"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H\e[0J\e[1;1H\e[2Kabcd\r\n\e[2Khi\r\e[1;4H\e[?25h")

      test(m"a width growth recovers with both models in agreement"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 4
        val root = new InlineRoot(() => w, () => 4)
        root.reframe(4, 2); root.move(Prim, Prim); root.put(t"ab\nhi"); root.flush()
        bytes.reset()
        w = 6
        root.invalidate()
        root.anchor(2, 1)
        root.reframe(6, 2); root.move(Prim, Prim); root.put(t"ab\nhi"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[2;1H\e[0J\e[3;1H\e[2Kab\r\n\e[2Khi\r\e[3;1H\e[?25h")

      // The anchor is consumed by the present that follows its resize: a later resize
      // without a fresh reply cannot reuse it, and falls back.
      test(m"a second resize without a fresh anchor falls back"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 6
        val root = new InlineRoot(() => w, () => 4)
        root.reframe(6, 2); root.move(Prim, Prim); root.put(t"abcdef\nhi"); root.flush()
        w = 4
        root.invalidate()
        root.anchor(2, 1)
        root.reframe(4, 2); root.move(Prim, Prim); root.put(t"abcd\nhi"); root.flush()
        bytes.reset()
        w = 3
        root.invalidate()
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\nhi"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H\e[0J\e[1;1H\e[2Kab\r\n\e[2Khi\r\e[1;1H\e[?25h")

      // A wide glyph that would straddle the new margin is pushed whole to the next
      // sub-row, so "ab中" at width 3 occupies two physical rows in the reflow model.
      test(m"a wide glyph is counted whole in the reflow prediction"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 4
        val root = new InlineRoot(() => w, () => 4)
        root.reframe(4, 2); root.move(Prim, Prim); root.put(t"ab中\nhi")
        root.showCaret(Prim, Sec)
        root.flush()
        bytes.reset()
        w = 3
        root.invalidate()
        root.anchor(4, 1)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\nhi"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[2;1H\e[0J\e[3;1H\e[2Kab\r\n\e[2Khi\r\e[4;1H\e[?25h")

      // Trailing default-styled blanks are trimmed from a full-row render (`el(2)`
      // has already cleared them), but a wide glyph's trailing sentinel is content:
      // the row trims to include the whole glyph.
      test(m"a row ending in a wide glyph trims to include the whole glyph"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(4, 4)
        root.reframe(4, 1); root.move(Prim, Prim); root.put(t"a中"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[9999B\r\n\e[4;1H\e[2Ka中\r\e[4;1H\e[?25h")

      // A styled blank is not a default cell: `el(2)` cannot reproduce it, so it is
      // never trimmed and its SGR is emitted.
      test(m"a styled trailing blank is not trimmed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio =
          Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.xtermTrueColorTermcap)
        val root = InlineRoot(4, 4)
        root.reframe(4, 1); root.move(Prim, Prim); root.put(e"a$Bold( )"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_.contains(t"\e[1m"))

    // The driver-side plumbing: the anchor reply flows from the event stream to the
    // inline root's resize recovery, and typing mid-resize never presents against
    // stale geometry.
    suite(m"Form resize plumbing"):
      def capturing(): (ji.ByteArrayOutputStream, Stdio) =
        val bytes = ji.ByteArrayOutputStream()
        (bytes, Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap))

      test(m"a resize with an anchor reply recovers the block position"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var w = 6
        val root = new InlineRoot(() => w, () => 4)

        val events = new Iterator[TerminalEvent]:
          private var remaining: scala.collection.immutable.List[() => TerminalEvent] =
            scala.collection.immutable.List(
              () => Signal.Winch,
              () => TerminalInfo.CursorPosition(2, 1),
              () => { w = 4; TerminalInfo.WindowSize(4, 4) },
              () => Keypress.Escape)

          def hasNext = remaining.nonEmpty

          def next() =
            val head = remaining.head
            remaining = remaining.tail
            head()

        Form(root, Mode.Inline, rank(panel()(Out.print(t"abcdef")))).run(events)
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_.contains(t"\e[2;1H\e[0J"))

      // With a real debounce window, a keypress between the WINCH and the resize
      // repaint updates the widget but presents nothing: the repaint coalesces into
      // the (never-woken, here) deferred resize flush, so the initial frame's row
      // draws are the only ones emitted.
      test(m"typing during a pending resize does not present"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = new InlineRoot(() => 6, () => 4)

        val events = List[TerminalEvent](
          Signal.Winch,
          Keypress.CharKey('x'),
          Keypress.Escape)

        Form(root, Mode.Inline, rank(editor()), debounce = 50).run(events.iterator)
        String(bytes.toByteArray.nn, "UTF-8").tt.cut(t"\e[2K").length - 1
      . assert(_ == 1)

    suite(m"ScreenRoot present"):
      def capturing(): (ji.ByteArrayOutputStream, Stdio) =
        val bytes = ji.ByteArrayOutputStream()
        (bytes, Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap))

      test(m"move and put emit nothing until flush"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = ScreenRoot(3, 2)
        root.move(Prim, Prim)
        root.put(t"ab")
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"")

      test(m"the first flush redraws every row absolutely"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = ScreenRoot(3, 2)
        root.move(Prim, Prim)
        root.put(t"ab")
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H\e[2Kab \e[2;1H\e[2K   \e[1;1H\e[?25h")

      test(m"an identical re-flush emits nothing at all"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = ScreenRoot(3, 2)
        root.move(Prim, Prim); root.put(t"ab"); root.flush()
        bytes.reset()
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"")

      test(m"a single changed cell is overprinted alone"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = ScreenRoot(3, 2)
        root.move(Prim, Prim); root.put(t"ab"); root.flush()
        bytes.reset()
        root.move(Sec, Prim); root.put(t"X"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;2HX\e[1;1H\e[?25h")

      // The SIGWINCH guarantee: `invalidate` (called by the form driver on every
      // WindowSize event) forces the next flush to redraw everything, even when no
      // cell changed.
      test(m"invalidate forces the next flush to redraw in full"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = ScreenRoot(3, 2)
        root.move(Prim, Prim); root.put(t"ab"); root.flush()
        bytes.reset()
        root.invalidate()
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H\e[2Kab \e[2;1H\e[2K   \e[1;1H\e[?25h")

      test(m"clearing the grid blanks exactly the cells that had content"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = ScreenRoot(3, 2)
        root.move(Prim, Prim); root.put(t"ab"); root.flush()
        bytes.reset()
        root.clear()
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H  \e[1;1H\e[?25h")

      // A WindowSize event re-tiles the layout against the live size: the form driver
      // invalidates the root and `reframe` re-fits the grid, so the present after a
      // resize is a full redraw of the re-solved layout.
      test(m"a WindowSize event re-tiles and fully redraws"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        var liveRows: Int = 4
        val root = new ScreenRoot(() => 10, () => liveRows)

        val resize = new Iterator[TerminalEvent]:
          private var pending = true
          def hasNext = pending

          def next() =
            pending = false
            liveRows = 2
            TerminalInfo.WindowSize(2, 10)

        Form(root, Mode.Fullscreen, rank(panel()(Out.print(t"A")), panel()(Out.print(t"B"))))
        . run(resize)

        root.render
      . assert(_ == t"A         \nB         ")

    suite(m"Dynamic panes"):
      def cell(): Pane = panel()(())

      test(m"append adds a pane at the end"):
        val a = cell()
        val b = cell()
        val panes = Panes(a)
        panes.append(b)
        panes.contents.transmute[List] == List(a, b)
      . assert(_ == true)

      test(m"prepend adds a pane at the start"):
        val a = cell()
        val b = cell()
        val panes = Panes(a)
        panes.prepend(b)
        panes.contents.transmute[List] == List(b, a)
      . assert(_ == true)

      test(m"insertBefore places a pane immediately before the reference"):
        val a = cell()
        val b = cell()
        val c = cell()
        val panes = Panes(a, b)
        panes.insertBefore(b, c)
        panes.contents.transmute[List] == List(a, c, b)
      . assert(_ == true)

      test(m"insertAfter places a pane immediately after the reference"):
        val a = cell()
        val b = cell()
        val c = cell()
        val panes = Panes(a, b)
        panes.insertAfter(a, c)
        panes.contents.transmute[List] == List(a, c, b)
      . assert(_ == true)

      test(m"remove deletes a pane by identity"):
        val a = cell()
        val b = cell()
        val panes = Panes(a, b)
        panes.remove(a)
        panes.contents.transmute[List] == List(b)
      . assert(_ == true)

      // Drive a running form, append a pane mid-loop (the synthetic iterator
      // yields a Redraw to wake it), and confirm the layout re-tiles to include it.
      test(m"a form picks up a pane appended while it runs"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalCanvas(10, 2), Rect(0, 0, 10, 2))
        val panes = Panes(panel()(Out.print(t"A")))

        val events = new Iterator[TerminalEvent]:
          private var pending = true
          def hasNext = pending

          def next() =
            pending = false
            panes.append(panel()(Out.print(t"B")))
            TerminalInfo.Redraw

        Form(root, Mode.Fullscreen, rank(panes)).run(events)
        root.render
      . assert(_ == t"A         \nB         ")

    suite(m"Focus indication"):
      def grid(): FlowExtent =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        FlowExtent(TerminalCanvas(12, 2), Rect(0, 0, 12, 2))

      def captured(block: Stdio ?=> Unit): Text =
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)
        block
        String(bytes.toByteArray.nn, "UTF-8").tt

      test(m"a focused menu marks its selection with a pointer"):
        val extent = grid()
        MenuField(SelectMenu(List(t"alpha", t"beta"), t"alpha")).render(extent, true)
        extent.render
      . assert(_ == t" > alpha    \n   beta     ")

      test(m"an unfocused menu marks its selection with a dot"):
        val extent = grid()
        MenuField(SelectMenu(List(t"alpha", t"beta"), t"alpha")).render(extent, false)
        extent.render
      . assert(_ == t" · alpha    \n   beta     ")

      test(m"a focused editor shows the hardware cursor"):
        captured: stdio ?=>
          EditorField(LineEditor(t"hi")).render(TerminalCanvas(20, 1), true)
      . assert(_.s.contains("[?25h"))

      test(m"an unfocused editor hides the hardware cursor"):
        captured: stdio ?=>
          EditorField(LineEditor(t"hi")).render(TerminalCanvas(20, 1), false)
      . assert(_.s.contains("[?25l"))

      // Tabbing focus away from the menu must repaint it, so its marker updates
      // from `>` to `·` (a regression: only the panel gaining focus was redrawn).
      test(m"a panel that loses focus is repainted so its marker updates"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalCanvas(12, 3), Rect(0, 0, 12, 3))
        val pane = rank(menu(List(t"alpha", t"beta"), t"alpha"), editor())
        Form(root, Mode.Fullscreen, pane).run(List(Keypress.Tab, Keypress.Escape).iterator)
        root.render
      . assert(_ == t" · alpha    \n   beta     \n            ")

    suite(m"Borders"):
      def render(width: Int, height: Int)(pane: Pane): Text =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalCanvas(width, height), Rect(0, 0, width, height))
        paint(root, pane)
        root.render

      test(m"a full border frames the content with corners and rules"):
        render(4, 3)(border()(panel()(Out.print(t"hi"))))
      . assert(_ == t"┌──┐\n│hi│\n└──┘")

      test(m"the border style selects the glyphs (rounded corners)"):
        render(3, 3)(border(BorderStyle.rounded)(panel()(Out.print(t"x"))))
      . assert(_ == t"╭─╮\n│x│\n╰─╯")

      test(m"a rule re-fills to the content's width"):
        render(6, 3)(border()(panel()(Out.print(t"wide"))))
      . assert(_ == t"┌────┐\n│wide│\n└────┘")

      test(m"a top-only border is a single rule with no corners"):
        render(2, 2)(border(top = true, right = false, bottom = false, left = false)
            (panel()(Out.print(t"ab"))))
      . assert(_ == t"──\nab")

      test(m"left-and-right-only borders omit every corner"):
        render(4, 1)(border(top = false, bottom = false)(panel()(Out.print(t"ab"))))
      . assert(_ == t"│ab│")

      test(m"a full border adds one cell on every side to the minimum size"):
        val bordered = border()(panel(minWidth = 3, minHeight = 2)(())).frame
        (bordered.measure(Axis.File).min, bordered.measure(Axis.Rank).min)
      . assert(_ == (5, 4))

// A test-only root `Canvas` that paints into a fixed in-memory grid but reports a
// settable size, so a layout can be re-tiled to a smaller `width`/`height` and
// the composed screen read back.
class ResizableRoot(maxWidth: Int, maxHeight: Int)(using Stdio) extends Canvas:
  private val flow = FlowExtent(TerminalCanvas(maxWidth, maxHeight), Rect(0, 0, maxWidth, maxHeight))
  private var size: (Int, Int) = (maxWidth, maxHeight)

  def resize(width: Int, height: Int): Unit = size = (width, height)
  def width: Int = size._1
  def height: Int = size._2
  def move(column: Ordinal, row: Ordinal): Unit = flow.move(column, row)
  def put(text: Text): Unit = flow.put(text)
  def put(text: Teletype): Unit = flow.put(text)
  def clear(): Unit = flow.clear()
  def clearLine(): Unit = flow.clearLine()
  def cursor(visible: Boolean): Unit = flow.cursor(visible)
  def showCaret(column: Ordinal, row: Ordinal): Unit = flow.showCaret(column, row)
  def flush(): Unit = flow.flush()
  def render: Text = flow.render
