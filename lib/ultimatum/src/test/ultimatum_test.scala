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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import java.io as ji

import soundness.*
import denominative.dysasymptotics.linearSize

object Tests extends Suite(m"Ultimatum Tests"):
  def run(): Unit =
    suite(m"TerminalBoard"):
      // Capture everything a surface writes into an in-memory buffer.
      def captured(block: Stdio ?=> Unit): Text =
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)
        block
        String(bytes.toByteArray.nn, "UTF-8").tt

      test(m"move emits an absolute CSI cursor-position sequence"):
        captured: stdio ?=>
          TerminalBoard(80, 24).move(10.z, 5.z)
      . assert(_ == t"\e[6;11H")

      test(m"move then put places text at the position"):
        captured: stdio ?=>
          val surface = TerminalBoard(80, 24)
          surface.move(10.z, 5.z)
          surface.put(t"X")
      . assert(_ == t"\e[6;11HX")

      test(m"clear erases the whole display"):
        captured(TerminalBoard(80, 24).clear())
      . assert(_ == t"\e[2J")

      test(m"hiding the cursor emits the DECTCEM reset"):
        captured(TerminalBoard(80, 24).cursor(false))
      . assert(_ == t"\e[?25l")

    suite(m"FlowExtent"):
      // A standalone extent over a muted parent; mutation tests never flush, so
      // the parent surface is unused.
      def extent(width: Int, height: Int): FlowExtent =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        FlowExtent(TerminalBoard(width, height), Rect(0, 0, width, height))

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
        val flow = FlowExtent(TerminalBoard(80, 24), Rect(2, 1, 3, 1))
        flow.put(t"xy")
        flow.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[2;3Hxy ")

    suite(m"Layout solver"):
      def cell(sizing: Sizing): Frame = Frame.Cell(sizing)
      def strip(children: Frame*): Frame = Frame.Split(Sizing(), ultimatum.Arrangement.Strip, children.to(List))
      def stack(children: Frame*): Frame = Frame.Split(Sizing(), ultimatum.Arrangement.Stack, children.to(List))

      test(m"fractions divide the arrangement proportionally"):
        val frame = strip(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 90, 10)).cells
      . assert(_ == List(Rect(0, 0, 20, 10), Rect(20, 0, 30, 10), Rect(50, 0, 40, 10)))

      test(m"largest-remainder rounding fills the arrangement exactly"):
        val frame = strip(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 100, 1)).cells.map(_.width)
      . assert(_ == List(22, 33, 45))

      test(m"the rounded sizes always sum to the available space"):
        val frame = strip(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 100, 1)).cells.map(_.width).fold(0)(_ + _)
      . assert(_ == 100)

      test(m"a child whose minimum exceeds its share is fixed at the minimum"):
        val frame = strip(cell(Sizing(1.0, minWidth = 8)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(8, 2))

      test(m"a child whose maximum is below its share is capped at the maximum"):
        val frame = strip(cell(Sizing(1.0, maxWidth = 3)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(3, 7))

      test(m"a container's minimum is forced up to the sum of its children's"):
        val frame = strip(cell(Sizing(1.0, minWidth = 5)), cell(Sizing(1.0, minWidth = 5)))
        frame.measure(ultimatum.Arrangement.Strip)
      . assert(_ == Limits(10, Unset))

      test(m"file children fill the cross arrangement (full height)"):
        val frame = strip(cell(Sizing(1.0)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 8, 4)).cells
      . assert(_ == List(Rect(0, 0, 4, 4), Rect(4, 0, 4, 4)))

      test(m"rank splits distribute height and fill width"):
        val frame = stack(cell(Sizing(1.0)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 8, 4)).cells
      . assert(_ == List(Rect(0, 0, 8, 2), Rect(0, 2, 8, 2)))

      test(m"nested ranks within files place rectangles correctly"):
        val frame = strip(cell(Sizing(1.0)), stack(cell(Sizing(1.0)), cell(Sizing(1.0))))
        frame.arrange(Rect(0, 0, 10, 4)).cells
      . assert(_ == List(Rect(0, 0, 5, 4), Rect(5, 0, 5, 2), Rect(5, 2, 5, 2)))

    suite(m"Grid arrangement"):
      def cell(sizing: Sizing): Frame = Frame.Cell(sizing)

      def grid(columns: Int, gap: Int = 0)(children: Frame*): Frame =
        Frame.Split(Sizing(), ultimatum.Arrangement.Grid(columns, gap), children.to(List))

      test(m"cells flow row-major into columns with content-sized rows"):
        val frame = grid(2)
          ( cell(Sizing(minHeight = 2)), cell(Sizing(minHeight = 2)),
            cell(Sizing(minHeight = 3)), cell(Sizing(minHeight = 1)) )

        frame.arrange(Rect(0, 0, 10, 10)).cells
      . assert:
          _ == List
            ( Rect(0, 0, 5, 2), Rect(5, 0, 5, 2),
              Rect(0, 2, 5, 3), Rect(5, 2, 5, 3) )

      test(m"column widths are negotiated across every row"):
        val frame = grid(2)
          ( cell(Sizing(minWidth = 3, minHeight = 1)), cell(Sizing(minHeight = 1)),
            cell(Sizing(minWidth = 7, minHeight = 1)), cell(Sizing(minHeight = 1)) )

        frame.arrange(Rect(0, 0, 10, 4)).cells.map(_.width)
      . assert(_ == List(7, 3, 7, 3))

      test(m"gaps separate both columns and rows"):
        val frame = grid(2, gap = 1)
          ( cell(Sizing(minHeight = 2)), cell(Sizing(minHeight = 2)),
            cell(Sizing(minHeight = 2)), cell(Sizing(minHeight = 2)) )

        frame.arrange(Rect(0, 0, 11, 10)).cells
      . assert:
          _ == List
            ( Rect(0, 0, 5, 2), Rect(6, 0, 5, 2),
              Rect(0, 3, 5, 2), Rect(6, 3, 5, 2) )

      test(m"an incomplete final row keeps its column's width"):
        val frame = grid(2)
          ( cell(Sizing(minHeight = 1)), cell(Sizing(minHeight = 1)),
            cell(Sizing(minHeight = 1)) )

        frame.arrange(Rect(0, 0, 10, 4)).cells
      . assert(_ == List(Rect(0, 0, 5, 1), Rect(5, 0, 5, 1), Rect(0, 1, 5, 1)))

      test(m"a grid's width minimum sums its column minima and gaps"):
        val frame = grid(2, gap = 1)
          ( cell(Sizing(minWidth = 4, minHeight = 1)), cell(Sizing(minWidth = 2, minHeight = 1)),
            cell(Sizing(minWidth = 6, minHeight = 1)), cell(Sizing(minWidth = 5, minHeight = 1)) )

        frame.measure(ultimatum.Arrangement.Strip)
      . assert(_ == Limits(12, Unset))

      test(m"a grid's height is rigid at its content-sized rows"):
        val frame = grid(2, gap = 1)
          ( cell(Sizing(minHeight = 2)), cell(Sizing(minHeight = 3)),
            cell(Sizing(minHeight = 1)), cell(Sizing(minHeight = 1)) )

        frame.measure(ultimatum.Arrangement.Stack)
      . assert(_ == Limits(5, 5))

    suite(m"TableFixture"):
      import escritoire.{Column, Scaffold}
      import tableStyles.thinRoundedTableStyle
      import hyphenations.englishHyphenation
      import textMetrics.uniformMetric

      case class Entry(name: Text, total: Int)
      val entries = List(Entry(t"alpha", 1), Entry(t"beta", 22))

      def fixture(data: List[Entry]): TableFixture = TableFixture:
        Scaffold[Entry, Teletype]
          ( Column[Entry, Text, Teletype](e"Name")(_.name),
            Column[Entry, Int, Teletype](e"Count")(_.total) )
        . tabulate(data)

      test(m"measure reports the table's rendered width and height"):
        fixture(entries).measure(40)
      . assert(_ == (17, 6))

      test(m"a narrower width renegotiates the same table"):
        val wrapping = List(Entry(t"the quick brown fox", 1))
        val wide = fixture(wrapping).measure(40)(0)
        val narrow = fixture(wrapping).measure(20)(0)
        narrow < wide
      . assert(_ == true)

    suite(m"layout / panel DSL"):
      test(m"side-by-side panels paint at their own offsets, no bleed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)

        paint(TerminalBoard(4, 1), strip(panel()(Out.print(t"AA")), panel()(Out.print(t"BB"))))

        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1;1HAA\e[1;3HBB")

      test(m"a panel's output wraps and scrolls within its own rectangle, no bleed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basicTermcap)

        // "HELLO" in a 2x1 panel wraps and scrolls until only "O" remains; the
        // sibling panel's "X" is unaffected, so neither bleeds past column 2.
        paint(TerminalBoard(4, 1), strip(panel()(Out.print(t"HELLO")), panel()(Out.print(t"X"))))

        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1;1HO \e[1;3HX ")

      test(m"a fixed-minimum panel squeezes its fractional sibling"):
        val frame = strip(panel(minWidth = 8)(()), panel()(())).frame
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
        val before = Sequence(Rect(0, 0, 10, 1), Rect(0, 1, 10, 1))
        val after  = Sequence(Rect(0, 0, 10, 2), Rect(0, 2, 10, 1))
        dirtyCells(before, after, Set())
      . assert(_ == Set(0, 1))

      test(m"an unchanged cell is not dirty"):
        dirtyCells(Sequence(Rect(0, 0, 10, 1)), Sequence(Rect(0, 0, 10, 1)), Set())
      . assert(_ == Set())

      test(m"a content-changed cell is dirty though its rectangle is unchanged"):
        val rects = Sequence(Rect(0, 0, 10, 1), Rect(0, 1, 10, 1))
        dirtyCells(rects, rects, Set(1))
      . assert(_ == Set(1))

      // Drive the whole interactive loop into an in-memory grid: type into the
      // first editor, TAB to the second, type into it, then exit, and read back
      // the composed screen.
      test(m"TAB moves focus so typing lands in the right panel"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalBoard(10, 4), Rect(0, 0, 10, 4))

        val events = List
         ( Keypress.CharKey('h'), Keypress.CharKey('i'),
           Keypress.Tab,
           Keypress.CharKey('y'), Keypress.CharKey('o'),
           Keypress.Escape )

        Form(root, Occupancy.Fullscreen, stack(editor(), editor())).run(events.iterator)
        root.render
      . assert(_ == t"hi        \n          \nyo        \n          ")

      // Typing 21 characters into the top editor wraps it onto three rows, raising
      // its panel's minimum height; the solver re-tiles and the bottom editor is
      // pushed from row 2 down to row 3.
      test(m"a growing editor re-tiles and pushes its sibling down"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalBoard(10, 4), Rect(0, 0, 10, 4))
        val events = List.fill(21)(Keypress.CharKey('a')) ++ List(Keypress.Escape)
        Form(root, Occupancy.Fullscreen, stack(editor(), editor())).run(events.iterator)
        root.render
      . assert(_ == t"aaaaaaaaaa\naaaaaaaaaa\na         \n          ")

      // A terminal resize surfaces as a WindowSize event (the SIGWINCH handler in
      // profanity's Terminal queries the new size); the layout re-tiles to it and
      // the rows freed by shrinking are cleared. The custom iterator shrinks the
      // root just before yielding the event, mimicking the live size update.
      test(m"a WindowSize event re-tiles to the new terminal size"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = ResizableRoot(10, 4)

        val resize = new Iterator[Terminal.Event]:
          @scala.caps.unsafe.untrackedCaptures
          private var pending = true
          def hasNext = pending

          def next() =
            pending = false
            root.resize(10, 2)
            Terminal.Info.WindowSize(2, 10)

        Form(root, Occupancy.Fullscreen, stack(panel()(Out.print(t"A")), panel()(Out.print(t"B")))).run(resize)
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
        import inlineAnchoring.bottomDockedAnchoring
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
        import inlineAnchoring.topAnchoring
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\e[1;1H\e[2Kab\r\n\e[2Kcd\r\e[1;1H\e[?25h")

      // With `fullscreen`, the first present enters the alternate screen buffer.
      test(m"fullscreen enters the alternate screen on the first present"):
        import inlineAnchoring.fullscreenAnchoring
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 4)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?1049h\e[?25l\e[1;1H\e[2Kab\r\n\e[2Kcd\r\e[1;1H\e[?25h")

      // ...and leaves it again on finish, restoring the pre-session screen.
      test(m"fullscreen leaves the alternate screen on finish"):
        import inlineAnchoring.fullscreenAnchoring
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
        import inlineShrink.keepTopShrink
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
        import inlineGrowth.clampedGrowth
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
        @scala.caps.unsafe.untrackedCaptures
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
        @scala.caps.unsafe.untrackedCaptures
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
        @scala.caps.unsafe.untrackedCaptures
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
        @scala.caps.unsafe.untrackedCaptures
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
        @scala.caps.unsafe.untrackedCaptures
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
        @scala.caps.unsafe.untrackedCaptures
        var w = 6
        val root = new InlineRoot(() => w, () => 4)

        val events = new Iterator[Terminal.Event]:
          @scala.caps.unsafe.untrackedCaptures
          // Capture-carrying elements do not flow through the opaque List (boxing), so
          // this event queue deliberately stays a stdlib list.
          private var remaining: scala.collection.immutable.List[() => Terminal.Event] =
            scala.collection.immutable.List(
              () => Interrupt.Winch,
              () => Terminal.Info.CursorPosition(2, 1),
              () => { w = 4; Terminal.Info.WindowSize(4, 4) },
              () => Keypress.Escape)

          def hasNext = remaining.nonEmpty

          def next() =
            val head = remaining.head
            remaining = remaining.tail
            head()

        Form(root, Occupancy.Inline, stack(panel()(Out.print(t"abcdef")))).run(events)
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

        val events = List[Terminal.Event](
          Interrupt.Winch,
          Keypress.CharKey('x'),
          Keypress.Escape)

        Form(root, Occupancy.Inline, stack(editor()), debounce = 50).run(events.iterator)
        String(bytes.toByteArray.nn, "UTF-8").tt.cut(t"\e[2K").size - 1
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
        @scala.caps.unsafe.untrackedCaptures
        var liveRows: Int = 4
        val root = new ScreenRoot(() => 10, () => liveRows)

        val resize = new Iterator[Terminal.Event]:
          @scala.caps.unsafe.untrackedCaptures
          private var pending = true
          def hasNext = pending

          def next() =
            pending = false
            liveRows = 2
            Terminal.Info.WindowSize(2, 10)

        Form(root, Occupancy.Fullscreen, stack(panel()(Out.print(t"A")), panel()(Out.print(t"B"))))
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
        panes.contents.to[List] == List(a, b)
      . assert(_ == true)

      test(m"prepend adds a pane at the start"):
        val a = cell()
        val b = cell()
        val panes = Panes(a)
        panes.prepend(b)
        panes.contents.to[List] == List(b, a)
      . assert(_ == true)

      test(m"insertBefore places a pane immediately before the reference"):
        val a = cell()
        val b = cell()
        val c = cell()
        val panes = Panes(a, b)
        panes.insertBefore(b, c)
        panes.contents.to[List] == List(a, c, b)
      . assert(_ == true)

      test(m"insertAfter places a pane immediately after the reference"):
        val a = cell()
        val b = cell()
        val c = cell()
        val panes = Panes(a, b)
        panes.insertAfter(a, c)
        panes.contents.to[List] == List(a, c, b)
      . assert(_ == true)

      test(m"remove deletes a pane by identity"):
        val a = cell()
        val b = cell()
        val panes = Panes(a, b)
        panes.remove(a)
        panes.contents.to[List] == List(b)
      . assert(_ == true)

      // Drive a running form, append a pane mid-loop (the synthetic iterator
      // yields a Redraw to wake it), and confirm the layout re-tiles to include it.
      test(m"a form picks up a pane appended while it runs"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalBoard(10, 2), Rect(0, 0, 10, 2))
        val panes = Panes(panel()(Out.print(t"A")))

        val events = new Iterator[Terminal.Event]:
          @scala.caps.unsafe.untrackedCaptures
          private var pending = true
          def hasNext = pending

          def next() =
            pending = false
            panes.append(panel()(Out.print(t"B")))
            Terminal.Info.Redraw

        Form(root, Occupancy.Fullscreen, stack(panes)).run(events)
        root.render
      . assert(_ == t"A         \nB         ")

    suite(m"Focus indication"):
      def grid(): FlowExtent =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        FlowExtent(TerminalBoard(12, 2), Rect(0, 0, 12, 2))

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
          EditorField(LineEditor(t"hi")).render(TerminalBoard(20, 1), true)
      . assert(_.s.contains("\u001B[?25h"))

      test(m"an unfocused editor hides the hardware cursor"):
        captured: stdio ?=>
          EditorField(LineEditor(t"hi")).render(TerminalBoard(20, 1), false)
      . assert(_.s.contains("\u001B[?25l"))

      // Tabbing focus away from the menu must repaint it, so its marker updates
      // from `>` to `·` (a regression: only the panel gaining focus was redrawn).
      test(m"a panel that loses focus is repainted so its marker updates"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalBoard(12, 3), Rect(0, 0, 12, 3))
        val pane = stack(menu(List(t"alpha", t"beta"), t"alpha"), editor())
        Form(root, Occupancy.Fullscreen, pane).run(List(Keypress.Tab, Keypress.Escape).iterator)
        root.render
      . assert(_ == t" · alpha    \n   beta     \n            ")

    suite(m"Borders"):
      def render(width: Int, height: Int)(pane: Pane): Text =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalBoard(width, height), Rect(0, 0, width, height))
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
        (bordered.measure(ultimatum.Arrangement.Strip).min, bordered.measure(ultimatum.Arrangement.Stack).min)
      . assert(_ == (5, 4))

    suite(m"Gauge designs"):
      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      // A design's plain text is what it draws; the styling is the palette's business and is
      // asserted separately. Deliberately monomorphic: these tests live in `package ultimatum`,
      // where the opaque status types are transparent, so a generic helper would infer the
      // underlying `Double` from a literal and then fail to match the design.
      def bar(design: Fraction is Gaugeable)(value: Fraction, width: Int): Text =
        design.rows(value, Tick.zero, width).stdlib.map(_.plain).mkString("\n").tt

      // Any design lifts to the same status made optional, via `Gaugeable.optional`; putting the
      // definite design in scope is what lets the lift derive the optional one.
      def sweeping(design: Fraction is Gaugeable)
         (value: Optional[Fraction], width: Int, tick: Tick)
      :   Text =

        given definite: (Fraction is Gaugeable) = design

        summon[Optional[Fraction] is Gaugeable].rows(value, tick, width).stdlib.map(_.plain)
        . mkString("\n").tt

      def spin(design: Fraction is Gaugeable)
         (value: Fraction, width: Int, tick: Tick = Tick.zero)
      :   Text =

        design.rows(value, tick, width).stdlib.map(_.plain).mkString("\n").tt

      test(m"a half-full smooth bar fills exactly half its cells"):
        bar(bars.smoothBar)(Fraction(0.5), 10)
      . assert(_ == t"█████     ")

      test(m"an eighth-block bar advances by a fraction of a cell"):
        bar(bars.smoothBar)(Fraction(0.05), 10)
      . assert(_ == t"▌         ")

      test(m"a full bar leaves no empty cells"):
        bar(bars.smoothBar)(Fraction(1.0), 8)
      . assert(_ == t"████████")

      test(m"an empty bar draws no filled cells"):
        bar(bars.smoothBar)(Fraction(0.0), 8)
      . assert(_ == t"        ")

      test(m"a block bar draws its own track glyph"):
        bar(bars.blockBar)(Fraction(0.5), 8)
      . assert(_ == t"████░░░░")

      test(m"an ASCII bar keeps its caps and fills between them"):
        bar(bars.asciiBar)(Fraction(0.5), 10)
      . assert(_ == t"[####----]")

      test(m"an arrowhead bar puts the boundary cell at the head of the fill"):
        bar(bars.arrowheadBar)(Fraction(0.5), 10)
      . assert(_ == t"[===>    ]")

      test(m"a segmented bar lights whole pips"):
        bar(bars.segmentedBar)(Fraction(0.5), 20)
      . assert(_ == t"▰▰▰▰▰▰▰▰▰▰▱▱▱▱▱▱▱▱▱▱")

      test(m"a marker bar shows a position, with nothing filled behind it"):
        bar(bars.markerBar)(Fraction(0.5), 9)
      . assert(_ == t"────◆────")

      test(m"a bar too narrow for caps drops them and still fills"):
        bar(bars.asciiBar)(Fraction(0.5), 6)
      . assert(_ == t"###---")

      test(m"a bar degrades to a percentage when it cannot be drawn"):
        bar(bars.smoothBar)(Fraction(0.42), 3)
      . assert(_ == t"42%")

      test(m"a bar degrades to a single shade glyph at one cell"):
        bar(bars.smoothBar)(Fraction(0.9), 1)
      . assert(_ == t"█")

      test(m"the percentage design pads to a stable width as it fills"):
        (bar(bars.percentageBar)(Fraction(0.07), 4),
            bar(bars.percentageBar)(Fraction(1.0), 4))
      . assert(_ == (t"  7%", t"100%"))

      test(m"a spinner advances one frame per period"):
        val design = spinners.brailleDotsSpinner
        (0 to 3).map { index => spin(design)(Fraction(0.0), 1, Tick.at(index*80, 80)) }.mkString.tt
      . assert(_ == t"⠋⠙⠹⠸")

      test(m"a spinner cycles back to its first frame"):
        spin(spinners.brailleDotsSpinner)(Fraction(0.0), 1, Tick.at(10*80, 80))
      . assert(_ == t"⠋")


      // Progress that may not be known is one status: a figure when there is one, a sweep when
      // there is not, so a job that learns its total does not change type half way through.
      test(m"a bar over unknown progress sweeps rather than sitting at zero"):
        sweeping(bars.smoothBar)(Fraction.indeterminate, 10, Tick.zero)
      . assert(_ == t"██░░░░░░░░")

      test(m"the sweep travels, and returns rather than jumping back"):
        (sweeping(bars.smoothBar)(Fraction.indeterminate, 10, Tick.at(240, 80)),
            sweeping(bars.smoothBar)(Fraction.indeterminate, 10, Tick.at(80*10, 80)))
      . assert(_ == (t"░░░██░░░░░", t"░░░░░░██░░"))

      test(m"the same design draws a bar once the fraction is known"):
        sweeping(bars.smoothBar)(Fraction(0.5), 10, Tick.zero)
      . assert(_ == t"█████     ")

      test(m"an unknown-progress design animates; a definite bar does not"):
        given definite: (Fraction is Gaugeable) = bars.smoothBar
        (summon[Optional[Fraction] is Gaugeable].period, bars.smoothBar.period)
      . assert(_ == (80, Unset))

      test(m"a spinner declares its frame interval as its animation period"):
        summon[Fraction is Gaugeable](using spinners.brailleDotsSpinner).period
      . assert(_ == 80)

      test(m"a bar declares no animation period"):
        summon[Fraction is Gaugeable](using bars.smoothBar).period
      . assert(_ == Unset)

      test(m"a wide spinner falls back to a narrower design in a narrow column"):
        spin(spinners.bouncingBarSpinner)(Fraction(0.0), 1, Tick.zero)
      . assert(_ == t"-")

      test(m"a multi-cell spinner draws at its full width when it fits"):
        spin(spinners.bouncingBarSpinner)(Fraction(0.0), 6, Tick.at(80, 80))
      . assert(_ == t"[=   ]")

    suite(m"Gauge glyph repertoires"):
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      def bar(design: Fraction is Gaugeable)(value: Fraction, width: Int): Text =
        design.rows(value, Tick.zero, width).stdlib.map(_.plain).mkString("\n").tt

      def spin(design: Fraction is Gaugeable)(value: Fraction, width: Int): Text =
        design.rows(value, Tick.zero, width).stdlib.map(_.plain).mkString("\n").tt

      test(m"an emoji spinner renders as emoji when they are permitted"):
        import gaugeGlyphs.emojiGlyphs
        spin(spinners.moonPhaseSpinner)(Fraction(0.0), 2)
      . assert(_ == t"🌑")

      test(m"an emoji spinner falls back to its BMP sibling when they are not"):
        import gaugeGlyphs.unicodeGlyphs
        spin(spinners.moonPhaseSpinner)(Fraction(0.0), 2)
      . assert(_ == t"◌ ")

      test(m"under ASCII glyphs every spinner degrades to seven-bit output"):
        import gaugeGlyphs.asciiGlyphs
        spin(spinners.brailleDotsSpinner)(Fraction(0.0), 2).s.forall(_ < 128)
      . assert(_ == true)

      test(m"under ASCII glyphs a bar degrades to seven-bit output"):
        import gaugeGlyphs.asciiGlyphs
        bar(bars.smoothBar)(Fraction(0.5), 1).s.forall(_ < 128)
      . assert(_ == true)

    suite(m"Gauge width invariants"):
      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      // Every design must render exactly the width it was given, at every width: a design that is
      // one cell out corrupts the row beside it, and there is no other way to catch that across a
      // catalogue this size.
      val designs: scala.List[(Text, Int => Text)] =
        scala.List
         ( (t"smoothBar", width => bars.smoothBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"blockBar", width => bars.blockBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"shadedBar", width => bars.shadedBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"risingBar", width => bars.risingBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"fineBar", width => bars.fineBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"dotBar", width => bars.dotBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"railBar", width => bars.railBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"squareBar", width => bars.squareBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"brailleBar", width => bars.brailleBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"capsuleBar", width => bars.capsuleBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"asciiBar", width => bars.asciiBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"equalsBar", width => bars.equalsBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"arrowheadBar", width => bars.arrowheadBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"gradientBar", width => bars.gradientBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"segmentedBar", width => bars.segmentedBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"pipBar", width => bars.pipBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"markerBar", width => bars.markerBar.rows(Fraction(0.37), Tick.zero, width)),
           (t"percentageBar", width => bars.percentageBar.rows(Fraction(0.37), Tick.zero, width)) )
        . map: (name, render) =>
            (name, (width: Int) => render(width).stdlib.head.plain)

      test(m"every bar renders exactly the width it is given, from 1 to 120 cells"):
        designs.flatMap: (name, render) =>
          (1 to 120).flatMap: width =>
            val drawn = render(width)
            if drawn.length == width then scala.Nil else scala.List((name, width, drawn.length))
      . assert(_ == scala.Nil)

      test(m"every bar renders one row"):
        scala.List
         ( bars.smoothBar.rows(Fraction(0.5), Tick.zero, 20).stdlib.length,
           bars.segmentedBar.rows(Fraction(0.5), Tick.zero, 20).stdlib.length,
           bars.percentageBar.rows(Fraction(0.5), Tick.zero, 20).stdlib.length )
      . assert(_ == scala.List(1, 1, 1))

      test(m"a bar's output is stable for a fixed tick"):
        val once = bars.smoothBar.rows(Fraction(0.37), Tick.zero, 30).stdlib.head.plain
        val twice = bars.smoothBar.rows(Fraction(0.37), Tick.zero, 30).stdlib.head.plain
        (once, twice)
      . assert((a, b) => a == b)

    suite(m"Facet shedding"):
      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      // A caption that goes first, a bar that stretches, and a figure that goes last.
      def row(width: Int): Text =
        Facet.solve
         ( List
            ( Facet.fixed(2, Teletype(t"compiling")),
              Facet.flexible(4)(w => Teletype(t"="*w)),
              Facet.fixed(1, Teletype(t"42%")) ),
           width )
        . plain

      test(m"a wide row keeps every facet and stretches the flexible one"):
        row(24)
      . assert(_ == t"compiling ========== 42%")

      test(m"a narrower row sheds the most expendable facet first"):
        row(12)
      . assert(_ == t"======== 42%")

      test(m"a narrower row still sheds in shed order"):
        row(8)
      . assert(_ == t"==== 42%")

      test(m"the flexible facet is never shed"):
        row(4)
      . assert(_ == t"====")

      test(m"a row below every minimum is blank rather than corrupt"):
        row(2)
      . assert(_ == t"  ")

      test(m"a solved row is always exactly the width it was given"):
        (1 to 60).map(row(_).length).toList.filter(_ != 0)
      . assert(_ == (1 to 60).toList)

    suite(m"Gauges in a layout"):
      def render(width: Int, height: Int)(pane: Pane): Text =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val root = FlowExtent(TerminalBoard(width, height), Rect(0, 0, width, height))
        paint(root, pane)
        root.render

      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      test(m"a gauge fixture paints its bar into the rectangle it is given"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val flow = FlowExtent(TerminalBoard(10, 1), Rect(0, 0, 10, 1))
        Gaugeable.Fixture(Reading(Fraction(0.3)))(using bars.blockBar).render(flow, false)
        flow.render
      . assert(_ == t"███░░░░░░░")

      test(m"a gauge reports its design's preferred width to the solver"):
        Gaugeable.Fixture(Reading(Fraction(0.5)))(using bars.smoothBar).measure(80)
      . assert(_ == (40, 1))

      test(m"a spinner reports a single cell and does not stretch"):
        given definite: (Fraction is Gaugeable) = spinners.brailleDotsSpinner
        Gaugeable.Fixture(Reading(Fraction.indeterminate)).measure(80)
      . assert(_ == (1, 1))

      test(m"a gauge is not focusable, so it stays out of the focus cycle"):
        given definite: (Fraction is Gaugeable) = spinners.brailleDotsSpinner
        Gaugeable.Fixture(Reading(Fraction.indeterminate)).isInstanceOf[Focus]
      . assert(_ == false)

      test(m"an updated reading is what the next paint draws"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val reading = Reading(Fraction(0.0))
        val fixture = Gaugeable.Fixture(reading)(using bars.blockBar)
        val flow = FlowExtent(TerminalBoard(8, 1), Rect(0, 0, 8, 1))
        reading() = Fraction(0.5)
        fixture.render(flow, false)
        flow.render
      . assert(_ == t"████░░░░")

      test(m"a bar in a stack is painted at the width the solver gave it"):
        render(8, 2):
          stack
           ( panel(minHeight = 1, maxHeight = 1)(Out.print(t"job")),
             Pane.Widget
              ( Sizing(minHeight = 1, maxHeight = 1),
                Gaugeable.Fixture(Reading(Fraction(0.5)))(using bars.blockBar) ) )
      . assert(_ == t"job     \n████░░░░")

    suite(m"Meters, sparklines, counters and processions"):
      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      def plain[status](design: status is Gaugeable)(value: status, width: Int): Text =
        design.rows(value, Tick.zero, width).stdlib.map(_.plain).mkString("\n").tt

      test(m"a column meter shows a bounded reading as one cell"):
        plain(meters.columnMeter)(Meter(0.5), 1)
      . assert(_ == t"▅")

      test(m"a meter reads against its own bounds, not against zero to one"):
        plain(meters.columnMeter)(Meter(50.0, 0.0, 100.0), 1)
      . assert(_ == t"▅")

      test(m"an ASCII meter fills between its brackets"):
        plain(meters.asciiMeter)(Meter(0.5), 10)
      . assert(_ == t"[####----]")

      test(m"a thermometer is a column of its own height"):
        plain(meters.thermometerMeter)(Meter(1.0), 1).cut(t"\n").size
      . assert(_ == 5)

      test(m"a block sparkline draws one cell per sample"):
        plain(sparklines.blockSparkline)(Sequence(0.0, 0.5, 1.0), 3)
      . assert(_ == t"▁▅█")

      test(m"a sparkline auto-scales to the range of its samples"):
        plain(sparklines.blockSparkline)(Sequence(10.0, 20.0), 2)
      . assert(_ == t"▁█")

      test(m"fixed bounds keep a sparkline's scale still between frames"):
        plain(Sparkline.Blocks.scaled(0.0, 10.0))(Sequence(0.0, 5.0), 2)
      . assert(_ == t"▁▅")

      // Decimation, not truncation: a narrow sparkline keeps the peaks rather than showing only
      // the oldest samples.
      test(m"a sparkline narrower than its series keeps the peaks"):
        plain(sparklines.blockSparkline)(Sequence(0.0, 1.0, 0.0, 0.0), 2)
      . assert(_ == t"█▁")

      test(m"a plain counter writes done over total"):
        plain(counters.plainCounter)(Reckoning(17, 120), 7)
      . assert(_ == t"17/120 ")

      test(m"a counter with no total writes only what is done"):
        plain(counters.plainCounter)(Reckoning(17), 2)
      . assert(_ == t"17")

      // The numerator is right-aligned to the total's width, so the field does not jitter.
      test(m"a padded counter holds its width as it counts up"):
        (plain(counters.paddedCounter)(Reckoning(7, 120), 7),
            plain(counters.paddedCounter)(Reckoning(117, 120), 7))
      . assert(_ == (t"  7/120", t"117/120"))

      test(m"a scaled counter abbreviates large figures"):
        plain(counters.scaledCounter)(Reckoning(1200, 8400), 9)
      . assert(_ == t"1.2k/8.4k")

      test(m"a tick standing is a single coloured glyph"):
        plain(standings.tickStanding)(Standing.Succeeded, 1)
      . assert(_ == t"✓")

      test(m"an ASCII standing stays within seven bits"):
        plain(standings.asciiStanding)(Standing.Failed, 1)
      . assert(_ == t"x")

      test(m"a word standing pads to a fixed width so a column aligns"):
        (plain(standings.wordStanding)(Standing.Succeeded, 4),
            plain(standings.wordStanding)(Standing.Failed, 4))
      . assert(_ == (t"  ok", t"FAIL"))

      val steps =
        Sequence
         ( Step(t"resolve", Standing.Succeeded),
           Step(t"compile", Standing.Running),
           Step(t"publish", Standing.Pending) )

      test(m"a checklist is one row per step"):
        plain(processions.checklistProcession)(steps, 12).cut(t"\n").size
      . assert(_ == 3)

      test(m"a checklist marks each step by its standing"):
        plain(processions.checklistProcession)(steps, 9)
      . assert(_ == t"✓ resolve\n⠋ compile\n· publish")

      test(m"a numbered procession counts the steps that have started"):
        plain(processions.numberedProcession)(steps, 14)
      . assert(_ == t"[2/3] compile ")

      test(m"a bead procession is a chain of two cells per step, less one"):
        plain(processions.beadProcession)(steps, 5)
      . assert(_ == t"●━◐━○")

      test(m"a breadcrumb procession joins the steps on one row"):
        plain(processions.breadcrumbProcession)(steps, 29)
      . assert(_ == t"resolve › compile › publish  ")

      test(m"a checklist declares an animation period; a breadcrumb does not"):
        (processions.checklistProcession.period, processions.breadcrumbProcession.period)
      . assert(_ == (80, Unset))

    suite(m"Elapsed time and countdowns"):
      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      // Monomorphic, as elsewhere in these suites: a generic helper infers the underlying
      // `Duration` from a literal, because the opaque types are transparent in this package.
      def spent(design: Duration is Gaugeable)(value: Duration, width: Int): Text =
        design.rows(value, Tick.zero, width).stdlib.map(_.plain).mkString("\n").tt

      def left(design: Countdown is Gaugeable)(value: Countdown, width: Int): Text =
        design.rows(value, Tick.zero, width).stdlib.map(_.plain).mkString("\n").tt

      test(m"a compact elapsed time gives the two largest useful units"):
        spent(timers.compactElapsed)(161.0*Second, 5)
      . assert(_ == t"2m41s")

      test(m"a compact elapsed time under a minute is just seconds"):
        spent(timers.compactElapsed)(41.0*Second, 3)
      . assert(_ == t"41s")

      test(m"a digital elapsed time keeps its shape as it crosses a minute"):
        (spent(timers.digitalElapsed)(59.0*Second, 5),
            spent(timers.digitalElapsed)(61.0*Second, 5))
      . assert(_ == (t"00:59", t"01:01"))

      test(m"a digital elapsed time grows an hours field only when there are hours"):
        spent(timers.digitalElapsed)(3661.0*Second, 7)
      . assert(_ == t"1:01:01")

      // A countdown is clamped at zero, so a deadline that has passed reads as `0s`.
      test(m"a countdown past its deadline reads as zero"):
        left(timers.compactCountdown)(Countdown(-5.0*Second), 2)
      . assert(_ == t"0s")

      test(m"a narrow timer keeps the seconds, which are what is moving"):
        spent(timers.compactElapsed)(161.0*Second, 3)
      . assert(_ == t"41s")

      test(m"a timer is inelastic, so it does not stretch across a row"):
        timers.compactElapsed.columns(161.0*Second)
      . assert(_ == 5)

      test(m"neither timer animates: they change only when their reading does"):
        (timers.compactElapsed.period, timers.urgentCountdown.period)
      . assert(_ == (Unset, Unset))

    suite(m"Captioned gauges"):
      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      def plain[status](design: status is Gaugeable)(value: status, width: Int): Text =
        design.rows(value, Tick.zero, width).stdlib.map(_.plain).mkString("\n").tt

      test(m"a caption follows the gauge it labels"):
        import bars.blockBar
        plain(summon[Captioned[Fraction] is Gaugeable])(Captioned(Fraction(0.5), t"copying"), 12)
      . assert(_ == t"██░░░ copyi…")

      // Both degradations compose: the caption is cut to its half-row allowance, and the three
      // cells that leaves the bar are too few to draw one, so it falls through to a figure.
      test(m"a narrow captioned gauge elides the label and degrades the bar"):
        import bars.blockBar
        plain(summon[Captioned[Fraction] is Gaugeable])(Captioned(Fraction(0.5), t"copying"), 8)
      . assert(_ == t"50% cop…")

      test(m"a leading caption precedes the gauge"):
        import bars.blockBar
        import captions.leadingCaption
        plain(summon[Captioned[Fraction] is Gaugeable])(Captioned(Fraction(0.5), t"go"), 8)
      . assert(_ == t"go ██░░░")

      test(m"a captioned spinner inherits the spinner's animation period"):
        import spinners.brailleDotsSpinner
        summon[Captioned[Optional[Fraction]] is Gaugeable].period
      . assert(_ == 80)

    suite(m"The form's frame clock"):
      import gaugeGlyphs.unicodeGlyphs
      import palettes.emberGaugePalette
      import textMetrics.uniformMetric

      // `scheduleWake` is a constructor parameter, so the ticker is tested by recording what it is
      // asked to schedule — no sleeping, and nothing timing-dependent.
      def wakes(pane: Pane): List[Long] =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basicTermcap)
        val recorded = scala.collection.mutable.ListBuffer[Long]()
        val root = ScreenRoot(20, 3)
        Form(root, Occupancy.Fullscreen, pane, () => (), 0, 0, delay => { recorded += delay; () })
        . run(scala.Iterator(Keypress.Escape))

        recorded.toList.to(List)

      test(m"a layout containing a spinner arms a wake at the design's period"):
        given definite: (Fraction is Gaugeable) = spinners.brailleDotsSpinner
        wakes(gauge(Reading(Fraction.indeterminate))).stdlib.headOption
      . assert(_ == Some(80L))

      test(m"a layout of static panels arms no wake at all"):
        wakes(panel()(Out.print(t"still"))).stdlib.length
      . assert(_ == 0)

      test(m"a bar alone does not animate"):
        wakes(gauge(Reading(Fraction(0.5)))(using bars.smoothBar)).stdlib.length
      . assert(_ == 0)

      test(m"the shortest period wins when several gauges animate"):
        val fast = gauge(Reading(Fraction(0.0)))(using spinners.starSpinner)
        val slow = gauge(Reading(Fraction(0.0)))(using spinners.toggleSpinner)
        wakes(stack(fast, slow)).stdlib.headOption
      . assert(_ == Some(70L))

// A test-only root `Board` that paints into a fixed in-memory grid but reports a
// settable size, so a layout can be re-tiled to a smaller `width`/`height` and
// the composed screen read back.
class ResizableRoot(maxWidth: Int, maxHeight: Int)(using Stdio) extends Board:
  private val flow = FlowExtent(TerminalBoard(maxWidth, maxHeight), Rect(0, 0, maxWidth, maxHeight))
  @scala.caps.unsafe.untrackedCaptures
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
