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
package ultimatum

import java.io as ji

import soundness.*

object Tests extends Suite(m"Ultimatum Tests"):
  def run(): Unit =
    suite(m"TerminalCanvas"):
      // Capture everything a surface writes into an in-memory buffer.
      def captured(block: Stdio ?=> Unit): Text =
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)
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
        given Stdio = Stdio(null, null, null, termcapDefinitions.basic)
        FlowExtent(TerminalCanvas(width, height), Rect(0, 0, width, height))

      test(m"text wraps at the rectangle's width"):
        val flow = extent(3, 2)
        flow.put(t"abcdef")
        flow.render
      . assert(_ == t"abc\ndef")

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
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)
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
        frame.arrange(Rect(0, 0, 100, 1)).cells.map(_.width).foldLeft(0)(_ + _)
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
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)

        paint(TerminalCanvas(4, 1), file(panel()(Out.print(t"AA")), panel()(Out.print(t"BB"))))

        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1;1HAA\e[1;3HBB")

      test(m"a panel's output wraps and scrolls within its own rectangle, no bleed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)

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
        given Stdio = Stdio(null, null, null, termcapDefinitions.basic)
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
        given Stdio = Stdio(null, null, null, termcapDefinitions.basic)
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
        given Stdio = Stdio(null, null, null, termcapDefinitions.basic)
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
        (bytes, Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic))

      test(m"a first inline render draws each row with CR/LF and parks the caret"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 24)
        root.reframe(3, 2)
        root.move(Prim, Prim)
        root.put(t"hi")
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\r\e[2Khi \r\n\e[2K   \r\e[1A\e[1G\e[?25h")

      test(m"a growing block scrolls a new row in with LF"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 24)
        root.reframe(3, 1); root.move(Prim, Prim); root.put(t"a"); root.flush()
        bytes.reset()
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\r\e[2Kab \r\n\e[2Kcd \r\e[1A\e[1G\e[?25h")

      test(m"a shrinking block clears the rows it no longer uses"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 24)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.reframe(3, 1); root.move(Prim, Prim); root.put(t"ef"); root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\r\e[2Kef \r\n\e[2K\e[1A\e[1G\e[?25h")

      test(m"the caret is parked at its block-local position"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 24)
        root.reframe(3, 2)
        root.move(Prim, Prim)
        root.put(t"ab\ncd")
        root.showCaret(Sec, Sec)
        root.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[?25l\r\e[2Kab \r\n\e[2Kcd \r\e[2G\e[?25h")

      test(m"finish drops the cursor onto a fresh line below the block"):
        val (bytes, stdio) = capturing()
        given Stdio = stdio
        val root = InlineRoot(3, 24)
        root.reframe(3, 2); root.move(Prim, Prim); root.put(t"ab\ncd"); root.flush()
        bytes.reset()
        root.finish()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1B\r\n\e[?25h")

    suite(m"Dynamic panes"):
      def cell(): Pane = panel()(())

      test(m"append adds a pane at the end"):
        val a = cell()
        val b = cell()
        val panes = Panes(a)
        panes.append(b)
        panes.contents.to(List) == List(a, b)
      . assert(_ == true)

      test(m"prepend adds a pane at the start"):
        val a = cell()
        val b = cell()
        val panes = Panes(a)
        panes.prepend(b)
        panes.contents.to(List) == List(b, a)
      . assert(_ == true)

      test(m"insertBefore places a pane immediately before the reference"):
        val a = cell()
        val b = cell()
        val c = cell()
        val panes = Panes(a, b)
        panes.insertBefore(b, c)
        panes.contents.to(List) == List(a, c, b)
      . assert(_ == true)

      test(m"insertAfter places a pane immediately after the reference"):
        val a = cell()
        val b = cell()
        val c = cell()
        val panes = Panes(a, b)
        panes.insertAfter(a, c)
        panes.contents.to(List) == List(a, c, b)
      . assert(_ == true)

      test(m"remove deletes a pane by identity"):
        val a = cell()
        val b = cell()
        val panes = Panes(a, b)
        panes.remove(a)
        panes.contents.to(List) == List(b)
      . assert(_ == true)

      // Drive a running form, append a pane mid-loop (the synthetic iterator
      // yields a Redraw to wake it), and confirm the layout re-tiles to include it.
      test(m"a form picks up a pane appended while it runs"):
        given Stdio = Stdio(null, null, null, termcapDefinitions.basic)
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
        given Stdio = Stdio(null, null, null, termcapDefinitions.basic)
        FlowExtent(TerminalCanvas(12, 2), Rect(0, 0, 12, 2))

      def captured(block: Stdio ?=> Unit): Text =
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)
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
