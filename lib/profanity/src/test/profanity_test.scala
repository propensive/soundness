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
package profanity

import java.lang as jl

import soundness.*

import proscenium.compat.*

import classloaders.systemClassloader
import environments.javaEnvironment
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import workingDirectories.defaultWorkingDirectory
import logging.silentLogging
import threading.platformThreading

import strategies.throwUnsafely
import backstops.silentBackstop
import probates.cancelProbate

import Shell.*

object Tests extends Suite(m"Profanity Tests"):
  def run(): Unit =
    CaptureTests()

    supervise:
      val launcher = Enclave(t"profanity-fixture").dispatch:
        ' {
            import executives.completions
            import interpreters.posixInterpreter
            import probates.cancelProbate

            cli:
              arguments match
                case Argument("echo") :: Nil =>
                  execute:
                    interactive: terminal ?=>
                      Out.println(t"READY")
                      val iter = terminal.eventIterator()
                      var done = false
                      while !done && iter.hasNext do iter.next() match
                        case Keypress.CharKey(c) =>
                          Out.println(t"GOT:$c")
                          done = true
                        case _ =>
                      Exit.Ok

                case Argument("line-editor") :: Nil =>
                  execute:
                    interactive: terminal ?=>
                      Out.println(t"READY")
                      LineEditor().ask: result =>
                        Out.println(t"RESULT:$result")
                      Exit.Ok

                case Argument("line-editor-sized") :: Argument(w) :: Argument(h) :: Nil =>
                  execute:
                    interactive: terminal ?=>
                      terminal.columns = w.as[Int]
                      terminal.rows = h.as[Int]
                      Out.println(t"READY")
                      LineEditor().ask: result =>
                        Out.println(t"RESULT:$result")
                      Exit.Ok

                case Argument("select-menu") :: Nil =>
                  execute:
                    interactive: terminal ?=>
                      Out.println(t"READY")
                      SelectMenu(List(t"alpha", t"beta", t"gamma"), t"alpha").ask: result =>
                        Out.println(t"RESULT:$result")
                      Exit.Ok

                case Argument("select-menu-long-sized") :: Argument(w) :: Argument(h) :: Nil =>
                  execute:
                    interactive: terminal ?=>
                      terminal.columns = w.as[Int]
                      terminal.rows = h.as[Int]
                      Out.println(t"READY")
                      val opts = List(t"first", t"averyverylongoptionnamethatwraps", t"third")
                      SelectMenu(opts, t"first").ask: result =>
                        Out.println(t"RESULT:$result")
                      Exit.Ok

                case _ =>
                  execute(Exit.Fail(1))

            t"finished"
          }

      def waitFor(text: Text, ms: Int = 5000)(using Tmux, Monitor, WorkingDirectory): Boolean =
        def matches: Boolean = Tmux.screenshot().screen.toList.exists(_.contains(text))
        // Bound the wait against a wall-clock deadline rather than counting fixed
        // 50 ms iterations: each poll spawns a `tmux` screenshot subprocess whose
        // latency varies wildly under load, so an iteration count made the real
        // timeout unbounded (a miss could cost tens of seconds under `make
        // attest`). A real deadline keeps a missed marker cheap and predictable.
        val deadline = jl.System.currentTimeMillis + ms
        var found = matches
        while !found && jl.System.currentTimeMillis < deadline do
          delay(0.05*Second)
          found = matches
        found

      def runFixture(arg: Text, marker: Text = t"RESULT:")(input: Tmux ?=> Unit)
        ( using Enclave.Tool, Monitor, WorkingDirectory, TemporaryDirectory )
      :   Text =

        Bash.tmux():
          val tool = summon[Enclave.Tool].command
          Tmux.enter(tool, ' ', arg)
          Tmux.enter('\r')
          if !waitFor(t"READY") then panic(m"profanity fixture did not become ready")
          input
          // Wait only for the marker this fixture actually prints. The `echo`
          // fixture emits `GOT:` and never `RESULT:`, so the old `RESULT:`-first
          // probe always burnt the full timeout before falling back to `GOT:`.
          waitFor(marker)
          Tmux.screenshot().screen.join(t"\n")

      launcher.sandbox:
        // Warmup run to spawn the daemon and avoid timing flake on the first real test
        runFixture(t"echo", marker = t"GOT:"):
          Tmux.enter('a')

        suite(m"Line buffering"):
          test(m"a single keypress reaches the app before Enter is pressed"):
            runFixture(t"echo", marker = t"GOT:"):
              Tmux.enter('a')
          . assert(_.contains(t"GOT:a"))

        suite(m"LineEditor"):
          test(m"submits accumulated text on Enter"):
            runFixture(t"line-editor"):
              Tmux.enter("hello")
              Tmux.enter('\r')
          . assert(_.contains(t"RESULT:hello"))

          test(m"backspace removes characters"):
            runFixture(t"line-editor"):
              Tmux.enter("helXX")
              Tmux.enter('', '')
              Tmux.enter("lo")
              Tmux.enter('\r')
          . assert(_.contains(t"RESULT:hello"))

          // Aspirational because, under Ethereal's daemon model, the socket round-trip
          // between consecutive bytes of \e[D can exceed Profanity's 30 ms ESC timeout in
          // Keyboard.process, which dismisses the widget before the arrow code completes.
          // The state-transition suite below covers Left-arrow handling deterministically.
          test(m"Left arrow moves the cursor"):
            runFixture(t"line-editor"):
              Tmux.enter("helo")
              Tmux.enter(t"Left")
              Tmux.enter("l")
              Tmux.enter('\r')
          . aspire(_.contains(t"RESULT:hello"))

          // Wrap-aware redraw: typing past the terminal width and then backspacing back
          // across the wrap boundary must clear the wrapped row and reposition the cursor.

          test(m"submits correct text after wrap and backspace"):
            Bash.tmux(width = 20, height = 10):
              val tool = summon[Enclave.Tool].command
              Tmux.enter(tool, ' ', t"line-editor-sized 20 10")
              Tmux.enter('\r')
              if !waitFor(t"READY") then panic(m"profanity fixture did not become ready")
              Tmux.enter(t"X"*25)
              Tmux.enter('', '', '', '', '')
              Tmux.enter('\r')
              waitFor(t"RESULT:")
              Tmux.screenshot().screen.toList.join
          . assert(_.contains(t"RESULT:${t"X"*20}"))

          test(m"backspace clears characters wrapped onto the next visual line"):
            Bash.tmux(width = 20, height = 10):
              val tool = summon[Enclave.Tool].command
              Tmux.enter(tool, ' ', t"line-editor-sized 20 10")
              Tmux.enter('\r')
              if !waitFor(t"READY") then panic(m"profanity fixture did not become ready")
              Tmux.attend(Tmux.enter(t"X"*25))
              delay(0.1*Second)
              Tmux.attend:
                Tmux.enter('', '', '', '', '')
              delay(0.2*Second)
              val mid = Tmux.screenshot()
              Tmux.enter('\r')
              waitFor(t"RESULT:")
              mid.screen.toList.map(_.count(_ == 'X')).sum
          . assert(_ == 20)

          // SelectMenu wrap-aware redraw: an option longer than the terminal width must
          // occupy the right number of visual rows when computing the move-back-to-anchor.
          // Capture the menu immediately after it appears (before any arrow-key navigation,
          // which is flaky under daemon-mode ESC timing) and confirm the wrapped option's
          // tail appears exactly once on screen.
          test(m"select-menu draws a wrapping option without ghost rows"):
            Bash.tmux(width = 20, height = 12):
              val tool = summon[Enclave.Tool].command
              Tmux.enter(tool, ' ', t"select-menu-long-sized 20 12")
              Tmux.enter('\r')
              if !waitFor(t"READY") then panic(m"profanity fixture did not become ready")
              delay(0.3*Second)
              val mid = Tmux.screenshot()
              Tmux.enter('\r')
              waitFor(t"RESULT:")
              // The third option ("third") must appear exactly once. If the renderer
              // miscounts visual rows for the wrapped second option, the menu drifts
              // on subsequent re-renders and stale copies of "third" pile up.
              mid.screen.toList.count(_.contains(t"third"))
          . assert(_ == 1)

      // Pure state-transition tests, bypassing terminal IO. These exercise the
      // Iterator[TerminalEvent] -> recur path with synthetic events and a no-op
      // renderer, so they validate the widget logic without daemon-mode IO
      // latency that affects ESC-sequence parsing.

      val noopMenu = new Interaction[Text, SelectMenu[Text]]:
        def render(old: Optional[SelectMenu[Text]], menu: SelectMenu[Text]): Unit = ()
        def result(menu: SelectMenu[Text]): Text = menu.current

      val noopEditor = new Interaction[Text, LineEditor]:
        def render(old: Optional[LineEditor], editor: LineEditor): Unit = ()
        def result(editor: LineEditor): Text = editor.value

      def selected(events: TerminalEvent*): Optional[Text] =
        val menu = SelectMenu(List(t"alpha", t"beta", t"gamma"), t"alpha")
        noopMenu(events.iterator, menu)(_(_))

      def edited(events: TerminalEvent*): Optional[Text] =
        noopEditor(events.iterator, LineEditor())(_(_))

      // A no-op interaction whose submit decision follows the editor's own mode.
      val multilineEditor = new Interaction[Text, LineEditor]:
        def render(old: Optional[LineEditor], editor: LineEditor): Unit = ()
        def result(editor: LineEditor): Text = editor.value

        override def submits(event: TerminalEvent, editor: LineEditor): Boolean =
          editor.submitsOn(event)

      def editedWith(editor: LineEditor)(events: TerminalEvent*): Optional[Text] =
        multilineEditor(events.iterator, editor)(_(_))

      val shiftSubmit: LineEditor = LineEditor(mode = LineEditor.Mode.Multiline(_ => false))

      suite(m"SelectMenu state transitions"):
        test(m"Enter selects the current item"):
          selected(Keypress.Enter)
        . assert(_ == t"alpha")

        test(m"Down then Enter selects the next item"):
          selected(Keypress.Down, Keypress.Enter)
        . assert(_ == t"beta")

        test(m"Down twice then Enter selects gamma"):
          selected(Keypress.Down, Keypress.Down, Keypress.Enter)
        . assert(_ == t"gamma")

        test(m"Down past the end clamps at gamma"):
          selected(Keypress.Down, Keypress.Down, Keypress.Down, Keypress.Down, Keypress.Enter)
        . assert(_ == t"gamma")

        test(m"Up before alpha clamps at alpha"):
          selected(Keypress.Up, Keypress.Enter)
        . assert(_ == t"alpha")

        test(m"End jumps to the last item"):
          selected(Keypress.End, Keypress.Enter)
        . assert(_ == t"gamma")

        test(m"Home returns to the first item"):
          selected(Keypress.End, Keypress.Home, Keypress.Enter)
        . assert(_ == t"alpha")

        test(m"Escape dismisses without a result"):
          selected(Keypress.Escape).absent
        . assert(_ == true)

        test(m"Ctrl+C dismisses without a result"):
          selected(Keypress.Ctrl('C')).absent
        . assert(_ == true)

      suite(m"LineEditor state transitions"):
        test(m"Typed characters accumulate"):
          edited(Keypress.CharKey('h'), Keypress.CharKey('i'), Keypress.Enter)
        . assert(_ == t"hi")

        test(m"cursorPosition counts a wide grapheme as two columns"):
          LineEditor.cursorPosition(t"中b", 2, 10)
        . assert(_ == (0, 3))

        test(m"cursorPosition wraps a wide grapheme that would straddle the edge"):
          LineEditor.cursorPosition(t"ab中", 3, 3)
        . assert(_ == (1, 2))

        test(m"Backspace removes the previous character"):
          edited
            ( Keypress.CharKey('h'), Keypress.CharKey('i'), Keypress.CharKey('x'),
              Keypress.Backspace, Keypress.Enter )
        . assert(_ == t"hi")

        test(m"Left arrow then a character inserts at the cursor"):
          edited
            ( Keypress.CharKey('h'), Keypress.CharKey('i'), Keypress.Left,
              Keypress.CharKey('e'), Keypress.Enter )
        . assert(_ == t"hei")

        test(m"Home then Delete removes the first character"):
          edited
            ( Keypress.CharKey('h'), Keypress.CharKey('i'), Keypress.Home, Keypress.Delete,
              Keypress.Enter )
        . assert(_ == t"i")

        test(m"End jumps to the end of the line"):
          edited
            ( Keypress.CharKey('a'), Keypress.CharKey('b'), Keypress.Home, Keypress.End,
              Keypress.CharKey('c'), Keypress.Enter )
        . assert(_ == t"abc")

        test(m"Ctrl+U deletes from cursor to start of line"):
          edited
            ( Keypress.CharKey('h'), Keypress.CharKey('i'), Keypress.Ctrl('U'), Keypress.Enter )
        . assert(_ == t"")

        test(m"Enter inserts a newline and Shift+Enter submits"):
          editedWith(shiftSubmit)
            ( Keypress.CharKey('a'), Keypress.Enter, Keypress.CharKey('b'),
              profanity.Keypress.Shift(profanity.Keypress.Enter) )
        . assert(_ == t"a\nb")

        test(m"the up arrow moves the cursor to the previous line"):
          editedWith(shiftSubmit)
            ( Keypress.CharKey('a'), Keypress.Enter, Keypress.CharKey('b'), Keypress.Up,
              Keypress.CharKey('X'), profanity.Keypress.Shift(profanity.Keypress.Enter) )
        . assert(_ == t"aX\nb")

        test(m"the down arrow moves the cursor to the next line"):
          editedWith(shiftSubmit)
            ( Keypress.CharKey('a'), Keypress.Enter, Keypress.CharKey('b'), Keypress.Home,
              Keypress.Up, Keypress.Down, Keypress.CharKey('Y'),
              profanity.Keypress.Shift(profanity.Keypress.Enter) )
        . assert(_ == t"a\nYb")

        test(m"a content predicate decides whether Enter submits or inserts a newline"):
          editedWith(LineEditor(mode = LineEditor.Mode.Multiline(_.contains(t"!"))))
            ( Keypress.CharKey('a'), Keypress.Enter, Keypress.CharKey('!'), Keypress.Enter )
        . assert(_ == t"a\n!")

        test(m"react can transform the editor state (e.g. completing on Tab)"):
          val completingEditor = new Interaction[Text, LineEditor]:
            def render(old: Optional[LineEditor], editor: LineEditor): Unit = ()
            def result(editor: LineEditor): Text = editor.value

            override def react(editor: LineEditor, event: TerminalEvent): LineEditor =
              event match
                case Keypress.Tab => LineEditor(t"${editor.value}ography")
                case _            => editor

          completingEditor
           ( Progression(Keypress.CharKey('g'), Keypress.CharKey('e'), Keypress.Tab,
                    Keypress.Enter).iterator,
             LineEditor() )
           (_(_))
        . assert(_ == t"geography")

    suite(m"Keyboard decoding"):
      test(m"Shift+Enter is decoded from its CSI-u sequence"):
        supervise:
          Keyboard.Standard().process(Progression('', '[', '1', '3', ';', '2', 'u')).head
      . assert:
          case Keypress.Shift(Keypress.Enter) => true
          case _                              => false

      test(m"plain Enter is decoded from its CSI-u sequence"):
        supervise:
          Keyboard.Standard().process(Progression('', '[', '1', '3', 'u')).head
      . assert:
          case Keypress.Enter => true
          case _              => false

      test(m"Escape is decoded from its CSI-u sequence"):
        supervise:
          Keyboard.Standard().process(Progression('', '[', '2', '7', 'u')).head
      . assert:
          case Keypress.Escape => true
          case _               => false

      test(m"Ctrl+C is decoded from its CSI-u sequence"):
        supervise:
          Keyboard.Standard().process(Progression('', '[', '9', '9', ';', '5', 'u')).head
      . assert:
          case Keypress.Ctrl('C') => true
          case _                  => false

      test(m"a plain letter is decoded from its CSI-u sequence"):
        supervise:
          Keyboard.Standard().process(Progression('', '[', '9', '7', 'u')).head
      . assert:
          case Keypress.CharKey('a') => true
          case _                     => false

      // The plain cursor-position report is a size-probe reply; the pump reclassifies
      // it as an anchor reply when the resize trap queued one.
      test(m"a plain CPR decodes to a WindowSize"):
        supervise:
          Keyboard.Standard().process(Progression('', '[', '1', '2', ';', '3', '4', 'R')).head
      . assert:
          case TerminalInfo.WindowSize(12, 34) => true
          case _                               => false

      // The `?`-prefixed DECXCPR form is unambiguous: it can only be a cursor position.
      test(m"a DECXCPR reply decodes to a CursorPosition"):
        supervise:
          Keyboard.Standard()
          . process(Progression('', '[', '?', '1', '2', ';', '3', '4', 'R'))
          . head
      . assert:
          case TerminalInfo.CursorPosition(12, 34) => true
          case _                                   => false

      // DECXCPR at VT level 4 appends the page number, which is ignored.
      test(m"a three-field DECXCPR reply decodes, dropping the page"):
        supervise:
          Keyboard.Standard()
          . process(Progression('', '[', '?', '1', '2', ';', '3', '4', ';', '1', 'R'))
          . head
      . assert:
          case TerminalInfo.CursorPosition(12, 34) => true
          case _                                   => false

      test(m"a malformed report is dropped and the stream continues"):
        supervise:
          Keyboard.Standard().process(Progression('', '[', '?', ';', 'R', 'z')).head
      . assert:
          case Keypress.CharKey('z') => true
          case _                     => false

      suite(m"Terminal features"):
        test(m"the kitty keyboard feature pushes the protocol on"):
          terminalFeatures.kittyKeyboardFeature.enable
        . assert(_ == t"\e[>1u")

        test(m"a query feature has an empty disable sequence"):
          terminalFeatures.backgroundColorFeature.disable
        . assert(_ == t"")

        test(m"a by-name imported feature is collected by Every"):
          import terminalFeatures.kittyKeyboardFeature
          summon[Every[TerminalFeature]].values.map(_.enable)
        . assert(_.contains(t"\e[>1u"))

      suite(m"Keypress rendering"):
        def rendered(keypress: profanity.Keypress): Text = keypress.show

        test(m"a modifier and a special key render with bracketed symbols"):
          rendered(profanity.Keypress.Shift(profanity.Keypress.Enter))
        . assert(_ == t"[⇧]+[↵]")

        test(m"a control-letter brackets the letter"):
          rendered(profanity.Keypress.Ctrl('C'))
        . assert(_ == t"[⌃]+[C]")

        test(m"an ordinary character is bracketed"):
          rendered(profanity.Keypress.CharKey('a'))
        . assert(_ == t"[a]")

        test(m"nested modifiers are joined with plus"):
          rendered(profanity.Keypress.Ctrl(profanity.Keypress.Shift(profanity.Keypress.Enter)))
        . assert(_ == t"[⌃]+[⇧]+[↵]")

      suite(m"Signal POSIX numbering"):
        test(m"SIGHUP is 1")  (Signal.Hup.id)   .assert(_ == 1)
        test(m"SIGINT is 2")  (Signal.Int.id)   .assert(_ == 2)
        test(m"SIGQUIT is 3") (Signal.Quit.id)  .assert(_ == 3)
        test(m"SIGKILL is 9") (Signal.Kill.id)  .assert(_ == 9)
        test(m"SIGTERM is 15")(Signal.Term.id)  .assert(_ == 15)
        test(m"SIGCHLD is 17")(Signal.Chld.id)  .assert(_ == 17)
        test(m"SIGCONT is 18")(Signal.Cont.id)  .assert(_ == 18)
        test(m"SIGSTOP is 19")(Signal.Stop.id)  .assert(_ == 19)
        test(m"SIGSYS is 31") (Signal.Sys.id)   .assert(_ == 31)

        test(m"every signal id is positive"):
          scala.collection.immutable.ArraySeq.unsafeWrapArray(Signal.values).to(List).map(_.id).forall(_ > 0)
        . assert(identity(_))

        test(m"signal ids are distinct"):
          val ids = scala.collection.immutable.ArraySeq.unsafeWrapArray(Signal.values).to(List).map(_.id)
          ids.length == ids.distinct.length
        . assert(identity(_))
