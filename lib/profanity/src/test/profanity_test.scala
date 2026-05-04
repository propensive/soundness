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
package profanity

import soundness.*

import classloaders.system
import environments.java
import systems.java
import temporaryDirectories.system
import workingDirectories.default
import supervisors.global
import logging.silent
import threading.platform

import strategies.throwUnsafely
import backstops.silent

import Shell.*

object Tests extends Suite(m"Profanity Tests"):
  def run(): Unit =
    val launcher = Enclave(t"profanity-fixture").dispatch:
      ' {
          import executives.completions
          import interpreters.posix
          import codicils.cancel

          given BracketedPasteMode      = () => false
          given LuminosityDetection     = () => false
          given TerminalFocusDetection  = () => false
          given TerminalSizeDetection   = () => false

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

              case Argument("select-menu") :: Nil =>
                execute:
                  interactive: terminal ?=>
                    Out.println(t"READY")
                    SelectMenu(List(t"alpha", t"beta", t"gamma"), t"alpha").ask: result =>
                      Out.println(t"RESULT:$result")
                    Exit.Ok

              case _ =>
                execute(Exit.Fail(1))

          t"finished"
        }

    def waitFor(text: Text, ms: Int = 5000)(using Tmux, Monitor, WorkingDirectory): Boolean =
      def matches: Boolean = Tmux.screenshot().screen.toList.exists(_.contains(text))
      var elapsed = 0
      var found = matches
      while !found && elapsed < ms do
        delay(0.05*Second)
        elapsed += 50
        found = matches
      found

    def runFixture(arg: Text)(input: Tmux ?=> Unit)
      ( using Enclave.Tool, Monitor, WorkingDirectory, TemporaryDirectory )
    :   Text =

      Bash.tmux():
        val tool = summon[Enclave.Tool].command
        Tmux.enter(tool, ' ', arg)
        Tmux.enter('\r')
        if !waitFor(t"READY") then panic(m"profanity fixture did not become ready")
        input
        if !waitFor(t"RESULT:") then waitFor(t"GOT:")
        Tmux.screenshot().screen.join(t"\n")

    launcher.sandbox:
      // Warmup run to spawn the daemon and avoid timing flake on the first real test
      runFixture(t"echo"):
        Tmux.enter('a')

      suite(m"Line buffering"):
        test(m"a single keypress reaches the app before Enter is pressed"):
          runFixture(t"echo"):
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

        test(m"Left arrow moves the cursor"):
          runFixture(t"line-editor"):
            Tmux.enter("helo")
            Tmux.enter(t"Left")
            Tmux.enter("l")
            Tmux.enter('\r')
        . assert(_.contains(t"RESULT:hello"))

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
