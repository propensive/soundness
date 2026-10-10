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
package exoskeleton

import java.lang as jl

import scala.caps

import soundness.*

import errorDiagnostics.stackTracesDiagnostics

object Pane:
  // The keys a test may name, as tmux names them, and the bytes a terminal sends for each. A
  // pane given any other text types it literally.
  val keys: Map[Text, Text] =
    Map
      ( t"Left"   -> t"\e[D",
        t"Right"  -> t"\e[C",
        t"Up"     -> t"\e[A",
        t"Down"   -> t"\e[B",
        t"BSpace" -> t"\u007f",
        t"Enter"  -> t"\r",
        t"Tab"    -> t"\t",
        t"Escape" -> t"\e",
        t"C-m"    -> t"\r",
        t"C-l"    -> t"\u000c",
        t"C-c"    -> t"\u0003" )

  def enter(keypresses: (Text | Char)*)(using pane: Pane)(using Tactic[Pane.Error]): Unit =
    keypresses.foreach:
      case text: Text => pane.press(text)
      case char: Char => pane.press(char.show)
      case _          => panic(m"unreachable case")

  def resize(width: Int, height: Int)(using pane: Pane)(using Tactic[Pane.Error]): Unit =
    pane.resize(width, height)

  def screenshot()(using pane: Pane)(using Tactic[Pane.Error]): Screenshot = pane.screenshot()

  // Whether some line of the screen satisfies `predicate` within `timeout` milliseconds.
  def waitFor(predicate: Text => Boolean, timeout: Long = 20000L)
    ( using Pane, Monitor, Tactic[Pane.Error] )
  :   Boolean =

    val deadline = jl.System.currentTimeMillis + timeout

    def recur(): Boolean =
      if screenshot().screen.readable.exists(predicate) then true
      else if jl.System.currentTimeMillis >= deadline then false
      else
        sleep(0.01*Second)
        recur()

    recur()

  // Types `keypresses`, then waits for the screen to change, and then to settle: a shell
  // completing a line asks the tool, through its daemon, and may draw what it hears in several
  // steps. The screen has settled once it has looked the same three times running, 20ms apart; if
  // it does not change at all within five seconds, the keys are taken to have changed nothing.
  // Taking the keys, rather than a block which types them, keeps the pane from being both an
  // argument and a capture of the same call, which the separation checker would reject.
  def attend(keypresses: (Text | Char)*)(using pane: Pane)(using Monitor, Tactic[Pane.Error])
  :   Unit =

    val init = screenshot().screen
    enter(keypresses*)
    val deadline = jl.System.currentTimeMillis + 5000L

    def change(): Boolean =
      if init !== screenshot().screen then true
      else if jl.System.currentTimeMillis >= deadline then false
      else
        sleep(0.01*Second)
        change()

    if change() then settle()

  // Waits until the screen has looked the same three times running, 20ms apart, or five seconds
  // have passed.
  def settle()(using pane: Pane)(using Monitor, Tactic[Pane.Error]): Unit =
    val deadline = jl.System.currentTimeMillis + 5000L

    def recur(previous: Array[Text]^{}, count: Int): Unit =
      if count < 3 && jl.System.currentTimeMillis < deadline then
        sleep(0.02*Second)
        val current = screenshot().screen
        recur(current, if current === previous then count + 1 else 0)

    recur(screenshot().screen, 0)


  def completions(text: Text)(using tool: Enclave.Tool, pane: Pane)
    ( using Monitor, Tactic[Pane.Error] )
  :   Text =

    pane.shell match
      case Shell.Powershell =>
        enter(t"""_completions "$text"""")
        attend('\r')
        waitFor(_ == t">", 10000L)

        // A named method, not a lambda: interpolating inside a lambda passed to a collection
        // combinator runs the interpolator's implicit search while the combinator's element type
        // is still uninstantiated, tripping dotc's `wildApprox` assertion (scala/scala3#24824).
        def described(line: Text): List[Text] = line.cut(t"@@") match
          case name :: desc :: Nil => List(t"$name  ($desc)")
          case name :: Nil         => List(name)
          case _                   => Nil

        val lines: List[Text] = screenshot().screen.to[List]
        val shown: List[Text] = lines.filter(!_.starts(t">"))
        val trimmed: List[Text] = shown.map(_.trim).filter(_.length > 0)

        trimmed.bind(described).join(t"  ")

      case _ =>
        enter(tool.command)
        enter(' ')
        enter(text)
        attend(Ht)
        screenshot().screen.filter(!_.starts(t"> ")).readable.toSeq.join(t"\n").trim


  def progress(text: Text, decorate: Char => Text = char => t"^")
    ( using tool: Enclave.Tool, pane: Pane )
    ( using Monitor, Tactic[Pane.Error] )
  :   Text =

    enter(tool.command)
    enter(' ')
    enter(text)

    pane.shell match
      case Shell.Powershell =>
        sleep(0.05*Second)
        val init = screenshot().screen
        enter(Ht)
        val deadline = jl.System.currentTimeMillis + 2000L

        def changed(): Boolean =
          if init !== screenshot().screen then true
          else if jl.System.currentTimeMillis >= deadline then false
          else
            sleep(0.01*Second)
            changed()

        // Once the screen has changed, it is read again only when it has held still for three
        // consecutive looks, since PowerShell redraws its line in several steps.
        def stable(previous: Array[Text]^{}, count: Int): Unit =
          if count < 3 && jl.System.currentTimeMillis < deadline then
            sleep(0.01*Second)
            val current = screenshot().screen
            stable(current, if current === previous then count + 1 else 0)

        if changed() then stable(screenshot().screen, 0)

      case _ =>
        attend(Ht)

    screenshot().currentLine(decorate).sub(t"> ${tool.command} ", t"")

  // What a pseudo-terminal shows, rendered by yossarian as its output arrives, and yossarian's
  // answers to the queries a shell makes of its terminal — the cursor's position, the terminal's
  // attributes — written back to it. The tasks which read the output and write the answers retain
  // the display on their own threads, so it is durable, and holds its state in atomic cells. A
  // sequence yossarian cannot render is kept, to be reported by the next `screenshot`, rather than
  // lost on the task which reads the output.
  class Display(columns: Int, rows: Int) extends anticipation.Durable:
    private val terminal: Atomic.Ref[yossarian.Pty] = Atomic.Ref(yossarian.Pty(columns, rows))
    private val problem: Atomic.Ref[Optional[Text]] = Atomic.Ref(Unset)
    private val size: Atomic.Ref[(Int, Int)] = Atomic.Ref((columns, rows))

    // Bytes of a UTF-8 character split between chunks, held until the rest of it arrives.
    private val partial: Atomic.Ref[scala.IArray[Byte]] = Atomic.Ref(scala.IArray[Byte]())

    def width: Int = size()(0)
    def height: Int = size()(1)

    // Writes to the terminal, from the test and from the task answering queries, one at a time.
    def send(job: Command.OnTerminal.Job[Label], bytes: Data)(using Tactic[Pane.Error]): Unit =
      synchronized:
        mitigate:
          case Truncation.Error(_) => Pane.Error(Pane.Error.Reason.SessionDied)

        . protect(bytes.writeTo(job))

    def resize(width: Int, height: Int): Unit =
      size() = (width, height)
      terminal() = yossarian.Pty(width, height)

    def screenshot()(using Tactic[Pane.Error]): Screenshot =
      problem().let: problem =>
        abort(Pane.Error(Pane.Error.Reason.Unrendered(problem)))

      val pty = terminal()

      val lines = pty.buffer.render.cut(t"\n").map: line =>
        Text(line.s.replaceAll(" +$", "").nn)

      val cursor = pty.cursor.n0
      Screenshot(lines.to[Array], (width, height), ((cursor%width).z, (cursor/width).z))

    // Renders a chunk of the terminal's output. The longest prefix of the bytes which ends on a
    // character boundary is decoded; the rest waits for the next chunk.
    def render(chunk: Data): Unit =
      val bytes = partial() ++ chunk.readable
      val complete = Pane.boundary(bytes)
      partial() = bytes.drop(complete)
      val text = Text(String(scala.IArray.genericWrapArray(bytes.take(complete)).toArray, "UTF-8"))

      try terminal() = terminal().consume(text)(using strategies.throwUnsafely)
      catch case error: yossarian.Pty.Error => problem() = error.message.text

    // Writes yossarian's answers to the shell's queries back to the shell, until `finish`.
    def answer(job: Command.OnTerminal.Job[Label])(using Tactic[Pane.Error]): Unit =
      terminal().output.chain.each: (reply: Text) =>
        send(job, reply.sysData)

    def finish(): Unit = terminal().output.stop()

  // A pane on a pseudo-terminal, showing what `display` renders of it.
  class Emulated(val shell: Shell, display: Display, job: Command.OnTerminal.Job[Label])
  extends Pane:
    def width: Int = display.width
    def height: Int = display.height
    def screenshot()(using Tactic[Pane.Error]): Screenshot = display.screenshot()

    def press(keys: Text)(using Tactic[Pane.Error]): Unit =
      display.send(job, Pane.keys.at(keys).or(keys).sysData)

    def resize(width: Int, height: Int)(using Tactic[Pane.Error]): Unit =
      display.resize(width, height)
      job.resize(width, height)

  // The length of the longest prefix of `bytes` which ends on a UTF-8 character boundary: a lead
  // byte among the last three whose character runs past the end begins an incomplete character.
  def boundary(bytes: scala.IArray[Byte]): Int =
    def recur(index: Int): Int =
      if index < 0 || index < bytes.length - 3 then bytes.length else
        val byte = bytes(index) & 0xff

        if (byte & 0xc0) == 0x80 then recur(index - 1)
        else
          val length =
            if byte >= 0xf0 then 4 else if byte >= 0xe0 then 3 else if byte >= 0xc0 then 2 else 1

          if index + length > bytes.length then index else bytes.length

    recur(bytes.length - 1)

  object Error:
    enum Reason(val number: Int):
      case ShellNotInstalled(shell: Text) extends Reason(1)
      case SessionDied                    extends Reason(2)
      case ExecFailed                     extends Reason(3)
      case Unrendered(problem: Text)      extends Reason(4)
      case NotReady(shell: Text)          extends Reason(5)
      case Unconfigured(problem: Text)    extends Reason(6)

    given communicable: Reason is Communicable =
      case Reason.ShellNotInstalled(shell) => m"the shell binary `$shell` is not installed"
      case Reason.SessionDied              => m"the terminal session terminated unexpectedly"
      case Reason.ExecFailed               => m"could not start the terminal session"
      case Reason.Unrendered(problem)      => m"the terminal could not render its output: $problem"
      case Reason.NotReady(shell)          => m"the shell `$shell` did not become ready"
      case Reason.Unconfigured(problem)    => m"the shell could not be configured: $problem"

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(271, reason.number)(m"can't drive the terminal: $reason")

// A terminal pane in which a shell runs for a test: keys are typed at it, and its screen is read.
// `Shell.pane` runs the shell on a pseudo-terminal, whose output yossarian renders; `Shell.tmux`
// runs it in a tmux session, an independent terminal emulator, against which yossarian's
// rendering is compared. A pane is a capability, whose lifetime is the block that creates it.
// `Exclusive` because a pane is driven by one test at a time.
trait Pane extends caps.ExclusiveCapability:
  def shell: Shell
  def width: Int
  def height: Int

  // Types `keys` literally or, if it names a key in `Pane.keys`, presses that key.
  def press(keys: Text)(using Tactic[Pane.Error]): Unit
  def screenshot()(using Tactic[Pane.Error]): Screenshot
  def resize(width: Int, height: Int)(using Tactic[Pane.Error]): Unit

case class Screenshot(screen: Array[Text]^{}, size: (Int, Int), cursor: (Ordinal, Ordinal)):
  def apply(): Text = screen.readable.toSeq.join(t"\n")

  def currentLine(decorate: Char => Text): Text =
    val line0 = screen.at(cursor(1)).or(t"")
    val line = line0+t" "*(size(0) - line0.length)

    t"${line.before(cursor(0))}${line(cursor(0)).let(decorate).or(t"?")}${line.from(cursor(0))}"
    . trim
