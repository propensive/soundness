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

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import iridescence.*
import parasite.*
import rudiments.*
import turbulence.*
import zephyrine.*
import vacuous.*

object Terminal:
  // The escape sequence to query the terminal size; also used by `terminalSize`
  // (in `terminalFeatures`) and by the `Winch` (resize) signal handler below.
  def reportSize: Text = t"\e7\e[4095C\e[4095B\e[6n\e8"

  // In the companion (not the class) so the pure pump-daemon body can call it without
  // charging `Terminal.this`.
  private[profanity] def dark(red: Int, green: Int, blue: Int): Boolean =
    (0.299*red + 0.587*green + 0.114*blue) < 32768

  // The terminal's mutable size/brightness state, in a plain (untracked) holder rather than
  // fields of `Terminal` itself: the termcap's live `width`, and the pump daemon's updates,
  // then close over this holder instead of `Terminal.this` — which matters because the
  // daemon body must be a pure context function and `Termcap`/`Stdio` values must stay pure.
  private[profanity] class Metrics:
    var mode: Optional[Brightness] = Unset
    var rows: Optional[Int] = Unset
    var columns: Optional[Int] = Unset

// A `Terminal` is a *capability*: it holds the live raw-mode tty session — the `Monitor` and
// `Probate` of its input-pump daemon, the event spool, and mutable size/brightness state —
// whose lifetime is the `interactive` block that introduces it (raw mode and stdin are torn
// down in its `finally`). `Exclusive` because a tty session has a single owner; nothing may
// retain it past the teardown.
case class Terminal()
  ( using console: Console, monitor: Monitor, probate: Probate, environment: Environment )
extends Interactivity[TerminalEvent], caps.ExclusiveCapability:

  export console.stdio.{in, out, err}

  // ESC disambiguation consults the live reader — buffered input decides
  // instantly and only a quiet line waits briefly (see `Keyboard.Lookahead`)
  // — so the keyboard no longer runs `async` and retains no monitor.
  val keyboard: Keyboard.Standard =
    Keyboard.Standard()(using Keyboard.Lookahead.tty(console.stdio))

  private val metrics: Terminal.Metrics = Terminal.Metrics()

  metrics.mode = safely:
    def hex(text: Text): Int = Integer.parseInt(text.s, 16)

    Environment.terminalBg.cut(t"/").to(List) match
      case red :: green :: blue :: Nil =>
        if Terminal.dark(hex(red), hex(green), hex(blue)) then Brightness.Dark
        else Brightness.Light

      case _ =>
        abort(EnvironmentError(t"TERMINAL_BG"))

  metrics.rows = safely(Environment.lines.as[Int])
  metrics.columns = safely(Environment.columns.as[Int])

  def mode: Optional[Brightness] = metrics.mode
  def mode_=(value: Optional[Brightness]): Unit = metrics.mode = value
  def rows: Optional[Int] = metrics.rows
  def rows_=(value: Optional[Int]): Unit = metrics.rows = value
  def columns: Optional[Int] = metrics.columns
  def columns_=(value: Optional[Int]): Unit = metrics.columns = value

  // The fallbacks when the size is not yet known (no `LINES`/`COLUMNS` in the
  // environment and no size-probe reply yet): the classic 80×24. Conservative on
  // purpose — drawing too few rows is benign, while assuming a taller terminal than
  // real scatters absolutely-addressed output off-screen and into clamped garbage.
  def knownColumns: Int = columns.or(80)
  def knownRows: Int = rows.or(24)

  // `width` reads the live column count through the untracked `Metrics` holder (bound to a
  // block local, not read through `this`), so the termcap — and the stdio built on it —
  // stay pure, as `Stdio`'s `termcap` member requires.
  val cap: Termcap =
    val metrics0 = metrics

    new Termcap:
      def ansi: Boolean = true
      def color: ColorDepth = ColorDepth.TrueColor
      override def width: Int = metrics0.columns.or(80)

  given stdio: Stdio = new Stdio:
    val termcap = cap
    val out = console.stdio.out
    val err = console.stdio.err
    val in = console.stdio.in

  val events: Relay[TerminalEvent] = Relay()

  // A fresh single-owner drain of the shared event queue per call, batching
  // queued events into windows across the producer boundary. Sealed: the
  // `Interactivity` interface is pure-typed, exactly as `Spool.iterator` was
  // before it — the endpoint is reachable only through this iterator.
  def eventIterator(): Iterator[TerminalEvent] =
    caps.unsafe.unsafeAssumePure(events.stream.records)

  // The handler is bound over block locals (not fields) so it stays pure: `Console.trap`
  // takes a pure `PartialFunction`, and a reference to `out` or `events` through `this`
  // would charge the closure with the terminal capability.
  locally:
    val out0 = console.stdio.out
    val events0 = events

    console.trap:
      case Signal.Winch =>
        out0.print(Terminal.reportSize)
        events0.put(Signal.Winch)
        SignalResponse.Accept

      case signal =>
        events0.put(signal)
        SignalResponse.Accept

  // The keyboard pump runs as a daemon under a trap: if reading or decoding stdin fails,
  // the event spool is stopped so the session consuming `events.stream` sees the input end
  // and can exit cleanly, rather than blocking forever on a pump that has silently died.
  //
  // A daemon body must be a pure context function, so everything it needs is bound to
  // block locals first: the untracked `Metrics` holder and `Spool` cross directly, the
  // capability-typed keyboard crosses as an `AnyRef` rim (the cordillera recipe), and the
  // stdin stream is a `LazyList`, which is not capture-tracked, so it crosses as a plain
  // value.
  val pumpInput: Daemon =
    val keyboard0: AnyRef = keyboard.asInstanceOf[AnyRef]
    val events0 = events
    val metrics0 = metrics
    // The terminal reads chars one at a time from the same stdio reader the
    // `Lookahead` consults (the former element-typed `Streamable by Char`
    // instance, now private to its one user).
    val chars: LazyList[Char] =
      def recur(): LazyList[Char] = console.stdio.readChar() match
        case -1  => LazyList()
        case int => int.toChar #:: recur()

      LazyList.defer(recur())

    contain:
      case _ => events0.stop(); Remedy.Accept

    . protect:
        daemon:
          // The teardown in `interactive` closes stdin under the pump; the blocked read
          // then surfaces as an `IOException`, which is the normal end of the session's
          // input, not a failure — so it must not escalate (a plain `Throwable` bypasses
          // the containment, which traps only typed `Error`s, and would reach the
          // uncaught handler and print a stack trace over the restored terminal). The
          // spool is stopped however the pump ends — exception, stdin EOF or decode
          // error — so the session consuming the events always sees the input end.
          try
            keyboard0.asInstanceOf[Keyboard.Standard].process(chars).each:
              case resize@TerminalInfo.WindowSize(rows2, columns2) =>
                metrics0.rows = rows2
                metrics0.columns = columns2
                events0.put(resize)

              case bgColor@TerminalInfo.BgColor(red, green, blue) =>
                metrics0.mode =
                  if Terminal.dark(red, green, blue) then Brightness.Dark else Brightness.Light

                events0.put(bgColor)

              case other =>
                events0.put(other)
          catch case _: java.io.IOException => ()
          finally events0.stop()
