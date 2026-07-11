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
package anticipation

import language.experimental.into

import gigantism.Every
import prepositional.*

object Loggable:
  // Derives the single `event is Loggable` that `Log.fine`/`info`/`warn`/`fail` summon: transcribe
  // the event to a common `carrier` once, then fan out to EVERY in-scope `LogSink` for that carrier
  // (contravariance means a `LogSink[Any, carrier]` is collected for any event). No sink in
  // scope ⇒ an
  // empty `Every` ⇒ silent; many ⇒ fan-out with per-sink routing. Living in `Loggable`'s companion
  // keeps it in implicit scope, so no import is needed at the use site.
  given fanOut: [event, carrier]
  =>  ( transcribable: event is Transcribable to carrier, sinks: Every[LogSink[event, carrier]] )
  =>  event is Loggable =

    new Loggable:
      type Self = event

      def log(level: Level, timestamp: Long, value: => event): Unit =
        // Force and transcribe the event only if some sink would record it at this level; otherwise
        // (including no sinks at all) the by-name `value` is never evaluated.
        if sinks.values.exists(_.accepts(level)) then
          val forced = value

          if !transcribable.skip(forced) then
            // Lazy, so an event no admitting sink records is never transcribed. `admits` tests the
            // concrete event against each sink's category filter (the log site knows only the
            // general event type, so this discrimination can only happen at runtime).
            lazy val message = transcribable.record(forced)

            sinks.values.foreach: sink =>
              if sink.accepts(level) && sink.admits(forced)
              then sink.submit(level, timestamp, message)

trait Loggable extends Typeclass:
  loggable: Loggable =>
    def log(level: Level, timestamp: Long, event: => Self): Unit

    // `^{lambda}` only: the compiler treats a bare `Loggable` receiver as untracked
    // (empty capture set), so `this` is not a legal capture reference here. The laundering is
    // for the Scala.js pipeline, which — unlike the JVM pipeline — insists the anonymous
    // instance's base class is pure and rejects the capture of `lambda`; the result type still
    // declares it. (Compiler divergence; the JVM pipeline accepts the direct form.)
    def contramap[self2](lambda: self2 => Self): (self2 is Loggable)^{lambda} =
      val lambda0: self2 -> Self = caps.unsafe.unsafeAssumePure(lambda)
      (level, timestamp, event) => loggable.log(level, timestamp, lambda0(event))
