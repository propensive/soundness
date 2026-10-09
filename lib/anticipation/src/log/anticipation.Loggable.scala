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
package anticipation

import scala.caps

import scala.language.experimental.into

import gigantism.Every
import prepositional.*

object Loggable:
  // A logger that records nothing. A `Loggable` is a capability class, so an instance built with
  // `new` is `^`-typed; this one captures nothing, and the fresh capability the instance itself
  // constitutes is laundered away here, once, so a silent logger is a pure value — storable in
  // a plain `val` or a package-level `given` (as `logging.silentLogging` is).
  def silent[event]: (event is Loggable)^{} =
    caps.unsafe.unsafeAssumePure:
      new Loggable:
        type Self = event
        def log(level: Level, timestamp: Long, event: => event): Unit = ()

  // Derives the single `event is Loggable` that `Log.fine`/`info`/`warn`/`fail` summon: transcribe
  // the event to a common `carrier` once, then fan out to EVERY in-scope `LogSink` for that carrier
  // (contravariance means a `LogSink[Any, carrier]` is collected for any event). No sink in
  // scope ⇒ an
  // empty `Every` ⇒ silent; many ⇒ fan-out with per-sink routing. Living in `Loggable`'s companion
  // keeps it in implicit scope, so no import is needed at the use site.
  // Capture-polymorphic over the sinks' capabilities (`cap^`, the Cursor pattern): the
  // derived instance honestly carries whatever the in-scope sinks capture.
  given fanOut: [event, carrier, cap^]
  =>  ( transcribable: event is Transcribable to carrier,
        sinks:         Every[LogSink[event, carrier]^{cap}] )
  =>  ((event is Loggable)^{cap, caps.any}) =

    // The derived logger is a shared capability, so what it retains must be shared: every sink
    // is (`SharedUnscoped`, ambient and storable statically), but the collection that carries
    // them is typed by whatever `cap` the summon inferred — an unclassified fresh root when no
    // sink is in scope at all — so the sinks are held through a pure-asserted view, once, here
    // (a cast: the element type's capture set is a type argument, out of `unsafeAssumePure`'s
    // reach).
    val sinks0: Every[LogSink[event, carrier]^{}] =
      sinks.asInstanceOf[Every[LogSink[event, carrier]^{}]]

    new Loggable:
      type Self = event

      def log(level: Level, timestamp: Long, value: => event): Unit =
        // Force and transcribe the event only if some sink would record it at this level; otherwise
        // (including no sinks at all) the by-name `value` is never evaluated.
        if sinks0.values.exists(_.accepts(level)) then
          val forced = value

          if !transcribable.skip(forced) then
            // Lazy, so an event no admitting sink records is never transcribed. `admits` tests the
            // concrete event against each sink's category filter (the log site knows only the
            // general event type, so this discrimination can only happen at runtime).
            lazy val message = transcribable.record(forced)

            sinks0.values.foreach: sink =>
              if sink.accepts(level) && sink.admits(forced)
              then sink.submit(level, timestamp, message)

// A shared capability: a logger is written to from every task and daemon at once, and the
// sinks it fans out to are shared (`SharedUnscoped`) and synchronised. An instance captures
// those sinks, and the self type says so.
trait Loggable extends Typeclass, Durable:
  loggable: Loggable^ =>
    def log(level: Level, timestamp: Long, event: => Self): Unit

    // `^{lambda}` only: the compiler treats a bare `Loggable` receiver as untracked
    // (empty capture set), so `this` is not a legal capture reference here. The laundering is
    // for the Scala.js pipeline, which — unlike the JVM pipeline — insists the anonymous
    // instance's base class is pure and rejects the capture of `lambda`; the result type still
    // declares it. (Compiler divergence; the JVM pipeline accepts the direct form.)
    // The transformer is shared-only, since the derived logger (a shared capability) retains it;
    // the result also carries the fresh capability the new instance constitutes.
    def contramap[self2](lambda: self2 ->{caps.any.only[anticipation.Durable]} Self)
    :   (self2 is Loggable)^{this, lambda, caps.any} =

      val lambda0: self2 -> Self = caps.unsafe.unsafeAssumePure(lambda)
      (level, timestamp, event) => loggable.log(level, timestamp, lambda0(event))
