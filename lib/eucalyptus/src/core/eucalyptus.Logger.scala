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
package eucalyptus

import language.experimental.pureFunctions

import java.util.concurrent as juc

import anticipation.*
import digression.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Logger:
  // Every construction site (a `Codepoint`) owns exactly one spool + writer daemon, so a `given`
  // re-evaluated on each summon (a context-parameterised given is a method, not a lazy val) reuses
  // the same resource instead of leaking a fresh thread per log call.
  private val registry: juc.ConcurrentHashMap[Codepoint, Spool[?]] = juc.ConcurrentHashMap()

  def apply[eventType, loggingType, format, target]
    ( destination: target,
      level:       Level               = Level.Info,
      name:        Optional[Text]      = Unset,
      categories:  Set[Log.Category]   = Set() )
    ( using inscribable: loggingType is Inscribable in format,
            writable:    target is Writable by format,
            monitor:     Monitor,
            codepoint:   Codepoint,
            probate:     Probate )
  :   Logger[eventType, loggingType] =

    val spool: Spool[format] =
      registry
      . computeIfAbsent(codepoint, _ => establish[format, target](destination))
      . nn
      . asInstanceOf[Spool[format]]

    def enqueue(message: loggingType, level: Level, timestamp: Long): Unit =
      spool.put(inscribable.formatter(message, level, timestamp))

    new Logger(level, categories, enqueue)

  // The write runs in a fire-and-forget daemon. A write failure `raise`s a `StreamError`, discharged
  // by the `Emit` captured when the caller summoned the `Writable` — typically a `handle` enclosing
  // the `Logger(…)` construction. Because that handler runs in place and control returns, the write
  // is simply re-established (a fresh `spool.stream` resumes from the same queue) so one failure
  // does not permanently silence the logger. The loop ends only once the spool is stopped.
  private def establish[format, target]
    ( destination: target )
    ( using writable:  target is Writable by format,
            monitor:   Monitor,
            codepoint: Codepoint,
            probate:   Probate )
  :   Spool[format] =

    val stopped: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(false)

    Spool[format]().tap: spool =>
      daemon:
        while !stopped.get() do spool.stream.writeTo(destination)

      Os.intercept[Shutdown]:
        stopped.set(true)
        spool.stop()

class Logger[-eventType, loggingType] private
  ( threshold: Level, categories: Set[Log.Category], enqueue: (loggingType, Level, Long) => Unit )
extends Sink[eventType, loggingType]:

  def accepts(level: Level): Boolean = level.ordinal >= threshold.ordinal

  // With no categories configured, record everything (the level threshold is the only filter);
  // otherwise record only events whose runtime type mixes in one of the selected categories.
  override def admits(event: eventType): Boolean =
    categories.isEmpty || categories.exists(_.reference.isInstance(event))

  def submit(level: Level, timestamp: Long, message: loggingType): Unit =
    enqueue(message, level, timestamp)
