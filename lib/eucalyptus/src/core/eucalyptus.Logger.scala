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

  def emitToStderr(error: Exception): Unit = System.err.nn.println(error.getMessage)

  def apply[eventType, loggingType, format, target]
    ( destination: target,
      level:       Level             = Level.Info,
      name:        Optional[Text]    = Unset,
      onError:     Exception => Unit = emitToStderr )
    ( using inscribable: loggingType is Inscribable in format,
            writable:    target is Writable by format,
            monitor:     Monitor,
            codepoint:   Codepoint,
            probate:     Probate )
  :   Logger[eventType, loggingType] =

    val spool: Spool[format] =
      registry
      . computeIfAbsent(codepoint, _ => establish[format, target](destination, onError))
      . nn
      . asInstanceOf[Spool[format]]

    def enqueue(message: loggingType, level: Level, timestamp: Long): Unit =
      spool.put(inscribable.formatter(message, level, timestamp))

    new Logger(level, enqueue)

  // The write runs in a fire-and-forget daemon. Whether a write failure surfaces as a thrown
  // exception is decided by the `Tactic` baked into the (caller-summoned) `Writable`; any exception
  // that does escape the write is reported to `onError`, after which the write is re-established (a
  // fresh `spool.stream` resumes taking from the same queue) so one failure does not permanently
  // silence the logger.
  private def establish[format, target]
    ( destination: target, onError: Exception => Unit )
    ( using writable:  target is Writable by format,
            monitor:   Monitor,
            codepoint: Codepoint,
            probate:   Probate )
  :   Spool[format] =

    Spool[format]().tap: spool =>
      daemon:
        def loop(): Unit =
          try spool.stream.writeTo(destination)
          catch case error: Exception => onError(error); loop()

        loop()

      Os.intercept[Shutdown](spool.stop())

class Logger[-eventType, loggingType] private
  ( threshold: Level, enqueue: (loggingType, Level, Long) => Unit ):

  def submit(level: Level, timestamp: Long, message: loggingType): Unit =
    if level.ordinal >= threshold.ordinal then enqueue(message, level, timestamp)
