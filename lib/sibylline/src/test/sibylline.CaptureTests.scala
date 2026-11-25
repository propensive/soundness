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
package sibylline

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely

// The session and its in-flight streamed response are capabilities scoped to the block a
// provider's `session` lends them to; capture checking prevents either being retained beyond
// it, while pure values — replies, messages, usage — may leave freely.
//
// Every name these samples use is the library's or the sample's own. `demilitarize` compiles
// this file by itself, so a name from another file of the test module is "not found" on a clean
// build: the sample then fails for that reason alone, and, outside a sample, the error stops
// the sub-compilation before capture checking, leaving every other sample with nothing to
// report. For the same reason each test names the failure it expects.
object CaptureTests extends Suite("llm-capture", m"Confinement tests"):
  def run(): Unit =
    test(m"the session cannot be stashed in an outer variable"):
      demilitarize:
        def attempt(using session: Llm.Session^): Unit =
          var stash: () => Optional[Llm.Reply] = () => Unset
          stash = () => session.ask(t"late")
          ()
      . map(_.message.tt)
    . assert(_.exists(_.contains(t"hides parameter session")))

    test(m"a streamed response cannot be stashed in an outer variable"):
      demilitarize:
        def attempt(using session: Llm.Session^): Unit =
          var stash: () => Unit = () => ()
          val response = session.stream(t"go")
          stash = () => response.reply().unit
          ()
      . map(_.message.tt)
    . assert(_.exists(_.contains(t"hides parameter session")))

    test(m"a pure reply may leave the session block"):
      demilitarize:
        def attempt(using session: Llm.Session^): Llm.Reply =
          session.ask(t"fine")
    . assert(_.nil)

    test(m"the history snapshot may leave the session block"):
      demilitarize:
        def attempt(using session: Llm.Session^): List[Llm.Message] =
          session.history
    . assert(_.nil)

    // The routes out of a provider's own `session` block (#2459). Each sample takes its
    // provider and HTTP backend as parameters, so that it needs no fixture.
    test(m"the session cannot be a session block's result"):
      demilitarize:
        def attempt(target: Anthropic)(using Http.Backend): Llm.Session^ = target.session(llm)
      . map(_.message.tt)
    . assert(_.exists(_.contains(t"outlives its scope")))

    test(m"the session cannot leave its block as an inferred value"):
      demilitarize:
        def attempt(target: Anthropic)(using Http.Backend): Llm.Reply =
          val escaped = target.session(llm)
          escaped.ask(t"Hello")
      . map(_.message.tt)
    . assert(_.exists(_.contains(t"outlives its scope")))

    test(m"a closure reading the session cannot leave its block"):
      demilitarize:
        def attempt(target: Anthropic)(using Http.Backend): Unit =
          val history = target.session(() => llm.history)
          history().unit
      . map(_.message.tt)
    . assert(_.exists(_.contains(t"outlives its scope")))

    test(m"a streamed response cannot leave its block"):
      demilitarize:
        def attempt(target: Anthropic)(using Http.Backend): Unit =
          val response = target.session(llm.stream(t"go"))
          response.reply().unit
      . map(_.message.tt)
    . assert(_.exists(_.contains(t"outlives its scope")))

    // Through the instance itself, rather than the `session` extension: the handle is known
    // there to be stateful, so a closure which only reads it captures its read-only view,
    // `session.rd`, whose level Scala 3.10.0 did not check.
    test(m"a closure reading the session cannot leave an instance's block"):
      demilitarize:
        def attempt(target: Anthropic)(using Http.Backend): Unit =
          val history = Anthropic.sessional.session(target) { session ?=> () => session.history }
          history().unit
      . map(_.message.tt)
    . assert(_.exists(_.contains(t"outlives its scope")))

    test(m"a pure reply may leave a provider's session block"):
      demilitarize:
        def attempt(target: Anthropic)(using Http.Backend): Llm.Reply =
          target.session(llm.ask(t"fine"))
    . assert(_.nil)
