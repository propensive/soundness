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
package tarantula

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import fulminate.*
import gossamer.*
import guillotine.*
import jacinta.*
import parasite.*
import rudiments.*
import telekinesis.*
import urticose.*

import WebDriverError.Reason
import abstractables.durationAbstractable

object BrowserSessional:
  // The interval between readiness probes, and the number of them. A driver binds its port in
  // well under a second; five seconds is generous enough that a loaded machine does not fail
  // spuriously, and short enough that a missing binary is reported promptly.
  private val interval: Long = 50L*1000L*1000L
  private val attempts: Int = 100

  // A free function of the companion, for the same reason as `WebDriverSession.malformed`: a
  // method would carry the instance in its prefix, and a lambda built from it would hide
  // capabilities overlapping the tactic it is applied to.
  private[tarantula] def unstarted(using Diagnostics): WebDriverError =
    WebDriverError(Reason.SessionNotCreated, t"the WebDriver process could not be started", Nil)

  private[tarantula] def unresponsive(using Diagnostics): WebDriverError =
    WebDriverError(Reason.Timeout, t"the WebDriver process did not start listening", Nil)

// A session on a driver that is already listening — one started by hand, or a Selenium grid.
// Split from `BrowserSessional` so that launching a driver is *composition* over this, in the
// shape `obligatory.GrpcSessional` uses over `cordillera.EndpointSessional`, rather than a second
// copy of the protocol.
//
// A named instance class rather than an anonymous given: an anonymous subclass would freshen the
// capability types in its inferred `Result` member.
class WebDriverSessional
  ( using online:      Online,
          backend:     Http.Backend,
          loggable:    (Http.Event is Loggable)^,
          tactic:      Tactic[WebDriverError],
          diagnostics: Diagnostics )
extends Sessional:
  type Self = WebDriver

  // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
  // existential, so returning it (or anything capturing it) from the block is a level violation
  // the capture checker rejects.
  type Result = WebDriverSession^

  def session[result](target: WebDriver)(lambda: (session: Result) ?=> result): result =
    val handle = WebDriverSession(target.base)
    handle.create(target.capabilities)

    // Deleting the session is what closes the browser. It runs even when the block threw, and
    // its own failure is swallowed: a driver that has already died cannot be told to tidy up,
    // and saying so would replace the caller's exception with a less interesting one.
    try lambda(using handle) finally safely(handle.close())

// A session on a browser: fork the driver, wait for it to answer, open a session on it, and stop
// it — process and all — when the scope ends. The `Job` is a local of this method: forked here,
// aborted in the `finally` here, never reaching the handle, so no freshly-minted capability
// crosses a constructor boundary.
class BrowserSessional
  ( using online:      Online,
          backend:     Http.Backend,
          working:     WorkingDirectory,
          monitor:     Monitor,
          loggable:    (Http.Event is Loggable)^,
          exec:        (ExecEvent is Loggable)^,
          tactic:      Tactic[WebDriverError],
          diagnostics: Diagnostics )
extends Sessional:
  type Self = Browser
  type Result = WebDriverSession^

  def session[result](target: Browser)(lambda: (session: Result) ?=> result): result =
    // Local, not fields: a translating tactic held as a field would put `any` in the instance's
    // own type, and a lambda reading `diagnostics` through `this` would hide capabilities that
    // overlap the `tactic` it is applied to.
    val note: Diagnostics = diagnostics

    given execTactic: (Tactic[ExecError]^) =
      tactic.contramap(_ => BrowserSessional.unstarted(using note))

    val job = target.driver.command(target.port).fork[Exit]()

    try
      val base = url"http://localhost:${target.port}"

      // `GET /status` is the specification's own readiness probe: a driver answers it as soon as
      // it has bound its port. This replaces a `sleep(100L)` which never waited at all —
      // parasite's `sleep` takes an *instant*, so it returned immediately — and which would have
      // been a guess at a figure that varies with machine and browser anyway.
      @tailrec
      def waitFor(remaining: Int): Unit =
        if WebDriverSession.listening(base) then ()
        else if remaining <= 0 then abort(BrowserSessional.unresponsive(using note))
        else
          snooze(BrowserSessional.interval)
          waitFor(remaining - 1)

      waitFor(BrowserSessional.attempts)

      // Ascribed, not inferred: the inferred type is a refinement of `WebDriver` pinning `base`
      // to this very val, and no `Sessional` instance matches a refinement of its `Self`.
      val remote: WebDriver = WebDriver(base, target.capabilities)
      remote.session(lambda)

    finally job.abort()

// An already-running WebDriver: a driver started by hand, or a remote grid. `Browser` is the
// usual entry point; this is what it delegates to once its driver is listening.
case class WebDriver(base: HttpUrl, capabilities: Json)
