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

import scala.caps

import ambience.*
import anticipation.*
import aperture.session
import contingency.*
import fulminate.*
import gossamer.*
import guillotine.*
import jacinta.*
import parasite.*
import rudiments.*
import telekinesis.*
import urticose.*
import vacuous.*

import WebDriverError.Reason
import abstractables.durationAbstractable
import adversaria.name

// The `alwaysMatch` capability objects, one per driver family. Vendor extension keys contain a
// colon, so they are spelled with `@name[Json]` rather than as Scala identifiers.
private case class Arguments(args: List[Text])
private case class Named(browserName: Text)
private case class Edgium(browserName: Text, @name[Json](t"ms:edgeOptions") opts: Arguments)
private case class Gecko(browserName: Text, @name[Json](t"moz:firefoxOptions") opts: Arguments)

private case class Chromium
  ( browserName: Text, @name[Json](t"goog:chromeOptions") opts: Arguments )

object WebDriver:
  // The W3C driver programs, as data: adding a browser is a new case with a command and a name,
  // not a subclass with its own launch/stop pair. The process lifecycle is identical for all of
  // them, and belongs to the session rather than to the browser.
  enum Driver:
    case Chromedriver, Geckodriver, Safaridriver, Edgedriver

    // chromedriver and its Edge fork take `--port=N`; geckodriver and safaridriver take the port
    // as a separate argument.
    def command(port: Int): Command = this match
      case Chromedriver => sh"chromedriver --port=$port"
      case Edgedriver   => sh"msedgedriver --port=$port"
      case Geckodriver  => sh"geckodriver --port $port"
      case Safaridriver => sh"safaridriver --port $port"

    def browserName: Text = this match
      case Chromedriver => t"chrome"
      case Geckodriver  => t"firefox"
      case Safaridriver => t"safari"
      case Edgedriver   => t"MicrosoftEdge"

    // Chromium took `--headless=new` as the supported spelling when the old `--headless` became
    // an alias for it; Firefox has always taken a single-dashed `-headless`. Safari has no
    // headless mode at all, so asking for one is a no-op rather than an error.
    def headless: List[Text] = this match
      case Chromedriver | Edgedriver => List(t"--headless=new")
      case Geckodriver               => List(t"-headless")
      case Safaridriver              => Nil

  object Local:
    // The interval between readiness probes, and the number of them. A driver binds its port in
    // well under a second; five seconds is generous enough that a loaded machine does not fail
    // spuriously, and short enough that a missing binary is reported promptly.
    private val interval: Long = 50L*1000L*1000L
    private val attempts: Int = 100

    // Free functions of the companion, for the same reason as `WebDriverSession.malformed`: a
    // method would carry the instance in its prefix, and a lambda built from it would hide
    // capabilities overlapping the tactic it is applied to.
    private def unstarted(using Diagnostics): WebDriverError =
      WebDriverError(Reason.SessionNotCreated, t"the WebDriver could not be started", Nil)

    private def unresponsive(using Diagnostics): WebDriverError =
      WebDriverError(Reason.Timeout, t"the WebDriver did not start listening", Nil)

    // A session on a browser: fork the driver, wait for it to answer, open a session on it,
    // and stop it — process and all — when the scope ends. The `Job` is a local of this method:
    // forked here, aborted in the `finally` here, never reaching the handle, so no freshly-minted
    // capability crosses a constructor boundary.
    class Sessional
      ( using online:      Online,
              backend:     Http.Backend,
              working:     WorkingDirectory,
              monitor:     Monitor,
              loggable:    (Http.Event is Loggable)^,
              exec:        (ExecEvent is Loggable)^,
              tactic:      Tactic[WebDriverError],
              diagnostics: Diagnostics )
    extends aperture.Sessional:
      type Self = Local
      type Result = WebDriverSession^

      def session[result](target: Local)(lambda: (session: Result) ?=> result): result =
        // Local, not fields: a translating tactic held as a field would put `any` in the instance's
        // own type, and a lambda reading `diagnostics` through `this` would hide capabilities that
        // overlap the `tactic` it is applied to.
        val note: Diagnostics = diagnostics

        given execTactic: (Tactic[ExecError]^) =
          tactic.contramap(_ => unstarted(using note))

        val job = target.driver.command(target.port).fork[Exit]()

        try
          val base = url"http://localhost:${target.port}"

          // `GET /status` is the specification's own readiness probe: a driver answers it as
          // soon as it has bound its port. This replaces a `sleep(100L)` which never waited at
          // all — parasite's `sleep` takes an *instant*, so it returned at once — and which
          // would have been a guess at a figure that varies with machine and browser anyway.
          @tailrec
          def waitFor(remaining: Int): Unit =
            if WebDriverSession.listening(base) then ()
            else if remaining <= 0 then abort(unresponsive(using note))
            else
              snooze(interval)
              waitFor(remaining - 1)

          waitFor(attempts)

          WebDriver(base, target.capabilities).session(lambda)

        finally job.abort()

    // Anchored in the companion rather than exported to `soundness`, so that summoning it needs
    // no import: the implicit scope of `Local is Sessional` includes this object. The capture set
    // is spelled out rather than inferred, since an inferred one would freshen the capabilities
    // the instance was built from and the `Result` refinement would then not match at the use site.
    given sessional
    :   ( online:      Online,
          backend:     Http.Backend,
          working:     WorkingDirectory,
          monitor:     Monitor,
          loggable:    (Http.Event is Loggable)^,
          exec:        (ExecEvent is Loggable)^,
          tactic:      Tactic[WebDriverError],
          diagnostics: Diagnostics )
    =>  ( Local.Sessional^{online, monitor, loggable, exec, tactic, caps.any} ) =

      Local.Sessional()

  // A browser to drive, and the driver program that drives it: a pure value — no process, no
  // connection, no session id — so it may be a `val`, shared between tests, and used for several
  // sessions in turn. `WebDriver.Chrome.headless.session: session ?=> …` opens one.
  case class Local
    ( driver:    WebDriver.Driver,
      port:      Int = 4444,
      arguments: List[Text] = Nil,
      requested: Optional[Json] = Unset ):

    def on(port: Int): Local = copy(port = port)
    def headless: Local = copy(arguments = List.of(arguments.stdlib ++ driver.headless.stdlib))
    def arguing(more: Text*): Local = copy(arguments = List.of(arguments.stdlib ++ more))

    // Replaces the generated capabilities wholesale, for the cases this type does not model:
    // proxies, timeouts, mobile emulation, a Selenium grid's own extensions.
    def requesting(capabilities: Json): Local = copy(requested = capabilities)

    // The `POST /session` payload. W3C capabilities are an `alwaysMatch`/`firstMatch` pair, not
    // the JSON Wire Protocol's `desiredCapabilities`; only `alwaysMatch` is generated here, since
    // `firstMatch` exists to let one request suit several drivers and a `Local` names exactly one.
    def capabilities: Json = requested.or:
      val always =
        val options = Arguments(arguments)

        driver match
          case Driver.Chromedriver => Chromium(driver.browserName, options).in[Json]
          case Driver.Edgedriver   => Edgium(driver.browserName, options).in[Json]
          case Driver.Geckodriver  => Gecko(driver.browserName, options).in[Json]
          case Driver.Safaridriver => Named(driver.browserName).in[Json]

      Json.make(capabilities = Json.make(alwaysMatch = always))

  val Chrome: Local = Local(Driver.Chromedriver)
  val Firefox: Local = Local(Driver.Geckodriver)
  val Safari: Local = Local(Driver.Safaridriver)
  val Edge: Local = Local(Driver.Edgedriver)

  // A session on a driver that is already listening — one started by hand, or a Selenium grid.
  // Split from `LocalSessional` so that launching a driver is *composition* over this, in the
  // shape `obligatory.GrpcSessional` uses over `cordillera.EndpointSessional`, rather than a second
  // copy of the protocol.
  //
  // A named instance class rather than an anonymous given: an anonymous subclass would freshen the
  // capability types in its inferred `Result` member.
  class Sessional
    ( using online:      Online,
            backend:     Http.Backend,
            loggable:    (Http.Event is Loggable)^,
            tactic:      Tactic[WebDriverError],
            diagnostics: Diagnostics )
  extends aperture.Sessional:
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

  given sessional
  :   ( online:      Online,
        backend:     Http.Backend,
        loggable:    (Http.Event is Loggable)^,
        tactic:      Tactic[WebDriverError],
        diagnostics: Diagnostics )
  =>  ( Sessional^{online, loggable, tactic, caps.any} ) =

    Sessional()

  // Not a case class, and this `apply` ascribes its result: a case class's synthetic `apply`
  // returns a type refined with each argument's singleton, and no `Sessional` instance matches a
  // refinement of its `Self`, so `WebDriver(…).session` would not resolve at any call site.
  def apply(base: HttpUrl, capabilities: Json): WebDriver = new WebDriver(base, capabilities)

// A WebDriver that is already listening: one started by hand, or a remote grid. `WebDriver.Chrome`
// and its siblings are the usual entry points; this is what they delegate to once the driver they
// launched is answering.
class WebDriver(val base: HttpUrl, val capabilities: Json)
