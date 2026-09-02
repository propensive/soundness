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

import adversaria.name
import ambience.*
import anticipation.*, abstractables.millisecondsAbstractable
import aperture.session
import clavichord.Keypress
import contingency.*
import denominative.nil
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import guillotine.*
import hieroglyph.*, charEncoders.utf8Encoder, charDecoders.utf8Decoder,
    textSanitizers.strictSanitizer
import jacinta.*, formatting.compactJsonFormatting, dynamicJsonAccess.enabled
import monotonous.*, alphabets.base64Standard
import parasite.*
import prepositional.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.`+`
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.memoize

// The payloads the WebDriver protocol defines, as case classes rather than method-local ones:
// a derivation macro cannot see a class declared inside the method that uses it.
private case class Locator(`using`: Text, value: Text)
private case class Address(url: Text)
private case class Keys(text: Text)
private case class Script(script: Text, args: List[Json])
private case class Empty()
private case class Handle(handle: Text)
private case class Frame(id: Json)
private case class Kind(`type`: Text)
private case class Alert(text: Text)
private case class Cookies(cookie: Cookie.Value)
private case class Sequence(id: Text, `type`: Text, actions: List[Json])
private case class Sequences(actions: List[Json])

// The W3C error object, with every field optional: a driver may omit the stacktrace, and a
// reply that is JSON but not this shape must still yield a `Error`, not a `Json.Error`.
private case class Failure
  ( error:      Optional[Text],
    message:    Optional[Text],
    stacktrace: Optional[Text],
    data:       Optional[Json] )

// The wire form of an element handle, under the W3C "web element identifier" — a key chosen to be
// one no page can collide with.
private case class ElementRef
  ( @name[Json](t"element-6066-11e4-a52e-4f735466cecf") elementId: Text )

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
    private val interval: Long = 50L // milliseconds
    private val attempts: Int = 100

    // Free functions of the companion, for the same reason as `Session.malformed`: a
    // method would carry the instance in its prefix, and a lambda built from it would hide
    // capabilities overlapping the tactic it is applied to.
    private def unstarted(using Diagnostics): Error =
      Error(Error.Reason.SessionNotCreated, t"the WebDriver could not be started", Nil)

    private def unresponsive(using Diagnostics): Error =
      Error(Error.Reason.Timeout, t"the WebDriver did not start listening", Nil)

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
              exec:        (Exec.Event is Loggable)^,
              tactic:      Tactic[Error],
              diagnostics: Diagnostics )
    extends aperture.Sessional:
      type Self = Local
      type Result = Session^

      def session[result](target: Local)(lambda: (session: Result) ?=> result): result =
        // Local, not fields: a translating tactic held as a field would put `any` in the instance's
        // own type, and a lambda reading `diagnostics` through `this` would hide capabilities that
        // overlap the `tactic` it is applied to.
        val note: Diagnostics = diagnostics

        given execTactic: (Tactic[Exec.Error]^) =
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
            if Session.listening(base) then ()
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
          exec:        (Exec.Event is Loggable)^,
          tactic:      Tactic[Error],
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
    def headless: Local = copy(arguments = arguments + driver.headless)
    def arguing(more: Text*): Local = copy(arguments = arguments + more.to(List))

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
  // shape `obligatory.GrpcSessional` uses over `telekinesis.Http2.EndpointSessional`, rather than a second
  // copy of the protocol.
  //
  // A named instance class rather than an anonymous given: an anonymous subclass would freshen the
  // capability types in its inferred `Result` member.
  class Sessional
    ( using online:      Online,
            backend:     Http.Backend,
            loggable:    (Http.Event is Loggable)^,
            tactic:      Tactic[Error],
            diagnostics: Diagnostics )
  extends aperture.Sessional:
    type Self = WebDriver

    // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
    // existential, so returning it (or anything capturing it) from the block is a level violation
    // the capture checker rejects.
    type Result = Session^

    def session[result](target: WebDriver)(lambda: (session: Result) ?=> result): result =
      val handle = Session(target.base)
      handle.create(target.capabilities)

      // Deleting the session is what closes the browser. It runs even when the block threw, and
      // its own failure is swallowed: a driver that has already died cannot be told to tidy up,
      // and saying so would replace the caller's exception with a less interesting one.
      try lambda(using handle) finally safely(handle.close())

  given sessional
  :   ( online:      Online,
        backend:     Http.Backend,
        loggable:    (Http.Event is Loggable)^,
        tactic:      Tactic[Error],
        diagnostics: Diagnostics )
  =>  ( Sessional^{online, loggable, tactic, caps.any} ) =

    Sessional()

  // Not a case class, and this `apply` ascribes its result: a case class's synthetic `apply`
  // returns a type refined with each argument's singleton, and no `Sessional` instance matches a
  // refinement of its `Self`, so `WebDriver(…).session` would not resolve at any call site.
  def apply(base: HttpUrl, capabilities: Json): WebDriver = new WebDriver(base, capabilities)

  object Error:
    object Reason:
      // Hand-written rather than derived from `values`: Scala generates `values`/`valueOf` only
      // for an enum whose cases are all singletons, and `Other` is parameterized. An
      // unrecognized code becomes `Other`, so decoding is total, and the code a driver sent is
      // never lost.
      def apply(code: Text): Reason = code match
        case t"no such element"            => NoSuchElement
        case t"stale element reference"    => StaleElement
        case t"no such window"             => NoSuchWindow
        case t"javascript error"           => JavascriptError
        case t"script timeout"             => ScriptTimeout
        case t"timeout"                    => Timeout
        case t"unknown command"            => UnknownCommand
        case t"element click intercepted"  => ElementClickIntercepted
        case t"element not interactable"   => ElementNotInteractable
        case t"insecure certificate"       => InsecureCertificate
        case t"invalid argument"           => InvalidArgument
        case t"invalid cookie domain"      => InvalidCookieDomain
        case t"invalid element state"      => InvalidElementState
        case t"invalid selector"           => InvalidSelector
        case t"invalid session id"         => InvalidSessionId
        case t"move target out of bounds"  => MoveTargetOutOfBounds
        case t"no such alert"              => NoSuchAlert
        case t"no such cookie"             => NoSuchCookie
        case t"no such frame"              => NoSuchFrame
        case t"no such shadow root"        => NoSuchShadowRoot
        case t"detached shadow root"       => DetachedShadowRoot
        case t"session not created"        => SessionNotCreated
        case t"unable to set cookie"       => UnableToSetCookie
        case t"unable to capture screen"   => UnableToCaptureScreen
        case t"unexpected alert open"      => UnexpectedAlertOpen
        case t"unknown error"              => UnknownError
        case t"unknown method"             => UnknownMethod
        case t"unsupported operation"      => UnsupportedOperation
        case other                         => Other(other)

    // The error codes a WebDriver may report, as the driver spells them in the `error` field of
    // the error object. The numbers are the `SN-589.e` subcodes, and are frozen: 1–8 keep the
    // meanings they were first published with — including `Other` at 8, out of alphabetical
    // order — and every code added since appends from 9.
    enum Reason(val number: Int, val code: Text) extends Clarification:
      case NoSuchElement           extends Reason(1, t"no such element")
      case StaleElement            extends Reason(2, t"stale element reference")
      case NoSuchWindow            extends Reason(3, t"no such window")
      case JavascriptError         extends Reason(4, t"javascript error")
      case ScriptTimeout           extends Reason(5, t"script timeout")
      case Timeout                 extends Reason(6, t"timeout")
      case UnknownCommand          extends Reason(7, t"unknown command")
      case Other(code0: Text)      extends Reason(8, code0)
      case ElementClickIntercepted extends Reason(9, t"element click intercepted")
      case ElementNotInteractable  extends Reason(10, t"element not interactable")
      case InsecureCertificate     extends Reason(11, t"insecure certificate")
      case InvalidArgument         extends Reason(12, t"invalid argument")
      case InvalidCookieDomain     extends Reason(13, t"invalid cookie domain")
      case InvalidElementState     extends Reason(14, t"invalid element state")
      case InvalidSelector         extends Reason(15, t"invalid selector")
      case InvalidSessionId        extends Reason(16, t"invalid session id")
      case MoveTargetOutOfBounds   extends Reason(17, t"move target out of bounds")
      case NoSuchAlert             extends Reason(18, t"no such alert")
      case NoSuchCookie            extends Reason(19, t"no such cookie")
      case NoSuchFrame             extends Reason(20, t"no such frame")
      case NoSuchShadowRoot        extends Reason(21, t"no such shadow root")
      case DetachedShadowRoot      extends Reason(22, t"detached shadow root")
      case SessionNotCreated       extends Reason(23, t"session not created")
      case UnableToSetCookie       extends Reason(24, t"unable to set cookie")
      case UnableToCaptureScreen   extends Reason(25, t"unable to capture screen")
      case UnexpectedAlertOpen     extends Reason(26, t"unexpected alert open")
      case UnknownError            extends Reason(27, t"unknown error")
      case UnknownMethod           extends Reason(28, t"unknown method")
      case UnsupportedOperation    extends Reason(29, t"unsupported operation")

    given communicable: Reason is Communicable =
      case Reason.NoSuchElement           => m"the requested element was not found"
      case Reason.StaleElement            => m"the element is no longer attached to the page"
      case Reason.NoSuchWindow            => m"the requested window was not found"
      case Reason.JavascriptError         => m"a JavaScript error occurred while evaluating it"
      case Reason.ScriptTimeout           => m"the asynchronous script did not complete in time"
      case Reason.Timeout                 => m"the operation timed out"
      case Reason.UnknownCommand          => m"the WebDriver does not recognise the command"
      case Reason.Other(code)             => m"the WebDriver reported the error code $code"
      case Reason.ElementClickIntercepted => m"the click was intercepted by another element"
      case Reason.ElementNotInteractable  => m"the element cannot be interacted with"
      case Reason.InsecureCertificate     => m"the page was served with an untrusted certificate"
      case Reason.InvalidArgument         => m"an argument to the command was not acceptable"
      case Reason.InvalidCookieDomain     => m"the cookie's domain does not match the current page"
      case Reason.InvalidElementState     => m"the element is in a state which forbids the action"
      case Reason.InvalidSelector         => m"the selector was not a valid one"
      case Reason.InvalidSessionId        => m"the session is no longer active"
      case Reason.MoveTargetOutOfBounds   => m"the pointer target lies outside the viewport"
      case Reason.NoSuchAlert             => m"no alert is currently open"
      case Reason.NoSuchCookie            => m"the requested cookie was not found"
      case Reason.NoSuchFrame             => m"the requested frame was not found"
      case Reason.NoSuchShadowRoot        => m"the element has no shadow root"
      case Reason.DetachedShadowRoot      => m"the shadow root is no longer attached to the page"
      case Reason.SessionNotCreated       => m"a new session could not be created"
      case Reason.UnableToSetCookie       => m"the cookie could not be set"
      case Reason.UnableToCaptureScreen   => m"the screenshot could not be taken"
      case Reason.UnexpectedAlertOpen     => m"an unexpected alert is open"
      case Reason.UnknownError            => m"the WebDriver encountered an unknown error"
      case Reason.UnknownMethod           => m"the command does not support the HTTP method used"
      case Reason.UnsupportedOperation    => m"the WebDriver does not support the operation"

  // `data` is the specification's optional extra object. It is what carries the prompt's text on
  // an `unexpected alert open`, which is precisely what a caller needs in order to decide whether
  // to accept or dismiss it, so dropping it would lose the only actionable part of that error.
  case class Error
    ( reason:            Error.Reason,
      detail:            Text,
      browserStacktrace: List[Text],
      data:              Optional[Json] = Unset )
    ( using Diagnostics )
  extends fulminate.Error
    ( 589, reason.number )
    ( m"the WebDriver action failed because $reason: $detail" )

  object Element:
    // Elements cross the wire as `{"element-6066-…": id}` — as a script argument, or as a
    // pointer action's origin. `ElementRef` carries the key, not a legal Scala identifier.
    given encodable: (Element is Encodable in Json) =
      element => ElementRef(element.elementId).in[Json]

  // An element handle: the opaque reference a driver returned for a node, meaningful only to the
  // session that produced it.
  //
  // A *pure* value, holding neither a capability nor a reference to its session — a deliberate
  // choice, not an omission. `session.elements(selector)` builds elements inside a `map` lambda,
  // and a capability-classed `Element` constructed there is fresh at each construction, so a
  // `List` of them cannot be formed ("any² is not visible from any"); that is precisely the
  // failure recorded in rep/DECISIONS.md which caused the earlier attempt to be reverted. The
  // `X^{this, caps.any}` borrow that `Http.Session#fetch` uses works for one value returned from a
  // method; it does not survive being built in a lambda and collected into a container.
  //
  // Nothing is lost by leaving elements pure. A leaked element is a dangling id, not a live
  // handle, and every operation on one needs a session in contextual scope — which capture
  // checking confines to its block. Using a stale one now raises `Error.Reason.StaleElement`, where
  // before it was silently swallowed.
  case class Element(elementId: Text)

  // The open shadow root hosted by an element: a separate handle because the protocol addresses it
  // separately, and because only `element`/`elements` are valid against one.
  case class ShadowRoot(shadowId: Text)

  object Session:
    // The bounding box of an element, or of a window, in CSS pixels.
    case class Rect(x: Double, y: Double, width: Double, height: Double)

    // The page geometry and margins a print takes. Lengths are `Quantity[Metres[1]]`, so they may
    // be written in whatever unit reads best — `21.0*Centi(Metre)`, `8.5*Inch` — and the
    // centimetres the protocol wants are computed rather than assumed. The defaults are the
    // specification's own (A4, 1cm margins).
    // A length given in centimetres, rather than `21.59*Centi(Metre)` in the defaults: the
    // named helper keeps the specification's own numbers legible in the table below.
    def cm(centimetres: Double): Quantity[Metres[1]] = Quantity(centimetres/100)

    case class Page
      ( width: Quantity[Metres[1]] = cm(21.59), height: Quantity[Metres[1]] = cm(27.94) )

    object Margin:
      def uniform(length: Quantity[Metres[1]]): Margin = Margin(length, length, length, length)

    case class Margin
      ( top:    Quantity[Metres[1]] = cm(1.0),
        bottom: Quantity[Metres[1]] = cm(1.0),
        left:   Quantity[Metres[1]] = cm(1.0),
        right:  Quantity[Metres[1]] = cm(1.0) )

    enum Orientation:
      case Portrait, Landscape

      def name: Text = this match
        case Portrait  => t"portrait"
        case Landscape => t"landscape"

    // The options `POST /print` takes. `pageRanges` is the specification's own string form —
    // `List(t"1-3", t"5")` — since a range there may be open-ended.
    case class Print
      ( orientation: Orientation      = Orientation.Portrait,
        scale:       Double           = 1.0,
        background:  Boolean          = false,
        page:        Page             = Page(),
        margin:      Margin           = Margin(),
        shrinkToFit: Boolean          = true,
        pageRanges:  List[Text]       = Nil )

    // The three timeouts a session keeps, in milliseconds. `implicit` is the wait a find retries
    // for before giving up, and is how a test waits for a page to settle without sleeping. The W3C
    // spelling replaced the JSON Wire Protocol's `{"type":…,"ms":…}` pair.
    case class Timeouts
      ( `implicit`: Optional[Long] = Unset,
        pageLoad:   Optional[Long] = Unset,
        script:     Optional[Long] = Unset )

    // A free function of the companion, not a method of the session: a method would carry the
    // session in its own prefix, and a lambda built from it would hide capabilities that overlap
    // the `tactic` it is applied to — an overlap separation checking rejects.
    private[tarantula] def malformed(detail: Text)(using Diagnostics): Error =
      Error(Error.Reason.UnknownError, detail, Nil)

    // Every decode of a driver's reply goes through one of these five, and each is sealed with
    // `unsafeAssumeSeparate` for the same reason: `Json#as` takes the `Tactic[Json.Error]` both
    // directly and inside the capture set of the decodable it summons, so a *capability* tactic —
    // which is what `raises` and `contramap` produce — reads as two overlapping uses. This is the
    // Foci/tracks cluster recorded in rep/DECISIONS.md, and the reason the module previously
    // imported `strategies.throwUnsafely`, whose tactic is pure and so never overlaps. Each body
    // here is a single decode of a single value against a single tactic: there is no second use to
    // interleave with, and confining the assertion to these five lines keeps every caller honest.
    private[tarantula] def text(json: Json): Text raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[Text])

    private[tarantula] def boolean(json: Json): Boolean raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[Boolean])

    private[tarantula] def rect(json: Json): Rect raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[Rect])

    private[tarantula] def list(json: Json): List[Json] raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[List[Json]])

    private[tarantula] def failure(json: Json): Failure raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[Failure])

    object Action:
      // The two kinds of input source the specification defines that carry actions. (A "none"
      // source exists too, but it can only pause, which either can do.) One `perform` call drives
      // one source; driving two in lockstep — a chord across keyboard and pointer — is rare enough
      // that it is left to `WebDriver.Session#actions`, which takes the assembled JSON.
      enum Source:
        case Key, Pointer

        def id: Text = this match
          case Key     => t"keyboard"
          case Pointer => t"pointer"

        def kind: Text = this match
          case Key     => t"key"
          case Pointer => t"pointer"

      // The buttons as the specification numbers them, which is the DOM's numbering and not the
      // one a user would guess: the middle button is 1 and the right button 2.
      val LeftButton: Int = 0
      val MiddleButton: Int = 1
      val RightButton: Int = 2

      // The specification assigns each non-typing key a codepoint in Unicode's private use area,
      // U+E000 upwards, so that a key with no character can still be "typed". These are those
      // codepoints, named by `clavichord`'s vocabulary rather than a table of our own.
      def codepoint(keypress: Keypress): Optional[Text] = keypress match
        case Keypress.CharKey(char)  => char.show
        case Keypress.Enter          => t"\uE007"
        case Keypress.Tab            => t"\uE004"
        case Keypress.Backspace      => t"\uE003"
        case Keypress.Escape         => t"\uE00C"
        case Keypress.Delete         => t"\uE017"
        case Keypress.Insert         => t"\uE016"
        case Keypress.Home           => t"\uE011"
        case Keypress.End            => t"\uE010"
        case Keypress.PageUp         => t"\uE00E"
        case Keypress.PageDown       => t"\uE00F"
        case Keypress.Up             => t"\uE013"
        case Keypress.Down           => t"\uE015"
        case Keypress.Left           => t"\uE012"
        case Keypress.Right          => t"\uE014"

        // F1 is U+E031, and the rest follow in order.
        case Keypress.FunctionKey(n) => if n < 1 || n > 12 then Unset else (0xE030 + n).toChar.show

        // A terminal escape sequence has no browser counterpart, and a modifier is not a key
        // press on its own — `steps` expands those into the keys held around what they modify.
        case _                       => Unset

      // The modifier a wrapper stands for, as its own codepoint.
      private def modifier(keypress: Keypress): Optional[Text] = keypress match
        case _: Keypress.Shift => t"\uE008"
        case _: Keypress.Ctrl  => t"\uE009"
        case _: Keypress.Alt   => t"\uE00A"
        case _: Keypress.Meta  => t"\uE03D"
        case _                 => Unset

      private def inner(keypress: Keypress): Optional[Keypress] = keypress match
        case shift: Keypress.Shift => shift.keypress
        case alt: Keypress.Alt     => alt.keypress
        case meta: Keypress.Meta   => meta.keypress
        case _                     => Unset

      // `Ctrl` is the one wrapper whose payload may be a bare `Char` rather than a `Keypress`.
      private def within(ctrl: Keypress.Ctrl): Keypress = ctrl.keypress.match
        case char: Char      => Keypress.CharKey(char)
        case other: Keypress => other

      // A keypress as the actions that produce it: the modifiers pressed, the key struck and
      // released, then the modifiers released in reverse. This is the only way the protocol can
      // express a chord — there is no "type this with control held" command.
      def steps(keypress: Keypress): List[Action] =
        val held = modifier(keypress)

        held.lay(codepoint(keypress).lay(Nil): key =>
          List(Action.KeyDown(key), Action.KeyUp(key))): held =>

          val nested = keypress.match
            case ctrl: Keypress.Ctrl => steps(within(ctrl))
            case other               => inner(other).lay(Nil)(steps(_))

          (Action.KeyDown(held) :: nested) + List(Action.KeyUp(held))

      // Written by hand rather than derived: the discriminator is a `type` field whose values are
      // the specification's camel-cased names, and each variant carries a different set of keys.
      given encodable: Action is Encodable in Json =
        case KeyDown(value)  => Json.make(`type` = t"keyDown".in[Json], value = value.in[Json])
        case KeyUp(value)    => Json.make(`type` = t"keyUp".in[Json], value = value.in[Json])
        case Pause(duration) => Json.make(`type` = t"pause".in[Json], duration = duration.in[Json])

        case PointerDown(button) =>
          Json.make(`type` = t"pointerDown".in[Json], button = button.in[Json])

        case PointerUp(button) =>
          Json.make(`type` = t"pointerUp".in[Json], button = button.in[Json])

        case PointerMove(x, y, duration, origin) =>
          Json.make
            ( `type`   = t"pointerMove".in[Json],
              x        = x.in[Json],
              y        = y.in[Json],
              duration = duration.in[Json],
              origin   = origin.lay(t"viewport".in[Json])(_.in[Json]) )

    // One step of an input sequence. This is the only way to send a modifier, a chord, a hover or a
    // drag: the specification replaced the JSON Wire Protocol's `/moveto`, `/click` and `/keys`
    // with one `/actions` endpoint, which is why `tarantula` could not simulate a keypress before.
    // Plain text entry into a field stays on `Element#value`, which is simpler and faster.
    enum Action:
      case KeyDown(value: Text)
      case KeyUp(value: Text)
      case Pause(duration: Int)
      case PointerDown(button: Int = Action.LeftButton)
      case PointerUp(button: Int = Action.LeftButton)

      // Coordinates are relative to `origin`: the viewport when it is `Unset`, or the centre of the
      // element when one is given.
      case PointerMove(x: Int, y: Int, duration: Int = 100, origin: Optional[Element] = Unset)

    // A generic `decode[value]` would not do: taking the decodable as a parameter moves its
    // summon to the call site, where the tactic is a field of the session again and the overlap
    // returns. So each shape the protocol replies with gets its own line here.
    // The literal `null`, parsed once. Constructing it here rather than in the session keeps the
    // `unsafely` — which cannot fail, the text being a literal — out of the request path.
    private[tarantula] val nul: Json = unsafely(t"null".read[Json])

    // Encoded by hand rather than derived: every length crosses the wire in centimetres, and
    // `Quantity#value` is in metres, so each needs the factor of 100 applying. A derivation would
    // serialize the metres and silently print a page a hundred times too small.
    private[tarantula] def encode(print: Print): Json =
      def cm(length: Quantity[Metres[1]]): Json = (length.value*100).in[Json]

      Json.make
        ( orientation = print.orientation.name.in[Json],
          scale       = print.scale.in[Json],
          background  = print.background.in[Json],
          shrinkToFit = print.shrinkToFit.in[Json],
          pageRanges  = print.pageRanges.in[Json],
          page        = Json.make(width = cm(print.page.width), height = cm(print.page.height)),

          margin      = Json.make
                         ( top    = cm(print.margin.top),
                           bottom = cm(print.margin.bottom),
                           left   = cm(print.margin.left),
                           right  = cm(print.margin.right) ) )

    private[tarantula] def texts(json: Json): List[Text] raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[List[Text]])

    private[tarantula] def cookie(json: Json): Cookie.Value raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[Cookie.Value])

    private[tarantula] def cookies(json: Json): List[Cookie.Value] raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[List[Cookie.Value]])

    private[tarantula] def timeouts(json: Json): Timeouts raises Json.Error =
      caps.unsafe.unsafeAssumeSeparate(json.as[Timeouts])

    // Whether a driver is listening yet. Every failure is swallowed: during startup a refused
    // connection is the expected outcome, not something to report. The `ready` flag in the reply
    // is deliberately *not* consulted — a driver that already holds a session reports `ready:
    // false`, and it is the port being bound, not the driver being idle, that is being waited for.
    // A local throwing strategy, so that `safely` sees the failure rather than the caller's tactic.
    private[tarantula] def listening(base: HttpUrl)
      ( using Online, Http.Backend, (Http.Event is Loggable)^ )
    :   Boolean =

      import strategies.throwUnsafely

      safely:
        Url(base.origin, t"${base.location}/status")
        . fetch(contentType = media"application/json")
        . status
        . category == Http.Status.Category.Successful

      . or(false)

  // A live WebDriver session: one browser instance, its window set and its cookie jar, addressed
  // at `$base/session/$sessionId`. A capability — it is the near end of a conversation with a
  // driver that something else started and will stop — so capture checking confines it, and every
  // closure over it, to the block that owns it.
  //
  // The driver process is deliberately *not* a field. Its lifetime is the `try`/`finally` of
  // `BrowserSessional#session`, so the handle has no reason to hold it, and passing a
  // freshly-forked `Job` into a constructor parameter is the fresh-root mismatch that sank the
  // earlier attempt to capability-class this type (see rep/DECISIONS.md).
  //
  // `ExclusiveCapability` but not `Stateful`: `update` only buys a guarantee when a method returns
  // something that borrows the receiver, and every WebDriver command is a complete round trip
  // yielding pure data. Promote it if a genuinely borrowing sub-handle — a live window or frame —
  // is ever added.
  class Session private[tarantula] (base: HttpUrl)
    ( using online:      Online,
            backend:     Http.Backend,
            loggable:    (Http.Event is Loggable)^,
            tactic:      Tactic[Error],
            diagnostics: Diagnostics )
  extends caps.ExclusiveCapability:

    // The W3C "web element identifier": the key under which a driver returns an element handle,
    // chosen to be one no page can collide with.
    private final val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

    // The shadow-root counterpart of `Wei`.
    private final val Shadow: Text = t"shadow-6066-11e4-a52e-4f735466cecf"

    // Set once by `create`, before the handle is lent to a block: neither exists until the driver
    // has minted them, and there is no session to be a method of until then. Both are pure values,
    // so the variables track nothing; the annotation says so, rather than making the whole session
    // `Stateful` for two write-once fields.
    @scala.caps.unsafe.untrackedCaptures
    private var id: Text = t""

    @scala.caps.unsafe.untrackedCaptures
    private var negotiated: Optional[Json] = Unset

    def sessionId: Text = id

    // What the driver actually agreed to, which is not what was asked for: it names the browser
    // and version that answered, and says whether optional behaviour — `setWindowRect`,
    // `acceptInsecureCerts` — is available. Vendor keys (`moz:*`, `goog:*`) are left as `Json`.
    def capabilities: Json = negotiated.or(Empty().in[Json])
    def browserName: Optional[Text] = capability(t"browserName")
    def browserVersion: Optional[Text] = capability(t"browserVersion")
    def platformName: Optional[Text] = capability(t"platformName")

    private def capability(name: Text): Optional[Text] =
      safely(Session.text(capabilities.selectDynamic(name.s)))

    // Built structurally from the base URL's origin rather than by parsing text, so addressing a
    // command cannot fail and no `UrlError` reaches the caller.
    private def address(path: Text): HttpUrl = Url(base.origin, t"${base.location}/$path")

    // Bound as a local before each lambda below: reading the field inside one would put the whole
    // session in the lambda's hidden set, overlapping the `tactic` it is applied to.
    private val note: Diagnostics = diagnostics

    // The failures a WebDriver conversation can suffer beneath the protocol — an unreachable
    // driver, a reply that is not the JSON the specification describes — are translated into
    // `Error` here rather than raised at the caller, so that driving a browser has one
    // error type and not five. `contramap` builds each translating tactic once, from the caller's
    // own, so the caller's diagnostics and recovery strategy still apply.
    private given connectTactic: (Tactic[Connect.Error]^) =
      tactic.contramap(_ => Session.malformed(t"the WebDriver could not be reached")(using note))

    private given jsonTactic: (Tactic[Json.Error]^) =
      tactic.contramap(_ => Session.malformed(t"the reply had an unexpected shape")(using note))

    private given mediaTactic: (Tactic[MediaType.Error]^) =
      tactic.contramap(_ => Session.malformed(t"the media type was not valid")(using note))

    private given base64Tactic: (Tactic[Serialization.Error]^) =
      tactic.contramap(_ => Session.malformed(t"the screenshot was not valid Base64")(using note))

    private given decodeTactic: (Tactic[CharDecoder.Error]^) =
      tactic.contramap(_ => Session.malformed(t"the reply was not valid UTF-8")(using note))

    // The `using`/`value` pair the specification requires, rendered by the `Focusable` instance.
    private def locator[focus: Focusable](value: focus): Json =
      Locator(focus.strategy, focus.focus(value)).in[Json]

    // The W3C error object lives in the *body* of a 4xx or 5xx, and `receive` aborts with an
    // `Http.Error` that has already discarded it — so the status is checked here and the body read
    // directly, as `apoplexy.Conformant` does. A reply that is not JSON at all (a crashed driver,
    // an interposed proxy) still becomes a `Error`, carrying the raw body as its detail.
    private def outcome(response: Http.Response): Json =
      // The body's own stream, not `read`/`receive`: both consult the status first and would
      // abort with an `Http.Error` that has already discarded what we came for.
      val body: Text = response.body.stream.memoize.read[Text]
      val json: Optional[Json] = safely(body.as[Json])

      if response.status.category == Http.Status.Category.Successful
      then json.lest(Session.malformed(body)(using note))
      else json.lay(abort(Session.malformed(body)(using note))): json =>
        val failure: Failure = Session.failure(json.value)

        abort:
          Error
            ( Error.Reason(failure.error.or(t"unknown error")),
              failure.message.or(body),
              failure.stacktrace.lay(Nil)(_.cut(t"\n")),
              failure.data )

    private def get(path: Text): Json =
      outcome(address(path).fetch(contentType = media"application/json"))

    private def post(path: Text, content: Json): Json = outcome(address(path).submit()(content))
    private def delete(path: Text): Json = outcome(address(path).fetch(Http.Delete))

    private def read(path: Text): Json = get(t"session/$id/$path")

    // The session-scoped `DELETE`s: dropping a cookie, closing a window, releasing held keys.
    private def drop(path: Text): Unit =
      delete(t"session/$id/$path")
      ()

    private def send(path: Text, content: Json): Json = post(t"session/$id/$path", content)

    // A command issued for its effect on the browser: the driver replies `{"value":null}`, and
    // there is nothing to read back.
    private def command(path: Text, content: Json): Unit =
      send(path, content)
      ()

    // Opens the remote session. Called by the `Sessional` that owns this handle, before the block
    // sees it — the constructor cannot do it, because a failure here must reach the caller as a
    // `Error` and not as an exception from a field initializer.
    private[tarantula] def create(capabilities: Json): Unit =
      val value = post(t"session", capabilities).value
      id = Session.text(value.sessionId)
      negotiated = safely(value.capabilities)

    // Ends the session, and with it the browser instance. Called from the `finally` of the
    // `Sessional` that opened it; a caller never needs it, and could not use the handle
    // afterwards anyway.
    private[tarantula] def close(): Unit =
      delete(t"session/$id")
      ()

    private def elementPath(element: Element, path: Text): Text =
      t"element/${element.elementId}/$path"

    private def handle(json: Json, key: Text): Text =
      Session.text(json.value.selectDynamic(key.s))

    private def handles(json: Json): List[Element] =
      Session.list(json.value).map: json => Element(Session.text(json(Wei)))

    // Navigation.

    def navigateTo[url: Abstractable across Urls to Text](url: url): Unit =
      command(t"url", Address(url.generic).in[Json])

    // Overloaded for the commonest case. `url"http://…"` has the singleton type `Url["http"]`,
    // while the `Abstractable` instance is declared for `HttpUrl` — `Url["http" | "https"]` —
    // whose `Self` member is invariant, so the generic form alone rejects the interpolator's own
    // result.
    def navigateTo(url: HttpUrl): Unit = command(t"url", Address(url.show).in[Json])

    def url[url: Instantiable across Urls from Text](): url =
      url(Session.text(read(t"url").value))

    def back(): Unit = command(t"back", Empty().in[Json])
    def forward(): Unit = command(t"forward", Empty().in[Json])
    def refresh(): Unit = command(t"refresh", Empty().in[Json])
    def title(): Text = Session.text(read(t"title").value)
    def source(): Text = Session.text(read(t"source").value)

    // The raw PNG bytes of the viewport. `screenshot()`, which decodes them into a `Raster in
    // Png`, is an extension method in `tarantula.image`, so that browser automation does not
    // depend on an image-codec library.
    def screenshotData(): Data =
      Session.text(read(t"screenshot").value).deserialize[Base64]

    // Windows and tabs. A handle is an opaque token, like an element's, and only the current
    // window responds to commands — hence `switchTo`.
    def window(): Text = Session.text(read(t"window").value)

    def closeWindow(): List[Text] =
      Session.texts(delete(t"session/$id/window").value)

    def switchTo(window: Text): Unit = command(t"window", Handle(window).in[Json])
    def windowRect(): Session.Rect = Session.rect(read(t"window/rect").value)
    def maximize(): Session.Rect = sized(t"maximize")
    def minimize(): Session.Rect = sized(t"minimize")
    def fullscreen(): Session.Rect = sized(t"fullscreen")

    def windows(): List[Text] =
      Session.texts(read(t"window/handles").value)

    def windowRect(rect: Session.Rect): Unit =
      command(t"window/rect", rect.in[Json])

    // The reply is the resulting geometry, which is worth returning: a window manager may refuse
    // to make a window the size that was asked for.
    private def sized(path: Text): Session.Rect =
      Session.rect(send(t"window/$path", Empty().in[Json]).value)

    // A new window or tab is *not* switched to: the specification leaves the current window where
    // it was, and hands back a handle to pass to `switchTo`.
    def newWindow(tab: Boolean = true): Text =
      val kind = if tab then t"tab" else t"window"
      Session.text(send(t"window/new", Kind(kind).in[Json]).value.handle)

    // Frames. Commands address the current browsing context, so entering a frame is a mode change
    // rather than an argument, and `topFrame()` is the way back out of any depth.
    def frame(element: Element): Unit = command(t"frame", Frame(element.in[Json]).in[Json])
    def frame(index: Int): Unit = command(t"frame", Frame(index.in[Json]).in[Json])
    def parentFrame(): Unit = command(t"frame/parent", Empty().in[Json])
    // `{"id":null}`, not `{}`: the protocol distinguishes an explicit null — return to the
    // top-level browsing context — from an absent key, which is invalid.
    def topFrame(): Unit = command(t"frame", Frame(Session.nul).in[Json])

    // User prompts. Every other command fails with `UnexpectedAlertOpen` while one of these is
    // open, unless the session asked for a different `unhandledPromptBehavior`.
    def acceptAlert(): Unit = command(t"alert/accept", Empty().in[Json])
    def dismissAlert(): Unit = command(t"alert/dismiss", Empty().in[Json])
    def alertText(): Text = Session.text(read(t"alert/text").value)
    def alertText(text: Text): Unit = command(t"alert/text", Alert(text).in[Json])

    // Timeouts.
    def timeouts(): Session.Timeouts =
      Session.timeouts(read(t"timeouts").value)

    def timeouts(timeouts: Session.Timeouts): Unit =
      command(t"timeouts", timeouts.in[Json])

    // Cookies, reusing telekinesis's own `Cookie.Value`, whose fields are already the names the
    // protocol uses. It has no `sameSite`, so that attribute is neither read nor set; a cookie
    // needing one can be installed with `execute`.
    def cookies(): List[Cookie.Value] =
      Session.cookies(read(t"cookie").value)

    def cookie(name: Text): Cookie.Value =
      Session.cookie(read(t"cookie/$name").value)

    def addCookie(cookie: Cookie.Value): Unit = command(t"cookie", Cookies(cookie).in[Json])
    def deleteCookie(name: Text): Unit = drop(t"cookie/$name")
    def deleteCookies(): Unit = drop(t"cookie")

    // Input actions. One call drives one input source; `actions` takes the assembled JSON for the
    // rare case of two sources moving in lockstep.
    def perform(source: Session.Action.Source, steps: List[Session.Action])
    :   Unit =

      val encoded = steps.map(_.in[Json])
      actions(Sequences(List(Sequence(source.id, source.kind, encoded).in[Json])).in[Json])

    def actions(json: Json): Unit = command(t"actions", json)

    // Types a sequence of keypresses, modifiers and all. `WebDriver.Element#value` remains the
    // way to fill a field with plain text — it is one request rather than four actions per
    // character — and this is for the chords and named keys that text entry cannot express.
    def press(keypresses: Keypress*): Unit =
      val steps = keypresses.to(List).bind(Session.Action.steps(_))
      perform(Session.Action.Source.Key, steps)

    // Releases every key and button an earlier `perform` left held, and clears the input state.
    def releaseActions(): Unit = drop(t"actions")

    // The page as a PDF, in raw bytes. Not universally implemented — Safari does not have it —
    // and unrelated to `screenshotData`, which captures what is rendered rather than what would
    // be printed.
    def printPage(options: Session.Print = Session.Print()): Data =
      Session.text(send(t"print", Session.encode(options)).value).deserialize[Base64]

    // Script execution: the escape hatch for anything the protocol does not model.
    def execute(script: Text, arguments: List[Json] = Nil): Json =
      send(t"execute/sync", Script(script, arguments).in[Json]).value

    def executeAsync(script: Text, arguments: List[Json] = Nil): Json =
      send(t"execute/async", Script(script, arguments).in[Json]).value

    // Finding elements, at page scope and relative to an element. `Element` is pure, so building
    // one inside this `map` constructs no capability and hoists no fresh root.
    def element[focus: Focusable](value: focus): Element =
      Element(handle(send(t"element", locator(value)), Wei))

    def elements[focus: Focusable](value: focus): List[Element] =
      handles(send(t"elements", locator(value)))

    def element[focus: Focusable](element: Element, value: focus): Element =
      Element(handle(send(elementPath(element, t"element"), locator(value)), Wei))

    def elements[focus: Focusable](element: Element, value: focus): List[Element] =
      handles(send(elementPath(element, t"elements"), locator(value)))

    def activeElement(): Element = Element(handle(read(t"element/active"), Wei))

    // Waiting. The protocol has no wait command: the `implicit` timeout covers find commands and
    // nothing else, so a button becoming enabled, a heading's text changing or a prompt appearing
    // has no server-side notion of waiting at all.
    //
    // These poll `POST /elements`, which answers with an empty list where `POST /element` raises
    // `NoSuchElement`. That choice is load-bearing rather than incidental: the session's
    // `Tactic[Error]` is fixed when the session is constructed, so a failure raised inside a
    // caller's block goes to the caller's handler and cannot be intercepted here to drive a
    // retry. Polling a command that reports absence as a *value* sidesteps that entirely.
    //
    // The policy is the ambient `Tenacity` — `exponentialFiveTimesTenacity` and its siblings are
    // exported from `soundness` — so the schedule is the caller's to choose.
    def awaitElements[focus: Focusable](value: focus)
      ( using Tenacity, Monitor, Tactic[Tenacity.Error] )
    :   List[Element] =

      retry: (surrender, persevere) ?=>
        val found = elements(value)
        if found.nil then persevere() else found

    def awaitElement[focus: Focusable](value: focus)
      ( using Tenacity, Monitor, Tactic[Tenacity.Error] )
    :   Element =

      awaitElements(value).prim.lest(Error(Error.Reason.NoSuchElement, t"nothing matched", Nil))

    // The general form, for a condition the caller can evaluate without raising — comparing a
    // title, counting elements, reading a cookie.
    def awaitUntil(condition: => Boolean)(using Tenacity, Monitor, Tactic[Tenacity.Error]): Unit =
      retry: (surrender, persevere) ?=>
        val satisfied = condition
        if satisfied then () else persevere()

    // Shadow DOM. Only *open* shadow roots are reachable; a closed one raises `NoSuchShadowRoot`.
    def shadowRoot(element: Element): ShadowRoot =
      ShadowRoot(handle(read(elementPath(element, t"shadow")), Shadow))

    def element[focus: Focusable](root: ShadowRoot, value: focus): Element =
      Element(handle(send(t"shadow/${root.shadowId}/element", locator(value)), Wei))

    def elements[focus: Focusable](root: ShadowRoot, value: focus): List[Element] =
      handles(send(t"shadow/${root.shadowId}/elements", locator(value)))

    // Acting on an element.
    def click(element: Element): Unit = command(elementPath(element, t"click"), Empty().in[Json])
    def clear(element: Element): Unit = command(elementPath(element, t"clear"), Empty().in[Json])

    def value(element: Element, text: Text): Unit =
      command(elementPath(element, t"value"), Keys(text).in[Json])

    // Reading an element's state. `attribute` reads the markup's attribute, `property` the live
    // DOM property; the two diverge as soon as a page's script touches the node.
    //
    // `innerText`, not `text`: the specification computes this with the `innerText` algorithm —
    // rendered text, not `textContent` — and `text` is already a top-level extension in the
    // `soundness` package, contributed by gossamer for `Array[Char]`.
    def innerText(element: Element): Text =
      Session.text(read(elementPath(element, t"text")).value)

    def tagName(element: Element): Text =
      Session.text(read(elementPath(element, t"name")).value)

    def role(element: Element): Text =
      Session.text(read(elementPath(element, t"computedrole")).value)

    def label(element: Element): Text =
      Session.text(read(elementPath(element, t"computedlabel")).value)

    def enabled(element: Element): Boolean =
      Session.boolean(read(elementPath(element, t"enabled")).value)

    def selected(element: Element): Boolean =
      Session.boolean(read(elementPath(element, t"selected")).value)

    def attribute(element: Element, name: Text): Optional[Text] =
      safely(Session.text(read(elementPath(element, t"attribute/$name")).value))

    def property(element: Element, name: Text): Optional[Text] =
      safely(Session.text(read(elementPath(element, t"property/$name")).value))

    def css(element: Element, name: Text): Text =
      Session.text(read(elementPath(element, t"css/$name")).value)

    def rect(element: Element): Session.Rect =
      Session.rect(read(elementPath(element, t"rect")).value)

    // Not a protocol command: `displayed` was dropped between the JSON Wire Protocol and the W3C
    // specification, which offers the "element displayedness" atom instead. Running it as a script
    // is what every conforming client does.
    def displayed(element: Element): Boolean =
      Session.boolean:
        execute(t"return arguments[0].getClientRects().length > 0", List(element.in[Json]))

    def screenshotData(element: Element): Data =
      Session.text(read(elementPath(element, t"screenshot")).value).deserialize[Base64]

// A WebDriver that is already listening: one started by hand, or a remote grid.
// `WebDriver.Chrome` and its siblings are the usual entry points; this is what they delegate to
// once the driver they launched is answering.
class WebDriver(val base: HttpUrl, val capabilities: Json)
