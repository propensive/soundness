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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder, charDecoders.utf8Decoder
import textSanitizers.strictSanitizer
import jacinta.*, formatting.compactJsonFormatting, dynamicJsonAccess.enabled
import monotonous.*, alphabets.base64Standard
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import zephyrine.memoize
import urticose.*
import vacuous.*

import WebDriverError.Reason
import WebDriverSession.malformed
import adversaria.name

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
// reply that is JSON but not this shape must still yield a `WebDriverError`, not a `JsonError`.
private case class Failure
  ( error: Optional[Text], message: Optional[Text], stacktrace: Optional[Text] )

// The wire form of an element handle, under the W3C "web element identifier" — a key chosen to be
// one no page can collide with.
private case class ElementRef
  ( @name[Json](t"element-6066-11e4-a52e-4f735466cecf") elementId: Text )

object WebDriverSession:
  // The bounding box of an element, or of a window, in CSS pixels.
  case class Rect(x: Double, y: Double, width: Double, height: Double)

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
  private[tarantula] def malformed(detail: Text)(using Diagnostics): WebDriverError =
    WebDriverError(Reason.UnknownError, detail, Nil)

  // Every decode of a driver's reply goes through one of these five, and each is sealed with
  // `unsafeAssumeSeparate` for the same reason: `Json#as` takes the `Tactic[JsonError]` both
  // directly and inside the capture set of the decodable it summons, so a *capability* tactic —
  // which is what `raises` and `contramap` produce — reads as two overlapping uses. This is the
  // Foci/tracks cluster recorded in rep/DECISIONS.md, and the reason the module previously
  // imported `strategies.throwUnsafely`, whose tactic is pure and so never overlaps. Each body
  // here is a single decode of a single value against a single tactic: there is no second use to
  // interleave with, and confining the assertion to these five lines keeps every caller honest.
  private[tarantula] def text(json: Json): Text raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[Text])

  private[tarantula] def boolean(json: Json): Boolean raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[Boolean])

  private[tarantula] def rect(json: Json): Rect raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[Rect])

  private[tarantula] def list(json: Json): List[Json] raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[List[Json]])

  private[tarantula] def failure(json: Json): Failure raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[Failure])

  object Action:
    // The two kinds of input source the specification defines that carry actions. (A "none" source
    // exists too, but it can only pause, which either of these can do.) One `perform` call drives
    // one source; driving two in lockstep — a chord across keyboard and pointer — is rare enough
    // that it is left to `WebDriverSession#actions`, which takes the assembled JSON.
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
  // drag: the specification replaced the JSON Wire Protocol's `/moveto`, `/click` and `/keys` with
  // a single `/actions` endpoint, which is why `tarantula` could not simulate a keypress before.
  // Plain text entry into a field stays on `WebElement#value`, which is simpler and faster.
  enum Action:
    case KeyDown(value: Text)
    case KeyUp(value: Text)
    case Pause(duration: Int)
    case PointerDown(button: Int = Action.LeftButton)
    case PointerUp(button: Int = Action.LeftButton)

    // Coordinates are relative to `origin`: the viewport when it is `Unset`, or the centre of the
    // element when one is given.
    case PointerMove(x: Int, y: Int, duration: Int = 100, origin: Optional[WebElement] = Unset)

  // A generic `decode[value]` would not do: taking the decodable as a parameter moves its
  // summon to the call site, where the tactic is a field of the session again and the overlap
  // returns. So each shape the protocol replies with gets its own line here.
  // The literal `null`, parsed once. Constructing it here rather than in the session keeps the
  // `unsafely` — which cannot fail, the text being a literal — out of the request path.
  private[tarantula] val nul: Json = unsafely(t"null".read[Json])

  private[tarantula] def texts(json: Json): List[Text] raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[List[Text]])

  private[tarantula] def cookie(json: Json): Cookie.Value raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[Cookie.Value])

  private[tarantula] def cookies(json: Json): List[Cookie.Value] raises JsonError =
    caps.unsafe.unsafeAssumeSeparate(json.as[List[Cookie.Value]])

  private[tarantula] def timeouts(json: Json): Timeouts raises JsonError =
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
class WebDriverSession private[tarantula] (base: HttpUrl)
  ( using online:      Online,
          backend:     Http.Backend,
          loggable:    (Http.Event is Loggable)^,
          tactic:      Tactic[WebDriverError],
          diagnostics: Diagnostics )
extends caps.ExclusiveCapability:

  // The W3C "web element identifier": the key under which a driver returns an element handle,
  // chosen to be one no page can collide with.
  private final val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

  // The shadow-root counterpart of `Wei`.
  private final val Shadow: Text = t"shadow-6066-11e4-a52e-4f735466cecf"

  // Set once by `create`, before the handle is lent to a block: the id does not exist until the
  // driver has minted it, and there is no session to be a method of until then.
  // `Text` is pure, so the variable tracks nothing; the annotation says so, rather than making
  // the whole session `Stateful` for one immutable-valued field.
  @scala.caps.unsafe.untrackedCaptures
  private var id: Text = t""

  def sessionId: Text = id

  // Built structurally from the base URL's origin rather than by parsing text, so addressing a
  // command cannot fail and no `UrlError` reaches the caller.
  private def address(path: Text): HttpUrl = Url(base.origin, t"${base.location}/$path")

  // Bound as a local before each lambda below: reading the field inside one would put the whole
  // session in the lambda's hidden set, overlapping the `tactic` it is applied to.
  private val note: Diagnostics = diagnostics

  // The failures a WebDriver conversation can suffer beneath the protocol — an unreachable
  // driver, a reply that is not the JSON the specification describes — are translated into
  // `WebDriverError` here rather than raised at the caller, so that driving a browser has one
  // error type and not five. `contramap` builds each translating tactic once, from the caller's
  // own, so the caller's diagnostics and recovery strategy still apply.
  private given connectTactic: (Tactic[ConnectError]^) =
    tactic.contramap(_ => malformed(t"the WebDriver could not be reached")(using note))

  private given jsonTactic: (Tactic[JsonError]^) =
    tactic.contramap(_ => malformed(t"the reply had an unexpected shape")(using note))

  private given mediaTactic: (Tactic[MediaType.Error]^) =
    tactic.contramap(_ => malformed(t"the media type was not valid")(using note))

  private given base64Tactic: (Tactic[SerializationError]^) =
    tactic.contramap(_ => malformed(t"the screenshot was not valid Base64")(using note))

  private given decodeTactic: (Tactic[CharDecodeError]^) =
    tactic.contramap(_ => malformed(t"the reply was not valid UTF-8")(using note))

  // The `using`/`value` pair the specification requires, rendered by the `Focusable` instance.
  private def locator[focus: Focusable](value: focus): Json =
    Locator(focus.strategy, focus.focus(value)).in[Json]

  // The W3C error object lives in the *body* of a 4xx or 5xx, and `receive` aborts with an
  // `Http.Error` that has already discarded it — so the status is checked here and the body read
  // directly, as `apoplexy.Conformant` does. A reply that is not JSON at all (a crashed driver,
  // an interposed proxy) still becomes a `WebDriverError`, carrying the raw body as its detail.
  private def outcome(response: Http.Response): Json =
    // The body's own stream, not `read`/`receive`: both consult the status first and would
    // abort with an `Http.Error` that has already discarded what we came for.
    val body: Text = response.body.stream.memoize.read[Text]
    val json: Optional[Json] = safely(body.as[Json])

    if response.status.category == Http.Status.Category.Successful
    then json.lest(malformed(body)(using note))
    else json.lay(abort(malformed(body)(using note))): json =>
      val failure: Failure = WebDriverSession.failure(json.value)

      abort:
        WebDriverError
          ( Reason(failure.error.or(t"unknown error")),
            failure.message.or(body),
            failure.stacktrace.lay(Nil)(_.cut(t"\n")) )

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
  // `WebDriverError` and not as an exception from a field initializer.
  private[tarantula] def create(capabilities: Json): Unit =
    id = WebDriverSession.text(post(t"session", capabilities).value.sessionId)

  // Ends the session, and with it the browser instance. Called from the `finally` of the
  // `Sessional` that opened it; a caller never needs it, and could not use the handle
  // afterwards anyway.
  private[tarantula] def close(): Unit =
    delete(t"session/$id")
    ()

  private def elementPath(element: WebElement, path: Text): Text =
    t"element/${element.elementId}/$path"

  private def handle(json: Json, key: Text): Text =
    WebDriverSession.text(json.value.selectDynamic(key.s))

  private def handles(json: Json): List[WebElement] =
    val values = WebDriverSession.list(json.value).stdlib
    List.of(values.map { json => WebElement(WebDriverSession.text(json(Wei))) })

  // Navigation.

  def navigateTo[url: Abstractable across Urls to Text](url: url): Unit =
    command(t"url", Address(url.generic).in[Json])

  // Overloaded for the commonest case. `url"http://…"` has the singleton type `Url["http"]`,
  // while the `Abstractable` instance is declared for `HttpUrl` — `Url["http" | "https"]` —
  // whose `Self` member is invariant, so the generic form alone rejects the interpolator's own
  // result.
  def navigateTo(url: HttpUrl): Unit = command(t"url", Address(url.show).in[Json])

  def url[url: Instantiable across Urls from Text](): url =
    url(WebDriverSession.text(read(t"url").value))

  def back(): Unit = command(t"back", Empty().in[Json])
  def forward(): Unit = command(t"forward", Empty().in[Json])
  def refresh(): Unit = command(t"refresh", Empty().in[Json])
  def title(): Text = WebDriverSession.text(read(t"title").value)
  def source(): Text = WebDriverSession.text(read(t"source").value)

  // The raw PNG bytes of the viewport. `screenshot()`, which decodes them into a `Raster in
  // Png`, is an extension method in `tarantula.image`, so that browser automation does not
  // depend on an image-codec library.
  def screenshotData(): Data =
    WebDriverSession.text(read(t"screenshot").value).deserialize[Base64]

  // Windows and tabs. A handle is an opaque token, like an element's, and only the current
  // window responds to commands — hence `switchTo`.
  def window(): Text = WebDriverSession.text(read(t"window").value)

  def closeWindow(): List[Text] =
    WebDriverSession.texts(delete(t"session/$id/window").value)

  def switchTo(window: Text): Unit = command(t"window", Handle(window).in[Json])
  def windowRect(): WebDriverSession.Rect = WebDriverSession.rect(read(t"window/rect").value)
  def maximize(): WebDriverSession.Rect = sized(t"maximize")
  def minimize(): WebDriverSession.Rect = sized(t"minimize")
  def fullscreen(): WebDriverSession.Rect = sized(t"fullscreen")

  def windows(): List[Text] =
    WebDriverSession.texts(read(t"window/handles").value)

  def windowRect(rect: WebDriverSession.Rect): Unit =
    command(t"window/rect", rect.in[Json])

  // The reply is the resulting geometry, which is worth returning: a window manager may refuse
  // to make a window the size that was asked for.
  private def sized(path: Text): WebDriverSession.Rect =
    WebDriverSession.rect(send(t"window/$path", Empty().in[Json]).value)

  // A new window or tab is *not* switched to: the specification leaves the current window where
  // it was, and hands back a handle to pass to `switchTo`.
  def newWindow(tab: Boolean = true): Text =
    val kind = if tab then t"tab" else t"window"
    WebDriverSession.text(send(t"window/new", Kind(kind).in[Json]).value.handle)

  // Frames. Commands address the current browsing context, so entering a frame is a mode change
  // rather than an argument, and `topFrame()` is the way back out of any depth.
  def frame(element: WebElement): Unit = command(t"frame", Frame(element.in[Json]).in[Json])
  def frame(index: Int): Unit = command(t"frame", Frame(index.in[Json]).in[Json])
  def parentFrame(): Unit = command(t"frame/parent", Empty().in[Json])
  // `{"id":null}`, not `{}`: the protocol distinguishes an explicit null — return to the
  // top-level browsing context — from an absent key, which is invalid.
  def topFrame(): Unit = command(t"frame", Frame(WebDriverSession.nul).in[Json])

  // User prompts. Every other command fails with `UnexpectedAlertOpen` while one of these is
  // open, unless the session asked for a different `unhandledPromptBehavior`.
  def acceptAlert(): Unit = command(t"alert/accept", Empty().in[Json])
  def dismissAlert(): Unit = command(t"alert/dismiss", Empty().in[Json])
  def alertText(): Text = WebDriverSession.text(read(t"alert/text").value)
  def alertText(text: Text): Unit = command(t"alert/text", Alert(text).in[Json])

  // Timeouts.
  def timeouts(): WebDriverSession.Timeouts =
    WebDriverSession.timeouts(read(t"timeouts").value)

  def timeouts(timeouts: WebDriverSession.Timeouts): Unit =
    command(t"timeouts", timeouts.in[Json])

  // Cookies, reusing telekinesis's own `Cookie.Value`, whose fields are already the names the
  // protocol uses. It has no `sameSite`, so that attribute is neither read nor set; a cookie
  // needing one can be installed with `execute`.
  def cookies(): List[Cookie.Value] =
    WebDriverSession.cookies(read(t"cookie").value)

  def cookie(name: Text): Cookie.Value =
    WebDriverSession.cookie(read(t"cookie/$name").value)

  def addCookie(cookie: Cookie.Value): Unit = command(t"cookie", Cookies(cookie).in[Json])
  def deleteCookie(name: Text): Unit = drop(t"cookie/$name")
  def deleteCookies(): Unit = drop(t"cookie")

  // Input actions. One call drives one input source; `actions` takes the assembled JSON for the
  // rare case of two sources moving in lockstep.
  def perform(source: WebDriverSession.Action.Source, steps: List[WebDriverSession.Action])
  :   Unit =

    val encoded = steps.map(_.in[Json])
    actions(Sequences(List(Sequence(source.id, source.kind, encoded).in[Json])).in[Json])

  def actions(json: Json): Unit = command(t"actions", json)

  // Releases every key and button an earlier `perform` left held, and clears the input state.
  def releaseActions(): Unit = drop(t"actions")

  // The page as a PDF, in raw bytes. Not universally implemented — Safari does not have it —
  // and unrelated to `screenshotData`, which captures what is rendered rather than what would
  // be printed.
  def printPage(): Data =
    WebDriverSession.text(send(t"print", Empty().in[Json]).value).deserialize[Base64]

  // Script execution: the escape hatch for anything the protocol does not model.
  def execute(script: Text, arguments: List[Json] = Nil): Json =
    send(t"execute/sync", Script(script, arguments).in[Json]).value

  def executeAsync(script: Text, arguments: List[Json] = Nil): Json =
    send(t"execute/async", Script(script, arguments).in[Json]).value

  // Finding elements, at page scope and relative to an element. `WebElement` is pure, so building
  // one inside this `map` constructs no capability and hoists no fresh root.
  def element[focus: Focusable](value: focus): WebElement =
    WebElement(handle(send(t"element", locator(value)), Wei))

  def elements[focus: Focusable](value: focus): List[WebElement] =
    handles(send(t"elements", locator(value)))

  def element[focus: Focusable](element: WebElement, value: focus): WebElement =
    WebElement(handle(send(elementPath(element, t"element"), locator(value)), Wei))

  def elements[focus: Focusable](element: WebElement, value: focus): List[WebElement] =
    handles(send(elementPath(element, t"elements"), locator(value)))

  def activeElement(): WebElement = WebElement(handle(read(t"element/active"), Wei))

  // Shadow DOM. Only *open* shadow roots are reachable; a closed one raises `NoSuchShadowRoot`.
  def shadowRoot(element: WebElement): ShadowRoot =
    ShadowRoot(handle(read(elementPath(element, t"shadow")), Shadow))

  def element[focus: Focusable](root: ShadowRoot, value: focus): WebElement =
    WebElement(handle(send(t"shadow/${root.shadowId}/element", locator(value)), Wei))

  def elements[focus: Focusable](root: ShadowRoot, value: focus): List[WebElement] =
    handles(send(t"shadow/${root.shadowId}/elements", locator(value)))

  // Acting on an element.
  def click(element: WebElement): Unit = command(elementPath(element, t"click"), Empty().in[Json])
  def clear(element: WebElement): Unit = command(elementPath(element, t"clear"), Empty().in[Json])

  def value(element: WebElement, text: Text): Unit =
    command(elementPath(element, t"value"), Keys(text).in[Json])

  // Reading an element's state. `attribute` reads the markup's attribute, `property` the live
  // DOM property; the two diverge as soon as a page's script touches the node.
  //
  // `innerText`, not `text`: the specification computes this with the `innerText` algorithm —
  // rendered text, not `textContent` — and `text` is already a top-level extension in the
  // `soundness` package, contributed by gossamer for `Array[Char]`.
  def innerText(element: WebElement): Text =
    WebDriverSession.text(read(elementPath(element, t"text")).value)

  def tagName(element: WebElement): Text =
    WebDriverSession.text(read(elementPath(element, t"name")).value)

  def role(element: WebElement): Text =
    WebDriverSession.text(read(elementPath(element, t"computedrole")).value)

  def label(element: WebElement): Text =
    WebDriverSession.text(read(elementPath(element, t"computedlabel")).value)

  def enabled(element: WebElement): Boolean =
    WebDriverSession.boolean(read(elementPath(element, t"enabled")).value)

  def selected(element: WebElement): Boolean =
    WebDriverSession.boolean(read(elementPath(element, t"selected")).value)

  def attribute(element: WebElement, name: Text): Optional[Text] =
    safely(WebDriverSession.text(read(elementPath(element, t"attribute/$name")).value))

  def property(element: WebElement, name: Text): Optional[Text] =
    safely(WebDriverSession.text(read(elementPath(element, t"property/$name")).value))

  def css(element: WebElement, name: Text): Text =
    WebDriverSession.text(read(elementPath(element, t"css/$name")).value)

  def rect(element: WebElement): WebDriverSession.Rect =
    WebDriverSession.rect(read(elementPath(element, t"rect")).value)

  // Not a protocol command: `displayed` was dropped between the JSON Wire Protocol and the W3C
  // specification, which offers the "element displayedness" atom instead. Running it as a script
  // is what every conforming client does.
  def displayed(element: WebElement): Boolean =
    WebDriverSession.boolean:
      execute(t"return arguments[0].getClientRects().length > 0", List(element.in[Json]))

  def screenshotData(element: WebElement): Data =
    WebDriverSession.text(read(elementPath(element, t"screenshot")).value).deserialize[Base64]
