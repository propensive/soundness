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

// The W3C error object, with every field optional: a driver may omit the stacktrace, and a
// reply that is JSON but not this shape must still yield a `WebDriverError`, not a `JsonError`.
private case class Failure
  ( error: Optional[Text], message: Optional[Text], stacktrace: Optional[Text] )

// The wire form of an element handle, under the W3C "web element identifier" — a key chosen to be
// one no page can collide with.
private case class ElementRef
  ( @name[Json](t"element-6066-11e4-a52e-4f735466cecf") elementId: Text )

object WebDriverSession:
  // The bounding box of an element, in CSS pixels relative to the document.
  case class Rect(x: Double, y: Double, width: Double, height: Double)

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

  private def elementPath(element: Element, path: Text): Text =
    t"element/${element.elementId}/$path"

  private def handle(json: Json, key: Text): Text =
    WebDriverSession.text(json.value.selectDynamic(key.s))

  private def handles(json: Json): List[Element] =
    val values = WebDriverSession.list(json.value).stdlib
    List.of(values.map { json => Element(WebDriverSession.text(json(Wei))) })

  // Navigation.

  def navigateTo[url: Abstractable across Urls to Text](url: url): Unit =
    command(t"url", Address(url.generic).in[Json])

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
  def text(element: Element): Text =
    WebDriverSession.text(read(elementPath(element, t"text")).value)

  def tagName(element: Element): Text =
    WebDriverSession.text(read(elementPath(element, t"name")).value)

  def role(element: Element): Text =
    WebDriverSession.text(read(elementPath(element, t"computedrole")).value)

  def label(element: Element): Text =
    WebDriverSession.text(read(elementPath(element, t"computedlabel")).value)

  def enabled(element: Element): Boolean =
    WebDriverSession.boolean(read(elementPath(element, t"enabled")).value)

  def selected(element: Element): Boolean =
    WebDriverSession.boolean(read(elementPath(element, t"selected")).value)

  def attribute(element: Element, name: Text): Optional[Text] =
    safely(WebDriverSession.text(read(elementPath(element, t"attribute/$name")).value))

  def property(element: Element, name: Text): Optional[Text] =
    safely(WebDriverSession.text(read(elementPath(element, t"property/$name")).value))

  def css(element: Element, name: Text): Text =
    WebDriverSession.text(read(elementPath(element, t"css/$name")).value)

  def rect(element: Element): WebDriverSession.Rect =
    WebDriverSession.rect(read(elementPath(element, t"rect")).value)

  // Not a protocol command: `displayed` was dropped between the JSON Wire Protocol and the W3C
  // specification, which offers the "element displayedness" atom instead. Running it as a script
  // is what every conforming client does.
  def displayed(element: Element): Boolean =
    WebDriverSession.boolean:
      execute(t"return arguments[0].getClientRects().length > 0", List(element.in[Json]))

  def screenshotData(element: Element): Data =
    WebDriverSession.text(read(elementPath(element, t"screenshot")).value).deserialize[Base64]
