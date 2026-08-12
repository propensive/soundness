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

import scala.language.experimental.pureFunctions

import soundness.*

import charDecoders.utf8Decoder
import charEncoders.utf8Encoder
import dynamicJsonAccess.enabled
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
import textSanitizers.skipSanitizer

object FakeDriver:
  // One request as the driver saw it. The body is `Unset` for a request that had none, so that a
  // `GET` and a `POST` of `{}` are not confused.
  case class Exchange(method: Http.Method, path: Text, body: Optional[Text])

  val ok: Http.Status = Http.Ok

  def reply(json: Text): Http.Response = Http.Response(Http.Ok)(json)
  def value(json: Text): Http.Response = reply(t"""{"value":$json}""")
  def none: Http.Response = value(t"null")
  def string(text: Text): Http.Response = value(t"\"$text\"")

  // The W3C error object: a 4xx or 5xx whose body says which error, in words the specification
  // fixes. This is the shape `WebDriverSession#outcome` exists to read.
  def failure(status: Http.Status, code: Text, message: Text, trace: Text = t""): Http.Response =
    Http.Response(status):
      t"""{"value":{"error":"$code","message":"$message","stacktrace":"$trace"}}"""

// A fake WebDriver: routes on method and path, records the conversation, and replies with canned
// W3C envelopes, so the whole protocol layer is exercised with no browser and no socket. The
// route is a function rather than a map because a response body may only be read once.
// A *pure* function, so that `FakeDriver` is itself pure: `Http.Backend` is required unadorned
// by everything that summons one, and a route capturing a capability would make the fake a
// capability too.
class FakeDriver(route: (Http.Method, Text) -> Http.Response) extends Http.Backend:
  @scala.caps.unsafe.untrackedCaptures
  var exchanges: List[FakeDriver.Exchange] = Nil

  def request
     ( url: Text, method: Http.Method, headers: List[Http.Header], body: Spring[Data]^ )
     ( using Tactic[ConnectError] )
  :   Http.Response =

    val data = body().memoize
    val sent = if data.readable.isEmpty then Unset else data.read[Text]
    val path = url.skip(t"http://localhost:4444".length)
    exchanges ::= FakeDriver.Exchange(method, path, sent)

    route(method, path)

object Tests extends Suite(m"Tarantula tests"):
  import FakeDriver.*

  val sessionId: Text = t"CE6B6B4A"

  // Every conversation starts by creating a session and ends by deleting it; a test's own route
  // is consulted first, so it may override either.
  def driver(route: (Http.Method, Text) -> Http.Response): FakeDriver =
    FakeDriver: (method, path) =>
      if method == Http.Post && path == t"/session"
      then value(t"""{"sessionId":"$sessionId"}""")
      else if method == Http.Delete && path == t"/session/$sessionId" then none
      else route(method, path)

  def run(): Unit =
    // The tag vocabulary the `Focusable` instance for `Tag` locates by. Scoped to `run`, since
    // `whatwg.*` also defines an `WebElement`, which the package's own would otherwise have to
    // out-rank at every use.
    import doms.html.whatwg
    import whatwg.*

    suite(m"Session lifecycle"):
      test(m"the session id is read from the W3C envelope"):
        val fake = driver((_, _) => none)
        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
          val id: Text = session.sessionId
          id

      . assert(_ == sessionId)

      test(m"capabilities are posted as an alwaysMatch object"):
        val fake = driver((_, _) => none)
        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", WebDriver.Chrome.headless.capabilities).session:
          session ?=> ()

        fake.exchanges.reverse.prim.let(_.body.or(t"").read[Json])
        . let(_.capabilities.alwaysMatch.browserName.as[Text])

      . assert(_ == t"chrome")

      test(m"a headless Chrome session asks for the new headless mode"):
        WebDriver.Chrome.headless.capabilities.show
      . assert(_.contains(t"--headless=new"))

      test(m"a headless Firefox session asks for the single-dashed flag"):
        WebDriver.Firefox.headless.capabilities.show
      . assert(_.contains(t"-headless"))

      test(m"the session is deleted when the block ends"):
        val fake = driver((_, _) => string(t"Example"))
        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
          session.title()

        fake.exchanges.prim.let { exchange => (exchange.method, exchange.path) }

      . assert(_ == (Http.Delete, t"/session/$sessionId"))

      test(m"the session is deleted even when the block throws"):
        val fake = driver((_, _) => none)
        given Http.Backend = fake

        safely:
          WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
            abort(WebDriverError(WebDriverError.Reason.UnknownError, t"deliberate", Nil))

        fake.exchanges.prim.let(_.method)

      . assert(_ == Http.Delete)

    suite(m"Locator strategies"):
      def located(lambda: (session: WebDriverSession^) ?=> Unit): Json =
        val fake = driver((_, _) => value(t"""{"element-6066-11e4-a52e-4f735466cecf":"E1"}"""))
        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session(lambda)

        fake.exchanges.reverse.stdlib.filter(_.path.ends(t"/element")).head.body.or(t"")
        . read[Json]

      test(m"a tag is located by tag name"):
        val json = located(browser.element(H1))
        (json.selectDynamic("using").as[Text], json.value.as[Text])
      . assert(_ == (t"tag name", t"h1"))

      test(m"link text is located as link text"):
        val json = located(browser.element(t"Read more"))
        (json.selectDynamic("using").as[Text], json.value.as[Text])
      . assert(_ == (t"link text", t"Read more"))

      test(m"a CSS selector is located as a CSS selector"):
        val json = located(browser.element(SelectorList.read(t".menu li")))
        json.selectDynamic("using").as[Text]
      . assert(_ == t"css selector")

    suite(m"WebElement handles"):
      test(m"an element handle is unwrapped from the W3C key"):
        val fake = driver: (_, _) =>
          value(t"""{"element-6066-11e4-a52e-4f735466cecf":"E17"}""")

        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
          browser.element(H1).elementId

      . assert(_ == t"E17")

      test(m"a legacy ELEMENT key is rejected rather than silently ignored"):
        val fake = driver((_, _) => value(t"""{"ELEMENT":"E17"}"""))
        given Http.Backend = fake

        capture[WebDriverError]:
          WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
            browser.element(H1)

        . reason

      . assert(_ == WebDriverError.Reason.UnknownError)

      test(m"an element command addresses the element by its handle"):
        val fake = driver: (method, path) =>
          if path.ends(t"/element") then value(t"""{"element-6066-11e4-a52e-4f735466cecf":"E9"}""")
          else none

        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
          browser.element(H1).click()

        fake.exchanges.stdlib.filter(_.path.ends(t"/click")).headOption.map(_.path)

      . assert(_ == Some(t"/session/$sessionId/element/E9/click"))

    suite(m"Error decoding"):
      def raised(status: Http.Status, code: Text, message: Text, trace: Text = t"")
      :   WebDriverError =

        val fake = driver((_, _) => failure(status, code, message, trace))
        given Http.Backend = fake

        capture[WebDriverError]:
          WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
            browser.title()

      test(m"a no-such-element reply decodes to NoSuchElement"):
        raised(Http.NotFound, t"no such element", t"nothing matched").reason
      . assert(_ == WebDriverError.Reason.NoSuchElement)

      test(m"the driver's message becomes the error's detail"):
        raised(Http.NotFound, t"no such element", t"nothing matched").detail
      . assert(_ == t"nothing matched")

      test(m"the browser stacktrace is split on newlines"):
        raised(Http.NotFound, t"no such element", t"gone", t"one\\ntwo").browserStacktrace
      . assert(_ == List(t"one", t"two"))

      test(m"a click intercepted reply decodes to a code added since"):
        raised(Http.BadRequest, t"element click intercepted", t"covered").reason
      . assert(_ == WebDriverError.Reason.ElementClickIntercepted)

      test(m"an unrecognized code is kept verbatim rather than lost"):
        raised(Http.BadRequest, t"future error", t"unknown").reason
      . assert(_ == WebDriverError.Reason.Other(t"future error"))

      test(m"a non-JSON body still raises WebDriverError, carrying the body"):
        val fake = driver((_, _) => Http.Response(Http.InternalServerError)(t"<html>crash</html>"))
        given Http.Backend = fake

        capture[WebDriverError]:
          WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
            browser.title()

        . detail

      . assert(_ == t"<html>crash</html>")

    suite(m"Protocol shapes"):
      val target: HttpUrl = url"http://example.com/"

      def sent(lambda: (session: WebDriverSession^) ?=> Unit): List[Exchange] =
        val fake = driver((_, _) => string(t"Example"))
        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session(lambda)
        fake.exchanges.reverse.stdlib.filter(_.path != t"/session").to(List)

      test(m"navigation posts the target URL"):
        sent(browser.navigateTo(target)).prim.let(_.body.or(t""))
      . assert(_ == t"""{"url":"http://example.com/"}""")

      test(m"script execution uses the W3C execute/sync path"):
        sent(browser.execute(t"return 1")).prim.let(_.path)
      . assert(_ == t"/session/$sessionId/execute/sync")

      test(m"script execution sends a script and an argument list"):
        sent(browser.execute(t"return 1")).prim.let(_.body.or(t""))
      . assert(_ == t"""{"script":"return 1","args":[]}""")

      test(m"the page source is read, not posted"):
        sent(browser.source().pipe(_ => ())).prim.let { e => (e.method, e.path) }
      . assert(_ == (Http.Get, t"/session/$sessionId/source"))

      test(m"timeouts are sent in the W3C shape, not the JSON Wire pair"):
        sent(browser.timeouts(WebDriverSession.Timeouts(`implicit` = 5000L)))
        . prim.let(_.body.or(t""))
      . assert(_ == t"""{"implicit":5000}""")

      test(m"accepting an alert uses the W3C path"):
        sent(browser.acceptAlert()).prim.let(_.path)
      . assert(_ == t"/session/$sessionId/alert/accept")

      test(m"switching to the top frame sends a null id"):
        sent(browser.topFrame()).prim.let(_.body.or(t""))
      . assert(_ == t"""{"id":null}""")

      test(m"deleting all cookies is a DELETE, not a POST"):
        sent(browser.deleteCookies()).prim.let { e => (e.method, e.path) }
      . assert(_ == (Http.Delete, t"/session/$sessionId/cookie"))

    suite(m"Input actions"):
      def performed(steps: List[WebDriverSession.Action]): Text =
        val fake = driver((_, _) => none)
        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
          browser.perform(WebDriverSession.Action.Source.Key, steps)

        fake.exchanges.stdlib.filter(_.path.ends(t"/actions")).head.body.or(t"")

      test(m"a keypress is one keyboard source with two steps"):
        performed(List(WebDriverSession.Action.KeyDown(t"a"), WebDriverSession.Action.KeyUp(t"a")))
      . assert:
          _ == t"""{"actions":[{"id":"keyboard","type":"key","actions":""" +
              t"""[{"type":"keyDown","value":"a"},{"type":"keyUp","value":"a"}]}]}"""

      test(m"a pause carries its duration"):
        performed(List(WebDriverSession.Action.Pause(250)))
      . assert(_.contains(t"""{"type":"pause","duration":250}"""))

      test(m"releasing actions is a DELETE"):
        val fake = driver((_, _) => none)
        given Http.Backend = fake
        WebDriver(url"http://localhost:4444", t"{}".read[Json]).session: session ?=>
          browser.releaseActions()

        fake.exchanges.stdlib.filter(_.path.ends(t"/actions")).head.method

      . assert(_ == Http.Delete)
