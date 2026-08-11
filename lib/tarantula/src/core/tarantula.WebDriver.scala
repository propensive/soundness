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

import anticipation.*
import contingency.*
import distillate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder, charDecoders.utf8Decoder
import textSanitizers.strictSanitizer
import jacinta.*, formatting.compactJsonFormatting, dynamicJsonAccess.enabled
import monotonous.*, alphabets.base64Standard
import prepositional.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*

import httpBackends.virtualMachine
import strategies.throwUnsafely

// A `WebDriver` and its `Session`/`Element` values are pure ID-holders over the automation
// server's port: the live resource is the `Server` capability, confined to the `session`
// block that launches it. (A leaked `Session` after the block is a dangling ID over a
// stopped server, not a live handle.)
case class WebDriver(port: Int):
  private transparent inline def wd: this.type = this

  case class Session(sessionId: Text):
    def webDriver: WebDriver = wd

    private def safe[result](block: => result): result = block
      // try block catch case error: Http.Error => error match
      //   case Http.Error(status, body) => error.read[Json]
      //     throw WebDriverError(json.error.as[Text], json.message.as[Text],
      //         json.stacktrace.as[Text].cut(t"\n"))

    final private val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: Text):
      private def get(address: Text): Json logs Http.Event = safe:
        given online: Online = Online()

        val url: HttpUrl =
          url"http://localhost:${port}/session/$sessionId/element/$elementId/$address"

        url.fetch(contentType = media"application/json").receive[Json]

      private def post(address: Text, content: Json): Json logs Http.Event = safe:
        given online: Online = Online()

        url"http://localhost:${port}/session/$sessionId/element/$elementId/$address"
        . submit()(content)
        . read[Text]
        . as[Json]

      def click(): Unit logs Http.Event = post(t"click", t"{}".read[Json])
      def clear(): Unit logs Http.Event = post(t"clear", t"{}".read[Json])

      // The raw PNG bytes. `screenshot()`, which decodes them into a `Raster in Png`, is an
      // extension method in `tarantula.image`, so that browser automation does not depend on
      // an image-codec library.
      def screenshotData(): Data logs Http.Event =
        get(t"screenshot").value.as[Text].deserialize[Base64]

      def value(text: Text): Unit logs Http.Event =
        case class Data(text: Text)
        post(t"value", Data(text).in[Json])

      @targetName("at")
      infix def / [element: Focusable](value: element): List[Element] logs Http.Event =
        case class Data(`using`: Text, value: Text)

        List.of:
          post(t"elements", Data(element.strategy, element.focus(value)).in[Json])
          . value
          . as[List[Json]]
          . stdlib.map(_(Wei).as[Text])
          . map(Element(_))

      def element[element: Focusable](value: element): Element logs Http.Event =
        case class Data(`using`: Text, value: Text)

        val e = post(t"element", Data(element.strategy, element.focus(value)).in[Json])
        Element(e.value.selectDynamic(Wei.s).as[Text])

    private def get(address: Text): Json logs Http.Event = safe:
      given online: Online = Online()

      url"http://localhost:${port}/session/$sessionId/$address"
      . fetch(contentType = media"application/json")
      . receive[Json]

    private def post(address: Text, content: Json): Json logs Http.Event = safe:
      given online: Online = Online()
      url"http://localhost:${port}/session/$sessionId/$address".submit()(content)
      . read[Text]
      . as[Json]

    def navigateTo[url: Abstractable across Urls to Text](url: url): Json logs Http.Event =
      case class Data(url: Text)
      post(t"url", Data(url.generic).in[Json])

    def refresh(): Unit logs Http.Event = post(t"refresh", t"{}".read[Json]).as[Json]
    def forward(): Unit logs Http.Event = post(t"forward", t"{}".read[Json]).as[Json]
    def back(): Unit logs Http.Event = post(t"back", t"{}".read[Json]).as[Json]
    def title(): Text logs Http.Event = get(t"title").as[Json].value.as[Text]

    def url[url: Instantiable across Urls from Text](): url logs Http.Event =
      url(get(t"url").url.as[Text])

    @targetName("at")
    infix def / [element: Focusable](value: element): List[Element] logs Http.Event =

      case class Data(`using`: Text, value: Text)

      List.of:
        post(t"elements", Data(element.strategy, element.focus(value)).in[Json])
        . value
        . as[List[Json]]
        . stdlib.map(_(Wei).as[Text])
        . map(Element(_))

    def element[element: Focusable](value: element): Element logs Http.Event =
      case class Data(`using`: Text, value: Text)

      val e = post(t"element", Data(element.strategy, element.focus(value)).in[Json])

      Element(e.value.selectDynamic(Wei.s).as[Text])

    def activeElement(): Element logs Http.Event =
      Element(get(t"element/active").value.selectDynamic(Wei.s).as[Text])

  def startSession(): Session logs Http.Event =
    given online: Online = Online()

    val url = url"http://localhost:${port}/session"
    val json = url.submit()(t"""{"capabilities":{}}""".read[Json]).read[Text].as[Json]

    Session(json.value.sessionId.as[Text])
