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
package tarantula

import anticipation.*
import contingency.*
import distillate.*
import gesticulate.*
import gossamer.*
import hallucination.*
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

case class WebDriver(server: Navigator#Server):
  private transparent inline def wd: this.type = this

  case class Session(sessionId: Text):
    def webDriver: WebDriver = wd

    private def safe[result](block: => result): result = block
      // try block catch case error: HttpError => error match
      //   case HttpError(status, body) => error.read[Json]
      //     throw WebDriverError(json.error.as[Text], json.message.as[Text],
      //         json.stacktrace.as[Text].cut(t"\n"))

    final private val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: Text):
      private def get(address: Text): Json logs HttpEvent = safe:
        given online: Online = Online

        val url: HttpUrl =
          url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"

        url.fetch(contentType = media"application/json").receive[Json]

      private def post(address: Text, content: Json): Json logs HttpEvent = safe:
        given online: Online = Online

        url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"
        . submit()(content)
        . read[Text]
        . decode[Json]

      def click(): Unit logs HttpEvent = post(t"click", t"{}".read[Json])
      def clear(): Unit logs HttpEvent = post(t"clear", t"{}".read[Json])

      def screenshot(): Raster in Png logs HttpEvent =
        get(t"screenshot").value.as[Text].deserialize[Base64].read[Raster in Png]

      def value(text: Text): Unit logs HttpEvent =
        case class Data(text: Text)
        post(t"value", Data(text).in[Json])

      @targetName("at")
      infix def / [element: Focusable](value: element): List[Element] logs HttpEvent =
        case class Data(`using`: Text, value: Text)

        post(t"elements", Data(element.strategy, element.focus(value)).in[Json])
        . value
        . as[List[Json]]
        . map(_(Wei).as[Text])
        . map(Element(_))

      def element[element: Focusable](value: element): Element logs HttpEvent =
        case class Data(`using`: Text, value: Text)

        val e = post(t"element", Data(element.strategy, element.focus(value)).in[Json])
        Element(e.value.selectDynamic(Wei.s).as[Text])

    private def get(address: Text): Json logs HttpEvent = safe:
      given online: Online = Online

      url"http://localhost:${server.port}/session/$sessionId/$address"
      . fetch(contentType = media"application/json")
      . receive[Json]

    private def post(address: Text, content: Json): Json logs HttpEvent = safe:
      given online: Online = Online
      url"http://localhost:${server.port}/session/$sessionId/$address".submit()(content)
      . read[Text]
      . decode[Json]

    def navigateTo[url: Abstractable across Urls to Text](url: url): Json logs HttpEvent =
      case class Data(url: Text)
      post(t"url", Data(url.generic).in[Json])

    def refresh(): Unit logs HttpEvent = post(t"refresh", t"{}".read[Json]).as[Json]
    def forward(): Unit logs HttpEvent = post(t"forward", t"{}".read[Json]).as[Json]
    def back(): Unit logs HttpEvent = post(t"back", t"{}".read[Json]).as[Json]
    def title(): Text logs HttpEvent = get(t"title").as[Json].value.as[Text]

    def url[url: Instantiable across Urls from Text](): url logs HttpEvent =
      url(get(t"url").url.as[Text])

    @targetName("at")
    infix def / [element: Focusable](value: element): List[Element] logs HttpEvent =

      case class Data(`using`: Text, value: Text)

      post(t"elements", Data(element.strategy, element.focus(value)).in[Json])
      . value
      . as[List[Json]]
      . map(_(Wei).as[Text])
      . map(Element(_))

    def element[element: Focusable](value: element): Element logs HttpEvent =
      case class Data(`using`: Text, value: Text)

      val e = post(t"element", Data(element.strategy, element.focus(value)).in[Json])

      Element(e.value.selectDynamic(Wei.s).as[Text])

    def activeElement(): Element logs HttpEvent =
      Element(get(t"element/active").value.selectDynamic(Wei.s).as[Text])

  def startSession(): Session logs HttpEvent =
    given online: Online = Online

    val url = url"http://localhost:${server.port}/session"
    val json = url.submit()(t"""{"capabilities":{}}""".read[Json]).read[Text].decode[Json]

    Session(json.value.sessionId.as[Text])
