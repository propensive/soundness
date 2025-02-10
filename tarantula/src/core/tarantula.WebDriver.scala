/*
    Tarantula, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package tarantula

import anticipation.*
import contingency.*
import gesticulate.*
import gossamer.*
import hallucination.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, textSanitizers.strict
import jacinta.*, jsonPrinters.minimal, dynamicJsonAccess.enabled
import monotonous.*, alphabets.base64.standard
import nettlesome.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*

import strategies.throwUnsafely

case class WebDriver(server: Browser#Server):
  private transparent inline def wd: this.type = this

  case class Session(sessionId: Text):
    def webDriver: WebDriver = wd

    private def safe[ResultType](block: => ResultType): ResultType = block
      // try block catch case error: HttpError => error match
      //   case HttpError(status, body) => error.read[Json]
      //     throw WebDriverError(json.error.as[Text], json.message.as[Text],
      //         json.stacktrace.as[Text].cut(t"\n"))

    final private val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: Text):

      private def get(address: Text): Json logs HttpEvent = safe:
        given Online = Online

        val url: HttpUrl =
          url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"

        url.fetch(contentType = media"application/json").receive[Json]

      private def post(address: Text, content: Json): Json logs HttpEvent = safe:
        given Online = Online

        url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"
        . submit()(content)
        . read[Text]
        . decode[Json]

      def click(): Unit logs HttpEvent = post(t"click", Json.parse(t"{}"))
      def clear(): Unit logs HttpEvent = post(t"clear", Json.parse(t"{}"))

      def screenshot(): Image in Png logs HttpEvent =
        Png.read(get(t"screenshot").value.as[Text].deserialize[Base64])

      def value(text: Text): Unit logs HttpEvent =
        case class Data(text: Text)
        post(t"value", Data(text).json)

      @targetName("at")
      infix def / [ElementType: Focusable](value: ElementType): List[Element] logs HttpEvent =
        case class Data(`using`: Text, value: Text)

        post(t"elements", Data(ElementType.strategy, ElementType.focus(value)).json)
        . value
        . as[List[Json]]
        . map(_(Wei).as[Text])
        . map(Element(_))

      def element[ElementType: Focusable](value: ElementType): Element logs HttpEvent =
        case class Data(`using`: Text, value: Text)
        val e = post(t"element", Data(ElementType.strategy, ElementType.focus(value)).json)
        Element(e.value.selectDynamic(Wei.s).as[Text])

    private def get(address: Text): Json logs HttpEvent = safe:
      given Online = Online

      url"http://localhost:${server.port}/session/$sessionId/$address"
      . fetch(contentType = media"application/json")
      . receive[Json]

    private def post(address: Text, content: Json): Json logs HttpEvent = safe:
      given Online = Online
      url"http://localhost:${server.port}/session/$sessionId/$address".submit()(content)
      . read[Text]
      . decode[Json]

    def navigateTo[UrlType: Abstractable across Urls into Text](url: UrlType): Json logs HttpEvent =
      case class Data(url: Text)
      post(t"url", Data(url.generic).json)

    def refresh(): Unit logs HttpEvent = post(t"refresh", Json.parse(t"{}")).as[Json]
    def forward(): Unit logs HttpEvent = post(t"forward", Json.parse(t"{}")).as[Json]
    def back(): Unit logs HttpEvent = post(t"back", Json.parse(t"{}")).as[Json]
    def title(): Text logs HttpEvent = get(t"title").as[Json].value.as[Text]
    def url[UrlType: Instantiable across Urls from Text](): UrlType logs HttpEvent =
      UrlType(get(t"url").url.as[Text])

    @targetName("at")
    infix def / [ElementType: Focusable](value: ElementType)
    :     List[Element] logs HttpEvent =

      case class Data(`using`: Text, value: Text)

      post(t"elements", Data(ElementType.strategy, ElementType.focus(value)).json)
      . value
      . as[List[Json]]
      . map(_(Wei).as[Text])
      . map(Element(_))

    def element[ElementType: Focusable](value: ElementType)
    :     Element logs HttpEvent =

      case class Data(`using`: Text, value: Text)
      val e = post(t"element", Data(ElementType.strategy, ElementType.focus(value)).json)

      Element(e.value.selectDynamic(Wei.s).as[Text])

    def activeElement(): Element logs HttpEvent =
      Element(get(t"element/active").value.selectDynamic(Wei.s).as[Text])

  def startSession(): Session logs HttpEvent =
    given Online = Online
    val url = url"http://localhost:${server.port}/session"
    val json = url.submit()(Json.parse(t"""{"capabilities":{}}""")).read[Text].decode[Json]

    Session(json.value.sessionId.as[Text])
