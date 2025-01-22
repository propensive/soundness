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

import anticipation.*, durationApi.javaLong
import cataclysm.*
import contingency.*
import eucalyptus.*
import fulminate.*
import gastronomy.*
import gesticulate.*
import gossamer.*
import guillotine.*
import hallucination.*
import hieroglyph.*, charEncoders.utf8
import honeycomb.*
import jacinta.*, jsonPrinters.minimal, dynamicJsonAccess.enabled
import monotonous.*
import nettlesome.*
import parasite.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*

import strategies.throwUnsafely

import unsafeExceptions.canThrowAny

case class WebDriver(server: Browser#Server):
  private transparent inline def wd: this.type = this

  case class Session(sessionId: Text):
    def webDriver: WebDriver = wd

    private def safe[ResultType](block: => ResultType): ResultType =
      try block catch case e: HttpError => e match
        case HttpError(status, body) =>
          val json = body match
            case HttpBody.Chunked(stream) => Json.parse(stream.reduce(_ ++ _).utf8).value
            case HttpBody.Empty           => throw e
            case HttpBody.Data(data)      => Json.parse(data.utf8).value

          throw WebDriverError(json.error.as[Text], json.message.as[Text],
              json.stacktrace.as[Text].cut(t"\n"))

    final private val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: Text):

      private def get(address: Text): Json logs Text = safe:
        given Online = Online

        val url: HttpUrl =
          url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"

        url.get(RequestHeader.ContentType(media"application/json")).as[Json]

      private def post(address: Text, content: Json): Json logs Text = safe:
        given Online = Online

        url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"
        . submit(content).as[Json]

      def click(): Unit logs Text = post(t"click", Json.parse(t"{}"))
      def clear(): Unit logs Text = post(t"clear", Json.parse(t"{}"))
      def screenshot(): Png logs Text = Png.read(get(t"screenshot").value.as[Text].decode[Base64])

      def value(text: Text): Unit logs Text =
        case class Data(text: Text)
        post(t"value", Data(text).json)

      @targetName("at")
      infix def / [ElementType: Focusable](value: ElementType): List[Element] logs Text =
        case class Data(`using`: Text, value: Text)

        post(t"elements", Data(ElementType.strategy, ElementType.value(value)).json)
        . value
        . as[List[Json]]
        . map(_(Wei).as[Text])
        . map(Element(_))

      def element[ElementType: Focusable](value: ElementType): Element logs Text =
        case class Data(`using`: Text, value: Text)
        val e = post(t"element", Data(ElementType.strategy, ElementType.value(value)).json)
        Element(e.value.selectDynamic(Wei.s).as[Text])

    private def get(address: Text): Json logs Text = safe:
      given Online = Online

      url"http://localhost:${server.port}/session/$sessionId/$address"
      . get(RequestHeader.ContentType(media"application/json")).as[Json]

    private def post(address: Text, content: Json): Json logs Text = safe:
      given Online = Online
      url"http://localhost:${server.port}/session/$sessionId/$address".post(content).as[Json]

    def navigateTo[UrlType: GenericUrl](url: UrlType): Json logs Text =
      case class Data(url: Text)
      post(t"url", Data(url.text).json)

    def refresh(): Unit logs Text = post(t"refresh", Json.parse(t"{}")).as[Json]
    def forward(): Unit logs Text = post(t"forward", Json.parse(t"{}")).as[Json]
    def back(): Unit logs Text = post(t"back", Json.parse(t"{}")).as[Json]
    def title(): Text logs Text = get(t"title").as[Json].value.as[Text]
    def url[UrlType: SpecificUrl](): UrlType logs Text = SpecificUrl(get(t"url").url.as[Text])

    @targetName("at")
    infix def / [ElementType: Focusable](value: ElementType)
            : List[Element] logs Text =

      case class Data(`using`: Text, value: Text)

      post(t"elements", Data(ElementType.strategy, ElementType.value(value)).json)
      . value
      . as[List[Json]]
      . map(_(Wei).as[Text])
      . map(Element(_))

    def element[ElementType: Focusable](value: ElementType)
            : Element logs Text =

      case class Data(`using`: Text, value: Text)
      val e = post(t"element", Data(ElementType.strategy, ElementType.value(value)).json)

      Element(e.value.selectDynamic(Wei.s).as[Text])

    def activeElement(): Element logs Text =
      Element(get(t"element/active").value.selectDynamic(Wei.s).as[Text])

  def startSession(): Session logs Text =
    given Online = Online
    val url = url"http://localhost:${server.port}/session"
    val json = url.post(Json.parse(t"""{"capabilities":{}}""")).as[Json]

    Session(json.value.sessionId.as[Text])
