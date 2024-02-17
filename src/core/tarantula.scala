/*
    Tarantula, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import guillotine.*
import gossamer.*
import jacinta.*, jsonPrinters.minimal, dynamicJsonAccess.enabled
import telekinesis.*
import cataclysm.*
import honeycomb.*
import contingency.*
import gastronomy.*
import hallucination.*
import eucalyptus.*
import spectacular.*
import rudiments.*
import fulminate.*
import turbulence.*
import nettlesome.*
import hieroglyph.*, charEncoders.utf8
import gesticulate.*
import parasite.*
import anticipation.*, timeInterfaces.long

import errorHandlers.throwUnsafely

import unsafeExceptions.canThrowAny

trait Browser(name: Text):
  transparent inline def browser = this
  
  case class Server(port: Int, value: Process[Label, Text]):
    def stop()(using Log[Text]): Unit = browser.stop(this)

  def launch(port: Int)(using WorkingDirectory, Log[Text], Monitor): Server
  def stop(server: Server)(using Log[Text]): Unit

  def session[T](port: Int = 4444)(block: (session: WebDriver#Session) ?=> T)(using WorkingDirectory, Log[Text], Monitor): T =
    val server = launch(port)
    try block(using WebDriver(server).startSession()) finally server.stop()

object Firefox extends Browser(t"firefox"):
  def launch(port: Int)(using WorkingDirectory, Log[Text], Monitor): Server =
    val server: Process["geckodriver", Text] = sh"geckodriver --port $port".fork()
    sleep(100L)
    Server(port, server)

  def stop(server: Server)(using Log[Text]): Unit = server.value.abort()

object Chrome extends Browser(t"chrome"):
  def launch(port: Int)(using WorkingDirectory, Log[Text], Monitor): Server =
    val server: Process["chromedriver", Text] = sh"chromedriver --port=$port".fork()
    sleep(100L)
    Server(port, server)

  def stop(server: Server)(using Log[Text]): Unit = server.value.abort()

def browser(using WebDriver#Session): WebDriver#Session = summon[WebDriver#Session]

case class WebDriverError(error: Text, wdMsg: Text, browserStacktrace: List[Text])
extends Error(msg"the action caused the error $error in the browser, with the message: $wdMsg")

case class WebDriver(server: Browser#Server):
  private transparent inline def wd: this.type = this
 
  case class Session(sessionId: Text):
    def webDriver: WebDriver = wd
    
    private def safe[T](block: => T): T =
      try block catch case e: HttpError => e match
        case HttpError(status, body) =>
          val json = body match
            case HttpBody.Chunked(stream) => Json.parse(stream.reduce(_ ++ _).uString).value
            case HttpBody.Empty           => throw e
            case HttpBody.Data(data)      => Json.parse(data.uString).value
          
          throw WebDriverError(json.error.as[Text], json.message.as[Text],
              json.stacktrace.as[Text].cut(t"\n"))
            
    final private val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: Text):
      
      private def get(address: Text)(using Log[Text]): Json = safe:
        given Online = Online
        val url: HttpUrl = url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"

          url.get(RequestHeader.ContentType(media"application/json")).as[Json]
      
      private def post(address: Text, content: Json)(using Log[Text]): Json = safe:
        given Online = Online
        url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"
          .post(content).as[Json]
      
      def click()(using Log[Text]): Unit = post(t"click", Json.parse(t"{}"))
      def clear()(using Log[Text]): Unit = post(t"clear", Json.parse(t"{}"))
      def screenshot()(using Log[Text]): Image[Png] = Png.read(get(t"screenshot").value.as[Text].decode[Base64])

      def value(text: Text)(using Log[Text]): Unit =
        case class Data(text: Text)
        post(t"value", Data(text).json)
    
      @targetName("at")
      infix def / [ElementType](value: ElementType)(using locator: ElementLocator[ElementType])(using Log[Text]): List[Element] =
        case class Data(`using`: Text, value: Text)
        post(t"elements", Data(locator.strategy, locator.value(value)).json)
          .value
          .as[List[Json]]
          .map(_(Wei).as[Text])
          .map(Element(_))
      
      def element[ElementType](value: ElementType)(using locator: ElementLocator[ElementType], log: Log[Text]): Element =
        case class Data(`using`: Text, value: Text)
        val e = post(t"element", Data(locator.strategy, locator.value(value)).json)
        Element(e.value.selectDynamic(Wei.s).as[Text])
      
    private def get(address: Text)(using Log[Text]): Json = safe:
      given Online = Online
      url"http://localhost:${server.port}/session/$sessionId/$address"
        .get(RequestHeader.ContentType(media"application/json")).as[Json]
  
    private def post(address: Text, content: Json)(using Log[Text]): Json = safe:
      given Online = Online
      url"http://localhost:${server.port}/session/$sessionId/$address".post(content).as[Json]
    
    def navigateTo[UrlType: GenericUrl](url: UrlType)(using Log[Text]): Json =
      case class Data(url: Text)
      post(t"url", Data(url.text).json)
    
    def refresh()(using Log[Text]): Unit = post(t"refresh", Json.parse(t"{}")).as[Json]
    def forward()(using Log[Text]): Unit = post(t"forward", Json.parse(t"{}")).as[Json]
    def back()(using Log[Text]): Unit = post(t"back", Json.parse(t"{}")).as[Json]
    def title()(using Log[Text]): Text = get(t"title").as[Json].value.as[Text]
    def url[UrlType: SpecificUrl]()(using Log[Text]): UrlType = SpecificUrl(get(t"url").url.as[Text])

    @targetName("at")
    infix def / [ElementType](value: ElementType)(using locator: ElementLocator[ElementType], log: Log[Text]): List[Element] =
      case class Data(`using`: Text, value: Text)
      post(t"elements", Data(locator.strategy, locator.value(value)).json)
        .value
        .as[List[Json]]
        .map(_(Wei).as[Text])
        .map(Element(_))
    
    def element[ElementType](value: ElementType)(using locator: ElementLocator[ElementType], log: Log[Text]): Element =
      case class Data(`using`: Text, value: Text)
      val e = post(t"element", Data(locator.strategy, locator.value(value)).json)
      
      Element(e.value.selectDynamic(Wei.s).as[Text])
    
    def activeElement()(using Log[Text]): Element =
      Element(get(t"element/active").value.selectDynamic(Wei.s).as[Text])

  def startSession()(using Log[Text]): Session =
    given Online = Online
    val url = url"http://localhost:${server.port}/session"
    val json = url.post(Json.parse(t"""{"capabilities":{}}""")).as[Json]
    
    Session(json.value.sessionId.as[Text])

case class ElementLocator[-T](strategy: Text, value: T => Text)

object ElementLocator:
  given ElementLocator[Text](t"link text", identity(_))
  given ElementLocator[Selector](t"css selector", _.normalize.value)
  given ElementLocator[TagType[?, ?, ?]](t"tag name", _.label)
  given ElementLocator[DomId](t"css selector", v => t"#${v.name}")
  given ElementLocator[CssClass](t"css selector", v => t".${v.name}")

given realm: Realm = realm"tarantula"
