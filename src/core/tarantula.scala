/*
    Tarantula, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

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
import jacinta.*, jsonPrinters.minimal
import telekinesis.*
import ambience.*
import cataclysm.*
import honeycomb.*
import eucalyptus.*
import rudiments.*
import turbulence.*, characterEncodings.utf8
import deviation.*
import gesticulate.*
import parasitism.*
import anticipation.*, timeApi.long

import unsafeExceptions.canThrowAny
import annotation.targetName

trait Browser(name: Text):
  transparent inline def browser = this
  
  case class Server(port: Int, value: Process[Text]):
    def stop()(using Log): Unit = browser.stop(this)

  def launch(port: Int)(using Environment, Log, Monitor): Server
  def stop(server: Server)(using Log): Unit

  def session[T](port: Int = 4444)(fn: WebDriver#Session ?=> T)(using Environment, Log, Monitor): T =
    val server = launch(port)
    try fn(using WebDriver(server).startSession()) finally server.stop()

object Firefox extends Browser(t"firefox"):
  def launch(port: Int)(using Environment, Log, Monitor): Server =
    val server: Process[Text] = sh"geckodriver --port $port".fork()
    sleep(100L)
    Server(port, server)

  def stop(server: Server)(using Log): Unit = server.value.abort()

object Chrome extends Browser(t"chrome"):
  def launch(port: Int)(using Environment, Log, Monitor): Server =
    val server: Process[Text] = sh"chromedriver --port=$port".fork()
    sleep(100L)
    Server(port, server)

  def stop(server: Server)(using Log): Unit = server.value.abort()

def browser(using WebDriver#Session): WebDriver#Session = summon[WebDriver#Session]

case class WebDriverError(error: Text, wdMsg: Text, browserStacktrace: List[Text])
extends Error(err"the action caused the error $error in the browser, with the message: $wdMsg")

case class WebDriver(server: Browser#Server):
  private transparent inline def wd: this.type = this
 
  case class Session(sessionId: Text):
    def webDriver: WebDriver = wd
    
    private def safe[T](fn: => T): T =
      try fn catch case e: HttpError => e match
        case HttpError(status, body) =>
          val json = body match
            case HttpBody.Chunked(stream) => Json.parse(stream.reduce(_ ++ _).uString).value
            case HttpBody.Empty           => throw e
            case HttpBody.Data(data)      => Json.parse(data.uString).value
          
          throw WebDriverError(json.error.as[Text], json.message.as[Text],
              json.stacktrace.as[Text].cut(t"\n"))
            
    final private val Wei: Text = t"element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: Text):
      
      private def get(address: Text)(using Log): Json = safe:
        internet:
          url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"
            .get(RequestHeader.ContentType(media"application/json")).as[Json]
      
      private def post(address: Text, content: Json)(using Log): Json = safe:
        internet:
          url"http://localhost:${server.port}/session/$sessionId/element/$elementId/$address"
            .post(content).as[Json]
      
      def click()(using Log): Unit = post(t"click", Json.parse(t"{}"))
      def clear()(using Log): Unit = post(t"clear", Json.parse(t"{}")) 

      def value(text: Text)(using Log): Unit =
        case class Data(text: Text)
        post(t"value", Data(text).json)
    
      @targetName("at")
      def /[T](value: T)(using el: ElementLocator[T])(using Log): List[Element] =
        case class Data(`using`: Text, value: Text)
        post(t"elements", Data(el.strategy, el.value(value)).json)
          .value
          .as[List[Json]]
          .map(_(Wei).as[Text])
          .map(Element(_))
      
      def element[T](value: T)(using el: ElementLocator[T], log: Log): Element =
        case class Data(`using`: Text, value: Text)
        val e = post(t"element", Data(el.strategy, el.value(value)).json)
        Element(e.value.selectDynamic(Wei.s).as[Text])
      
    private def get(address: Text)(using Log): Json = safe:
      internet:
        url"http://localhost:${server.port}/session/$sessionId/$address"
          .get(RequestHeader.ContentType(t"application/json")).as[Json]
  
    private def post(address: Text, content: Json)(using Log): Json = safe:
      internet:
        url"http://localhost:${server.port}/session/$sessionId/$address".post(content).as[Json]
    
    def navigateTo[U: GenericUrl](url: U)(using Log): Json =
      case class Data(url: Text)
      post(t"url", Data(readUrl(url).show).json)
    
    def refresh()(using Log): Unit = post(t"refresh", Json.parse(t"{}")).as[Json]
    def forward()(using Log): Unit = post(t"forward", Json.parse(t"{}")).as[Json]
    def back()(using Log): Unit = post(t"back", Json.parse(t"{}")).as[Json]
    def title()(using Log): Text = get(t"title").as[Json].value.as[Text]
    def url()(using Log): Text = get(t"url").url.as[Text]

    @targetName("at")
    def /[T](value: T)(using el: ElementLocator[T], log: Log): List[Element] =
      case class Data(`using`: Text, value: Text)
      post(t"elements", Data(el.strategy, el.value(value)).json)
        .value
        .as[List[Json]]
        .map(_(Wei).as[Text])
        .map(Element(_))
    
    def element[T](value: T)(using el: ElementLocator[T], log: Log): Element =
      case class Data(`using`: Text, value: Text)
      val e = post(t"element", Data(el.strategy, el.value(value)).json)
      
      Element(e.value.selectDynamic(Wei.s).as[Text])
    
    def activeElement()(using Log): Element =
      Element(get(t"element/active").value.selectDynamic(Wei.s).as[Text])

  def startSession()(using Log): Session = internet:
    val url = url"http://localhost:${server.port}/session"
    val json = url.post(Json.parse(t"""{"capabilities":{}}""")).as[Json]
    
    Session(json.value.sessionId.as[Text])

extension (elems: List[WebDriver#Session#Element])
  @targetName("at")
  def /[T](value: T)(using el: ElementLocator[T])(using Log): List[WebDriver#Session#Element] =
    elems.flatMap(_ / value)
    
case class ElementLocator[-T](strategy: Text, value: T => Text)

object ElementLocator:
  given ElementLocator[Text](t"link text", identity(_))
  given ElementLocator[Selector](t"css selector", _.normalize.value)
  given ElementLocator[TagType[?, ?, ?]](t"tag name", _.label)
  given ElementLocator[DomId](t"css selector", v => t"#${v.name}")
  given ElementLocator[Cls](t"css selector", v => t".${v.name}")

given realm: Realm(t"tarantula")
