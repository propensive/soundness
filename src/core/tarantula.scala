/*
    Tarantula, version 0.1.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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
import euphemism.*
import scintillate.*
import cataract.*
import honeycomb.*
import eucalyptus.*
import rudiments.*

import unsafeExceptions.canThrowAny

trait Browser(name: Txt):
  transparent inline def browser = this
  
  case class Server(port: Int, value: Process[Txt]):
    def stop()(using Log): Unit = browser.stop(this)

  def launch(port: Int)(using Env, Log): Server
  def stop(server: Server)(using Log): Unit

  def session[T](port: Int = 4444)(fn: WebDriver#Session ?=> T)(using Env, Log): T =
    val server = launch(port)
    try
      val driver = WebDriver(server)
      fn(using driver.startSession())
    finally server.stop()

object Firefox extends Browser(str"firefox"):
  def launch(port: Int)(using Env, Log): Server =
    val server: Process[Txt] = sh"geckodriver --port $port".fork()
    Thread.sleep(100)
    Server(port, server)

  def stop(server: Server)(using Log): Unit = server.value.abort()

object Chrome extends Browser(str"chrome"):
  def launch(port: Int)(using Env, Log): Server =
    val server: Process[Txt] = sh"chromedriver --port=$port".fork()
    Thread.sleep(100)
    Server(port, server)

  def stop(server: Server)(using Log): Unit = server.value.abort()

def browser(using WebDriver#Session): WebDriver#Session = summon[WebDriver#Session]

case class WebDriverError(error: Txt, message: Txt, stacktrace: List[Txt])
extends Exception(s"tarantula: $message")

case class WebDriver(server: Browser#Server):
  private transparent inline def wd: this.type = this
 
  case class Session(sessionId: Txt):
    def webDriver: WebDriver = wd
    
    private def safe[T](fn: => T): T =
      try fn catch case e@HttpError(status, body) =>
        val json = body match
          case scintillate.Body.Chunked(stream) => Json.parse(stream.reduce(_ ++ _).uString).value
          case scintillate.Body.Empty           => throw e
          case scintillate.Body.Data(data)      => Json.parse(data.uString).value
        
        throw WebDriverError(json.error.as[Txt], json.message.as[Txt],
            json.stacktrace.as[Txt].cut(str"\n"))
            
    final private val Wei: Txt = str"element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: Txt):
      
      private def get(address: Txt)(using Log): Json = safe {
        uri"http://localhost:${server.port.show}/session/$sessionId/element/$elementId/$address"
          .get(RequestHeader.ContentType(str"application/json")).as[Json]
      }
      
      private def post(address: Txt, content: Json)(using Log): Json = safe {
        uri"http://localhost:${server.port.show}/session/$sessionId/element/$elementId/$address"
          .post(content.show, RequestHeader.ContentType(str"application/json")).as[Json]
      }
      
      def click()(using Log): Unit = post(str"click", Json.parse(str"{}"))

      def clear()(using Log): Unit = post(str"clear", Json.parse(str"{}")) 

      def value(text: Txt)(using Log): Unit =
        case class Data(text: Txt)
        post(str"value", Data(text).json)
    
      def /[T](value: T)(using el: ElementLocator[T])(using Log): List[Element] =
        case class Data(`using`: Txt, value: Txt)
        post(str"elements", Data(el.strategy, el.value(value)).json)
          .value
          .as[List[Json]]
          .map(_(Wei).as[Txt])
          .map(Element(_))
      
      def element[T](value: T)(using el: ElementLocator[T], log: Log): Element =
        case class Data(`using`: Txt, value: Txt)
        val e = post(str"element", Data(el.strategy, el.value(value)).json)
        Element(e.value.selectDynamic(Wei.s).as[Txt])
      
    private def get(address: Txt)(using Log): Json = safe {
      uri"http://localhost:${server.port.show}/session/$sessionId/$address"
        .get(RequestHeader.ContentType(str"application/json")).as[Json]
    }
  
    private def post(address: Txt, content: Json)(using Log): Json = safe {
      uri"http://localhost:${server.port.show}/session/$sessionId/$address"
        .post(content.show, RequestHeader.ContentType(str"application/json")).as[Json]
    }
    
    def navigateTo(url: Uri)(using Log): Json =
      case class Data(url: Txt)
      post(str"url", Data(Txt(url.toString)).json)
    
    def refresh()(using Log): Unit = post(str"title", Json.parse(str"{}")).as[Json]
    def forward()(using Log): Unit = post(str"forward", Json.parse(str"{}")).as[Json]
    def back()(using Log): Unit = post(str"forward", Json.parse(str"{}")).as[Json]
    def title()(using Log): Txt = get(str"title").as[Json].value.as[Txt]

    def url()(using Log): Txt = get(str"url").url.as[Txt]

    def /[T](value: T)(using el: ElementLocator[T], log: Log): List[Element] =
      case class Data(`using`: Txt, value: Txt)
      post(str"elements", Data(el.strategy, el.value(value)).json)
        .value
        .as[List[Json]]
        .map(_(Wei).as[Txt])
        .map(Element(_))
    
    def element[T](value: T)(using el: ElementLocator[T], log: Log): Element =
      case class Data(`using`: Txt, value: Txt)
      val e = post(str"element", Data(el.strategy, el.value(value)).json)
      Element(e.value.selectDynamic(Wei.s).as[Txt])
    
    def activeElement()(using Log): Element =
      Element(get(str"element/active").value.selectDynamic(Wei.s).as[Txt])

  def startSession()(using Log): Session =
    val json = uri"http://localhost:${server.port.show}/session".post(str"""{"capabilities":{}}""",
        RequestHeader.ContentType(str"application/json")).as[Json]
    
    Session(json.value.sessionId.as[Txt])

extension (elems: List[WebDriver#Session#Element])
  def /[T](value: T)(using el: ElementLocator[T])(using Log): List[WebDriver#Session#Element] =
    elems.flatMap(_ / value)
    

case class ElementLocator[-T](strategy: Txt, value: T => Txt)

object ElementLocator:
  given ElementLocator[Txt](str"link text", identity(_))
  given ElementLocator[Selector](str"css selector", _.normalize.value)
  given ElementLocator[Tag[?, ?, ?]](str"tag name", _.label)
  given ElementLocator[DomId](str"css selector", v => str"#${v.name}")
  given ElementLocator[Cls](str"css selector", v => str".${v.name}")

given realm: Realm(str"tarantula")