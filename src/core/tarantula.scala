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

trait Browser(name: String):
  transparent inline def browser = this
  
  case class Server(port: Int, value: Process[String]):
    def stop(): Unit = browser.stop(this)

  def launch(port: Int)(using Env): Server
  def stop(server: Server): Unit

  def session[T](port: Int = 4444)(fn: WebDriver#Session ?=> T)(using Env): T =
    val server = launch(port)
    try
      val driver = WebDriver(server)
      fn(using driver.startSession())
    finally server.stop()

object Firefox extends Browser("firefox"):
  def launch(port: Int)(using Env, Log): Server =
    val server: Process[String] = sh"geckodriver --port $port".fork()
    Thread.sleep(100)
    Server(port, server)

  def stop(server: Server): Unit = server.value.abort()

object Chrome extends Browser("chrome"):
  def launch(port: Int)(using Env, Log): Server =
    val server: Process[String] = sh"chromedriver --port=$port".fork()
    Thread.sleep(100)
    Server(port, server)

  def stop(server: Server)(using Log): Unit = server.value.abort()

def browser(using WebDriver#Session): WebDriver#Session = summon[WebDriver#Session]

case class WebDriverError(error: String, message: String, stacktrace: IArray[String])
extends Exception(str"tarantula: $message")

case class WebDriver(server: Browser#Server):
  private transparent inline def wd: this.type = this
 
  case class Session(sessionId: String):
    def webDriver: WebDriver = wd
    
    private def safe[T](fn: => T): T =
      try fn catch case e@HttpError(status, body) =>
        val json = body match
          case scintillate.Body.Chunked(stream) => Json.parse(stream.reduce(_ ++ _).uString).value
          case scintillate.Body.Empty           => throw e
          case scintillate.Body.Data(data)      => Json.parse(data.uString).value
        
        throw WebDriverError(json.error.as[String], json.message.as[String],
            json.stacktrace.as[String].cut("\n"))
            
    final private val Wei: String = "element-6066-11e4-a52e-4f735466cecf"

    case class Element(elementId: String):
      
      private def get(address: String): Json = safe {
        uri"http://localhost:${server.port.toString}/session/$sessionId/element/$elementId/$address"
          .get(RequestHeader.ContentType("application/json")).as[Json]
      }
      
      private def post(address: String, content: Json): Json = safe {
        uri"http://localhost:${server.port.toString}/session/$sessionId/element/$elementId/$address"
          .post(content.toString, RequestHeader.ContentType("application/json")).as[Json]
      }
      
      def click(): Unit = post("click", Json.parse("{}"))

      def clear(): Unit = post("clear", Json.parse("{}")) 

      def value(text: String): Unit =
        case class Data(text: String)
        post("value", Data(text).json)
    
      def /[T](value: T)(using el: ElementLocator[T]): List[Element] =
        case class Data(`using`: String, value: String)
        post("elements", Data(el.strategy, el.value(value)).json)
          .value
          .as[List[Json]]
          .map(_(Wei).as[String])
          .map(Element(_))
      
      def element[T](value: T)(using el: ElementLocator[T]): Element =
        case class Data(`using`: String, value: String)
        val e = post("element", Data(el.strategy, el.value(value)).json)
        Element(e.value.selectDynamic(Wei).as[String])
      
    private def get(address: String): Json = safe {
      uri"http://localhost:${server.port.toString}/session/$sessionId/$address"
        .get(RequestHeader.ContentType("application/json")).as[Json]
    }
  
    private def post(address: String, content: Json): Json = safe {
      uri"http://localhost:${server.port.toString}/session/$sessionId/$address"
        .post(content.toString, RequestHeader.ContentType("application/json")).as[Json]
    }
    
    def navigateTo(url: Uri): Json =
      case class Data(url: String)
      post("url", Data(url.toString).json)
    
    def refresh(): Unit = post("title", Json.parse("{}")).as[Json]
    def forward(): Unit = post("forward", Json.parse("{}")).as[Json]
    def back(): Unit = post("forward", Json.parse("{}")).as[Json]
    def title(): String = get("title").as[Json].value.as[String]

    def url(): String = get("url").url.as[String]

    def /[T](value: T)(using el: ElementLocator[T]): List[Element] =
      case class Data(`using`: String, value: String)
      post("elements", Data(el.strategy, el.value(value)).json)
        .value
        .as[List[Json]]
        .map(_(Wei).as[String])
        .map(Element(_))
    
    def element[T](value: T)(using el: ElementLocator[T]): Element =
      case class Data(`using`: String, value: String)
      val e = post("element", Data(el.strategy, el.value(value)).json)
      Element(e.value.selectDynamic(Wei).as[String])
    
    def activeElement(): Element = Element(get("element/active").value.selectDynamic(Wei).as[String])

  def startSession(): Session =
    val json = uri"http://localhost:${server.port.toString}/session".post("""{"capabilities":{}}""",
        RequestHeader.ContentType("application/json")).as[Json]
    
    Session(json.value.sessionId.as[String])

extension (elems: List[WebDriver#Session#Element])
  def /[T](value: T)(using el: ElementLocator[T]): List[WebDriver#Session#Element] =
    elems.flatMap(_ / value)
    

case class ElementLocator[-T](strategy: String, value: T => String)

object ElementLocator:
  given ElementLocator[String]("link text", identity(_))
  given ElementLocator[Selector]("css selector", _.normalize.value)
  given ElementLocator[Tag[?, ?, ?]]("tag name", _.label)
  given ElementLocator[DomId]("css selector", "#"+_.name)
  given ElementLocator[Cls]("css selector", "."+_.name)