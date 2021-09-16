package tarantula

import guillotine.*
import gossamer.*
import euphemism.*
import scintillate.*

trait Browser(name: String):
  transparent inline def browser = this
  type S
  
  case class Server(value: S):
    def stop(): Unit = browser.stop(this)

  def launch(port: Int)(using Env): Server
  def stop(server: Server): Unit

  def session[T](port: Int = 4444)(fn: WebDriver ?=> T): T =
    val server = launch(port)
    val driver = WebDriver(port)
    val result = fn(driver)
    server.stop()
    result


object Firefox extends Browser("firefox"):
  type S = LazyList[String]
  def launch(port: Int)(using Env): Server =
    val server: LazyList[String] = sh"geckodriver --port $port".exec[LazyList[String]]()
    Server(server)

  def stop(server: Server): Unit = ()

object Chrome extends Browser("chrome"):
  type S = LazyList[String]
  def launch(port: Int)(using Env): Server =
    val server: LazyList[String] = sh"chromedriver --port=$port".exec[LazyList[String]]()
    Server(server)

  def stop(server: Server): Unit = ()

case class WebDriver(port: Int):
  case class Session(id: String)
  def startSession(): Session =
    url"http://localhost:$port/"