package scintillate

import rudiments.*
import probably.*

object Tests extends Suite("Scintillate tests"):
  def run(using Runner): Unit =

    given RequestHandler = SimpleHttpServer

    val server: HttpServer = HttpServer.listen(8081) { request =>
      println(request)

      Response("This is a response")
    }

    test("Send an HTTP request") {
      String(Http.post("http://localhost:8081/helloworld", "Here's some content", Set()).asInstanceOf[Array[Byte]])

    }.assert(_ == "This is a response")

    server.shutdown()
