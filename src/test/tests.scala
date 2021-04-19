package scintillate

import rudiments.*
import probably.*

import scala.collection.immutable.ListMap

object Tests extends Suite("Scintillate tests"):
  def run(using Runner): Unit =

    given RequestHandler = SimpleHttpServer(8081)

    val server: HttpServer = Http.listen {

      println(request)

      request match
        case Path("/other")     => Response(Redirect("/somewhere"))
        case Path("/somewhere") => Response("Somewhere")
        case Path("/notfound")  => Response(NotFound("Not-found message"))
        case Path(_)            => Response("This is a response")
    }

    test("Send an HTTP POST request") {
      Http.post("http://localhost:8081/helloworld", "Here's some content").as[String]

    }.assert(_ == "This is a response")

    test("Send an HTTP GET request") {
      String(Http.get("http://localhost:8081/helloworld").asInstanceOf[Array[Byte]])

    }.assert(_ == "This is a response")
    
    // test("Send an HTTP GET request to redirect") {
    //   String(Http.get("http://localhost:8081/other").asInstanceOf[Array[Byte]])
    // }.assert(_ == "Somewhere")
    
    test("Send an HTTP GET request with a parameter") {
      Http.get("http://localhost:8081/other", ListMap("foo" -> "bar")).as[String]
    }.assert(_ == "Somewhere")
    
    test("Check and recover from not found") {
      try Http.get("http://localhost:8081/notfound").as[String]
      catch
        case error@HttpError(HttpStatus.NotFound, _) => error.as[String]
        case other => other.printStackTrace; ???
    }.assert(_ == "Not-found message")
    
    server.shutdown()
