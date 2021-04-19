/*

    Scintillate, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package scintillate

import rudiments.*
import probably.*

import scala.collection.immutable.ListMap

object Tests extends Suite("Scintillate tests"):
  def run(using Runner): Unit =

    given RequestHandler = SimpleHttpServer(8081)
    
    val IntParam = Param[Int]("one")

    val server: HttpServer = Http.listen {
      request match
        case Path("/other")     => Response(Redirect("/somewhere"))
        case Path("/somewhere") => Response("Somewhere")
        case Path("/notfound")  => Response(NotFound("Not-found message"))
        case Path("/withparam") => Response((param(IntParam).getOrElse(100)*2).toString)
        case Path(_)            => Response("This is a response")
    }

    test("Send an HTTP POST request") {
      Http.post("http://localhost:8081/helloworld", "Here's some content").as[String]
    }.assert(_ == "This is a response")

    test("Send an HTTP GET request") {
      Http.get("http://localhost:8081/helloworld").as[String]
    }.assert(_ == "This is a response")
    
    test("Send an HTTP GET request to redirect") {
      Http.get("http://localhost:8081/other").as[String]
    }.assert(_ == "Somewhere")
    
    test("Send an HTTP GET request with a parameter") {
      Http.get("http://localhost:8081/other", ListMap("foo" -> "bar")).as[String]
    }.assert(_ == "Somewhere")
    
    test("Check and recover from not found") {
      try Http.get("http://localhost:8081/notfound").as[String]
      catch
        case error@HttpError(HttpStatus.NotFound, _) => error.as[String]
        case other => other.printStackTrace; ???
    }.assert(_ == "Not-found message")
    
    test("Get a typed parameter") {
      Http.get("http://localhost:8081/withparam", ListMap("one" -> "9")).as[String]
    }.assert(_ == "18")
    
    server.shutdown()
