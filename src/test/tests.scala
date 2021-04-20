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

    val IntParam = Param[Int]("one")

    val server: HttpService = HttpServer(8081).listen {
      request match
        case Path("/other")     => Response(Redirect(uri"/elsewhere"))
        case Path("/elsewhere") => Response("Elsewhere")
        case Path("/somewhere") & IntParam(value) => Response(s"Somewhere: ${value + 1}")
        case Path("/notfound")  => Response(NotFound("Not-found message"))
        case Path("/withparam") => Response((param(IntParam).getOrElse(100)*2).toString)
        case Path(_)            => Response("This is a response")
    }

    test("Send an HTTP POST request") {
      Http.post(uri"http://localhost:8081/helloworld", "Here's some content").as[String]
    }.assert(_ == "This is a response")

    test("Send an HTTP GET request") {
      uri"http://localhost:8081/helloworld".get().as[String]
    }.assert(_ == "This is a response")
    
    test("Send an HTTP GET request to redirect") {
      uri"http://localhost:8081/other".get().as[String]
    }.assert(_ == "Elsewhere")
    
    test("Send an HTTP GET request with a parameter") {
      uri"http://localhost:8081/somewhere".query(one = "1").get().as[String]
    }.assert(_ == "Somewhere: 2")
    
    test("Send an HTTP POST request with a parameter") {
      uri"http://localhost:8081/somewhere".post(Map("one" -> "1")).as[String]
    }.assert(_ == "Somewhere: 2")
    
    test("Check and recover from not found") {
      try Http.get(uri"http://localhost:8081/notfound").as[String]
      catch
        case error@HttpError(HttpStatus.NotFound, _) => error.as[String]
        case other => other.printStackTrace; ???
    }.assert(_ == "Not-found message")
    
    test("Get a typed parameter") {
      Http.get(uri"http://localhost:8081/withparam".query(Map("one" -> "9"))).as[String]
    }.assert(_ == "18")
    
    server.stop()
