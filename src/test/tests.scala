/*
    Scintillate, version 0.16.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package scintillate

import rudiments.*
import probably.*
import eucalyptus.*
import gossamer.*
import rudiments.*

import scala.collection.immutable.ListMap

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

case class Address(house: Int, street: Txt, city: Txt, country: Txt)
case class Person(name: Txt, address: Address)

object Tests extends Suite(str"Scintillate tests"):
  def run(using Runner): Unit =

    val IntParam = RequestParam[Int](str"one")

    object Path:
      def unapply(request: Request): Some[String] = Some(request.path.s)

    val server: HttpService = HttpServer(8081).listen {
      request match
        case Path("/other") =>
          Log.info(str"Received request to /other")
          Response(Redirect(uri"/elsewhere"))
        case Path("/elsewhere") =>
          Response("Elsewhere")
        case Path("/somewhere") & IntParam(value) =>
          Response(str"Somewhere: ${value + 1}")
        case Path("/notfound") =>
          Response(NotFound(str"Not-found message"))
        case Path("/withparam") =>
          Response(Txt((IntParam()*2).toString))
        case Path("/address") =>
          val address = List(str"house", str"street", str"city", str"country").map(param(_).get).join(str", ")
          Response(address)
        case Path("/person") =>
          val address = List(str"name", str"address.house", str"address.street", str"address.city",
              str"address.country").map(param(_).get).join(str", ")
       
          Response(address)
        case Path(_) =>
          Response(str"This is a response")
    }

    test(str"Send an HTTP POST request") {
      Http.post(uri"http://localhost:8081/helloworld", str"Here's some content").as[Txt]
    }.check(_ == str"This is a response")

    test(str"Send an HTTP GET request to redirect") {
      uri"http://localhost:8081/other".get().as[Txt]
    }.check(_ == str"Elsewhere")
    
    test(str"Send an HTTP GET request") {
      uri"http://localhost:8081/helloworld".get().as[Txt]
    }.check(_ == str"This is a response")
    
    test(str"Send an HTTP GET request with a parameter") {
      uri"http://localhost:8081/somewhere".query(one = "1").get().as[Txt]
    }.check(_ == str"Somewhere: 2")
    
    test(str"Send an HTTP POST request with a parameter") {
      uri"http://localhost:8081/somewhere".post(Map(str"one" -> str"1")).as[Txt]
    }.check(_ == str"Somewhere: 2")
    
    test(str"Send a case class instance as a GET request") {
      val address = Address(1, str"High Street", str"London", str"UK")
      uri"http://localhost:8081/address".query(address).get().as[Txt]
    }.check(_ == str"1, High Street, London, UK")

    test(str"Send a nested class instance as a GET request") {
      val address = Address(1, str"High Street", str"London", str"UK")
      val person = Person(str"John Smith", address)
      uri"http://localhost:8081/person".query(person).get().as[Txt]
    }.check(_ == str"John Smith, 1, High Street, London, UK")
    
    test(str"Send a case class instance as the query to a POST request") {
      val address = Address(1, str"High Street", str"London", str"UK")
      uri"http://localhost:8081/address".query(address).post(()).as[Txt]
    }.check(_ == str"1, High Street, London, UK")

    test(str"Send a case class instance as the body to a POST request") {
      val address = Address(1, str"High Street", str"London", str"UK")
      uri"http://localhost:8081/address".post(address).as[Txt]
    }.check(_ == str"1, High Street, London, UK")
    
    test(str"Check and recover from not found") {
      try Http.get(uri"http://localhost:8081/notfound").as[Txt]
      catch
        case error@HttpError(HttpStatus.NotFound, _) => error.as[Txt]
        case other => other.printStackTrace; ???
    }.check(_ == str"Not-found message")
    
    test(str"Get a typed parameter") {
      Http.get(uri"http://localhost:8081/withparam".query(Map(str"one" -> str"9"))).as[Txt]
    }.check(_ == str"18")
    
    server.stop()
