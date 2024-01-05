/*
    Scintillate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import digression.*
import probably.*
import eucalyptus.*
import gossamer.*, stdouts.stdout
import turbulence.*
import rudiments.*

import scala.collection.immutable.ListMap

import unsafeExceptions.canThrowAny

given Log(Everything |-> SystemOut)

case class Address(house: Int, street: Text, city: Text, country: Text)
case class Person(name: Text, address: Address)

object Tests extends Suite(t"Scintillate tests"):
  def run(): Unit =

    // val IntParam = RequestParam[Int](t"one")

    // object Path:
    //   def unapply(request: Request): Some[String] = Some(request.path.s)

    // val server: HttpService = HttpServer(8081).listen:
    //   request match
    //     case Path("/other") =>
    //       Log.info(t"Received request to /other")
    //       Response(Redirect(uri"/elsewhere"))
    //     case Path("/elsewhere") =>
    //       Response("Elsewhere")
    //     case Path("/somewhere") & IntParam(value) =>
    //       Response(t"Somewhere: ${value + 1}")
    //     case Path("/notfound") =>
    //       Response(NotFound(t"Not-found message"))
    //     case Path("/withparam") =>
    //       Response(IntParam()*2)
    //     case Path("/address") =>
    //       val address = List(t"house", t"street", t"city", t"country").map(param(_).get).join(t", ")
    //       Response(address)
    //     case Path("/person") =>
    //       val address = List(t"name", t"address.house", t"address.street", t"address.city",
    //           t"address.country").map(param(_).get).join(t", ")
       
    //       Response(address)
    //     case Path(_) =>
    //       Response(t"This is a response")

    // test(t"Send an HTTP POST request"):
    //   Http.post(uri"http://localhost:8081/helloworld", t"Here's some content").as[Text]
    // .check(_ == t"This is a response")

    // test(t"Send an HTTP GET request to redirect"):
    //   uri"http://localhost:8081/other".get().as[Text]
    // .check(_ == t"Elsewhere")
    
    // test(t"Send an HTTP GET request"):
    //   uri"http://localhost:8081/helloworld".get().as[Text]
    // .check(_ == t"This is a response")
    
    // test(t"Send an HTTP GET request with a parameter"):
    //   uri"http://localhost:8081/somewhere".query(one = t"1").get().as[Text]
    // .check(_ == t"Somewhere: 2")
    
    // test(t"Send an HTTP POST request with a parameter"):
    //   uri"http://localhost:8081/somewhere".post(Map(t"one" -> t"1")).as[Text]
    // .check(_ == t"Somewhere: 2")
    
    // test(t"Send a case class instance as a GET request"):
    //   val address = Address(1, t"High Street", t"London", t"UK")
    //   uri"http://localhost:8081/address".query(address).get().as[Text]
    // .check(_ == t"1, High Street, London, UK")

    // test(t"Send a nested class instance as a GET request"):
    //   val address = Address(1, t"High Street", t"London", t"UK")
    //   val person = Person(t"John Smith", address)
    //   uri"http://localhost:8081/person".query(person).get().as[Text]
    // .check(_ == t"John Smith, 1, High Street, London, UK")
    
    // test(t"Send a case class instance as the query to a POST request"):
    //   val address = Address(1, t"High Street", t"London", t"UK")
    //   uri"http://localhost:8081/address".query(address).post(()).as[Text]
    // .check(_ == t"1, High Street, London, UK")

    // test(t"Send a case class instance as the body to a POST request"):
    //   val address = Address(1, t"High Street", t"London", t"UK")
    //   uri"http://localhost:8081/address".post(address).as[Text]
    // .check(_ == t"1, High Street, London, UK")
    
    // test(t"Check and recover from not found"):
    //   try Http.get(uri"http://localhost:8081/notfound").as[Text]
    //   catch
    //     case error: HttpError => error.as[Text]
    //     case other => other.printStackTrace; ???
    // .check(_ == t"Not-found message")
    
    // test(t"Get a typed parameter"):
    //   Http.get(uri"http://localhost:8081/withparam".query(Map(t"one" -> t"9"))).as[Text]
    // .check(_ == t"18")
    
    // server.stop()

    suite(t"Parsing tests"):
      test(t"parse Authority with username and password"):
        Authority.parse(t"username:password@example.com")
      .check(_ == Authority(Host(t"example", t"com"), Some(t"username:password"), None))
      
      test(t"parse Authority with username but not password"):
        Authority.parse(t"username@example.com")
      .check(_ == Authority(Host(t"example", t"com"), Some(t"username"), None))
      
      test(t"parse Authority with username, password and port"):
        Authority.parse(t"username:password@example.com:8080")
      .check(_ == Authority(Host(t"example", t"com"), Some(t"username:password"), Some(8080)))
      
      test(t"parse Authority with username and port"):
        Authority.parse(t"username@example.com:8080")
      .check(_ == Authority(Host(t"example", t"com"), Some(t"username"), Some(8080)))
      
      test(t"parse Authority with username, numerical password and port"):
        Authority.parse(t"username:1234@example.com:8080")
      .check(_ == Authority(Host(t"example", t"com"), Some(t"username:1234"), Some(8080)))

      test(t"Authority with invalid port fails"):
        capture:
          Authority.parse(t"username@example.com:no")
      .check:
        case InvalidUrlError(_, 21, InvalidUrlError.Expectation.Number) => true
        case _ => false
      
      test(t"Parse full URI"):
        Url.parse(t"http://user:pw@example.com:8080/path/to/location?query=1#ref")
      .check(_ == Url(Scheme(t"http"), Some(Authority(Host(t"example", t"com"), Some(t"user:pw"),
          Some(8080))), t"/path/to/location", Some(t"query=1"), Some(t"ref")))
      
      test(t"Parse simple URI"):
        Url.parse(t"https://example.com/foo")
      .check(_ == Url(Scheme(t"https"), Some(Authority(Host(t"example", t"com"))), t"/foo", None,
          None))
      
      test(t"Show simple URI"):
        Url.parse(t"http://example.com/foo").show
      .check(_ == t"http://example.com/foo")
      
