/*
    Jacinta, version 0.4.0. Copyright 2019-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package jacinta

import eucalyptus.*
import gossamer.*
import probably.*
import rudiments.*
import turbulence.*, characterEncodings.utf8, badEncodingHandlers.strict
import scala.util.*

import unsafeExceptions.canThrowAny

import logging.stdout
import jsonPrinters.minimal

case class Foo(x: Int, y: Text) derives CanEqual

case class InvalidState(name: String) extends Exception("Not a valid state: "+name)

object Tests extends Suite(t"Jacinta Tests"):
  def run(): Unit =
    suite(t"Parsing tests"):
      test(t"Parse a number"):
        Json.parse(t"42").as[Int]
      .assert(_ == 42)

      test(t"Parse a string"):
        val s = Json.parse(t"\"string\"")
        s.as[Text]
      .check(_ == t"string")
    
      test(t"Parse true"):
        Json.parse(t"true").as[Boolean]
      .assert(identity)

      test(t"Parse false"):
        Json.parse(t"false").as[Boolean]
      .assert(!_)

      test(t"Parse float"):
        Json.parse(t"3.1415").as[Float]
      .assert(_ == 3.1415f)
      
      test(t"Parse double"):
        Json.parse(t"3.1415926").as[Double]
      .assert(_ == 3.1415926)

    suite(t"Serialization"):
      test(t"Serialize string"):
        t"foo".json.show
      .check(_ == t""""foo"""")

      test(t"Serialize double"):
        3.14159.json.show
      .check(_ == t"3.14159")
      
      test(t"Serialize true"):
        true.json.show
      .check(_ == t"true")
    
      test(t"Serialize false"):
        false.json.show
      .check(_ == t"false")
    
    suite(t"Basic tests"):
      test(t"Serialize to Json"):
        Foo(1, t"two").json
      .assert(_ == Json.of(x = 1.json, y = t"two".json))

      test(t"Parse from JSON"):
        Json.parse(t"""{"x": 1}""")
      .assert(_ == Json.of(x = 1.json))

      test(t"Read case class"):

        Json.parse(t"""{"x": 1, "y": "two"}""").as[Foo]
      .assert(_ == Foo(1, t"two"))

      test(t"Extract an option"):
        case class OptFoo(x: Option[Int])
        Json.parse(t"""{"x": 1}""").as[OptFoo].x
      .assert(_ == Some(1))
      
      test(t"Extract a None"):
        case class OptFoo(x: Option[Int])
        Json.parse(t"""{"y": 1}""").as[OptFoo].x
      .assert(_ == None)
