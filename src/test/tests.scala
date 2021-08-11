/*
    Euphemism, version 0.8.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package euphemism

import probably.*
import scala.util.*

case class Foo(x: Int, y: String) derives CanEqual, Json.Serializer, Json.Deserializer

object Tests extends Suite("Euphemism Tests"):
  def run(using Runner): Unit =
    suite("Parsing tests") {
      test("Parse a number") {
        Json.parse("42").as[Int]
      }.assert(_ == 42)

      test("Parse a string") {
        Json.parse("\"string\"").as[String]
      }.assert(_ == "string")
    
      test("Parse true") {
        Json.parse("true").as[Boolean]
      }.assert(identity)

      test("Parse false") {
        Json.parse("false").as[Boolean]
      }.assert(!_)

      test("Parse float") {
        Json.parse("3.1415").as[Float]
      }.assert(_ == 3.1415f)
      
      test("Parse double") {
        Json.parse("3.1415926").as[Double]
      }.assert(_ == 3.1415926)
    }

    suite("Serialization") {
      test("Serialize string") {
        "foo".json.toString
      }.assert(_ == """"foo"""")

      test("Serialize double") {
        3.14159.json.toString
      }.assert(_ == "3.14159")
      
      test("Serialize true") {
        true.json.toString
      }.assert(_ == "true")
    
      test("Serialize false") {
        false.json.toString
      }.assert(_ == "false")
    }
    
    suite("Basic tests") {
      // test("Serialize to Json") {
      //   Foo(1, "two").json
      // }.assert(_ == Json.of(x = 1.json, y = "two".json))

      // test("Parse from JSON") {
      //   Json.parse("""{"x": 1}""").debug
      // }.assert(_ == Json.of(x = 1.json))

      test("Read case class") {
        Json.parse("""{"x": 1, "y": "two"}""").as[Foo]
      }.assert(_ == Foo(1, "two"))

      test("Extract an option") {
        case class OptFoo(x: Option[Int])
        Json.parse("""{"x": 1}""").as[OptFoo].x
      }.assert(_ == Some(1))
      
      test("Extract a None") {
        case class OptFoo(x: Option[Int])
        Json.parse("""{"y": 1}""").as[OptFoo].x
      }.assert(_ == None)
    }