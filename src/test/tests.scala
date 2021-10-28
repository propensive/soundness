/*
    Euphemism, version 0.13.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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

import eucalyptus.*
import gossamer.*
import probably.*
import rudiments.*
import scala.util.*
import org.typelevel.jawn.ast.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

case class Foo(x: Int, y: Txt) derives CanEqual

case class InvalidState(name: String) extends Exception("Not a valid state: "+name)

object Tests extends Suite(str"Euphemism Tests"):
  def run(using Runner): Unit =
    suite(str"Parsing tests") {
      test(str"Parse a number") {
        Json.parse(str"42").as[Int]
      }.assert(_ == 42)

      test(str"Parse a string") {
        val s = Json.parse(str"\"string\"")
        s.as[Txt]
      }.check(_ == str"string")
    
      test(str"Parse true") {
        Json.parse(str"true").as[Boolean]
      }.assert(identity)

      test(str"Parse false") {
        Json.parse(str"false").as[Boolean]
      }.assert(!_)

      test(str"Parse float") {
        Json.parse(str"3.1415").as[Float]
      }.assert(_ == 3.1415f)
      
      test(str"Parse double") {
        Json.parse(str"3.1415926").as[Double]
      }.assert(_ == 3.1415926)
    }

    suite(str"Serialization") {
      test(str"Serialize string") {
        str"foo".json.show
      }.check(_ == str""""foo"""")

      test(str"Serialize double") {
        3.14159.json.show
      }.check(_ == str"3.14159")
      
      test(str"Serialize true") {
        true.json.show
      }.check(_ == str"true")
    
      test(str"Serialize false") {
        false.json.show
      }.check(_ == str"false")
    }
    
    suite(str"Basic tests") {
      test(str"Serialize to Json") {
        Foo(1, str"two").json
      }.assert(_ == Json.of(x = 1.json, y = str"two".json))

      test(str"Parse from JSON") {
        Json.parse(str"""{"x": 1}""")
      }.assert(_ == Json.of(x = 1.json))

      test(str"Read case class") {

        Json.parse(str"""{"x": 1, "y": "two"}""").as[Foo]
      }.assert(_ == Foo(1, str"two"))

      test(str"Extract an option") {
        case class OptFoo(x: Option[Int])
        Json.parse(str"""{"x": 1}""").as[OptFoo].x
      }.assert(_ == Some(1))
      
      test(str"Extract a None") {
        case class OptFoo(x: Option[Int])
        Json.parse(str"""{"y": 1}""").as[OptFoo].x
      }.assert(_ == None)
    }
