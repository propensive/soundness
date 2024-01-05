/*
    Jacinta, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import anticipation.*
import perforate.*
import probably.*
import rudiments.*
import turbulence.*
import spectacular.*
import hieroglyph.*, charEncoders.utf8

import errorHandlers.throwUnsafely

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
      .assert(_ == t"string")
    
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
      .assert(_ == t""""foo"""")

      test(t"Serialize double"):
        3.14159.json.show
      .assert(_ == t"3.14159")
      
      test(t"Serialize true"):
        true.json.show
      .assert(_ == t"true")
    
      test(t"Serialize false"):
        false.json.show
      .assert(_ == t"false")
    
    suite(t"Misc tests"):
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
    
    suite(t"Generic derivation tests"):
      case class Person(name: Text, age: Int)
      case class Band(guitarists: List[Person], drummer: Person, bassist: Option[Person])

      val paul =
        test(t"Serialize a simple case class"):
          Person(t"Paul", 81).json.show
        .check(_ == t"""{"name":"Paul","age":81}""")
      
      val john = t"""{"name": "John", "age": 40}"""
      val george = t"""{"name": "George", "age": 58}"""
      val ringo = t"""{"name": "Ringo", "age": 82}"""

      val beatles = t"""{"guitarists": [$john, $george], "drummer": $ringo, "bassist": $paul}"""

      val paulObj = test(t"Extract a Person"):
        Json.parse(paul).as[Person]
      .check(_ == Person(t"Paul", 81))

      val ringoObj = test(t"Extract a different person"):
        Json.parse(ringo).as[Person]
      .check(_ == Person(t"Ringo", 82))

      test(t"Extract a band"):
        Json.parse(beatles).as[Band]
      .assert(_ == Band(List(Person(t"John", 40), Person(t"George", 58)), ringoObj, Some(paulObj)))

      enum Player:
        case Guitarist(person: Person)
        case Drummer(person: Person)
        case Bassist(person: Person)
      
      val paulCoproduct = test(t"Serialize a coproduct"):
        val paul: Player = Player.Bassist(paulObj)
        paul.json.show
      .check(_ == t"""{"person":{"name":"Paul","age":81},"_type":"Bassist"}""")
    
      test(t"Decode a coproduct"):
        Json.parse(paulCoproduct).as[Player]
      .assert(_ == Player.Bassist(paulObj))
      
      test(t"Decode a coproduct as a precise subtype"):
        Json.parse(paulCoproduct).as[Player.Bassist]
      .assert(_ == Player.Bassist(paulObj))
    
      case class NewBand(members: Set[Player])
      
      import Player.*
      val newBand = NewBand(Set(Bassist(paulObj), Drummer(ringoObj), Guitarist(Person(t"John", 40)),
          Guitarist(Person(t"George", 58))))
      
      val newBandText = test(t"Serialize NewBand"):
        newBand.json.show
      .check(_ == t"""{"members":[{"person":{"name":"Paul","age":81},"_type":"Bassist"},{"person":{"name":"Ringo","age":82},"_type":"Drummer"},{"person":{"name":"John","age":40},"_type":"Guitarist"},{"person":{"name":"George","age":58},"_type":"Guitarist"}]}""")
      
      test(t"Decode a NewBand"):
        Json.parse(newBandText).as[NewBand]
      .assert(_ == newBand)
