                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.41.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package jacinta

import soundness.*

import charEncoders.utf8
import strategies.throwUnsafely
import jsonPrinters.minimal

case class Foo(x: Int, y: Text) derives CanEqual

case class InvalidState(name: String) extends Exception("Not a valid state: "+name)

object Tests extends Suite(m"Jacinta Tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      test(m"Parse a number"):
        Json.parse(t"42").as[Int]
      .assert(_ == 42)

      test(m"Parse a string"):
        val s = Json.parse(t"\"string\"")
        s.as[Text]
      .assert(_ == t"string")

      test(m"Parse true"):
        Json.parse(t"true").as[Boolean]
      .assert(identity)

      test(m"Parse false"):
        Json.parse(t"false").as[Boolean]
      .assert(!_)

      test(m"Parse float"):
        Json.parse(t"3.1415").as[Float]
      .assert(_ == 3.1415f)

      test(m"Parse double"):
        Json.parse(t"3.1415926").as[Double]
      .assert(_ == 3.1415926)

    suite(m"Serialization"):
      test(m"Serialize string"):
        t"foo".json.show
      .assert(_ == t""""foo"""")

      test(m"Serialize double"):
        3.14159.json.show
      .assert(_ == t"3.14159")

      test(m"Serialize true"):
        true.json.show
      .assert(_ == t"true")

      test(m"Serialize false"):
        false.json.show
      .assert(_ == t"false")

      test(m"Serialize case class with Option as None"):
        case class Foo(x: Int, y: Option[Int])
        Foo(1, None).json.show
      .assert(_ == t"""{"x":1}""")

      test(m"Serialize case class with Option as Some"):
        case class Foo(x: Int, y: Option[Int])
        Foo(1, Some(2)).json.show
      .assert(_ == t"""{"x":1,"y":2}""")

      test(m"Serialize case class with Optional as Unset"):
        case class Foo(x: Int, y: Optional[Int])
        Foo(1, Unset).json.show
      .assert(_ == t"""{"x":1}""")

      test(m"Serialize case class with present Optional"):
        case class Foo(x: Int, y: Optional[Int])
        Foo(1, 2).json.show
      .assert(_ == t"""{"x":1,"y":2}""")

    suite(m"Misc tests"):
      test(m"Serialize to Json"):
        Foo(1, t"two").json
      .assert(_ == Json.of(x = 1.json, y = t"two".json))

      test(m"Parse from JSON"):
        Json.parse(t"""{"x": 1}""")
      .assert(_ == Json.of(x = 1.json))

      test(m"Read case class"):
        Json.parse(t"""{"x": 1, "y": "two"}""").as[Foo]
      .assert(_ == Foo(1, t"two"))

      test(m"Extract an absent Option"):
        case class OptFoo(x: Option[Int])
        Json.parse(t"""{"y": 1}""").as[OptFoo].x
      .assert(_ == None)

      test(m"Extract an option"):
        case class OptFoo(x: Option[Int])
        Json.parse(t"""{"x": 1}""").as[OptFoo].x
      .assert(_ == Some(1))

      test(m"Extract a present Optional"):
        case class OptionalFoo(x: Optional[Int])
        Json.parse(t"""{"x": 1}""").as[OptionalFoo].x
      .assert(_ == 1)

      test(m"Extract an absent Optional"):
        case class OptionalFoo(x: Optional[Int])
        Json.parse(t"""{"y": 1}""").as[OptionalFoo].x
      .assert(_ == Unset)

      test(m"Extract a None"):
        case class OptFoo(x: Option[Int])
        Json.parse(t"""{"y": 1}""").as[OptFoo].x
      .assert(_ == None)

    suite(m"Generic derivation tests"):
      case class Person(name: Text, age: Int)
      case class Band(guitarists: List[Person], drummer: Person, bassist: Option[Person])

      val paul =
        test(m"Serialize a simple case class"):
          Person(t"Paul", 81).json.show
        .check(_ == t"""{"name":"Paul","age":81}""")

      val john = t"""{"name": "John", "age": 40}"""
      val george = t"""{"name": "George", "age": 58}"""
      val ringo = t"""{"name": "Ringo", "age": 82}"""

      val beatles = t"""{"guitarists": [$john, $george], "drummer": $ringo, "bassist": $paul}"""

      val paulObj = test(m"Extract a Person"):
        Json.parse(paul).as[Person]
      .check(_ == Person(t"Paul", 81))

      val ringoObj = test(m"Extract a different person"):
        Json.parse(ringo).as[Person]
      .check(_ == Person(t"Ringo", 82))

      test(m"Extract a band"):
        Json.parse(beatles).as[Band]
      .assert(_ == Band(List(Person(t"John", 40), Person(t"George", 58)), ringoObj, Some(paulObj)))

      enum Player:
        case Guitarist(person: Person)
        case Drummer(person: Person)
        case Bassist(person: Person)

      val paulCoproduct = test(m"Serialize a coproduct"):
        val paul: Player = Player.Bassist(paulObj)
        paul.json.show
      .check(_ == t"""{"_type":"Bassist","person":{"name":"Paul","age":81}}""")

      test(m"Decode a coproduct"):
        summon[Int is Decodable in Json]
        Json.parse(paulCoproduct).as[Player]
      .assert(_ == Player.Bassist(paulObj))

      test(m"Decode a coproduct as a precise subtype"):
        Json.parse(paulCoproduct).as[Player.Bassist]
      .assert(_ == Player.Bassist(paulObj))

      case class NewBand(members: Set[Player])

      import Player.*
      val newBand = NewBand(Set(Bassist(paulObj), Drummer(ringoObj), Guitarist(Person(t"John", 40)),
          Guitarist(Person(t"George", 58))))

      val newBandText = test(m"Serialize NewBand"):
        newBand.json.show
      .check(_ == t"""{"members":[{"_type":"Bassist","person":{"name":"Paul","age":81}},{"_type":"Drummer","person":{"name":"Ringo","age":82}},{"_type":"Guitarist","person":{"name":"John","age":40}},{"_type":"Guitarist","person":{"name":"George","age":58}}]}""")

      test(m"Decode a NewBand"):
        Json.parse(newBandText).as[NewBand]
      .assert(_ == newBand)
