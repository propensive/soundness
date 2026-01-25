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
┃    Soundness, version 0.53.0.                                                                    ┃
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
import autopsies.contrastExpectations

import jsonDiscriminables.discriminatedUnionByKind

case class Foo(x: Int, y: Text) derives CanEqual

case class InvalidState(name: String) extends Exception("Not a valid state: "+name)

object Tests extends Suite(m"Jacinta Tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      test(m"Parse a number"):
        t"42".read[Json].as[Int]
      .assert(_ == 42)

      test(m"Parse a string"):
        val s = t"\"string\"".read[Json]
        s.as[Text]
      .assert(_ == t"string")

      test(m"Parse true"):
        t"true".read[Json].as[Boolean]
      .assert(identity)

      test(m"Parse false"):
        t"false".read[Json].as[Boolean]
      .assert(!_)

      test(m"Parse float"):
        t"3.1415".read[Json].as[Float]
      .assert(_ == 3.1415f)

      test(m"Parse double"):
        t"3.1415926".read[Json].as[Double]
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
        t"""{"x": 1}""".read[Json]
      .assert(_ == Json.of(x = 1.json))

      test(m"Read case class"):
        t"""{"x": 1, "y": "two"}""".read[Json].as[Foo]
      .assert(_ == Foo(1, t"two"))

      test(m"Extract an absent Option"):
        case class OptFoo(x: Option[Int])
        t"""{"y": 1}""".read[Json].as[OptFoo].x
      .assert(_ == None)

      test(m"Extract an option"):
        case class OptFoo(x: Option[Int])
        t"""{"x": 1}""".read[Json].as[OptFoo].x
      .assert(_ == Some(1))

      test(m"Extract a present Optional"):
        case class OptionalFoo(x: Optional[Int])
        t"""{"x": 1}""".read[Json].as[OptionalFoo].x
      .assert(_ == 1)

      test(m"Extract an absent Optional"):
        case class OptionalFoo(x: Optional[Int])
        t"""{"y": 1}""".read[Json].as[OptionalFoo].x
      .assert(_ == Unset)

      test(m"Extract a None"):
        case class OptFoo(x: Option[Int])
        t"""{"y": 1}""".read[Json].as[OptFoo].x
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
        paul.read[Json].as[Person]
      .check(_ == Person(t"Paul", 81))

      val ringoObj = test(m"Extract a different person"):
        ringo.read[Json].as[Person]
      .check(_ == Person(t"Ringo", 82))

      test(m"Extract a band"):
        beatles.read[Json].as[Band]
      .assert(_ == Band(List(Person(t"John", 40), Person(t"George", 58)), ringoObj, Some(paulObj)))

      test(m"Extract a band directly"):
        beatles.read[Band over Json]
      .assert(_ == Band(List(Person(t"John", 40), Person(t"George", 58)), ringoObj, Some(paulObj)))

      enum Player:
        case Guitarist(person: Person)
        case Drummer(person: Person)
        case Bassist(person: Person)

      val paulCoproduct = test(m"Serialize a coproduct"):
        val paul: Player = Player.Bassist(paulObj)
        paul.json.show
      .check(_ == t"""{"person":{"name":"Paul","age":81},"kind":"Bassist"}""")

      test(m"Decode a coproduct"):
        summon[Int is Decodable in Json]
        paulCoproduct.read[Json].as[Player]
      .assert(_ == Player.Bassist(paulObj))

      test(m"Decode a coproduct as a precise subtype"):
        paulCoproduct.read[Json].as[Player.Bassist]
      .assert(_ == Player.Bassist(paulObj))

      case class NewBand(members: Set[Player])

      import Player.*
      val newBand = NewBand(Set(Bassist(paulObj), Drummer(ringoObj), Guitarist(Person(t"John", 40)),
          Guitarist(Person(t"George", 58))))

      val newBandText = test(m"Serialize NewBand"):
        newBand.json.show
      .check(_ == t"""{"members":[{"person":{"name":"Paul","age":81},"kind":"Bassist"},{"person":{"name":"Ringo","age":82},"kind":"Drummer"},{"person":{"name":"John","age":40},"kind":"Guitarist"},{"person":{"name":"George","age":58},"kind":"Guitarist"}]}""")

      test(m"Decode a NewBand"):
        newBandText.read[Json].as[NewBand]
      .assert(_ == newBand)

      test(m"Update a JSON object dynamically"):
        import dynamicJsonAccess.enabled
        val john = t"""{"name": "John", "age": 40}""".decode[Json]
        val john2 = john.age = 41
        john2.as[Person]
      . assert(_ == Person("John", 41))

      test(m"Update a JSON array dynamically"):
        import dynamicJsonAccess.enabled
        val array = t"""[1, 2, 3]""".decode[Json]
        val array2 = array(1) = 5
        array2.as[List[Int]]
      . assert(_ == List(1, 5, 3))

      case class Role(name: String)
      case class Entity(name: String, age: Int, roles: List[Role])
      case class Org(name: String, leader: Entity)

      val org = Org("The Beatles", Entity("John", 40, List(Role("Leader")))).json

      test(m"Lens update on JSON"):
        import dynamicJsonAccess.enabled
        val org2 = org.lens(_.leader.age = 41.json)
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 41, List(Role("Leader")))))

      test(m"Lens update with optic on JSON"):
        import dynamicJsonAccess.enabled
        val org2 = org.lens(_.leader.roles(Prim) = Role("-").json)
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 40, List(Role("-")))))

      test(m"Deeper lens update with optic on JSON"):
        import dynamicJsonAccess.enabled
        val org2 = org.lens(_.leader.roles(Prim).name = "-".json)
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 40, List(Role("-")))))
