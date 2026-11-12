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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package panopticon

import soundness.*
import denominative.dysasymptotics.linearAccess // the suite indexes small fixture lists

case class Play(title: String, hero: Person)
case class Person(name: String, age: Int, speech: Speech)
case class Speech(opening: String, lines: Int)

object Tests extends Suite("optics", m"Panopticon tests"):
  def run(): Unit =
    case class Play(troupe: Troupe, title: Text)
    case class Troupe(name: Text, players: List[Person])
    case class Person(name: Text, age: Int)

    case class Cast(title: Text, parts: Map[Text, Person])

    val flute = Person("Flute", 20)
    val quince = Person("Quince", 45)
    val bottom = Person("Bottom", 40)

    val play = Play(Troupe("Mechanicals", List(flute, quince, bottom)), "Pyramus")

    test(m"update play troupe"):
      play.lens(_.troupe = Troupe("Players", List(Person("Snug", 30))))
    . assert(_ == Play(Troupe("Players", List(Person("Snug", 30))), "Pyramus"))

    test(m"update play troupe name"):
      play.lens(_.troupe.name = "Players")
    . assert(_ == Play(Troupe("Players", List(flute, quince, bottom)), "Pyramus"))

    test(m"update play troupe players"):
      play.lens(_.troupe.players = List.empty[Person])
    . assert(_ == Play(Troupe("Mechanicals", Nil), "Pyramus"))

    test(m"update play troupe players and name"):
      play.lens
        ( _.troupe.players = List.empty[Person],
          _.troupe.name = "Players" )
    . assert(_ == Play(Troupe("Players", Nil), "Pyramus"))

    test(m"update head player"):
      play.lens(_.troupe.players(Prim) = Person("Snout", 35))
    . assert(_ == Play(Troupe("Mechanicals", List(Person("Snout", 35), quince, bottom)), "Pyramus"))

    test(m"update head player name"):
      play.lens(_.troupe.players(Prim).name = "Snout")
    . assert(_ == Play(Troupe("Mechanicals", List(Person("Snout", 20), quince, bottom)), "Pyramus"))

    test(m"adjust each player names"):
      play.lens(_.troupe.players(Each).name = prior+"!")
    . assert: result =>
        val players = List(Person("Flute!", 20), Person("Quince!", 45), Person("Bottom!", 40))
        result == Play(Troupe("Mechanicals", players), "Pyramus")

    val cast = Cast("Pyramus", Map(t"thisbe" -> flute, t"prologue" -> quince, t"pyramus" -> bottom))

    test(m"adjust cast part"):
      cast.lens(_.parts(t"prologue") = Person("Quince!", 45))
    . assert: result =>
        val parts =
          Map(t"thisbe" -> flute, t"prologue" -> Person("Quince!", 45), t"pyramus" -> bottom)

        result == Cast("Pyramus", parts)

    test(m"adjust cast part name"):
      cast.lens(_.parts(t"prologue").name = "Quince!")
    . assert: result =>
        val parts =
          Map(t"thisbe" -> flute, t"prologue" -> Person("Quince!", 45), t"pyramus" -> bottom)

        result == Cast("Pyramus", parts)

    test(m"filter traversal"):
      play.lens(_.troupe.players(Filter[Person](_.age > 30)) = Person("Snug", 0))
    . assert: result =>
        val players = List(flute, Person("Snug", 0), Person("Snug", 0))
        result == Play(Troupe("Mechanicals", players), "Pyramus")

    test(m"filter traversal inner"):
      play.lens(_.troupe.players(Filter[Person](_.age > 30)).age = 0)
    . assert: result =>
        val players = List(flute, Person("Quince", 0), Person("Bottom", 0))
        result == Play(Troupe("Mechanicals", players), "Pyramus")

    case class Address(street: Text, city: Text, postcode: Text)
    case class Noble(name: Text, age: Int, home: Address, kin: Person)
    case class House(name: Text, lord: Noble, members: List[Noble])
    case class City(name: Text, square: Address, houses: List[House])

    val addr1 = Address("1 Way", "Verona", "AA1")
    val addr2 = Address("2 Way", "Verona", "AA2")
    val addr3 = Address("3 Way", "Mantua", "BB3")

    val noble1 = Noble("Capulet",      50, addr1, Person("Juliet", 13))
    val noble2 = Noble("Lady Capulet", 28, addr2, Person("Tybalt", 25))
    val noble3 = Noble("Montague",     45, addr3, Person("Romeo",  16))

    val house1 = House("Capulet", noble1, List(noble1, noble2))
    val house2 = House("Montague", noble3, List(noble3))

    val city = City("Verona", addr1, List(house1, house2))

    test(m"3 updates sharing depth-1 prefix"):
      city.lens
        ( _.name   = "Mantua",
          _.square = addr2,
          _.houses = List.empty[House] )
    . assert(_ == City("Mantua", addr2, Nil))

    test(m"2 updates sharing depth-3 prefix"):
      city.lens
        ( _.houses(Prim).lord.home.city     = "Freetown",
          _.houses(Prim).lord.home.postcode = "ZZ9" )
      . houses.head.let(_.lord.home)
    . assert(_ == Address("1 Way", "Freetown", "ZZ9"))

    test(m"4 updates sharing depth-2 prefix"):
      city.lens
        ( _.houses(Prim).lord.home.city     = "Freetown",
          _.houses(Prim).lord.home.postcode = "ZZ9",
          _.houses(Prim).lord.kin.name      = "Rosaline",
          _.houses(Prim).lord.kin.age       = 999 )
      . houses.head.let(house => (house.lord.home, house.lord.kin))
    . assert(_ == (Address("1 Way", "Freetown", "ZZ9"), Person("Rosaline", 999)))

    test(m"4 updates, mixed shared and disjoint"):
      val r = city.lens
       ( _.houses(Prim).lord.home.city = "X",
         _.houses(Prim).lord.kin.name  = "Y",
         _.name                        = "Z",
         _.square.city                 = "W" )
      r.houses.head.lay((t"", t"", t"", t"")): h =>
        (h.lord.home.city, h.lord.kin.name, r.name, r.square.city)
    . assert(_ == ("X", "Y", "Z", "W"))

    test(m"leaf collision: later write wins"):
      city.lens(_.name = "First", _.name = "Second")
    . assert(_.name == "Second")

    test(m"outer write after inner: outer wins"):
      city.lens(_.square.city = "Inner", _.square = addr3)
    . assert(_.square == addr3)

    test(m"inner write after outer: inner refines outer"):
      city.lens(_.square = addr3, _.square.city = "Inner")
    . assert(_.square == Address("3 Way", "Inner", "BB3"))

    test(m"two updates under shared Each traversal"):
      val r = city.lens
       ( _.houses(Each).lord.kin.name = "Paris",
         _.houses(Each).lord.kin.age  = 0 )
      r.houses.map(_.lord.kin)
    . assert(_ == List(Person("Paris", 0), Person("Paris", 0)))

    test(m"two updates under shared Prim traversal"):
      val r = city.lens
       ( _.houses(Prim).name      = "Renamed",
         _.houses(Prim).lord.name = "NewLord" )
      r.houses.head.lay((t"", t"")) { h => (h.name, h.lord.name) }
    . assert(_ == ("Renamed", "NewLord"))

    test(m"prior used inside multi-update"):
      val r = city.lens
       ( _.name        = prior+"!",
         _.square.city = prior+"?" )
      ( r.name, r.square.city )
    . assert(_ == ("Verona!", "Verona?"))

    // ─── singleton-typed traversal fusion ─────────────────────────────────

    test(m"two updates sharing a Each traversal"):
      // both leaves under `_.houses(Each).lord.kin.…` — must rebuild the houses list
      // once and each House/Noble/Person once.
      val r = city.lens
       ( _.houses(Each).lord.kin.name = "Paris",
         _.houses(Each).lord.kin.age  = 0 )
      r.houses.map(_.lord.kin)
    . assert(_ == List(Person("Paris", 0), Person("Paris", 0)))

    test(m"different singleton traversals do not fuse, apply sequentially"):
      // First sets ALL houses' lord.kin.name to "Paris" (Each).
      // Then sets the FIRST house's lord.kin.age to 0 (Prim).
      // Second update only modifies the first house; second house keeps Paris + age=16
      // (it had Montague's kin, Romeo, with age=16).
      val r = city.lens
       ( _.houses(Each).lord.kin.name = "Paris",
         _.houses(Prim).lord.kin.age  = 0 )
      r.houses.at(Prim).let { a => r.houses.at(Sec).let { b => (a.lord.kin, b.lord.kin) } }
    . assert(_ == (Person("Paris", 0), Person("Paris", 16)))

    test(m"Filter[T] traversal forces fallback"):
      // Filter has a non-singleton operand type; the macro must fall back to the
      // foldLeft path. Result still correct.
      city.lens
       ( _.houses(Prim).members(Filter[Noble](_.age < 35)).age = 99 )
      . houses.head.let(_.members.map(_.age))
    . assert(_ == List(50, 99))  // noble1 age 50, noble2 age 28 → only noble2 changed

    test(m"two Each updates after non-fusable, still correct"):
      // Mix a Filter (non-fusable) with two Each updates. The whole call falls back
      // to foldLeft; we verify the result is still right.
      val r = city.lens
       ( _.houses(Prim).members(Filter[Noble](_.age < 35)).age = 99,
         _.houses(Each).lord.kin.name = "Paris",
         _.houses(Each).lord.kin.age  = 0 )
      r.houses.at(Prim).let { h => (h.lord.kin, h.members.map(_.age)) }
    . assert(_ == (Person("Paris", 0), List(50, 99)))

    test(m"three updates sharing a Prim traversal"):
      // All under `_.houses(Prim).lord.…` — House + Noble should rebuild once.
      val r = city.lens
       ( _.houses(Prim).lord.name     = "Renamed",
         _.houses(Prim).lord.age      = 99,
         _.houses(Prim).lord.kin.name = "Paris" )
      r.houses.head.lay((t"", 0, t"")) { h => (h.lord.name, h.lord.age, h.lord.kin.name) }
    . assert(_ == ("Renamed", 99, "Paris"))

    case class Bag(items: Sequence[Int])
    test(m"ordinal optic on a Sequence field"):
      Bag(Sequence(1, 2, 3)).lens(_.items(Sec) = 9)
    . assert(_ == Bag(Sequence(1, 9, 3)))

    case class Chorus(voices: List[Text])
    test(m"ordinal optic on a List field"):
      Chorus(List(t"a", t"b", t"c")).lens(_.voices(Prim) = t"z")
    . assert(_ == Chorus(List(t"z", t"b", t"c")))

    test(m"filter-by-key traversal over a Map field"):
      cast.lens(_.parts(Filter[Text](_ == t"prologue")).name = "X")
    . assert: result =>
        val parts = Map(t"thisbe" -> flute, t"prologue" -> Person("X", 45), t"pyramus" -> bottom)
        result == Cast("Pyramus", parts)

    import htmlDoms.whatwg.*

    test(m"adjust an HTML value"):
      val table: Html = Table(Tbody(
        Tr(
          Td("Name"),
          Td("Part"),
          Td("Age")
        ),
        Tr(
          Td("Flute"),
          Td("Thisbe"),
          Td("20")
        ),
        Tr(
          Td("Quince"),
          Td("Prologue"),
          Td("45")
        ),
        Tr(
          Td("Bottom"),
          Td("Pyramus"),
          Td("40")
        )
      ))

      table.lens(_(Tbody)(Tr)(Td) = Td("-"))
    . assert(_ == Table(Tbody(
        Tr(
          Td("-"),
          Td("-"),
          Td("-")
        ),
        Tr(
          Td("-"),
          Td("-"),
          Td("-")
        ),
        Tr(
          Td("-"),
          Td("-"),
          Td("-")
        ),
        Tr(
          Td("-"),
          Td("-"),
          Td("-")
        )
      )))
