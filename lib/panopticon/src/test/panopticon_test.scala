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
┃    Soundness, version 0.54.0.                                                                    ┃
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

case class Organization(name: String, leader: Person)
case class Person(name: String, age: Int, role: Role)
case class Role(name: String, salary: Int)

object Tests extends Suite(m"internal tests"):
  def run(): Unit =
    case class Company(ceo: Person, name: Text)
    case class Person(name: Text, roles: List[Role])
    case class Role(name: Text, count: Int)

    case class User(name: Text, roles: Map[Text, Role])

    val company = Company(Person("John", List(Role("CEO", 1), Role("CFO", 2), Role("CIO", 3))), "Acme")

    test(m"update company CEO"):
      company.lens(_.ceo = Person("John Doe", List(Role("CTO", 7))))
    . assert(_ == Company(Person("John Doe", List(Role("CTO", 7))), "Acme"))

    test(m"update company CEO name"):
      company.lens(_.ceo.name = "Bill")
    . assert(_ == Company(Person("Bill", List(Role("CEO", 1), Role("CFO", 2), Role("CIO", 3))), "Acme"))

    test(m"update company CEO roles"):
      company.lens(_.ceo.roles = Nil)
    . assert(_ == Company(Person("John", Nil), "Acme"))

    test(m"update company CEO roles and name"):
      company.lens
        ( _.ceo.roles = Nil,
          _.ceo.name = "Bill" )
    . assert(_ == Company(Person("Bill", Nil), "Acme"))

    test(m"update head role"):
      company.lens(_.ceo.roles(Prim) = Role("Changed", 13))
    . assert(_ == Company(Person("John", List(Role("Changed", 13), Role("CFO", 2), Role("CIO", 3))), "Acme"))

    test(m"update head role name"):
      company.lens(_.ceo.roles(Prim).name = "Changed")
    . assert(_ == Company(Person("John", List(Role("Changed", 1), Role("CFO", 2), Role("CIO", 3))), "Acme"))

    test(m"adjust each role names"):
      company.lens(_.ceo.roles(Each).name = prior+"!")
    . assert(_ == Company(Person("John", List(Role("CEO!", 1), Role("CFO!", 2), Role("CIO!", 3))), "Acme"))

    val user = User("John", Map(t"ceo" -> Role("CEO", 1), t"cfo" -> Role("CFO", 2), t"cio" -> Role("CIO", 3)))

    test(m"adjust user role"):
      user.lens(_.roles(t"cfo") = Role("CFO!", 2))
    . assert(_ == User("John", Map(t"ceo" -> Role("CEO", 1), t"cfo" -> Role("CFO!", 2), t"cio" -> Role("CIO", 3))))

    test(m"adjust user role name"):
      user.lens(_.roles(t"cfo").name = "CFO!")
    . assert(_ == User("John", Map(t"ceo" -> Role("CEO", 1), t"cfo" -> Role("CFO!", 2), t"cio" -> Role("CIO", 3))))

    test(m"filter traversal"):
      company.lens(_.ceo.roles(Filter[Role](_.count > 1)) = Role("Changed", 0))
    . assert(_ == Company(Person("John", List(Role("CEO", 1), Role("Changed", 0), Role("Changed", 0))), "Acme"))

    test(m"filter traversal inner"):
      company.lens(_.ceo.roles(Filter[Role](_.count > 1)).count = 0)
    . assert(_ == Company(Person("John", List(Role("CEO", 1), Role("CFO", 0), Role("CIO", 0))), "Acme"))

    import doms.html.whatwg.*

    test(m"adjust an HTML value"):
      val table: Html = Table(Tbody(
        Tr(
          Td("Name"),
          Td("Role"),
          Td("Count")
        ),
        Tr(
          Td("John"),
          Td("CEO"),
          Td("1")
        ),
        Tr(
          Td("John"),
          Td("CFO"),
          Td("2")
        ),
        Tr(
          Td("John"),
          Td("CIO"),
          Td("3")
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
