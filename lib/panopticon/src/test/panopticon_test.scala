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

object Tests extends Suite(m"Panopticon tests"):
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

    case class Address(street: Text, city: Text, postcode: Text)
    case class Employee(name: Text, age: Int, addr: Address, role: Role)
    case class Department(name: Text, lead: Employee, members: List[Employee])
    case class Org(name: Text, hq: Address, depts: List[Department])

    val addr1 = Address("1 Way", "Townville", "AA1")
    val addr2 = Address("2 Way", "Townville", "AA2")
    val addr3 = Address("3 Way", "Cityburg", "BB3")

    val emp1 = Employee("Alice", 30, addr1, Role("CEO", 100))
    val emp2 = Employee("Bob",   40, addr2, Role("CTO", 200))
    val emp3 = Employee("Carol", 35, addr3, Role("CFO", 300))

    val dept1 = Department("Eng", emp1, List(emp1, emp2))
    val dept2 = Department("Fin", emp3, List(emp3))

    val org = Org("Acme", addr1, List(dept1, dept2))

    test(m"3 updates sharing depth-1 prefix"):
      org.lens
        ( _.name = "Beta",
          _.hq   = addr2,
          _.depts = Nil )
    . assert(_ == Org("Beta", addr2, Nil))

    test(m"2 updates sharing depth-3 prefix"):
      org.lens
        ( _.depts(Prim).lead.addr.city     = "Newville",
          _.depts(Prim).lead.addr.postcode = "ZZ9" )
      . depts.head.lead.addr
    . assert(_ == Address("1 Way", "Newville", "ZZ9"))

    test(m"4 updates sharing depth-2 prefix"):
      org.lens
        ( _.depts(Prim).lead.addr.city     = "Newville",
          _.depts(Prim).lead.addr.postcode = "ZZ9",
          _.depts(Prim).lead.role.name     = "Lead",
          _.depts(Prim).lead.role.count    = 999 )
      . depts.head.lead
    . assert: lead =>
        lead.addr == Address("1 Way", "Newville", "ZZ9") && lead.role == Role("Lead", 999)

    test(m"4 updates, mixed shared and disjoint"):
      val r = org.lens
       ( _.depts(Prim).lead.addr.city = "X",
         _.depts(Prim).lead.role.name = "Y",
         _.name                       = "Z",
         _.hq.city                    = "W" )
      ( r.depts.head.lead.addr.city, r.depts.head.lead.role.name, r.name, r.hq.city )
    . assert(_ == ("X", "Y", "Z", "W"))

    test(m"leaf collision: later write wins"):
      org.lens(_.name = "First", _.name = "Second")
    . assert(_.name == "Second")

    test(m"outer write after inner: outer wins"):
      org.lens(_.hq.city = "Inner", _.hq = addr3)
    . assert(_.hq == addr3)

    test(m"inner write after outer: inner refines outer"):
      org.lens(_.hq = addr3, _.hq.city = "Inner")
    . assert(_.hq == Address("3 Way", "Inner", "BB3"))

    test(m"two updates under shared Each traversal"):
      val r = org.lens
       ( _.depts(Each).lead.role.name  = "Boss",
         _.depts(Each).lead.role.count = 0 )
      r.depts.map(_.lead.role)
    . assert(_ == List(Role("Boss", 0), Role("Boss", 0)))

    test(m"two updates under shared Prim traversal"):
      val r = org.lens
       ( _.depts(Prim).name      = "Renamed",
         _.depts(Prim).lead.name = "NewLead" )
      ( r.depts.head.name, r.depts.head.lead.name )
    . assert(_ == ("Renamed", "NewLead"))

    test(m"prior used inside multi-update"):
      val r = org.lens
       ( _.name    = prior+"!",
         _.hq.city = prior+"?" )
      ( r.name, r.hq.city )
    . assert(_ == ("Acme!", "Townville?"))

    // ─── singleton-typed traversal fusion ─────────────────────────────────

    test(m"two updates sharing a Each traversal"):
      // both leaves under `_.depts(Each).lead.role.…` — must rebuild the depts list
      // once and each Department/Employee/Role once.
      val r = org.lens
       ( _.depts(Each).lead.role.name  = "Boss",
         _.depts(Each).lead.role.count = 0 )
      r.depts.map(_.lead.role)
    . assert(_ == List(Role("Boss", 0), Role("Boss", 0)))

    test(m"different singleton traversals do not fuse, apply sequentially"):
      // First sets ALL depts' lead.role.name to "Boss" (Each).
      // Then sets the FIRST dept's lead.role.count to 0 (Prim).
      // Second update only modifies the first dept; second dept keeps Boss + count=300
      // (it had Carol's CFO role with count=300).
      val r = org.lens
       ( _.depts(Each).lead.role.name  = "Boss",
         _.depts(Prim).lead.role.count = 0 )
      ( r.depts(0).lead.role, r.depts(1).lead.role )
    . assert(_ == (Role("Boss", 0), Role("Boss", 300)))

    test(m"Filter[T] traversal forces fallback"):
      // Filter has a non-singleton operand type; the macro must fall back to the
      // foldLeft path. Result still correct.
      org.lens
       ( _.depts(Prim).members(Filter[Employee](_.age > 35)).age = 99 )
      . depts.head.members.map(_.age)
    . assert(_ == List(30, 99))  // emp1 age 30, emp2 age 40 → only emp2 changed

    test(m"two Each updates after non-fusable, still correct"):
      // Mix a Filter (non-fusable) with two Each updates. The whole call falls back
      // to foldLeft; we verify the result is still right.
      val r = org.lens
       ( _.depts(Prim).members(Filter[Employee](_.age > 35)).age = 99,
         _.depts(Each).lead.role.name  = "Boss",
         _.depts(Each).lead.role.count = 0 )
      ( r.depts(0).lead.role, r.depts(0).members.map(_.age) )
    . assert(_ == (Role("Boss", 0), List(30, 99)))

    test(m"three updates sharing a Prim traversal"):
      // All under `_.depts(Prim).lead.…` — Department + Employee should rebuild once.
      val r = org.lens
       ( _.depts(Prim).lead.name     = "Renamed",
         _.depts(Prim).lead.age      = 99,
         _.depts(Prim).lead.role.name = "Boss" )
      ( r.depts.head.lead.name, r.depts.head.lead.age, r.depts.head.lead.role.name )
    . assert(_ == ("Renamed", 99, "Boss"))

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
