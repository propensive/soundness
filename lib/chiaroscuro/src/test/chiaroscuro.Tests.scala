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
┃    Soundness, version 0.34.0.                                                                    ┃
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
package chiaroscuro

import soundness.*

case class Person(name: Text, age: Int)
case class Organization(name: Text, ceo: Person, staff: List[Person])

case class IdName(id: Text, name: Text)

import Decomposition.*

object Tests extends Suite(m"Chiaroscuro tests"):
  def run(): Unit =
    suite(m"Decomposition tests"):
      // test(m"Decompose an unknown type"):
      //   24.decompose

      // . assert(_ == Decomposition.Primitive(t"Int", t"24", 24))

      // test(m"Decompose a known type"):
      //   t"hello".decompose

      // . assert(_ == Decomposition.Primitive(t"Text", t"hello", t"hello"))


      // test(m"Decompose a person"):
      //   Person(t"Bill", 29).decompose

      // . assert(_ == Decomposition.Product
      //                (t"Person",
      //                 Map(t"name" -> Primitive(t"Text", t"Bill", t"Bill"),
      //                     t"age"  -> Primitive(t"Int", t"29", 29)),
      //                 Person(t"Bill", 29)))

      // test(m"Decompose a sequence"):
      //   List('a', 'b').decompose

      // . assert:
      //     _ == Sequence(List(Primitive(t"Char", t"a", 'a'),
      //                        Primitive(t"Char", t"b", 'b')), List('a', 'b'))


      Decomposable.derived[Optional[Int]]

      // test(m"Decompose an optional value"):
      //   val x: Optional[Int] = 12
      //   x.decompose

      // . assert(_ == Sum(t"Optional", Primitive(t"Int", t"12", 12), 12))

      // test(m"Decompose an unset optional"):
      //   val x: Optional[Int] = Unset
      //   x.decompose

      // . assert(_ == Sum(t"Optional", Primitive(t"Unset", t"∅", Unset), Unset))
