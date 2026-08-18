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
package austronesian

import soundness.*


case class Person(name: Text, age: Int)
case class Group(persons: List[Person], size: Int)

// Recursion through a collection (#1431).
case class Tree(value: Text, children: List[Tree]) derives CanEqual

enum Color:
  case Red, Green, Blue

object Tests extends Suite(m"Austronesian tests"):
  def run(): Unit =
    test(m"Serialize a case class")(Person("John", 30).pojo)
    . assert(_ === Pojo(scala.Array("John", java.lang.Integer.valueOf(30).nn)))

    test(m"Serialize a list of longs")(List(1L, 99L, 203L).pojo)
    . assert(_ === Pojo(scala.Array[Object](java.lang.Long.valueOf(1L).nn, java.lang.Long.valueOf(99L).nn, java.lang.Long.valueOf(203L).nn)))

    test(m"Serialize a list of case classes")(List(Person("John", 12), Person("Jane", 93)).pojo)
    . assert(_ === Pojo(scala.Array(scala.Array("John", java.lang.Integer.valueOf(12).nn), scala.Array("Jane", java.lang.Integer.valueOf(93).nn))))

    test(m"Serialize a nested case class structure"):
      Group(List(Person("John", 30), Person("Jane", 25)), 2).pojo
    . assert(_ === Pojo(scala.Array(scala.Array(scala.Array("John", 30), scala.Array("Jane", 25)), java.lang.Integer.valueOf(2).nn)))

    val group = Group(List(Person("John", 30), Person("Jane", 25)), 2)

    test(m"Roundtrip a nested case class"):
      unsafely(group.pojo.as[Group])
    . assert(_ == group)

    test(m"Encode an enum"):
      val color: Color = Color.Green
      color.pojo
    . assert(_ === Pojo(scala.Array("Green", scala.Array[Any]())))

    test(m"Roundtrip an enum"):
      val color: Color = Color.Green
      unsafely(color.pojo.as[Color])
    . assert(_ == Color.Green)

    val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))

    test(m"Roundtrip a type recursive through a List"):
      unsafely(tree.pojo.as[Tree])
    . assert(_ == tree)
