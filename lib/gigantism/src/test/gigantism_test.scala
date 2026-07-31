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
package gigantism

import soundness.*

case class Colour(name: Text)

case class Shape(name: Text)

case class Missing(name: Text)

object Colours:
  given red: Colour = Colour(t"red")
  given green: Colour = Colour(t"green")
  given blue: Colour = Colour(t"blue")

object Shapes:
  given square: Shape = Shape(t"square")

object Tests extends Suite(m"Gigantism Tests"):
  def run(): Unit =
    suite(m"Summoning every instance"):
      test(m"No instances are found when none are in scope"):
        every[Missing].values
      . assert(_ == Nil)

      test(m"A single instance in scope is found"):
        import Shapes.square
        every[Shape].values
      . assert(_ == List(Shape(t"square")))

      test(m"Every mutually-ambiguous instance in scope is found"):
        import Colours.{red, green, blue}
        every[Colour].values.map(_.name).to(Set)
      . assert(_ == Set(t"red", t"green", t"blue"))

      test(m"Only the imported instances are found"):
        import Colours.{red, blue}
        every[Colour].values.map(_.name).to(Set)
      . assert(_ == Set(t"red", t"blue"))

      test(m"Instances are not found outside their import scope"):
        every[Colour].values
      . assert(_ == Nil)

      test(m"A locally-defined instance is found"):
        given local: Shape = Shape(t"triangle")
        every[Shape].values
      . assert(_ == List(Shape(t"triangle")))

      test(m"Local and imported instances are both found"):
        import Shapes.square
        given local: Shape = Shape(t"triangle")
        every[Shape].values.map(_.name).to(Set)
      . assert(_ == Set(t"square", t"triangle"))

      test(m"The default given collects every instance"):
        import Colours.{red, green}
        summon[Every[Colour]].values.map(_.name).to(Set)
      . assert(_ == Set(t"red", t"green"))
