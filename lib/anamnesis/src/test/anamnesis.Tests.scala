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
┃    Soundness, version 0.38.0.                                                                    ┃
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
package anamnesis

import soundness.*

import strategies.throwUnsafely

case class Cabinet(name: Text)
case class Shelf(name: Text)
case class Box(name: Text)
case class Pencil(name: Text)

val top = Shelf(t"top")
val middle = Shelf(t"middle")
val bottom = Shelf(t"bottom")

val red = Pencil(t"red")
val green = Pencil(t"green")
val blue = Pencil(t"blue")

object Tests extends Suite(m"Anamnesis tests"):
  def run(): Unit =
    given db: Database of (
           Cabinet -< Shelf,
                      Shelf -< Box,
                               Box -< Pencil,
                       Text >- Box) = Database()

    val alpha: Ref of Box in db.type = Box(t"Alpha").store()
    val beta: Ref of Box in db.type = Box(t"Beta").store()

    test(m"Database is initally empty"):
      alpha.lookup[Pencil]

    . assert(_ == Set())

    test(m"Can add an item"):
      red.store()
      alpha.assign(red.ref())

    . assert()

    test(m"Table now contains item"):
      alpha.lookup[Pencil].map(_())

    . assert(_ == Set(red))

    test(m"Can insert multiple items"):
      val greenRef = green.store()
      alpha.assign(greenRef)
      alpha.lookup[Pencil].map(_())

    . assert(_ == Set(red, green))

    test(m"Can delete an item"):
      val redRef = red.store()
      alpha.unassign(redRef)
      alpha.lookup[Pencil].map(_())

    . assert(_ == Set(green))

    test(m"Other values are unaffected"):
      beta.lookup[Pencil]

    . assert(_ == Set())

    test(m"Can't insert a pencil onto a shelf"):
      demilitarize:
        top.store().assign(red.ref())

    . assert(_.nonEmpty)
