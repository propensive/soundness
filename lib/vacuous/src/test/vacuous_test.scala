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
package vacuous

import soundness.*

object Tests extends Suite(m"Vacuous Tests"):
  def run(): Unit =
    suite(m"Vacuous tests"):
      test(m"String is concrete"):
        val x = summon[String is Concrete]
      . assert()

      test(m"Abstract method type is not concrete"):
        demilitarize:
          def abstractType[notConcrete]: Unit =
            summon[notConcrete is Concrete]
        . map(_.message)
      . assert(_ == List(t"vacuous: type notConcrete is abstract"))

      test(m"Unexpanded inlined abstract method type is not concrete"):
        demilitarize:
          inline def abstractType[notConcrete]: Unit =
            summon[notConcrete is Concrete]
        . map(_.message)
      . assert(_ == Nil)

      test(m"Expanded inlined abstract method type is not concrete"):
        demilitarize:
          inline def abstractType[notConcrete]: Unit =
            summon[notConcrete is Concrete]

          val x = abstractType[Int]

        . map(_.message)
      . assert(_ == Nil)

      test(m"Int and String are distinct types"):
        demilitarize:
          val x = compiletime.summonInline[Int is Distinct from String]

        . map(_.message)
      . assert(_ == Nil)

      test(m"Int and (Int | String) are not distinct types"):
        demilitarize:
          val x = compiletime.summonInline[Int is Distinct from (Int | String)]

        . map(_.message)
      . assert(_.nonEmpty)

      test(m"String is not distinct from itself"):
        demilitarize:
          val x = compiletime.summonInline[String is Distinct from String]

        . map(_.message)
      . assert(_.nonEmpty)

      test(m"Abstract type not proven distinct from String"):
        demilitarize:
          def foo[T]: Unit = infer[T is Distinct from String].unit
          foo[String]

        . map(_.message)
      . assert(_.contains(t"vacuous: type T is abstract"))

      test(m"Abstract type is not proven distinct from anything, e.g. Int"):
        demilitarize:
          def foo[T]: Unit = infer[T is Distinct from String].unit
          foo[Int]

        . map(_.message)
      . assert(_.contains(t"vacuous: type T is abstract"))

      test(m"Inline abstract type not distinct from Int"):
        demilitarize:
          inline def foo[T]: Unit =
            val x = infer[T is Distinct from String]

          foo[Int]

        . map(_.message)
      . assert(_ == Nil)

      test(m"Inline abstract type not distinct from String"):
        demilitarize:
          inline def foo[T]: Unit =
            val x = infer[T is Distinct from String]

          foo[String]

        . map(_.message)
      . assert(_.nonEmpty)

      test(m"String is not distinct from String | Int"):
        demilitarize:
          inline def foo[T]: Unit =
            val x = infer[T is Distinct from (String | Int)]

          foo[String]

        . map(_.message)
      . assert(_.nonEmpty)

      test(m"String singleton is not distinct from String"):
        demilitarize:
          inline def foo[T]: Unit =
            val x = infer[T is Distinct from (String | Int)]

          foo[""]

        . map(_.message)
      . assert(_.nonEmpty)


      test(m"String singleton not distinct from another"):
        demilitarize:
          inline def foo[T]: Unit =
            val x = infer[T is Distinct from "foo"]

          foo[""]

        . map(_.message)
      . assert(_ == Nil)
