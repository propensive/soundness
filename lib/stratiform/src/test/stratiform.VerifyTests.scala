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
package stratiform

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import larceny.*
import prepositional.*
import probably.*
import rudiments.*
import vacuous.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces
import Tel.given

// NB: `dynamicTelAccess.enabled` is deliberately *not* imported here — verified
// `Tel of T` navigation must work without it.

case class Worker(name: Text, age: Int) derives CanEqual
case class Office(street: Text, city: Text) derives CanEqual
case class Assignment(worker: Worker, office: Office) derives CanEqual
case class Crew(lead: Text, members: List[Worker]) derives CanEqual
case class TextAge(name: Text, age: Text) derives CanEqual

object VerifyTests extends Suite(m"Stratiform verify tests"):
  def run(): Unit =
    suite(m"Runtime verification"):
      test(m"A conformant value verifies and still decodes"):
        Worker(t"Alice", 30).encode.verify[Worker].as[Worker]
      . assert(_ == Worker(t"Alice", 30))

      test(m"A conformant value verifies successfully"):
        safely(Worker(t"Bob", 4).encode.verify[Worker]).present
      . assert(_ == true)

      test(m"A wrong-typed field fails to verify"):
        safely(TextAge(t"Bob", t"old").encode.verify[Worker]).absent
      . assert(_ == true)

    suite(m"Typed navigation (no enabler import)"):
      test(m"Access a verified field"):
        Worker(t"Alice", 30).encode.verify[Worker].name.as[Text]
      . assert(_ == t"Alice")

      test(m"Access a verified Int field"):
        Worker(t"Alice", 30).encode.verify[Worker].age.as[Int]
      . assert(_ == 30)

      test(m"Access a nested verified field"):
        Assignment(Worker(t"Bob", 2), Office(t"Main", t"Town"))
          .encode.verify[Assignment].office.city.as[Text]
      . assert(_ == t"Town")

      test(m"Index into a verified collection field"):
        Crew(t"Z", List(Worker(t"A", 1), Worker(t"B", 2)))
          .encode.verify[Crew].members(1).name.as[Text]
      . assert(_ == t"B")

    suite(m"Collection round-trip"):
      test(m"A list field round-trips through TEL"):
        Crew(t"Z", List(Worker(t"A", 1), Worker(t"B", 2))).encode.as[Crew]
      . assert(_ == Crew(t"Z", List(Worker(t"A", 1), Worker(t"B", 2))))

    suite(m"Compile-time schema checks"):
      test(m"An unknown field is rejected"):
        demilitarize:
          Worker(t"Alice", 30).encode.verify[Worker].nope
        . head.message
      . assert(_.contains("has no field"))

      test(m"Indexing a non-collection field is rejected"):
        demilitarize:
          Worker(t"Alice", 30).encode.verify[Worker].name(0)
        . head.message
      . assert(_.contains("not an indexable collection"))

      test(m"Field access on a scalar position is rejected"):
        demilitarize:
          Worker(t"Alice", 30).encode.verify[Worker].name.deeper
        . head.message
      . assert(_.contains("has no field"))

      test(m"Plain (unverified) field access requires the enabler"):
        demilitarize:
          Worker(t"Alice", 30).encode.name
        . head.message
      . assert(_.contains("dynamicTelAccess.enabled"))
