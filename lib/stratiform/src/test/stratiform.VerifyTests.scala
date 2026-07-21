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
import Tel.given

// NB: `dynamicTelAccess.enabled` is deliberately *not* imported here — verified
// `Tel of T` navigation must work without it.

case class Worker(name: Text, age: Int) derives CanEqual
case class Office(street: Text, city: Text) derives CanEqual
case class Assignment(worker: Worker, office: Office) derives CanEqual
case class Crew(lead: Text, members: List[Worker]) derives CanEqual
case class TextAge(name: Text, age: Text) derives CanEqual
case class Nicked(name: Text, nick: Optional[Text]) derives CanEqual
case class Config(name: Text, prefs: Map[Text, Int]) derives CanEqual

// A type whose TEL codec routes through its `… in Text` codec (a scalar atom),
// although structurally it would derive a `Struct`. Used to check the fused TEL
// schema is coherent with the codec (a `Scalar`), not the derived structure.
enum Tint derives CanEqual:
  case Pale, Deep

object Tint:
  given encodable: Tint is Encodable in Text = _.toString.tt.lower

object VerifyTests extends Suite(m"Stratiform verify tests"):
  def keywords(struct: Tels.Struct): List[Text] = struct.members.to(List).collect:
    case field: Tels.Field => field.keyword

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

      test(m"A map field round-trips through TEL"):
        Config(t"c", Map(t"a" -> 1, t"b" -> 2)).encode.as[Config]
      . assert(_ == Config(t"c", Map(t"a" -> 1, t"b" -> 2)))

    suite(m"Schema derivation"):
      test(m"A product derives a Struct with kebab-cased field keywords"):
        keywords(Tels.tels[Worker](t"worker").document)
      . assert(_ == List(t"name", t"age"))

      test(m"A required field has Tight polarity"):
        Tels.tels[Worker](t"worker").document.members.to(List).collect:
          case field: Tels.Field if field.keyword == t"name" => field.required
      . assert(_ == List(Tels.Polarity.Tight))

      test(m"An Optional field loosens to Loose polarity"):
        Tels.tels[Nicked](t"nicked").document.members.to(List).collect:
          case field: Tels.Field if field.keyword == t"nick" => field.required
      . assert(_ == List(Tels.Polarity.Loose))

      test(m"A collection field is repeatable, typed as the element struct"):
        Tels.tels[Crew](t"crew").document.members.to(List).collect:
          case field: Tels.Field if field.keyword == t"members" =>
            field.repeatable -> field.fieldType
        . collect:
            case (repeatable, struct: Tels.Struct) => repeatable -> keywords(struct)
      . assert(_ == List(Tels.Polarity.Loose -> List(t"name", t"age")))

      test(m"A map field's type is a Struct of repeatable `entries` of key/value"):
        Tels.tels[Config](t"config").document.members.to(List).collect:
          case field: Tels.Field if field.keyword == t"prefs" => field.fieldType
        . collect:
            case struct: Tels.Struct => struct.members.to(List).collect:
              case field: Tels.Field if field.keyword == t"entries" => field.fieldType
        . flatten.collect:
            case struct: Tels.Struct => keywords(struct)
      . assert(_ == List(List(t"key", t"value")))

    suite(m"Schema coherence (the carried shape tracks the codec)"):
      test(m"A Text-branch encoder's fused TEL schema is a Scalar, matching the codec"):
        telSchematics.encodable[Tint].schema()
      . assert:
          case _: Tels.Scalar => true
          case _              => false

    suite(m"Encodable & Schematic fusion"):
      val worker = Worker(t"Alice", 30)

      test(m"A fused encoder encodes (and round-trips) as Tel"):
        telSchematics.encodable[Worker].encoded(worker).as[Worker]
      . assert(_ == worker)

      test(m"A fused encoder yields a schema"):
        telSchematics.encodable[Worker].schema()
      . assert:
          case _: Tels.Struct => true
          case _              => false

      test(m"A fused decoder decodes from Tel"):
        telSchematics.decodable[Worker].decoded(worker.encode)
      . assert(_ == worker)

      test(m"A fused decoder yields a schema"):
        telSchematics.decodable[Worker].schema()
      . assert:
          case _: Tels.Struct => true
          case _              => false

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
